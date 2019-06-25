
# enable packages
library(censusapi)
library(gdata)
library(dplyr)
library(readxl)
library(yelpr)
library(ggmap)
library(Hmisc)
library(reshape2)


# set up Census API
cs_key <- ""


# establish list of ZCTAs for NYC
url_zctalist <- "http://faculty.baruch.cuny.edu/geoportal/resources/nyc_geog/nyc_zcta10_to_puma10.xls"
zcta_sheet <- read.xls(url_zctalist)
zctas <- as.double(unique(zcta_sheet$zcta10))

# narrow down to Brooklyn
zcta_sheet$borough <- sub('....', '', zcta_sheet$pumaname)
zcta_sheet$borough <- sub("([A-Za-z]+).*", "\\1", zcta_sheet$borough)
zctas_bk <- zcta_sheet %>%
  filter(grepl('Brooklyn', borough)) %>%
  distinct(zcta10)
zctas_bk <- as.double(zctas_bk$zcta10)
zctas_bk <- zctas_bk[1:37] # delete empty ZIP

# import desired values from Census API
census_zcta17 <- NULL
census_zcta12 <- NULL

for (f in zctas_bk) {
  zctaget <- paste("zip code tabulation area:", f, sep="")  
  temp <- getCensus(name = "acs/acs5",
                    vintage = 2017,
                    vars = c("B01003_001E", "B15003_022E", "B15003_023E", 
                             "B15003_024E", "B15003_025E", "B01001_011E", 
                             "B01001_012E", "B01001_035E", "B01001_036E", 
                             "B03002_003E", "B17001_002E"), 
                    region = zctaget, 
                    key = cs_key)
  census_zcta17 <- rbind(census_zcta17, temp)
}

for (f in zctas_bk) {
  zctaget <- paste("zip code tabulation area:", f, sep="")  
  temp <- getCensus(name = "acs/acs5",
                    vintage = 2012,
                    vars = c("B01003_001E", "B15003_022E", "B15003_023E", 
                             "B15003_024E", "B15003_025E", "B01001_011E", 
                             "B01001_012E", "B01001_035E", "B01001_036E", 
                             "B03002_003E", "B17001_002E"), 
                    region = zctaget, 
                    key = cs_key)
  census_zcta12 <- rbind(census_zcta12, temp)
}


# calculate percentages
census_zcta_percs17 <- census_zcta17 %>%
  mutate(college17 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B01003_001E) %>%
  mutate(youngadult17 = (B01001_011E + B01001_012E + B01001_035E + B01001_036E) / B01003_001E) %>%
  mutate(white17 = B03002_003E / B01003_001E) %>%
  mutate(pov17 = B17001_002E / B01003_001E) %>%
  select(zip_code_tabulation_area, college17, youngadult17, white17, pov17)

census_zcta_percs12 <- census_zcta12 %>%
  mutate(college12 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B01003_001E) %>%
  mutate(youngadult12 = (B01001_011E + B01001_012E + B01001_035E + B01001_036E) / B01003_001E) %>%
  mutate(white12 = B03002_003E / B01003_001E) %>%
  mutate(pov12 = B17001_002E / B01003_001E) %>%
  select(zip_code_tabulation_area, college12, youngadult12, white12, pov12)

head(census_zcta_percs17)
head(census_zcta_percs12)


# import data from FHFA
url <- "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_BDL_ZIP5.xlsx"
destfile <- "fhfa_sheet.xlsx"
curl::curl_download(url, destfile)
fhfa_sheet <- read_excel(destfile, skip = 6)

# FHFA data preparation
colnames(fhfa_sheet) <- c("zipcode", "year", "annualchg", "hpi", "hpi90", "hpi00")
fhfa_sheet$zipcode <- as.numeric(fhfa_sheet$zipcode)
fhfa_sheet_bk <- subset(fhfa_sheet, subset = zipcode %in% zctas_bk)

fhfa_sheet_bk_1317 <- fhfa_sheet_bk %>% filter((year > 2012) & (year < 2018))
fhfa_sheet_bk_1317$annualchg <- as.numeric(fhfa_sheet_bk_1317$annualchg)
fhfa_sheet_bk_1317$hpi <- as.numeric(fhfa_sheet_bk_1317$hpi)

# import data from Yelp API
yelp_key <- ""


# Yelp search methods test, restaurants in radius from central point: 16 results
food_bk_center <- NULL

food_bk_center <- business_search(yelp_key, 
                                  categories="restaurants",
                                  latitude= 40.645970,
                                  longitude= -73.957270,
                                  radius = 11000)
food_bk_center <- food_bk_center$businesses
food_bk_center <- food_bk_center %>%
  mutate(zip = location$zip_code) %>%
  filter(zip %in% zctas_bk) %>%
  mutate(lat = coordinates$latitude) %>%
  mutate(long = coordinates$longitude) %>%
  select(alias, name, review_count, categories, rating, price, zip, lat, long)

food_bk_center <- unique(food_bk_center)

# Yelp search methods test, restaurants by ZIP: 389 results
food_bk_zip <- NULL

for (f in zctas_bk) {
  temp <- business_search(yelp_key, 
                          location = f, 
                          term = "restaurants",
                          radius = 4000) 
  temp <- temp$businesses
  temp <- temp %>%
    mutate(zip = location$zip_code) %>%
    mutate(lat = coordinates$latitude) %>%
    mutate(long = coordinates$longitude) %>%
    filter(zip %in% zctas_bk) %>%
    select(alias, name, review_count, categories, rating, price, zip, lat, long)
  
  food_bk_zip <- rbind(food_bk_zip, temp)
}

food_bk_zip <- unique(food_bk_zip)

# Yelp search methods test, restaurants by 70x70 grid of search points: 4743 results (max)
latsbk_70 <- seq(40.571474, 40.73911, length.out = 70)
longsbk_70 <- seq(-73.855727, -74.04151, length.out = 70)

# saving 70x70 search as function taking file name plus category names from Yelp documentation
# https://www.yelp.com/developers/documentation/v3/all_category_list
# reqs: grid of search points based on city lat-long coordinates, list of local ZIP codes
yelp70x70 <- function(filename, catname) {
  savename <- deparse(substitute(filename))
  filename <- NULL
  
  for (lat in latsbk_70) {
    for (long in longsbk_70) {
      temp <- business_search(yelp_key, 
                              latitude = lat,
                              longitude = long,
                              categories = catname,
                              radius = 380) 
      temp <- temp$businesses 
      if(length(temp) > 0 && is.na(match('price', names(temp)))) {
        temp$price <- NA
      }
      if(length(temp) > 0) {
        temp <- temp %>%
          mutate(zip = location$zip_code) %>%
          mutate(lat = coordinates$latitude) %>%
          mutate(long = coordinates$longitude) %>%
          filter(zip %in% zctas_bk) %>%
          select(alias, name, review_count, categories, rating, price, zip, lat, long)
      }
      filename <- rbind(filename, temp)
    }
  }
  
  filename <- unique(filename)
  
  saveRDS(filename, file = paste0(savename,'.rds'))
}

# Yelp searches for restaurants, coffee shops, bars (70x70 grid):
yelp70x70(food_bk_ll70,"restaurants")
food_bk_ll70 <- readRDS("food_bk_ll70.rds")

yelp70x70(coffee_bk_ll70,"coffee")
coffee_bk_ll70 <- readRDS("coffee_bk_ll70.rds")

yelp70x70(bars_bk_ll70,"bars")
bars_bk_ll70 <- readRDS("bars_bk_ll70.rds")


# map restaurant results from different search methods
qmplot(long, lat, data = food_bk_ll70, maptype = "toner-lite", size = I(0.5),
       alpha = I(0.45))

qmplot(long, lat, data = food_bk_ll10, maptype = "toner-lite", size = I(0.5),
       alpha = I(0.45))

qmplot(long, lat, data = food_bk_zip, maptype = "toner-lite", size = I(0.5),
       alpha = I(0.45))

qmplot(long, lat, data = food_bk_center, maptype = "toner-lite", size = I(0.5),
       alpha = I(0.45))

# map all results color-coded by type of establishment
bars_bk_ll70$cat <- "bars"
food_bk_ll70$cat <- "food"
coffee_bk_ll70$cat <- "coffee"
listings_bk_ll70 <- rbind(bars_bk_ll70, food_bk_ll70, coffee_bk_ll70)
listings_bk_ll70 <- listings_bk_ll70[!duplicated(listings_bk_ll70$alias), ]

qmplot(long, lat, data = listings_bk_ll70, maptype = "toner-lite", size = I(0.5),
       alpha = I(0.45), color = cat)

# sum and average reviews
reviews_by_zip <- aggregate(listings_bk_ll70$review_count, list(listings_bk_ll70$zip), sum)
ratings_by_zip <- aggregate(listings_bk_ll70$rating, list(listings_bk_ll70$zip), mean)

# plot reviews against poverty
colnames(reviews_by_zip) <- c("zip_code_tabulation_area", "reviews")
reviews_pov <- merge(reviews_by_zip, census_zcta_percs17, by="zip_code_tabulation_area")

ggplot(reviews_pov) +
  geom_point(aes(x = pov, y = reviews), size = 2) +
  ylim(0, 80000)

# calculate change from 2012 to 2017 by ZCTA
census_zcta_change <- cbind(census_zcta_percs17, census_zcta_percs12)
census_zcta_change[,6] <- NULL # remove duplicate zcta column
census_zcta_change <- census_zcta_change %>%
  mutate(chg_college = college17 - college12) %>%
  mutate(chg_youngadult = youngadult17 - youngadult12) %>%
  mutate(chg_white = white17 - white12) %>%
  mutate(chg_pov = pov17 - pov12) %>%
  select(zip_code_tabulation_area, chg_college, chg_youngadult, chg_white, chg_pov)

reviews_change <- merge(reviews_by_zip, census_zcta_change, by="zip_code_tabulation_area")

ggplot(reviews_change) +
  geom_point(aes(x = chg_pov, y = reviews), size = 2) +
  ylim(0, 80000)

# break up by price level and type of establishment
reviews_by_cat <- group_by(listings_bk_ll70, zip, cat) %>% 
  dplyr::summarize(sum_cat = sum(review_count)) %>%
  spread(cat, sum_cat) %>%
  rename(zip_code_tabulation_area = zip)

reviews_by_price <- group_by(listings_bk_ll70, zip, price) %>%
  dplyr::summarize(sum_price = sum(review_count)) %>%
  spread(price, sum_price) %>%
  rename(price1 = `$`, price2 = `$$`, price3 = `$$$`, price4 = `$$$$`, priceNA = `<NA>`, zip_code_tabulation_area = zip) %>%
  mutate(price3_4 = price3 + price4)

reviews_change <- merge(reviews_change, reviews_by_cat, by="zip_code_tabulation_area")
reviews_change <- merge(reviews_change, reviews_by_price, by="zip_code_tabulation_area")

ggplot(reviews_change) +
  geom_point(aes(x = chg_pov, y = reviews), size = 2) +
  ylim(0, 80000)

reviews_change_corrs <- rcorr(as.matrix(reviews_change[,c(2:13,15)]))

# FHFA correlations
fhfa_hpi_bk_1317 <- dcast(fhfa_sheet_bk_1317, zipcode ~ year, value.var = "hpi")
fhfa_hpi_bk_1317 <- fhfa_hpi_bk_1317 %>%
  mutate(pctchg1317 = (fhfa_hpi_bk_1317$`2017` - fhfa_hpi_bk_1317$`2013`) / fhfa_hpi_bk_1317$`2013`) %>%
  rename(zip_code_tabulation_area = zipcode, hpi13 = `2013`, hpi14 = `2014`, hpi15 = `2015`, 
         hpi16 = `2016`, hpi17 = `2017`)

reviews_change_fhfa <- merge(reviews_change, fhfa_hpi_bk_1317, by="zip_code_tabulation_area")

ggplot(reviews_change_fhfa, aes(pctchg1317, reviews)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm")

ggplot(reviews_change_fhfa, aes(hpi17, reviews)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm")

# add number of establishments
listings_count <- listings_bk_ll70 %>%
  count(zip,cat) %>%
  spread(cat,n) %>%
  mutate(total = bars + coffee + food) %>%
  rename(zip_code_tabulation_area = zip)

listings_count <- merge(listings_count, census_zcta_change, by="zip_code_tabulation_area")
listings_count <- merge(listings_count, fhfa_hpi_bk_1317, by="zip_code_tabulation_area")

ggplot(listings_count, aes(chg_pov, total)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm")

ggplot(listings_count, aes(pctchg1317, total)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm")

# complete correlation table
allvars_bk <- merge(listings_count, reviews_change, by="zip_code_tabulation_area")
allvars_bk <- allvars_bk %>%
  select(-c(chg_college.y,chg_youngadult.y,chg_white.y,chg_pov.y,hpi13,hpi14,hpi15,hpi16,priceNA)) %>%
  rename(bars_reviews = bars, coffee_reviews = coffee, food_reviews = food, hpichg1317 = pctchg1317)

allcorrs_bk <- rcorr(as.matrix(allvars_bk[2:20]))
corrplot(allcorrs_bk$r, type="upper", order="alphabet", 
         p.mat = allcorrs_bk$P, sig.level = 0.05, insig = "blank")
