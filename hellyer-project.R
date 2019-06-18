
# enable packages
library(censusapi)
library(gdata)
library(dplyr)
library(readxl)
library(yelpr)
library(ggmap)


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
  mutate(college = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B01003_001E) %>%
  mutate(youngadult = (B01001_011E + B01001_012E + B01001_035E + B01001_036E) / B01003_001E) %>%
  mutate(white = B03002_003E / B01003_001E) %>%
  mutate(pov = B17001_002E / B01003_001E) %>%
  select(zip_code_tabulation_area, college, youngadult, white, pov)

census_zcta_percs12 <- census_zcta12 %>%
  mutate(college = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B01003_001E) %>%
  mutate(youngadult = (B01001_011E + B01001_012E + B01001_035E + B01001_036E) / B01003_001E) %>%
  mutate(white = B03002_003E / B01003_001E) %>%
  mutate(pov = B17001_002E / B01003_001E) %>%
  select(zip_code_tabulation_area, college, youngadult, white, pov)

head(census_zcta_percs17)
head(census_zcta_percs12)

# deal with NAs?

# import data from FHFA
url <- "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_BDL_ZIP5.xlsx"
destfile <- "fhfa_sheet.xlsx"
curl::curl_download(url, destfile)
fhfa_sheet <- read_excel(destfile, skip = 6)
view(fhfa_sheet)


# FHFA data preparation
fhfa_sheet_test <- fhfa_sheet
colnames(fhfa_sheet_test) <- c("zipcode", "year", "annualchg", "hpi", "hpi90", "hpi00")
fhfa_sheet_test$zipcode <- as.numeric(fhfa_sheet_test$zipcode)
fhfa_sheet_nyc <- subset(fhfa_sheet_test, subset = zipcode %in% zctas)


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
