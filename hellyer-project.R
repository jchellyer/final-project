
# enable packages
library(censusapi)
library(gdata)
library(dplyr)
library(readxl)

# set up Census API
cs_key <- "1f19c3a8ce87e6dd0f6058fc4cbb8edb8fba5bfb"

# establish list of ZCTAs for NYC
url_zctalist <- "http://faculty.baruch.cuny.edu/geoportal/resources/nyc_geog/nyc_zcta10_to_puma10.xls"
zcta_sheet <- read.xls(url_zctalist)
zctas <- as.double(unique(zcta_sheet$zcta10))

# import desired values from Census API
census_zcta17 <- NULL
census_zcta12 <- NULL

for (f in zctas) {
  zctaget <- paste("zip code tabulation area:", f, sep="")  
  temp <- getCensus(name = "acs/acs5",
                    vintage = 2017,
                    vars = c("B01003_001E", "B15003_022E", "B15003_023E", 
                             "B15003_024E", "B15003_025E", "B01001_011E", 
                             "B01001_012E", "B01001_035E", "B01001_036E", 
                             "B03002_003E"), 
                    region = zctaget, 
                    key = cs_key)
  census_zcta17 <- rbind(census_zcta17, temp)
}

for (f in zctas) {
  zctaget <- paste("zip code tabulation area:", f, sep="")  
  temp <- getCensus(name = "acs/acs5",
                    vintage = 2012,
                    vars = c("B01003_001E", "B15003_022E", "B15003_023E", 
                             "B15003_024E", "B15003_025E", "B01001_011E", 
                             "B01001_012E", "B01001_035E", "B01001_036E", 
                             "B03002_003E"), 
                    region = zctaget, 
                    key = cs_key)
  census_zcta12 <- rbind(census_zcta12, temp)
}

# calculate percentages (using dplyr/tidyverse notation)
census_zcta_percs17 <- census_zcta17 %>%
  mutate(college = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B01003_001E) %>%
  mutate(youngadult = (B01001_011E + B01001_012E + B01001_035E + B01001_036E) / B01003_001E) %>%
  mutate(white = B03002_003E / B01003_001E) %>%
  select(zip_code_tabulation_area, college, youngadult, white)

census_zcta_percs12 <- census_zcta12 %>%
  mutate(college = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B01003_001E) %>%
  mutate(youngadult = (B01001_011E + B01001_012E + B01001_035E + B01001_036E) / B01003_001E) %>%
  mutate(white = B03002_003E / B01003_001E) %>%
  select(zip_code_tabulation_area, college, youngadult, white)

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