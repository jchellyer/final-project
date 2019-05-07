
# set up Census API

# enable packages
library(censusapi)
library(gdata)
library(dplyr)

# set up Census API
cs_key <- "1f19c3a8ce87e6dd0f6058fc4cbb8edb8fba5bfb"

# establish list of ZCTAs for NYC
url_zctalist <- "http://faculty.baruch.cuny.edu/geoportal/resources/nyc_geog/nyc_zcta10_to_puma10.xls"
zcta_sheet <- read.xls(url_zctalist)
zctas <- as.double(unique(zcta_sheet$zcta10))

# pull desired values from Census API
census_zcta <- NULL

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
  census_zcta <- rbind(census_zcta, temp)
}

# calculate percentages (using dplyr/tidyverse notation)
census_zcta_percs <- census_zcta %>%
  mutate(college = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B01003_001E) %>%
  mutate(youngadult = (B01001_011E + B01001_012E + B01001_035E + B01001_036E) / B01003_001E) %>%
  mutate(white = B03002_003E / B01003_001E) %>%
  select(college, youngadult, white)

head(census_zcta_percs)

