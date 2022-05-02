library(tidyverse)
library(magrittr)
library(readxl)
library(ggplot2)
library(sf) # for handling shape files

#### get spatial data and define radius ----------------------------------------

# setwd("C:/Users/aathens/Documents/School Stuff/SLCo Library/ACS and Spatial Data/ACS and Spatial Data")

setwd("/Users/alexisathens/Documents/UofU/5 - Spring 2022/Program Evaluation/SLCO Library/ACS and Spatial Data")

### get library data

libs <- st_read("Utah_Public_Libraries/PublicLibraries.shp")

libs %<>% filter(str_detect(LIBRARY, "Salt Lake County"))

libs %<>% # get acronyms / final lib list from library
  mutate(lib_code = case_when(str_detect(LIBRARY, "Bingham") ~ "BCR",
                              str_detect(LIBRARY, "Draper") ~ "DRA",
                              str_detect(LIBRARY, "Herriman") ~ "HER",
                              str_detect(LIBRARY, "Hunter") ~ "HUN",
                              str_detect(LIBRARY, "Magna") ~ "MAG",
                              str_detect(LIBRARY, "Millcreek") ~ "MCC",
                              str_detect(LIBRARY, "Riverton") ~ "RIV",
                              str_detect(LIBRARY, "Sandy") ~ "SAN",
                              str_detect(LIBRARY, "Smith") ~ "SMI",
                              str_detect(LIBRARY, "South Jordan") ~ "SJO",
                              str_detect(LIBRARY, "Taylorsville") ~ "TAY",
                              str_detect(LIBRARY, "Tyler") ~ "TYL",
                              str_detect(LIBRARY, "West Jordan") ~ "WJO",
                              str_detect(LIBRARY, "West Valley") ~ "WVA",
                              str_detect(LIBRARY, "Whitmore") ~ "WHI",
                              TRUE ~ NA_character_))

# libs %>% filter(is.na(lib_code)) %>% select(LIBRARY)
# confirmed that these were under construction, closed, or specialty - remove

libs %<>% filter(!is.na(lib_code))



### get census tracts

tracts <- st_read("tl_2019_49_tract/tl_2019_49_tract.shp")
tracts

# switch to same CRS system
tracts %<>% st_set_crs(4326) %>% st_transform(crs = 4326) # WGS 84 EPSG code

# narrow down to SLCo-ish
slco_tracts <- tracts %>% 
  mutate(lat = as.numeric(str_sub(INTPTLAT, 2, -1))) %>% 
  mutate(long = as.numeric(str_sub(INTPTLON, 2, -1))) %>% 
  filter(lat > 40.4 & lat < 40.8) %>% 
  filter(long > 111.75 & long < 112.2) %>% 
  mutate(tract_no = row_number())

# calculate centroids for census tracts
slco_tracts$centroids <- slco_tracts %>% 
  st_centroid() %>% 
  st_geometry()


## plot everything - tracts, centroids, and library branches
ggplot() +
  geom_sf(data = slco_tracts, size = 1, color = "black") +
  geom_sf(data = slco_tracts$centroids, color = "blue") +
  geom_sf(data = libs, size = 3, color = "red", pch = 18) +
  ggtitle("Boundary Plot") +
  coord_sf()
# Magna library on the LHS, Alta Reading Room RHS


# test appropriate radius distance - don't want too much overlap
meter_conv <- 1609.34 # equals one mile
libs_radii <- st_buffer(libs, dist = 2 * meter_conv)

# iterative testing of buffer dist
ggplot() +
  geom_sf(data = slco_tracts, size = 1, color = "black") +
  geom_sf(data = slco_tracts$centroids, color = "blue") +
  geom_sf(data = libs, size = 3, color = "red", pch = 18) +
  geom_sf(data = libs_radii, size = 1, color = "red", fill = NA) + 
  ggtitle("Boundary Plot") +
  coord_sf()

## make it pretty for report
ggplot() +
  geom_sf(data = slco_tracts, size = 0.5, color = "black") +
  # geom_sf(data = slco_tracts$centroids, color = "blue") +
  geom_sf(data = libs, size = 4, color = "dark red", pch = 18) +
  geom_sf(data = libs_radii, size = 1, color = "dark red", fill = NA) + 
  ggtitle("Census Tracts by Library Branch") +
  coord_sf()

# the 2 mile radius looks best. more overlap in N SLCo, but the spacing looks great in S SLCo.
# rules should apply fine even for outliers (where just one tract will be calculated)


## get tracts within 
in_tracts <- st_intersects(libs_radii$geometry, slco_tracts$centroids)

# plot one library to double check this is working
lib15 <- in_tracts[15][[1]]

ggplot() +
  geom_sf(data = slco_tracts[lib15,], size = 1, color = "black") +
  geom_sf(data = slco_tracts$centroids[lib15], color = "blue") +
  geom_sf(data = libs[15,], size = 3, color = "red", pch = 18) +
  geom_sf(data = libs_radii[15,], size = 1, color = "red", fill = NA) + 
  ggtitle("Boundary Plot") +
  coord_sf()


# get list of tracts in study
study_tracts <- slco_tracts[unique(unlist(in_tracts)),]


# double check
ggplot() +
  geom_sf(data = study_tracts, size = 1, color = "black") +
  geom_sf(data = study_tracts$centroids, color = "blue") +
  geom_sf(data = libs, size = 3, color = "red", pch = 18) +
  geom_sf(data = libs_radii, size = 1, color = "red", fill = NA) + 
  ggtitle("Boundary Plot") +
  coord_sf()




#### gather census data ---------------------------------------------------------

## get average median income
income <- read_csv("ACS2019 - S1901 - Income/S1901_data.csv", skip = 1)

# just keep id and median income fields
income %>% select(id, contains("Median")) %>% names()
# multiple levels of calculating income - choose household (most general)

income %<>% select(id, `Estimate!!Households!!Median income (dollars)`) %>% 
  rename(med_income = `Estimate!!Households!!Median income (dollars)`)

income %<>% 
  mutate(med_income = as.numeric(med_income)) %>% 
  mutate(tract = str_sub(id, -11, -1)) %>%  # get digits that match to tracts
  select(tract, med_income)

# join to tract data
study_tracts %<>% left_join(income, by = c("GEOID" = "tract"))


## % without computer or internet
internet <- read_csv("ACS2019 - S2801 - Internet/S2801_data.csv", skip = 1)

internet %>% select(id, contains("No computer"), contains("Without an Internet")) %>% names()

internet %<>% select(id, no_comp = `Estimate!!Percent!!Total households!!TYPES OF COMPUTER!!No computer`,
                   no_int = `Estimate!!Percent!!Total households!!TYPE OF INTERNET SUBSCRIPTIONS!!Without an Internet subscription`)

internet %<>% 
  mutate(no_comp = as.numeric(no_comp)) %>% 
  mutate(no_int = as.numeric(no_int)) %>% 
  mutate(tract = str_sub(id, -11, -1))

# make "digital divide" calc of average of both
internet %<>% 
  mutate(dig_div = (no_comp + no_int) / 2)

internet %<>% 
  select(tract, no_comp, no_int, dig_div)

# join to tract data
study_tracts %<>% left_join(internet, by = c("GEOID" = "tract"))

summary(study_tracts)

# visualize dem patterns
study_tracts %>% 
  ggplot() +
  geom_sf(aes(fill = no_comp)) +
  scale_fill_gradient2(low = "white", high = "dark red") +
  geom_sf(data = libs, size = 3, color = "black", pch = 18) +
  geom_sf(data = libs_radii, size = 1, color = "black", fill = NA) + 
  ggtitle("% Without Computer by Library Tract") +
  labs(fill = "% No Computer") +
  coord_sf()



#### calculate area demographics ------------------------------------------------

libs %<>% mutate(med_income = NA, no_comp = NA, no_int = NA, dig_div = NA)

for(i in 1:nrow(libs)){
  census <- study_tracts %>% 
    filter(tract_no %in% in_tracts[i][[1]]) %>% 
    summarize(med_income = mean(med_income),
              no_comp = mean(no_comp),
              no_int = mean(no_int),
              dig_div = mean(dig_div)) %>% 
    st_drop_geometry()
  
  libs[i, 14:17] <- census
}

# join onto radii df
libs_radii %<>% 
  bind_cols(libs[, 14:17] %>% st_drop_geometry())

# color plots by dig div
libs_radii %>% 
  ggplot() +
  geom_sf(data = study_tracts, size = 1, color = "black") +
  geom_sf(aes(fill = dig_div, alpha = 0.6)) +
  scale_fill_gradient2(low = "white", high = "blue") +
  geom_sf(data = libs, size = 3, color = "black", pch = 18) +
  geom_sf(data = libs_radii, size = 1, color = "black", fill = NA) + 
  coord_sf()



### export library data
names(libs)

libs %<>% rename(library = LIBRARY, address = ADDRESS, city = CITY) %>% 
  select(library, lib_code, address, city, med_income:dig_div) %>% 
  st_drop_geometry()

libs %<>% 
  mutate(library = str_remove(library, ", Salt Lake County Library"))

cor.test(libs$med_income, libs$dig_div) # median income and digital divide have a pretty strong correlation - maybe just keep the latter

# write_csv(libs, "C:/Users/aathens/Documents/School Stuff/SLCo Library/ACS and Spatial Data/ACS and Spatial Data/Census Demographics by Library.csv")
