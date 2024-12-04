### Biodiversity Database Access Tools ###

# Libraries and Environment Setup
library(rgbif)
library(spocc)      # Interface to many species occurrence data sources
                    # Include rgbif among a number of others
library(tidyverse)
library(sf)
library(mapview)

## GBIF

# In order to access occurrence data, an account is needed. 
# There are instructions here to cache credentials: 
# https://docs.ropensci.org/rgbif/articles/gbif_credentials.html
# I have already done this.

# View credentials
usethis::edit_r_environ()

## Download bowen data using GADM code

# Bowen GADM code is CAN.2.14.4_1

# Queue a download on the GBIF server
gbif_download <- occ_download(pred_in("gadm", "CAN.2.14.4_1"),
                              format = "SIMPLE_CSV")
gbif_download

# Check status with
occ_download_wait(gbif_download)

# Get the download and import in one step
gbif_dat <- occ_download_get(gbif_download) %>% 
  occ_download_import()

# Load downloaded data
gbif_dat <- occ_download_get("0015650-241126133413365") %>% 
  occ_download_import()

gbif_dat <- occ_download_import("0015650-241126133413365")


# Lots of variables. Lets clean it up a bit. 
dat <- gbif_dat %>% 
  select(species, locality, stateProvince, occurrenceStatus,
         lat = decimalLatitude, long = decimalLongitude,
         xy_uncert_m = coordinateUncertaintyInMeters,
         eventDate, collectionCode, issue)

# Check the records
summary(dat)

# Another way to check for NAs
colSums(is.na(dat))

# Have a look at locations

# Do all records have locations? No. 
dat %>% is.nul

# First make an sf object and then use mapView to map
dat_sf <-dat %>% 
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

m <- mapView(dat_sf, cex = "xy_uncert_m")
m


# For cleaning data, see the links at end of this page:
# https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html

# Number of records by kingdom
table(gbif_dat$kingdom)

# number of species
gbif_dat %>% 
  group_by(kingdom) %>% 
  summarize(n_species = n_distinct(verbatimScientificName))  

# Have a look at the record locations
dat %>% 
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mapView(cex = "xy_uncert_m", zcol  = "kingdom")

## Add the BCSEE info.
## Run separate script to load BCSEE data if not in memory

# source(bcsee.R)

dat_join <- dat %>%
  left_join(bcsee, by = "species", relationship = "many-to-many") %>% 
  arrange(`Element Code`)

# Nest the data to summarize by species but keeping the location records
#   which could be used for mapping, if useful.
datn <- nest(dat_join, data = c("gbifID", "locality":"issue")) %>% 
  rowwise() %>% 
  mutate(n_records = nrow(data)) %>% 
  mutate(flag = if_else(`Classification Level` == "Species", "", 
                        "Check subspecies/var/pop")
         ) %>% 
  





# Records in GBIF that are not in BCSEE
# There would be some records not in BCSEE. Could be taxonomy differences.
# Could also be ornamental plants or perhaps not considered an EO.
dat_join %>% 
  filter(is.na(`Scientific Name`))

# Records where there is more than one join, e.g., more than one subspecies.
dat_join %>% 
  



# Have another look at the location now that common names are added.
dat_join %>% 
  unnest("data") %>% 
  select(kingdom, `English Name`, `Scientific Name`, `BC List`, 
         COSEWIC, `SARA Status`, `SARA Schedule`, n_records, long, lat,
         `xy_uncert_m`) %>% 
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mapView(cex = "xy_uncert_m", zcol  = "kingdom")









## Combine with BC Data

library(bcdata)

x <- bcdc_tidy_resources("wildlife")

bcdc_search("BC Species and Ecosystems Explorer")
bcdc_get_record("be4582bd-738a-44ca-800a-6a3ba4b74f9c")

bcsee <- bcdc_get_data("bc-species-and-ecosystems-explorer")
bcdc_tidy_resources('be4582bd-738a-44ca-800a-6a3ba4b74f9c')


# 5: Conservation Data Centre iMap (other)
# ID: 5e22ac2d-227d-4a8d-909d-04be6f7a9638
Name: conservation-data-centre-imap

imap <- bcdc_get_data("5e22ac2d-227d-4a8d-909d-04be6f7a9638")


bcdc_search("Wildlife Species Inventory")



# 1: Wildlife Species Inventory Survey Observations - All (multiple)
# ID: b0a8767d-b78d-40f9-a9ad-5f0418090e39
# Name: wildlife-species-inventory-survey-observations-all
# 2: Wildlife Species Inventory Survey Observations - Non-sensitive
# (multiple, wms, kml)
# ID: 8f45a611-ce07-4e9f-a4b5-27e123972816
# Name: wildlife-species-inventory-survey-observations-non-sensitive

bcdc_tidy_resources("8f45a611-ce07-4e9f-a4b5-27e123972816")


wsi_wms <- bcdc_get_data("8f45a611-ce07-4e9f-a4b5-27e123972816")



bcdc_get_record("d3651b8c-f560-48f7-a34e-26b0afc77d84")


library(leaflet)

wsi_wms <- "https://openmaps.gov.bc.ca/geo/pub/WHSE_WILDLIFE_INVENTORY.SPI_SURVEY_OBS_NONSENS_SP/ows?service=WMS&request=GetCapabilities"

leaflet() %>% 
  addTiles() %>% 
  setView(lng = -123, lat = 50, zoom = 8) %>% 
  addWMSTiles(
    wsi_wms,
    layers = "WSI_Survey_Observations_SO_All_Birds_Nonsensitive"
  )

