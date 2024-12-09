### Biodiversity Database Access Tools ###

# Libraries and Environment Setup
library(rgbif)
library(tidyverse)
library(sf)
library(mapview)

## Skip down if data has already been downloaded.

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

# Check status with the following, or check online account
occ_download_wait(gbif_download)

# Get the download and import in one step
gbif_dat <- occ_download_get(gbif_download) %>% 
  occ_download_import()

write_csv(gbif_dat, "dat_bowen/gbif_bowen.csv")

# Read previously saved data
gbif_dat <- read_csv("dat_bowen/gbif_bowen.csv")

## Some cleaning should probably be done. Do that later. 

# Check the records
summary(dat)

# Another way to check for NAs
colSums(is.na(gbif_dat))

# For cleaning data, see the links at end of this page:
# https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html

# Number of records by kingdom
table(gbif_dat$kingdom)

# number of species
gbif_dat %>% 
  group_by(kingdom) %>% 
  summarize(n_species = n_distinct(verbatimScientificName))  

# Have a look at the record locations
gbif_dat %>% 
  filter(!is.na(decimalLatitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  mapView(zcol  = "kingdom")


## Checklist

# Add the BCSEE info.
# Run separate script to load BCSEE data if not in memory
bcsee_gbif <- read_csv("out/bcsee_gbif.csv")

checklist <- gbif_dat %>%
  group_by(taxonKey, kingdom, phylum, class, order, family, species, 
           infraspecificEpithet, verbatimScientificName) |> 
  summarise(GBIF_records = n()) |> 
  filter(!is.na(species)) |> 
  ungroup() |> 
  left_join(bcsee_gbif, join_by("taxonKey" == "usageKey"), multiple = "first") |> 
  arrange(taxonKey)

# Note that not all GBIF records are tracked by CDC.
# Can add common names from GBIF to make list more approachable. 

vnames <- read_tsv("backbone/VernacularName.tsv")

# This is an approach to select an english name for each taxonKey.
# Selects most common english vern name, then if tied, the first one. 
# Its not perfect

gbif_vnames_top <- vnames |>
  filter(language == "en") |> 
  group_by(taxonID, vernacularName) |> 
  summarise(n = n()) |> 
  group_by(taxonID) |> 
  slice_max(n) |> 
  slice_head(n = 1)

checklist <- checklist |> 
  left_join(gbif_vnames_top, join_by("taxonKey" == "taxonID"))  |> 
  mutate(SciName_Harmonized = if_else(!is.na(`Scientific Name`),
                                      `Scientific Name`,
                                      verbatimScientificName),
         EngName_Harmonized = if_else(!is.na(`English Name`),
                                      `English Name`,
                                      vernacularName)) |> 
  select(taxonKey, kingdom, phylum, class, order, family, 
         SciName_Harmonized, EngName_Harmonized, 
         `Element Code CDC` = `Element Code`, 
         `Global Status`, `Prov Status`, 
         `BC List`, COSEWIC, `SARA Schedule`, `SARA Status`, SAR, GBIF_records) |> 
  arrange(taxonKey)


write_csv(checklist, "dat_bowen/checklist.csv")










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

