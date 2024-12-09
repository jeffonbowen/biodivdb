### Biodiversity Database Access Tools ###

# Some scripts that can be used to retrieve biodiversity data from repositories.

# Libraries and Environment Setup ------------------------------------------
{
library(rgbif)
library(spocc)      # Interface to many species occurrence data sources
                    # Include rgbif among a number of others
library(rinat)
library(tidyverse)
library(sf)
library(mapview)
}

# GBIF Download ------------------------------------------------------------

# In order to access occurrence data, an account is needed. 
# There are instructions here to cache credentials: 
# https://docs.ropensci.org/rgbif/articles/gbif_credentials.html
# I have already done this.

# View credentials
usethis::edit_r_environ()

# Get some occurrences using the search API. Limited to 500 records. 
sp <- "Cardellina canadensis"
occ_search(scientificName = sp)
raw <- occ_search(scientificName = sp, 
                  country = "CA",
                  stateProvince = "British Columbia")

# Lets go straight to download, which has no record limit.
# GBIF recommends using taxon key instead of species

sp <- "Cardellina canadensis"
sp_tk <- name_backbone(sp)$usageKey

# Queue a download on the GBIF server
gbif_download <- occ_download(pred("taxonKey", sp_tk),
                              pred_in("stateProvince", c("British Columbia", "Alberta")),
                              format = "SIMPLE_CSV")
gbif_download

# Check status with
occ_download_wait(gbif_download)

# Get the download and import in one step
gbif_dat <- occ_download_get(gbif_download) %>% 
  occ_download_import()

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


## Now lets look at all the records within a certain area.

# Need polygon coordinates. This is an easy tool. 
# https://geojson.io/

# aoi is about 1 km buffer of footprint
aoi <- "POLYGON ((-123.12722582568067 49.798493675402625, -123.16039461446243 49.798493675402625, -123.16039461446243 49.77536786957066, -123.12722582568067 49.77536786957066, -123.12722582568067 49.798493675402625))"

gbif_download <- occ_download(pred_within(aoi),
                              pred("hasGeospatialIssue", FALSE),
                              pred("hasCoordinate", TRUE),
                              pred("occurrenceStatus","PRESENT"), 
                              pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
                              format = "SIMPLE_CSV"
                              )
gbif_download
occ_download_wait(gbif_download)

gbif_dat <- occ_download_get(gbif_download) %>% 
  occ_download_import()

# Select columns of interest.
# Remove records with no species name (i.e., ID is to genus)
dat <- gbif_dat %>% 
  select(gbifID, kingdom, species, locality, occurrenceStatus,
         lat = decimalLatitude, long = decimalLongitude,
         xy_uncert_m = coordinateUncertaintyInMeters,
         eventDate, collectionCode, issue) 

write_csv(dat, "gbif_dat_cleaned.csv")
dat <- read_csv("gbif_dat_cleaned.csv")

# Number of records by kingdom
table(dat$kingdom)

# number of species
dat %>% 
  group_by(kingdom) %>% 
  summarize(n_species = n_distinct(species))  

# Have a look at the record locations
dat %>% 
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mapView(cex = "xy_uncert_m", zcol  = "kingdom")


# GBIF Backbone -----------------------------------------------------------

# Example

name_list <- c(
  "Cirsium arvense (L.) Scop.", 
  "Calopteryx splendens", 
  "Puma concolor (Linnaeus, 1771)", 
  "Ceylonosticta alwisi", 
  "Fake species (John Waller 2021)", 
  "Calopteryx")

l <- name_backbone_checklist(name_list)


# Get common names

# 2481955 Pacific Loon
# 2335124 culpin
# 1310524 wasp

# Can only do one at a time. 

result <- name_usage(key = 2481955, data = "vernacularNames")
common_names <- result$data
view(common_names)

common_names <- result$data |> filter(language == "eng")
view(common_names)

common_names <- result$data |> filter(language == "eng" & 
                                        preferred == TRUE)
view(common_names)

# Using downloaded backbone files

vnames <- read_tsv("backbone/VernacularName.tsv")

# This is an approach to select an english name for each taxonKey.
# Selects most common english vern name, then if tied, the first one. 

gbif_vnames_top <- vnames |>
  filter(language == "en") |> 
  group_by(taxonID, vernacularName) |> 
  summarise(n = n()) |> 
  group_by(taxonID) |> 
  slice_max(n) |> 
  slice_head(n = 1)
  
checklist_vname <- checklist |> 
  select(taxonKey, `English Name`, `Scientific Name`) |> 
  left_join(vnames_top, join_by("taxonKey" == "taxonID"))

