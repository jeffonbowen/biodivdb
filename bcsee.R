#### Working with BCSEE Data ####

# This script imports the BC Species and Ecosystems Explorer data and then
# looks up the GBIF taxon key to join the bcsee data with GBIF data. 

# The only way to get the complete BC list of element occurrences is to use 
# the web interface. Download the "summary report" with no filters or refine
# the list if you like. 

# Strangely, the summary report says it is an excel file, but its really an
# html file with an xls extension. To read the file in to R, the file must be
# opened and then saved as .xlsx. Put it in a folder called "bcsee" or edit
# the path below. 

library(tidyverse)
library(readxl)
library(rgbif)

# Load the bcsee data
bcsee <- read_excel("bcsee/summaryExport.xlsx", col_types = "text")

# Review the fields
names(bcsee)

# Keep the columns that are most likely to be used. 
# Remove the Local Terrestrial Community so that its species only.
bcsee <- bcsee %>% 
  select("Element Code", "Scientific Name", "English Name", "Classification Level",
        "Species Code", `Name Category`, "Class (English)", "Kingdom", "Family", 
        "Global Status", "Prov Status", "BC List", "COSEWIC", "SARA Schedule", 
        "SARA Status") %>% 
  # mutate(species = word(`Scientific Name`, 1, 2)) %>% 
  # select("Element Code", "Scientific Name", everything()) %>% 
  filter(`Classification Level` != "Local Terrestrial Community") |> 
  mutate(SAR = case_when(
    `BC List` %in% c("Red", "Blue") ~ "Yes",
    !COSEWIC %in% c("Data Deficient", "Not at Risk", NA) ~ "Yes",
    `SARA Status` != NA ~ "Yes",
    .default = ""
  )
  )

# Use GBIF backbone to get the taxonKey.

# Create name field
bcsee$name <- bcsee$`Scientific Name`

gbif_lookup <- name_backbone_checklist(bcsee)

bcsee_gbif <- 
  bind_cols(bcsee, gbif_lookup) |> 
  select("Element Code", "Scientific Name", "English Name", "Classification Level",
         "Class (English)", "Kingdom", "Family", "Global Status", 
         "Prov Status", "BC List", "COSEWIC", "SARA Schedule", "SARA Status",
         "usageKey", "scientificName", "rank", "status", "confidence", 
         "matchType") |> 
  arrange("Element Code")

table(bcsee_gbif$`Name Category`, bcsee_gbif$Kingdom)

write_csv(bcsee_gbif, "out/bcsee_gbif.csv")




