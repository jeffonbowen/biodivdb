#### Working with BCSEE Data ####

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

bcsee <- read_excel("bcsee/summaryExport.xlsx", col_types = "text")

# Review the fields
names(bcsee)

# High-level look at what has been downloaded
table(bcsee$Kingdom, bcsee$`Classification Level`)
# or 
# table(bcsee$`Class (English)`, bcsee$`Classification Level`)

# So many columns. Keep the columns that are most likely to be used. 
# Also add a new column with just genus-species binomial for linking to GBIF
# Remove the Local Terrestrial Community so that its species only.

bcsee <- bcsee %>% 
  select("Element Code", "Scientific Name", "English Name", "Classification Level",
        "Species Code", "Class (English)", "Kingdom", "Family", "Global Status", 
        "Prov Status", "BC List", "COSEWIC", "SARA Schedule", "SARA Status") %>% 
  mutate(species = word(`Scientific Name`, 1, 2)) %>% 
  select("Element Code", "Scientific Name", species, everything()) %>% 
  filter(`Classification Level` != "Local Terrestrial Community")

# Create a subset for testing
t <- bcsee |> 
  group_by(Kingdom) |> 
  slice_sample(n= 1000) |> 
  select("Class (English)", "Scientific Name", "English Name", "Classification Level")

t$name <- t$`Scientific Name`

l <- name_backbone_checklist(t)

out <- bind_cols(t, l)

table(out$`Class (English)`, out$matchType)

# Tally the number of records that have a conservation status
# Fix this later
# x <- bcsee %>% 
#   filter(`BC List` == c("Red", "Blue") | 
#            COSEWIC == c())




