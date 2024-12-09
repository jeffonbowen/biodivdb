### BC Data ###

library(rgbif)
library(tidyverse)
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
