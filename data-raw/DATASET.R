## code to prepare `DATASET` dataset goes here

library(sf)
sf_use_s2(FALSE)
vulnerability <- read_sf(file.path("SVI2018_US_COUNTY", "SVI2018_US_county.shp"))
vulnerability <- vulnerability |>
                 as.data.frame() |>
                 select(-geometry)

diabetes <- read.csv("diabetes.csv",
                     colClasses = c("County.Code" = "character"))

usethis::use_data(vulnerability, diabetes, overwrite = TRUE)
