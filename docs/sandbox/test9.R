rm(list = ls())
gc()

fsave <- function(x, file, location = "./data/processed/", ...) {
  if (!dir.exists(location))
    dir.create(location)
  datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
  totalname <- paste(location, datename, file, sep = "")
  print(paste("SAVED: ", totalname, sep = ""))
  save(x, file = totalname)
}

fpackage.check <- function(packages) {
  lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

colorize <- function(x, color) {
  sprintf("<span style='color: %s;'>%s</span>", color, x)
}

packages = c("sf", "ggplot2", "ggmap", "leaflet")
fpackage.check(packages)

if (!dir.exists("./data/rawGIS/")) dir.create("./data/rawGIS/")


# Loading 100m-by-100m raster data:
rast <- sf::st_read(dsn = "./data/rawGIS/cbs_vk100_2021_v1.gpkg")

neighbShape <- sf::st_read(dsn = "./data/rawGIS", layer = "buurt_2021_v1")
districtShape <- sf::st_read(dsn = "./data/rawGIS", layer = "wijk_2021_v1")
postcode4Shape <- sf::st_read(dsn = "./data/rawGIS", layer = "CBS_pc4_2020_v1")
postcode5Shape <- sf::st_read(dsn = "./data/rawGIS", layer = "CBS_pc5_2020_v1")
postcode6Shape <- sf::st_read(dsn = "./data/rawGIS", layer = "CBS_pc6_2020_v1")


