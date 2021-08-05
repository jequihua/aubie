# Load packages.
library("here")
library("readxl")
library("sp")
library("raster")
library("rgdal")

# Read original coordinates file.
oxl <- read.csv(here("data","Coordinates.csv"), sep = ";")

# Remove unsampled sites (rows).
oxl <- oxl[!is.na(oxl$site.number),]

# Coordinates to numeric.
oxl$latitude <- gsub(",", ".", oxl$latitude)
oxl$latitude <- as.numeric(oxl$latitude)
oxl$longitude <- gsub(",", ".", oxl$longitude)
oxl$longitude <- as.numeric(oxl$longitude)

# To a spatial object.
coordinates(oxl) <- ~ longitude+latitude
projection(oxl) <- "+init=epsg:4326"
writeOGR(oxl,"aubie_sites.shp", "aubie_sites", driver="ESRI Shapefile")
