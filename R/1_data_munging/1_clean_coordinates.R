# Load packages.
library("here")
library("readxl")

# Read original coordinates file.
oxl <- read.csv(here("data","coordinates", "Coordinates.csv"), sep = ";")

# Remove unsampled sites (rows).
oxl <- oxl[!is.na(oxl$site.number),]

