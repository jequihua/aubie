# Load packages.
library("raster")
library("rgdal")
library("here")
library("rgeos")

# Load roi shapefile.
roi <- readOGR(here("data_gis", "shapefiles", "roi.shp"),"roi")

# Process Small Woody Features layer (Copernicus).
swf <- raster(here("data_gis","rasters","swfawf_2015_100m_EU_3035_V013","data","swfawf_2015_100m_EU_3035_V013.tif"))
swf <- crop(swf, spTransform(roi,CRS(projection(swf))), method = "ngb")
writeRaster(swf, filename = here("data_gis","rasters","harmonized","SWF.tif"), format="GTiff", overwrite=TRUE)

swf_mean <- focal(swf, w=matrix(1/(101*101),nrow=101,ncol=101)) 
writeRaster(swf_mean, filename = here("data_gis","rasters","harmonized","SWF_10kmMean.tif"), format="GTiff", overwrite=TRUE)

# Process Tree Cover (Copernicus).
tcd <- raster(here("data_gis","rasters","TCD_2018_100m_eu_03035_v020","data","TCD_2018_100m_eu_03035_V2_0.tif"))
tcd <- crop(tcd, spTransform(roi,CRS(projection(tcd))), method = "ngb")
writeRaster(tcd, filename = here("data_gis","rasters","harmonized","TCD.tif"), format="GTiff", overwrite=TRUE)

tcd_mean <- focal(tcd, w=matrix(1/(101*101),nrow=101,ncol=101)) 
writeRaster(tcd_mean, filename = here("data_gis","rasters","harmonized","TCD_10kmMean.tif"), format="GTiff", overwrite=TRUE)

# Process Corine land cover (Copernicus).
#clc <- raster(here("data_gis","rasters","u2018_clc2018_v2020_20u1_raster100m","data","U2018_CLC2018_V2020_20u1.tif"))
#clc <- crop(clc, spTransform(roi,CRS(projection(clc))), method = "ngb")
#writeRaster(clc, filename = here("data_gis","rasters","harmonized","CLC2018.tif"), format="GTiff", overwrite=TRUE)

# Process GLAD tree height.
glad <- raster(here("data_gis", "rasters", "Forest_height_2019_NAFR.tif"))
glad <- crop(glad, gBuffer(roi, width = 1))
glad <- aggregate(glad, fun = mean, fact = 3, ) # to 90m
glad <- projectRaster(glad, swf, method = "ngb")
glad <- crop(glad, spTransform(roi,CRS(projection(swf))), method = "ngb")
glad[glad>35 & !is.na(swf)] <- 0
writeRaster(glad,
            filename = here("data_gis","rasters","harmonized","GLAD_canopy_height.tif"), format="GTiff", overwrite=TRUE)

glad_max <- focal(glad, w=matrix(1/(101*101),nrow=101,ncol=101), fun=max) 
writeRaster(tcd_mean, filename = here("data_gis","rasters","harmonized","GLAD_10kmMax.tif"), format="GTiff", overwrite=TRUE)

# Process Landsat 8 bands.
landsat <- brick(here("data_gis", "rasters", "Hansen_GFC-2019-v1.7_last_60N_010E.tif"))
landsat <- crop(landsat, gBuffer(roi, width = 1))
landsat <- aggregate(landsat, fun = mean, fact = 3) # to 90m
landsat <- projectRaster(landsat, swf, method = "ngb")
landsat <- crop(landsat, spTransform(roi,CRS(projection(swf))), method = "ngb")

writeRaster(subset(landsat,1),
            filename = here("data_gis","rasters","harmonized","landsat_3red.tif"), format="GTiff", overwrite=TRUE)
writeRaster(subset(landsat,2),
            filename = here("data_gis","rasters","harmonized","landsat_4NIR.tif"), format="GTiff", overwrite=TRUE)
writeRaster(subset(landsat,3),
            filename = here("data_gis","rasters","harmonized","landsat_5SWIR.tif"), format="GTiff", overwrite=TRUE)
writeRaster(subset(landsat,4),
            filename = here("data_gis","rasters","harmonized","landsat_7SWIR.tif"), format="GTiff", overwrite=TRUE)
