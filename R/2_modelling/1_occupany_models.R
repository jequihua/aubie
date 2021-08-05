# Load packages.
library("here")
library("raster")
library("rgdal")
library("unmarked")

# Load raster covariates.
rast_list <- list.files(here("data_gis/rasters/harmonized/"),
                        full.names = TRUE)
brik <- brick()
for (i in 1:length(rast_list)){
  brik <- addLayer(brik,raster(rast_list[[i]]))
}

# Load sites data.
sites <- readOGR(here("aubie_sites.shp"),"aubie_sites")
site_coordinates <- data.frame(coordinates(sites), site = sites$st_nmbr)

# Observations per site.
table(binded_df$site, binded_df$Common.Name)

# Select a species.
species <- "Eurasian Marsh-Harrier"

obs_sub <- binded_df[binded_df$Common.Name==species,]

binded_df_spatial <- merge(binded_df, site_coordinates, by = "site")

# To a spatial object.
coordinates(binded_df_spatial) <- ~ coords.x1+coords.x2
projection(binded_df_spatial) <- "+init=epsg:4326"

# Extract covariate data from rasters.
sitecoords <- spTransform(binded_df_spatial, CRS(raster::projection(brik)))
sitecoords <- sitecoords[!duplicated(sitecoords$site),]
extraction <- extract(brik, sitecoords)

occ_data <- data.frame(site=binded_df_spatial$site, date = binded_df_spatial$datetime,
                       species = binded_df_spatial$Common.Name, extraction)

# To wide format.
dates <- unique(binded_df$datetime)
sites <- unique(binded_df$site)

occ_data_wide <- data.frame(matrix(0,length(sites), length(dates)+1))
colnames(occ_data_wide) <- c("site",as.character(dates))
occ_data_wide$site <- sites
for (i in 1:length(sites)){
  s <- occ_data_wide$site[i]
  for (j in 1:length(dates)){
    d <- colnames(occ_data_wide)[j+1]
    
    obs_sub <- occ_data[occ_data$species == species &
                          occ_data$site == s &
                          occ_data$date == d,]
    if (nrow(obs_sub) > 0){
      colnames(occ_data_wide)
      occ_data_wide[i,j+1] <-1
    }
  }
}

umf <- unmarkedFrameOccu(y = occ_data_wide[,-1], 
                         siteCovs = as.data.frame(extraction))

colnames(extraction)

umffm <- occu(formula = ~ GLAD_1kmMax+landsat_3red+landsat_4NIR+landsat_5SWIR+     
                          landsat_7SWIR+SWF+SWF_1kmMean+TCD_1kmMean ~ 1,
              data = umf)
umffm

pred_table <- as.data.frame(rasterToPoints(brik))
pred_table <- pred_table[,c("GLAD_1kmMax","landsat_3red","landsat_4NIR","landsat_5SWIR",     
                         "landsat_7SWIR","SWF","SWF_1kmMean","TCD_1kmMean")] 
occ_pred <- predict(umffm, 
                    newdata = as.data.frame(pred_table), 
                    type = "state")

