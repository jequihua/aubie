# Load packages.
library("here")
library("raster")
library("rgdal")
library("randomForest")
library("isotree")

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

suit_data <- data.frame(site=binded_df_spatial$site, date = binded_df_spatial$datetime,
                       species = binded_df_spatial$Common.Name, extraction)

# To wide format.
dates <- unique(binded_df$datetime)
sites <- unique(binded_df$site)

suit_data_wide <- data.frame(matrix(0,length(sites), length(dates)+1))
colnames(suit_data_wide) <- c("site",as.character(dates))
suit_data_wide$site <- sites
for (i in 1:length(sites)){
  s <- suit_data_wide$site[i]
  for (j in 1:length(dates)){
    d <- colnames(suit_data_wide)[j+1]
    
    obs_sub <- suit_data[suit_data$species == species &
                          suit_data$site == s &
                          suit_data$date == d,]
    if (nrow(obs_sub) > 0){
      colnames(suit_data_wide)
      suit_data_wide[i,j+1] <-1
    }
  }
}

suit_spatial <- merge(suit_data_wide, site_coordinates, by = "site")

# To a spatial object.
coordinates(suit_spatial) <- ~ coords.x1+coords.x2
projection(suit_spatial) <- "+init=epsg:4326"

# Extract covariate data from rasters.
sitecoords <- spTransform(suit_spatial, CRS(raster::projection(brik)))
#sitecoords <- sitecoords[!duplicated(sitecoords$site),]
extraction <- extract(brik, sitecoords)


label <- rowSums(suit_data_wide[,2:ncol(suit_data_wide)])
label[label>0] <- 1

train_data <- data.frame(label,extraction)

ssize = floor(0.7*sum(train_data$label))

rf <- randomForest(y=as.factor(train_data$label),x=train_data[,2:ncol(train_data)],
                   ntree = 500, sampsize = ssize)
            
iso_train <- train_data[train_data$label==1,]
       
# Isolation forest.
irf <- isolation.forest(iso_train[,2:ncol(iso_train)], ntrees = 500, nthreads = 6)

# prediction
out <- data.frame(rasterToPoints(brik))
prediction <- predict(irf, out[,3:ncol(out)])

out_rast <- data.frame(x = out$x, y = out$y, prediction)
coordinates(out_rast) = ~ x+y
gridded(out_rast) = TRUE
out_rast <- raster(out_rast)
projection(out_rast) <- projection(brik)
writeRaster(out_rast, filename = "test.tif", format="GTiff", overwrite=TRUE)
