# Load packages.
library("here")
library("raster")
library("rgdal")
library("unmarked")
library("AICcmodavg")
library("ggplot2")

# Load sites data.
sites <- readOGR(here("aubie_sites.shp"),"aubie_sites")
site_coordinates <- data.frame(raster::coordinates(sites), site = sites$st_nmbr, sites)

# Observations per site.
table(binded_df$site, binded_df$Common.Name)
table(binded_df$Common.Name)

#obs_sub <- binded_df[binded_df$Common.Name==species,]

binded_df_spatial <- merge(binded_df, site_coordinates, by = "site")
names(binded_df_spatial)
occ_data <- data.frame(site=binded_df_spatial$site, date = binded_df_spatial$datetime,
                       species = binded_df_spatial$Common.Name,
                       binded_df_spatial[,c("cntrl__","edg_lng", "edg_ln_", 
                                            "prmnn__", "prmn___")])



### To wide format.

# Select a species.
species <- "Eurasian Skylark"

dates <- unique(binded_df$datetime)
sites <- unique(binded_df$site)

occ_data_wide <- data.frame(matrix(0,length(sites), length(dates)+6))
colnames(occ_data_wide) <- c("site",as.character(dates),
                             c("cntrl__","edg_lng", "edg_ln_", 
                               "prmnn__", "prmn___"))

for (i in 1:length(sites)){
  print(i)
  s <- sites[i]
  site_data <- occ_data[occ_data$site == s,]
  occ_data_wide[i,c("cntrl__","edg_lng", "edg_ln_", 
                    "prmnn__", "prmn___")] <- site_data[1,c("cntrl__","edg_lng", "edg_ln_", 
                                                         "prmnn__", "prmn___")]
  for (j in 1:length(dates)){
    d <- as.character(dates)[j]
    
    obs_sub <- occ_data[occ_data$species == species &
                          occ_data$site == s &
                          as.character(occ_data$date) == d,]
    
    if (nrow(obs_sub) > 0){
      colnames(occ_data_wide)
      occ_data_wide[i,j+1] <-1
    }
  }
}

names(occ_data_wide)
occ_data_wide
umf <- unmarkedFrameOccu(y = occ_data_wide[,2:16], 
                         siteCovs = occ_data_wide[,17:21])

View(occ_data_wide[,2:16])
View(occ_data_wide[,17:21])

umf@siteCovs$cntrl__ <- scale(umf@siteCovs$cntrl__)
umf@siteCovs$edg_lng <- scale(umf@siteCovs$edg_lng)
umf@siteCovs$prmnn__ <- scale(umf@siteCovs$prmnn__)

umffm <- occu(formula = ~ 1
                        ~ edg_lng + prmnn__ ,
              data = umf)
umffm

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_forest_newdata <- data.frame(edg_lng = seq(min(umf@siteCovs$edg_lng), 
                                             max(umf@siteCovs$edg_lng), by = 0.1),
                                  prmnn__= mean(umf@siteCovs$prmnn__)) # set constant on mean 

occu_forest_newdata <- data.frame(prmnn__ = seq(min(umf@siteCovs$prmnn__), 
                                                max(umf@siteCovs$prmnn__), by = 0.1),
                                  edg_lng= mean(umf@siteCovs$edg_lng)) # set constant on mean 


# Model-averaged prediction of occupancy and confidence interval
occu_forest_pred <- modavgPred(list(umffm),
                               # c.hat =    # to change variance inflation factor, default = 1) 
                               parm.type = "psi", # psi = occupancy
                               newdata = occu_forest_newdata)[c("mod.avg.pred",
                                                                "lower.CL",
                                                                "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_forest_pred_df <- data.frame(Predicted = occu_forest_pred$mod.avg.pred,
                                  lower = occu_forest_pred$lower.CL,
                                  upper = occu_forest_pred$upper.CL,
                                  occu_forest_newdata)

# Plot the relationship
occu_forest_pred_plot <- ggplot(occu_forest_pred_df, aes(x = prmnn__, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") +
  geom_path(size = 1) +
  labs(x = "Grassland prop (standardized)", y = "Occupancy probability") +
  theme_classic() +
  coord_cartesian(ylim = c(0,1)) +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"))
occu_forest_pred_plot
