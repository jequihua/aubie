# Load packages
library("here")
library("lubridate")

# List site folders.
folders <- list.dirs(here("data"), recursive = FALSE)

# Initialize table list.
tables_list <- list()

# Initialize counter.
counter = 0
# Sweep through folders.
for (f in 1:length(folders)){
print(f)
  folder <- folders[f]
  site <- as.numeric(strsplit(strsplit(folder,split = "/")[[1]][7],split = "e")[[1]][2])
  
  # List text files.
  txts <- list.files(folder, pattern = "\\.txt$")
  
  for (t in seq(2,length(txts),2)){
  
    
    
    txt <- txts[t]
    txt_df <- read.table(file.path(folder,txt), sep = "\t", header = TRUE)
    
    # Remove redundant obs.
    txt_df <- txt_df[txt_df$Rank == 1,]
    txt_df <- txt_df[order(txt_df$Confidence, decreasing = TRUE),]
    txt_df <- txt_df[txt_df$Confidence > 0.5,]
    
    if (nrow(txt_df) > 0){
      
      counter = counter + 1
      
      # Remove repeated species.
      txt_df <- txt_df[!duplicated(txt_df$Species.Code),]
      
      txt_df$datetime <- filenameDate(txt_df)
      
      txt_df$site <- site
      
      tables_list[[counter]] <- txt_df
      
    }
    
  }
  
}

binded_df <- do.call("rbind", tables_list)

View(binded_df)
