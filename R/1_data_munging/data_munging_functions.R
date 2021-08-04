# Create date from file name.
filenameDate <- function(filename, var = "Begin.File", date_type = "date"){
  datetimes <- list()
  for (i in 1:nrow(txt_df)){
    file_name <- txt_df[i, var]
    time_string <- gsub(".*[_]([^.]+)[.].*", "\\1", file_name)
    date_string <- strsplit(file_name, split = "_")[[1]][1]
    
    if (date_type == "date"){
      datetime_string <- paste0(substr(date_string, 1, 4), "-",
                                substr(date_string, 5, 6), "-",
                                substr(date_string, 7, 8))
      datetimes[[i]] <- ymd(datetime_string)
      
    } else if (date_type == "time"){
      datetime_string <- paste0(substr(time_string, 1, 2), ":",
                                substr(time_string, 3, 4), ":",
                                substr(time_string, 5, 6))
      datetimes[[i]] <- hms(datetime_string)+seconds()
      
    } else if (date_type == "datetime"){
    datetime_string <- paste0(substr(date_string, 1, 4), "-",
                              substr(date_string, 5, 6), "-",
                              substr(date_string, 7, 8), " ",
                              substr(time_string, 1, 2), ":",
                              substr(time_string, 3, 4), ":",
                              substr(time_string, 5, 6)) 
    datetimes[[i]] <- ymd_hms(datetime_string)
    }
    
    
  }
  datetimes <- Reduce(c, datetimes)
  return(datetimes)
}
