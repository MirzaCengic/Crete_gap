# Script to process NDVI data for Crete.
# SPOT-VGT time series is available from 2004 - 2014.
# For Crete gap project we will use average values of time series for the period April - September
# Mirza Cengic | mirzaceng@gmail.com | 24.04.2017.
# Rprofile on the local machine by default runs library(pacman)

Console_output <- file(paste0("/vol/milkun1/Mirza_Cengic/Projects/Crete_gap/Crete_R_project/Outputs/Console/Unzip_NDVI_", format(Sys.Date(), "%d%m%y"),".txt"), open = "wt")

sink(Console_output, append = TRUE)
sink(Console_output, append = TRUE, type = "message")
print(paste("The process started at: ", Sys.time()))
# Load packages
library(pacman)
p_load(raster, dplyr, stringr, gdalUtils)

# Get names of directories with the NDVI data. Specific months for Europe will be filtered from this.
# my_dirs <- list.dirs("//milkun1-srv.science.ru.nl/milkun1/Mirza_Cengic/Data_RAW/SPOT_NDVI", recursive = TRUE)
my_dirs <- list.dirs("/vol/milkun1/Mirza_Cengic/Data_RAW/SPOT_NDVI", recursive = TRUE)

# my_dirs_df <- as.data.frame(my_dirs)

# df <- t(my_dirs_df)



my_dirs_Europe <- my_dirs[grep("Europe", my_dirs)]

splitted <- str_split(my_dirs_Europe, "SPOT_NDVI", simplify = TRUE)[, 2] %>% substring(7) 


months_index <- which(sub('/.*', '', splitted) %in% 4:9)

months_to_extract <- my_dirs_Europe[months_index]
print(months_to_extract)
files_to_unzip <- list.files(months_to_extract, pattern = "*.zip$", 
                             full.names = TRUE, ignore.case = TRUE)


# out_path <- "//milkun1-srv.science.ru.nl/milkun1/Mirza_Cengic/Projects/Crete_gap/Data/Predictors/RAW/SPOT_NDVI_Crete"
out_path <- "/vol/milkun1/Mirza_Cengic/Projects/Crete_gap/Data/Predictors/RAW/SPOT_NDVI_Crete"

# unzip(files_to_unzip[[1]], exdir = out_path)

for(file in files_to_unzip)
{
  print(file)
  unzip(file, exdir = out_path)
}
  
print(paste("The process ended at: ", Sys.time()))


sink(type = "message")
sink()

