# Script to load rasterstack for Crete gap analysis biomod runs
# 10 July 2017 | Mirza Cengic

library(raster)
library(dplyr)

# Load files ####

# Get file names
file_names <- list.files("E:/Projects/Thesis_paper/Thesis_paper/Crete_gap/Data/Predictors_intermediate/", pattern = ".tif$", full.names = TRUE)


#
Crete_rasters <- stack(file_names)

# Different extents, check.

# loop to check for different extents
for(i in seq_along(file_names))
{
  print(raster(file_names[i]))
}

# Distance layers with different extents

# Get no distance rasters
file_names_no_dist <- file_names[-which(grepl("Dist", file_names))]

Crete_rasters_no_dist <- stack(file_names_no_dist)

# Get distance rasters
file_names_dist <- file_names[which(grepl("Dist", file_names))]

# Resample roads 
Crete_roads <- resample(raster(file_names_dist[1]), Crete_rasters_no_dist[[1]], method = "bilinear")
# Resample water 
Crete_water <- resample(raster(file_names_dist[2]), Crete_rasters_no_dist[[1]], method = "bilinear")

# Check
stack(Crete_water, Crete_roads, Crete_rasters_no_dist)

# Save data
writeRaster(Crete_water, "E:/Projects/Thesis_paper/Thesis_paper/Crete_gap/Data/Rasters/Predictors_final/Distance_water.tif", format = "GTiff")
writeRaster(Crete_roads, "E:/Projects/Thesis_paper/Thesis_paper/Crete_gap/Data/Rasters/Predictors_final/Distance_roads.tif", format = "GTiff")
