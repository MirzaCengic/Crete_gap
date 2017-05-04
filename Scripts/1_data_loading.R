
# Load libraries ----

library(pacman)
p_load(raster, rgdal, dplyr, readr, mapview, sf, sp, stringr, spThin, biomod2, rasterVis, gdalUtils, stringr)

# set raster options
rasterOptions(tmpdir = "R_temp/")

# Getting the data ----

# climate data
bioclim_names <- list.files("Data/CHELSA_Crete/", pattern = "*.tif$", full.names = TRUE)
bioclim_rasters <- stack(bioclim_names)

# DEM
DEM_large <- raster("Y:\\Mirza_Cengic\\my_files\\Pers\\GIS_data\\Greece_data\\SRTM DEM\\dem_ageansea.img")

mapview(DEM_large)
Crete_poly <- shapefile("E:\\Projects\\Thesis_paper\\Thesis_paper\\Crete_gap\\Data\\New folder\\New folder\\Crete_boundary_UTM35N.shp")

DEM_large@crs
Crete_poly@proj4string

Crete_poly_wgs <- spTransform(Crete_poly, CRSobj = crs(DEM_large, asText = TRUE))

Crete_DEM <- crop(DEM_large, Crete_poly_wgs)
Crete_DEM <- mask(Crete_DEM, Crete_poly_wgs)

Crete_DEM_UTM <- projectRaster(Crete_DEM, crs = Crete_poly@proj4string, method = "bilinear", alignOnly = TRUE, format = "GTiff")
mapview(Crete_DEM_UTM)

####
Aster_DEM <- raster("Data/aster-gdem-30m-crete/mosaic-utm.img")
mapview(Aster_DEM)
Aster_DEM_deratified <- deratify(Aster_DEM, att = "ID")

# Resample all data to 30x30 spatial resolution (ASTER resolution) --------

bioclim_rasters_UTM <- projectRaster(bioclim_rasters, crs = crs(Aster_DEM, asText = TRUE))

bioclim_resampled <- resample(bioclim_rasters_UTM, Aster_DEM_deratified, method = "bilinear")
mapview(bioclim_resampled[[1]])
names(bioclim_resampled)

names(bioclim_resampled) <- str_split(names(bioclim_resampled), "_", simplify = TRUE)[, 2]

Crete_slope <- terrain(Aster_DEM_deratified, opt = "slope", unit = "degrees", neighbors = 8)
Crete_aspect <- terrain(Aster_DEM_deratified, opt = "aspect", unit = "degrees", neighbors = 8)

Crete_northness <- cos(Crete_aspect)

Crete_rasters <- stack(Aster_DEM_deratified, Crete_slope, Crete_northness, bioclim_resampled)
names(Crete_rasters)[c(1,3)] <- c("Aster_DEM", "Northness")
writeRaster(Crete_rasters, "Data/Predictors_intermediate/",  format = "GTiff", bylayer = TRUE, names(Crete_rasters))
#
Crete_rasters <- stack(list.files("E:\\Projects\\Thesis_paper\\Thesis_paper\\Crete_gap\\Data\\Predictors_intermediate\\", full.names = TRUE))

# Solar radiation data ----
solar_radiation <- raster("E:/Temp/Crete/Solar_radiation_Aster_v2.tif")
levelplot(solar_radiation)

# To do: ----

# NDVI April
# NDVI September
# Landcover DONE!
# Distance to rivers and water
# Distance to roads # Done
# Human footprint

# Create processing mask

Crete_mask <- Crete_rasters$Aster_DEM
# Land cover data Crete ----
land_cover_global <- raster("Y:\\Mirza_Cengic\\Data_RAW\\ESA_CCI\\TIFF\\YearByYear\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2012-v2.0.7.tif")

Crete_poly <- spTransform(Crete_poly, CRSobj = crs(land_cover_global, asText = TRUE))

landcover_crete <- crop(land_cover_global, Crete_poly)
landcover_crete_mask <- mask(landcover_crete, Crete_poly)

# Get values 
landcover_crete_values <- getValues(landcover_crete)

# Reclassify landcover layer
landcover_crete_values[landcover_crete_values %in% c(10, 11, 12, 20, 30, 40)] <- "Agriculture"

landcover_crete_values[landcover_crete_values %in% c(50, 60, 61, 62, 70, 71,
                                                     72, 80, 81, 82, 90, 100,
                                                     160, 170)] <- "Forest"

landcover_crete_values[landcover_crete_values %in% c(110, 130, 120, 121, 122)] <- "Grassland"

landcover_crete_values[landcover_crete_values %in% c(140, 150, 151, 152, 153, 200, 201)] <- "Bare"

landcover_crete_values[landcover_crete_values %in% c(180, 210)] <- "Water"

landcover_crete_values[landcover_crete_values %in% 190] <- "Urban"
landcover_crete_values <- as.factor(landcover_crete_values)

# Set values back
landcover_crete_reclassified <- setValues(landcover_crete, landcover_crete_values)

# Wrangle around
landcover_crete_reclassified <- projectRaster(landcover_crete_reclassified, crs = crs(Crete_mask, asText = TRUE), method = "ngb")
landcover_crete_reclassified <- mask(landcover_crete_reclassified, Crete_poly_wgs)
landcover_crete_resampled <- resample(landcover_crete_reclassified, Crete_mask, method = "ngb")

# Save
writeRaster(landcover_crete_resampled, "E:/Projects/Thesis_paper/Thesis_paper/Crete_gap/Data/Predictors_intermediate/landcover.tif", format = "GTiff")

# Roads distance ----
library(pacman)
p_load(raster, dplyr, readr, mapview, sf, sp, stringr, spThin, biomod2, rasterVis, gdalUtils)

roads_distance <- raster("E:\\Temp\\Crete\\Roads_distance.tif")

crete_mask <- raster("E:/Projects/Thesis_paper/Thesis_paper/Crete_gap/Data/Predictors_intermediate/Aster_DEM.tif")

Crete_poly <- shapefile("E:\\Projects\\Thesis_paper\\Thesis_paper\\Crete_gap\\Data\\New folder\\New folder\\Crete_boundary_UTM35N.shp")

Crete_poly_wgs <- spTransform(Crete_poly, CRSobj = crs(crete_mask, asText = TRUE))




roads_distance <- mask(roads_distance, Crete_poly_wgs)

roads_values <- getValues(roads_distance)
histogram(roads_values)

roads_log <- log(roads_distance)
plot(roads_log)
roads_values_log <- getValues(roads_log)

histogram(roads_values_log)

writeRaster(roads_log, "E:/Projects/Thesis_paper/Thesis_paper/Crete_gap/Data/Predictors_intermediate/Distance_roads.tif", format = "GTiff")


# NDVI - continue; find script that I was working on ----

folders_NDVI <- list.dirs("Y:/Mirza_Cengic/Data_RAW/SPOT_NDVI/", full.names = TRUE)
folders_NDVI_c <- folders_NDVI

folders_NDVI <- folders_NDVI[grep("Europe", folders_NDVI)]
folders_NDVI_string <- str_split(folders_NDVI, "V2KRNS10__", simplify = TRUE)[,2]

substring(folders_NDVI_string, 4)

# Distance to rivers and water - continue and finish merging lakes and rivers layer. Started in arcgis with raster algebra approach

lakes <- raster("E:/Temp/Crete/Lakes_raster_v3.tif")
lakes <- mask(lakes, rivers)

rivers <- raster("E:/Temp/Crete/Rivers_raster_v2.tif")



mapview(lakes)


water_bodies <- rivers * lakes


writeOGR(water_bodies, "E:/Temp/Crete", "Water_bodies_poly_v2", driver = "ESRI Shapefile")
mapview(water_bodies)
# Species data ------------------------------------------------------------

Crete_species <- read_csv("E:\\Projects\\Thesis_paper\\Thesis_paper\\Crete_gap\\Data\\Species data\\Crete_species_filtered_wGBIF_final.csv")
head(Crete_species)
Crete_species


my_BM_Data<-BIOMOD_FormatingData(resp.var = rep(1,length(Crete_species_BufV$Name[Crete_species_BufV$Name==i])),
                                 expl.var = Crete_raster,
                                 resp.xy = Species_XY,
                                 eval.resp.xy = NULL,
                                 PA.nb.rep = 2, #nr of preudoabsence realisations, 10 runs from Barbet-Massin
                                 PA.nb.absences = length(Crete_species_BufV$Name[Crete_species_BufV$Name==i])*2,  # 1000 From Barbet-Massin et al. 2012
                                 PA.strategy = "disk",
                                 PA.dist.min = 5000, # if PA.strategy is "disk"
                                 PA.dist.max = 200000,
                                 na.rm = TRUE,
                                 resp.name = as.character(i))
