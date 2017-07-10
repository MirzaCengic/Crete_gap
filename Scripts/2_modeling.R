##############################################
#  Crete gap analysis - SDM modeling script ##
#        Mirza Cengic - 02.04.2017          ##
##############################################

# Clear workspace
rm(list = ls())

# Get libraries
library(pacman)
p_load(readr, biomod2, raster, dplyr)


# Load files --------------------------------------------------------------

Crete_rasters <- stack(list.files("Data/Predictors_intermediate/",
                                    pattern = "*.tif$", full.names = TRUE))

# 
# TO DO: insert a section to change the names of the variables (with a switch?)
names(Crete_rasters)

# Temp section 1 - aggregate predictors for speed -------------------------
Crete_rasters_aggregated <- aggregate(Crete_rasters[[c(1, 2, 5)]], fact = 33)
mapview(Crete_rasters_aggregated[[1]])
writeRaster(Crete_rasters_aggregated, "Data/Predictors_aggregated/",  format = "GTiff",
            bylayer = TRUE, names(Crete_rasters_aggregated))
# --------------------------


Crete_species <- read_csv("E:\\Projects\\Thesis_paper\\Thesis_paper\\Crete_gap\\Data\\Species data\\Crete_species_filtered_wGBIF_final.csv")

Crete_species_XY <- cbind(Crete_species$X, Crete_species$Y)


Species_XY <- Crete_species[Crete_species$Name== "Chalcides ocellatus", c("X","Y")]


table(Crete_species$Name)

my_BM_Data <- BIOMOD_FormatingData(resp.var = rep(1,length(Crete_species$Name[Crete_species$Name == "Chalcides ocellatus"])),
                                 expl.var = Crete_rasters,
                                 resp.xy = Species_XY,
                                 eval.resp.xy = NULL,
                                 PA.nb.rep = 2, #nr of preudoabsence realisations, 10 runs from Barbet-Massin
                                 PA.nb.absences = length(Crete_species$Name[Crete_species$Name == "Chalcides ocellatus"]) * 2,  # 1000 From Barbet-Massin et al. 2012
                                 PA.strategy = "disk",
                                 PA.dist.min = 5000, # if PA.strategy is "disk"
                                 PA.dist.max = 200000,
                                 na.rm = TRUE,
                                 resp.name = "my_test")



stop()

# Species modeling function -----------------------------------------------
Crete_species <- subset(Crete_species, Name == "Lacerta trilineata" |
                          Name == "Podarcis cretensis" |
                          Name == "Chalcides ocellatus")
# 
sp_names <- c("Chalcides ocellatus", 
              "Lacerta trilineata",
              "Podarcis cretensis") 


MyBiomodSF <- function(Species_name, Raster_data, PA_number = 100, PA_rep = 1){
  
  my_species_name  <-  Species_name
  
  cat('\n',my_species_name,'modeling...') 
  
  my_presences <- rep(1, length(Crete_species$Name[Crete_species$Name==my_species_name]))
  my_presences_XY <-  Crete_species[Crete_species$Name==my_species_name,c("X","Y")]
  
  # Capture console output -----------------------
  
  Console_output <- file(paste(my_species_name,"_console_output.txt", sep = ""), open = "wt")
  sink(Console_output, append = TRUE)
  sink(Console_output, append = TRUE, type = "message")
  
  ### Initialisation
  myBiomodData <- BIOMOD_FormatingData(resp.var = my_presences, expl.var = Raster_data, resp.xy = my_presences_XY,
                                       resp.name = my_species_name, PA.nb.rep = PA_rep, PA.nb.absences = PA_number,
                                       PA.strategy = "random")
  
  
  
  
  
  ### Options definition
  
  myBiomodOption <- BIOMOD_ModelingOptions()
  
  ### Modelling
  
  myBiomodModelOut <- BIOMOD_Modeling(
    myBiomodData,
    models = c("GLM", "GBM", "CTA", "RF", "ANN"),
    models.options = myBiomodOption,
    NbRunEval=2,
    DataSplit=80,
    VarImport=2,
    models.eval.meth = c("KAPPA","TSS","ROC"),
    SaveObj = TRUE,
    rescal.all.models = TRUE,
    do.full.models = FALSE,
    modeling.id = paste(my_species_name,"FirstModeling",sep=""))
  
  
  
  write.table(get_evaluations(myBiomodModelOut),
              file=paste("Model_evaluation_",my_species_name,".csv", sep=""), sep = ",")          
  
  write.table(get_variables_importance(myBiomodModelOut),
              file=paste("Variables_importance_",my_species_name,".csv", sep =""), sep = ",")
  
  
  #Ensemble modeling section
  myBiomodEM <- BIOMOD_EnsembleModeling( 
    modeling.output = myBiomodModelOut,
    chosen.models = 'all',
    em.by='all',
    eval.metric = c('TSS'),
    eval.metric.quality.threshold = c(0.7),
    prob.mean = T,
    prob.cv = T,
    prob.ci = T,
    prob.ci.alpha = 0.05,
    prob.median = T,
    committee.averaging = T,
    prob.mean.weight = T,
    prob.mean.weight.decay = 'proportional' )
  
  myBiomodProj <- BIOMOD_Projection(
    modeling.output = myBiomodModelOut,
    new.env = Crete_raster,
    proj.name = 'current',
    selected.models = 'all',
    binary.meth = NULL,
    compress = 'xz',
    clamping.mask = F,
    output.format = '.img')    
  
  
  sink(type = "message")
  
  sink()
  
}



# Run parallel ------------------------------------------------------------

no_cores <- detectCores() - 1
cl <- makeSOCKcluster(no_cores)

clusterExport(cl, list("Crete_raster",
                       "Crete_species",
                       "sp_names",
                       "BIOMOD_FormatingData",
                       "BIOMOD_ModelingOptions",
                       "BIOMOD_Modeling",
                       "BIOMOD_Projection",
                       "get_evaluations",
                       "get_variables_importance",
                       "get_predictions",
                       "BIOMOD_EnsembleModeling",
                       "BIOMOD_Projection"),
              envir=environment())
#add get_evaluations


parLapply(cl, sp_names, MyBiomodSF)

