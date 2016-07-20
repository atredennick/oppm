##################################################################
##  makeFittingDataframes.R: fetches demographic data and makes ##
##  R dataframes for fitting vital rate statistical models      ##
##################################################################

# Created by ATT on July 20, 2016
# Updated by ... on .............

# !!! SET WORKING DIRECTORY TO SOURCE FILE LOCATION !!! #

# Clear the workspace
rm(list=ls(all.names = TRUE))



####
####  MAKE DATAFRAMES FOR A PRIORI CLIMATE COVARIATES
####
# Source import function
source("./fetchDemoData_aprioriClimate.R")

# Loop over states and species
# Make a big list of single species dataframes
sites_species <- read.csv("../data/site_species_list.csv")
states <- unique(sites_species$site)

grow_aprioriclimate_list <- list()
surv_aprioriclimate_list <- list()
recr_aprioriclimate_list <- list()
for(do_state in states){
  species <- sites_species[which(sites_species$site==do_state), "species"]
  grow_aprioriclimate_spplist <- list()
  surv_aprioriclimate_spplist <- list()
  recr_aprioriclimate_spplist <- list()
  for(do_species in species){
    data_dir <- paste0("../data/",do_state,"/")
    growData <- fetchDemoData_aprioriClimate(doSpp=do_species, state=do_state, grow=TRUE, dataDir=data_dir)
    survData <- fetchDemoData_aprioriClimate(doSpp=do_species, state=do_state, surv=TRUE, dataDir=data_dir)
    recrData <- fetchDemoData_aprioriClimate(doSpp=do_species, state=do_state, recr=TRUE, dataDir=data_dir)
    
    grow_aprioriclimate_spplist[[do_species]] <- growData
    surv_aprioriclimate_spplist[[do_species]] <- survData
    recr_aprioriclimate_spplist[[do_species]] <- recrData
    
    cat("\n")
    cat(paste("done with species", do_species, "for", do_state))
  }
  grow_aprioriclimate_list[[do_state]] <- grow_aprioriclimate_spplist
  surv_aprioriclimate_list[[do_state]] <- surv_aprioriclimate_spplist
  recr_aprioriclimate_list[[do_state]] <- recr_aprioriclimate_spplist
}

# Save lists of data frames
saveRDS(grow_aprioriclimate_list, file = "../data/fitting_dataframes/grow_aprioriClimate.RDS")
saveRDS(surv_aprioriclimate_list, file = "../data/fitting_dataframes/surv_aprioriClimate.RDS")
saveRDS(recr_aprioriclimate_list, file = "../data/fitting_dataframes/recr_aprioriClimate.RDS")



