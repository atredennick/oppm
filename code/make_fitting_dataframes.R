#####################################################################
##  make_fitting_dataframes.R: collates raw data into R dataframes ##
##  ready for statistical modeling. These are made once, and used  ##
##  across all other analyses for consistency.                     ##
#####################################################################

### !!! SET WORKING DIRECTORY TO SOURCE FILE LOCATION !!! ###

### Author: Andrew Tredennick
### Last update: 09-27-2016

##  Clear the workspace
rm(list=ls(all.names = TRUE))

##  Load libraries, and other preliminaries
library(plyr)
library(reshape2)
library(dplyr)
source("fetchDemoData_aprioriClimate.R")

data_dir <- "../data/site_data"
num_states <- 5
if(length(list.files(data_dir)) == num_states) { state_names <- list.files("../data/site_data") }
if(length(list.files(data_dir)) != num_states) { stop("check data directory, too many/few sites") }



####
####  CREATE DATA FRAMES AND SAVE -- A PRIORI CLIMATE COVARIATES
####
saving_dir <- "../data/fitting_dataframes/apriori_climate/"
for(state_now in state_names){
  state_species <- filter(read.csv("../data/aux_data/site_species_list.csv"),
                          site == state_now)
  surv_list <- list()
  grow_list <- list()
  recr_list <- list()
  
  for(species_now in state_species$species){
    dir_now <- paste0(data_dir,"/",state_now)
    surv_dat <- fetchDemoData_aprioriClimate(doSpp = species_now, state=state_now, 
                                             surv=TRUE, dataDir = dir_now)
    grow_dat <- fetchDemoData_aprioriClimate(doSpp = species_now, state=state_now, 
                                             grow=TRUE, dataDir = dir_now)
    recr_dat <- fetchDemoData_aprioriClimate(doSpp = species_now, state=state_now, 
                                             recr=TRUE, dataDir = dir_now)
    surv_list[[species_now]] <- surv_dat
    grow_list[[species_now]] <- grow_dat
    recr_list[[species_now]] <- recr_dat
  } # next species
  
  saveRDS(surv_list, paste0(saving_dir, "surv_apriori_climate_", state_now, ".RDS"))
  saveRDS(grow_list, paste0(saving_dir, "grow_apriori_climate_", state_now, ".RDS"))
  saveRDS(recr_list, paste0(saving_dir, "recr_apriori_climate_", state_now, ".RDS"))
} # next state

