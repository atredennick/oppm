#####################################################################
##  make_fitting_dataframes.R: collates raw data into R dataframes ##
##  ready for statistical modeling. These are made once, and used  ##
##  across all other analyses for consistency.                     ##
#####################################################################

### !!! SET WORKING DIRECTORY TO SOURCE FILE LOCATION !!! ###

### Author: Andrew Tredennick
### Last update: 07-15-2016

##  Clear the workspace
rm(list=ls(all.names = TRUE))

##  Load libraries, and other preliminaries
library(plyr)
library(reshape2)

data_dir <- "../data/"
num_sites <- 5
if(length(list.files) == num_sites) { site_names <- list.files("../data/") }
if(length(list.files) != num_sites) { stop("check data directory, too many/few sites") }



####
####  Create survival data frames
####
# Loop over sites, then species within site
# Returns a list of dataframes for each species
surv_fname <- "survD.csv"





