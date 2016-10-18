
##
##  R script for growth model as run on Utah State University's
##  High Performance Computing system.
##
##  Script takes command line argument 'do_grid' to run all levels of
##  regularization and each cross-validation set in parallel. Script launches
##  n*k models: 'n' prior standard deviations and 'k' validation sets.
##
##  Saved output:
##    1. Summed log pointwise predictive density based on 
##       out-of-sample predictions
##
##  Author:       Andrew Tredennick
##  Email:        atredenn@gmail.com
##  Date created: 9-27-2016
##

# For PC run or HPC run
run_on_hpc <- FALSE

# Change state and species four letter code here...
do_state <- "Idaho"
do_species <- "ARTR"
dataframe_file <- paste0("grow_apriori_climate_", do_state, ".RDS")


####
####  Set SD Prior and CV Set from Command Line Arguments
####
if(run_on_hpc==TRUE){
  args <- commandArgs(trailingOnly = F)
  myargument <- args[length(args)]
  myargument <- sub("-","",myargument)
  do_grid <- as.numeric(myargument)
}

if(run_on_hpc==FALSE){
  do_grid <- 1
}



####
####  Load Libraries and Subroutines
####
library(rstan)
library(StanHeaders)
library(plyr)
library(reshape2)
library(ggmcmc)
library(matrixStats)

source("waic_fxns.R")
source("growth_fxns.R")



####
####  Load Growth Data
####
if(run_on_hpc==TRUE) { in_dir <- "./" }
if(run_on_hpc==FALSE) { in_dir <-  "../../../data/fitting_dataframes/apriori_climate/"}

grow_data <- readRDS(paste0(in_dir,dataframe_file))[[do_species]]



####
####  Compile Stan Model
####
holdyear <- unique(grow_data$year)[1] # grab first year as holdout
train_data <- subset(grow_data, year!=holdyear)

##  Create and scale interaction covariates
climate_columns <- c(grep("ppt",colnames(grow_data)),
                     grep("Tmean", colnames(grow_data)))
climate_matrix <- train_data[,climate_columns]

# Get scalers for climate covariates from training data
clim_means <- colMeans(climate_matrix)
clim_sds <- apply(climate_matrix, 2, FUN = sd)
climate_matrix <- scale(climate_matrix, center = TRUE, scale = TRUE)
groups <- as.numeric(train_data$Group)
G <- length(unique(train_data$Group))
nyrs <- length(unique(train_data$year))
W <- train_data$W
yid <- as.numeric(as.factor(train_data$year))

# Holdout data
hold_data <- subset(grow_data, year==holdyear)
climate_matrix_oos <- hold_data[,climate_columns]
for(j in 1:ncol(climate_matrix_oos)){
  climate_matrix_oos[,j] <- (climate_matrix_oos[,j] - clim_means[j])/clim_sds[j]
}
W_oos <- hold_data$W
gid_out <- as.numeric(hold_data$Group) 

if(length(gid_out)==1){
  datalist <- list(n=nrow(train_data), nyrs=nyrs, yid=yid,
                   ncovars=ncol(climate_matrix), y=train_data$logarea.t1, x=train_data$logarea.t0,
                   C=climate_matrix, W=W, ngrp=G, gid=groups, sd_beta=1,
                   npreds=nrow(hold_data), yhold=array(hold_data$logarea.t1,dim=1), xhold=array(hold_data$logarea.t0,dim=1),
                   Chold=climate_matrix_oos, Whold=array(W_oos,dim=1), gid_out=array(gid_out,dim=1))
}
if(length(gid_out)>1){
datalist <- list(n=nrow(train_data), nyrs=nyrs, yid=yid,
                 ncovars=ncol(climate_matrix), y=train_data$logarea.t1, x=train_data$logarea.t0,
                 C=climate_matrix, W=W, ngrp=G, gid=groups, sd_beta=1,
                 npreds=nrow(hold_data), yhold=hold_data$logarea.t1, xhold=hold_data$logarea.t0,
                 Chold=climate_matrix_oos, Whold=W_oos, gid_out=gid_out)
}
pars <- c("log_lik")
mcmc_oos <- stan(file="grow_oos.stan", data=datalist, 
                 pars=pars, chains=0)


####
####  Set up CV x Regularization grid
####
n.beta <- 24
sd_vec <- seq(0.1,1.5,length.out = n.beta)
yrs.vec <- unique(grow_data$year)
K <- length(yrs.vec)
cv.s2.grid <- expand.grid(1:n.beta,1:K)
n.grid <- dim(cv.s2.grid)[1]
fold.idx.mat <- matrix(1:length(yrs.vec),ncol=K)



####
####  Source Growth Model Function and Fit Model
####
if(run_on_hpc==TRUE) num_chains <- 3
if(run_on_hpc==FALSE) num_chains <- 1
out_lpd <- cv_oos(do_grid, n_chains = num_chains)
saveRDS(out_lpd, paste0(do_state, "_", do_species,"_oos_cv_dogrid_",do_grid,".RDS"))

