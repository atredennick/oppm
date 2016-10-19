##
##  R script for fitting vital rate regressions using functional linear models
##  (splines) for climate effects.
##
##
##  Saved output:
##    1. A list of fitted parameters.
##
##  Author:       Andrew Tredennick
##  Email:        atredenn@gmail.com
##  Date created: 9-27-2016
##



####
####  Load Libraries and Subroutines
####
library(mgcv)
library(plyr)
library(reshape2)

source("fetch_demo_data_flm.R")
source("growth_fxns.R")