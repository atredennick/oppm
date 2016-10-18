##
##  R Functions for Parallelized Stan Fits for survival logistic model (for IPM)
##
##
##  Author:       Andrew Tredennick
##  Email:        atredenn@gmail.com
##  Date created: 10-18-2016
##



####
####  Regularization and Cross-Validation (collects lppd)
####
cv_oos <- function(i, n_chains){
  require(StanHeaders)
  require(rstan)
  require(matrixStats)
  
  k <- cv.s2.grid[i,2]
  fold.idx <- fold.idx.mat[,k] 
  yr.out <- yrs.vec[fold.idx]
  sd.now <- sd_vec[cv.s2.grid[i,1]]
  
  df_train <- subset(grow_data, year!=yr.out)
  
  ##  Create and scale interaction covariates
  climate_columns <- c(grep("ppt", colnames(df_train)),
                       grep("Tmean", colnames(df_train)))
  climate_matrix <- df_train[,climate_columns]
  climate_matrix <- scale(climate_matrix, center = TRUE, scale = TRUE)
  
  groups <- as.numeric(df_train$Group)
  G <- length(unique(df_train$Group))
  nyrs <- length(unique(df_train$year))
  W <- df_train$W
  yid <- as.numeric(as.factor(df_train$year))
  
  df_hold <- subset(grow_data, year==yr.out) 
  climate_matrix_oos <- df_hold[,climate_columns]
  for(j in 1:ncol(climate_matrix_oos)){
    climate_matrix_oos[,j] <- (climate_matrix_oos[,j] - clim_means[j])/clim_sds[j]
  }
  W_oos <- df_hold$W
  gid_out <- as.numeric(df_hold $Group) 
  
  ##  Set up data list for Stan
  if(length(gid_out)==1){
    datalist <- list(n=nrow(df_train), nyrs=nyrs, yid=yid,
                     ncovars=ncol(climate_matrix), y=df_train$logarea.t1, x=df_train$logarea.t0,
                     C=climate_matrix, W=W, ngrp=G, gid=groups, sd_beta=1,
                     npreds=nrow(df_hold), yhold=array(df_hold$logarea.t1,dim=1), xhold=array(df_hold$logarea.t0,dim=1),
                     Chold=climate_matrix_oos, Whold=array(W_oos,dim=1), gid_out=array(gid_out,dim=1))
  }
  if(length(gid_out)>1){
    datalist <- list(n=nrow(df_train), nyrs=nyrs, yid=yid,
                     ncovars=ncol(climate_matrix), y=df_train$logarea.t1, x=df_train$logarea.t0,
                     C=climate_matrix, W=W, ngrp=G, gid=groups, sd_beta=1,
                     npreds=nrow(df_hold), yhold=df_hold$logarea.t1, xhold=df_hold$logarea.t0,
                     Chold=climate_matrix_oos, Whold=W_oos, gid_out=gid_out)
  }
  
  pars <- c("log_lik")
  inits <- list()
  inits[[1]] <- list(a_mu=0, a=rep(0,nyrs), b1_mu=0.01,
                     b1=rep(0.01,nyrs), gint=rep(0,G), 
                     w=0, sig_b1=0.5, sig_a=0.5, 
                     tau=0.5, tauSize=0.5, sig_G=0.5, 
                     b2=rep(0,ncol(climate_matrix)))
  inits[[2]] <- list(a_mu=0.5, a=rep(0.5,nyrs), b1_mu=0.1,
                     b1=rep(0.1,nyrs), gint=rep(0.4,G), 
                     w=0.1, sig_b1=0.2, sig_a=0.2, 
                     tau=0.2, tauSize=0.2, sig_G=0.2, 
                     b2=rep(0.5,ncol(climate_matrix)))
  inits[[3]] <- list(a_mu=1, a=rep(1,nyrs), b1_mu=0.2,
                     b1=rep(0.2,nyrs), gint=rep(0.2,G), 
                     w=0.5, sig_b1=0.01, sig_a=0.01, 
                     tau=0.05, tauSize=0.05, sig_G=0.05, 
                     b2=rep(-0.5,ncol(climate_matrix)))
  if(n_chains > 1) {
    fit <- stan(fit = mcmc_oos, data=datalist, init=inits,
                pars=pars, chains=3, iter = 2000, warmup = 1000)
  }
  if(n_chains == 1) {
    fit <- stan(fit = mcmc_oos, data=datalist, init=list(inits[[1]]),
                pars=pars, chains=1, iter = 1000, warmup = 500)
  }
  
  waic_metrics <- waic(fit)
  lpd <- waic_metrics[["total"]]["lpd"]
  return(lpd)
}

