data{
  // Training data
  int<lower=0> n;               // number of observations
  int<lower=0> nyrs;            // number of years
  int<lower=0> yid[n];          // year id for each observation
  int<lower=0> ncovars;         // number of climate covariates
  real<lower=0> sd_beta;        // prior sdev for climate effects
  int<lower=0> ngrp;            // number of groups
  int<lower=0> gid[n];          // group id for each observation
  vector[n] y;                  // observation vector (logarea.t1)
  vector[n] x;                  // size vector (logarea.t0)
  matrix[n,ncovars] C;          // climate covariate matrix
  matrix[n,2] W;                // crowding matrix for species interactions
  
  // Heldout data
  int<lower=0> npreds;          // number of predictions to make
  int<lower=0> gid_out[npreds]; // group id holdout
  vector[npreds] yhold;         // prediction observation vector
  vector[npreds] xhold;         // prediction size vector
  matrix[npreds,Covs] Chold;    // prediction climate covariate matrix
  matrix[npreds,2] Whold;       // prediction crowding matrix
}

parameters{
  real a_mu;                    // mean intercept    
  vector[nyrs] a;               // random year intercepts
  real b1_mu;                   // mean size effect
  vector[nyrs] b1;              // random year size effects
  vector[ncovars] b2;           // climate effects
  vector[2] w;                  // crowding effects
  real gint[ngrp];              // quadrat group effects
  real tau;                     // size-based variance parameter 1
  real tauSize;                 // size-based variance parameter 2
  real<lower=0> sig_a;          // standard deviation for yearly intercepts
  real<lower=0> sig_b1;         // standard deviation for yearly size effects
  real<lower=0> sig_g;          // standard deviation for quadrat group effects
}

transformed parameters{
  vector[n] mu;                 // deterministic model prediction
  real<lower=0> sigma[n];       // size-based variance; estimated in model
  vector[n] climate;            // sum of covar*coef for climate
  vector[n] crowding;           // sum of W*w for crowding
  climate <- C*b2;              // vectorized climate effects
  crowding <- W*w;              // vectorized crowding effects
  
  // Determinstic model
  for(i in 1:n){
    mu[i] <- a[yid[i]] + gint[gid[i]] + b1[yid[i]]*x[i] + crowding[i] + climate[i];
    sigma[n] <- sqrt((fmax(tau*exp(tauSize*mu[i]), 0.0000001)));  // don't allow variance to be vanishingly small with fmax
  }
}

model{
  // Priors
  a_mu ~ normal(0,10);
  w ~ normal(0,10);
  b1_mu ~ normal(0,10);
  tau ~ normal(0,10);
  tauSize ~ normal(0,10);
  sig_a ~ cauchy(0,5);
  sig_b1 ~ cauchy(0,5);
  sig_g ~ cauchy(0,5);
  b2 ~ normal(0, sd_beta);
  for(g in 1:ngrp)
    gint[g] ~ normal(0, sig_g);
  for(y in 1:nyrs){
    a[y] ~ normal(a_mu, sig_a);
    b1[y] ~ normal(b1_mu, sig_b1);
  }

  // Likelihood
  y ~ normal(mu, sigma);                 // vectorized likelihood
}

generated quantities {
  real int_t;                            // initialize scalar for random year intercept
  real dens_dep;                         // initialize scalar for random year size effect
  vector[npreds] climpred;               // vector for climate effects
  vector[npreds] crowdpred;              // vector for crowding effects
  vector[npreds] sigmahat;               // vector for size-based standarad deviations
  vector[npreds] muhat;                  // out-of sample prediction vector
  vector[npreds] log_lik;                // vector for computing log pointwise predictive density
  climpred <- Chold*b2;                  // vectorized climate effects
  crowdpred <- Whold*w;                  // vectorized crowding effects
  int_t <- normal_rng(a_mu, sig_a);      // draw random year intercept
  dens_dep <- normal_rng(b1_mu, sig_b1); // draw random year size effect
  
  // Make predictions and draw log-likelihood
  for(i in 1:npreds){
    muhat[i] <- int_t + gint[gid_out[i]] + b1_mu*xhold[i] + crowdpred[i] + climpred[i];
    sigmahat[i] <- sqrt((fmax(tau*exp(tauSize*muhat[i]), 0.0000001))); 
    log_lik[i] <- normal_log(yhold[i], muhat[i], sigmahat[i]);
  }
}