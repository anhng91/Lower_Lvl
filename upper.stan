data {
  int Ndata; 
  int Nind ;
  int Nhh ;
  vector[Ndata] year04_r; 
  vector[Ndata] year06_r; 
  vector[Ndata] year10_r; 
  vector[Ndata] year12_r;
  int X_ind_wo_dim ;
  int X_hh_dim ;
  int X_hh_wo_dim ;
  matrix[Nind, X_ind_wo_dim] X_ind_wo;
  matrix[Ndata, X_ind_wo_dim] X_ind_wo_full;
  matrix[Nhh, X_hh_dim] X_hh ;
  matrix[Nhh, X_hh_wo_dim] X_hh_wo ;
  int sick_dummy[Ndata];
  # Import from lower files 
    int N_zeta_0;
    int N_zeta_1;
    int zeta_observed_0[N_zeta_0]; 
    int zeta_observed_1[N_zeta_1];
    int N_zeta_observed;
    int zeta_observed_index[N_zeta_observed];
    int Nrepeat; 
    int repeat_1[Nrepeat]; 
    int repeat_2[Nrepeat]; 

  # Import from lower files 
    vector[Ndata] THETA;
    vector[Nind] GAMMA; 
    vector[Nhh] OMEGA; 
    vector[Nhh] R;  
}

parameters {
  vector[X_ind_wo_dim] beta_theta;
  vector[X_hh_wo_dim] beta_omega;
  vector[X_ind_wo_dim] beta_gamma;
  vector[X_ind_wo_dim] beta_zeta1;
  vector[X_ind_wo_dim] beta_zeta2; 
  vector[X_ind_wo_dim] beta_lambda;
  vector[X_hh_wo_dim] beta_r;
  real log_sthetabar;
  real log_nu;
  real log_sgamma;
  real log_somega;
  real log_sr; 
  real beta_06_1;
  real beta_04_1;
  real beta_10_1;
  real beta_12_1;
  real beta_06_2;
  real beta_04_2;
  real beta_10_2;
  real beta_12_2; 
  real corr_; 
}

model {
  vector[Ndata] mu_zeta1; 
  vector[Ndata] mu_zeta2; 
  vector[Ndata] mu_lambda; 
  beta_theta ~ normal(0,2);
  beta_omega ~ normal(0,2);
  beta_gamma ~ normal(0,2);
  beta_zeta1 ~ normal(0,2);
  beta_zeta2 ~ normal(0,2); 
  beta_lambda ~ normal(0,2);
  beta_r ~ normal(0,2);
  log_sthetabar ~ normal(0,2);
  log_nu ~ normal(0,2);
  log_sgamma ~ normal(0,2);
  log_somega ~ normal(0,2);
  log_sr ~ normal(0,2); 
  beta_06_1 ~ normal(0,2);
  beta_04_1 ~ normal(0,2);
  beta_10_1 ~ normal(0,2);
  beta_12_1 ~ normal(0,2);
  beta_06_2 ~ normal(0,2);
  beta_04_2 ~ normal(0,2);
  beta_10_2 ~ normal(0,2);
  beta_12_2 ~ normal(0,2);
  corr_ ~ uniform(0,1); 
  mu_lambda = (X_ind_wo_full * beta_lambda); 

  mu_zeta1 = (X_ind_wo_full * beta_zeta1) + beta_04_1 * year04_r + beta_06_1 * year06_r + beta_10_1 * year10_r 
    + beta_12_1 * year12_r; 
  mu_zeta2 = (X_ind_wo_full * beta_zeta2) + beta_04_2 * year04_r + beta_06_2 * year06_r + beta_10_2 * year10_r 
    + beta_12_2 * year12_r; 

  target += -sum(log((1 + exp(mu_zeta1[zeta_observed_index]) + exp(mu_zeta2[zeta_observed_index]))))
              + sum(mu_zeta1[zeta_observed_0]) - sum(log((1 + exp(mu_zeta1[zeta_observed_0]) + exp(mu_zeta2[zeta_observed_0]))))
              + sum(mu_zeta2[zeta_observed_1]) - sum(log((1 + exp(mu_zeta1[zeta_observed_1]) + exp(mu_zeta2[zeta_observed_1])))); 

  sick_dummy ~ bernoulli(exp(mu_lambda) ./ (1 + exp(mu_lambda))); 

  THETA ~ normal(X_ind_wo_full * beta_theta, exp(log_sr)); 
  OMEGA ~ normal(X_hh_wo * beta_omega, exp(log_somega)); 
  GAMMA ~ normal(X_ind_wo * beta_gamma, exp(log_sgamma)); 
  R ~ normal(X_hh_wo * beta_r, exp(log_sr)); 

  (THETA[repeat_2] - corr_ * THETA[repeat_1])*inv(exp(log_sthetabar)*sqrt(1 - square(corr_))) ~ normal(0, 1); 
}
