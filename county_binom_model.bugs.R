model {

  for(i in 1:n_obs){
    
    hrc_votes[i] ~ dbin(p[i], n_votes[i])
    p[i] ~ dbeta(alpha[i], beta[i])
    
    # reparameterization of the beta distribution taken from this site
    # http://bit.ly/2i880Oj
    
    alpha[i] <- mu[i] * phi[i]
    beta[i]  <- (1-mu[i]) * phi[i]


    logit(mu[i]) <-
      mu_inter +
      beta_white * white[i] +
      beta_age * age[i] +
      beta_diabetes * diabetes[i] + 
      beta_gini * gini[i] + 
      beta_income * income[i] +
      beta_high_school * high_school[i]+
      beta_uninsured * uninsured[i] +
      beta_unemployment * unemployment[i] +
      beta_crime * crime[i]
    
    log(phi[i]) <- 
      phi_inter + 
      phi_white * white[i] +
      phi_age * age[i] +
      phi_diabetes * diabetes[i] + 
      phi_gini * gini[i] + 
      phi_income * income[i] +
      phi_high_school * high_school[i]+
      phi_uninsured * uninsured[i] +
      phi_unemployment * unemployment[i] +
      phi_crime * crime[i] 
  
  }
  
  mu_inter ~ dnorm(0, 0.00001)
  mu_white ~ dnorm(0, 0.00001)
  mu_age ~ dnorm(0, 0.00001)
  mu_diabetes ~ dnorm(0, 0.00001)
  mu_gini ~ dnorm(0, 0.00001)
  mu_income ~ dnorm(0, 0.00001)
  mu_high_school ~ dnorm(0, 0.00001)
  mu_uninsured ~ dnorm(0, 0.00001)
  mu_unemployment ~ dnorm(0, 0.00001)
  mu_crime ~ dnorm(0, 0.00001)
  
  phi_inter ~ dnorm(0, 0.00001)
  phi_white ~ dnorm(0, 0.00001)
  phi_age ~ dnorm(0, 0.00001)
  phi_diabetes ~ dnorm(0, 0.00001)
  phi_gini ~ dnorm(0, 0.00001)
  phi_income ~ dnorm(0, 0.00001)
  phi_high_school ~ dnorm(0, 0.00001)
  phi_uninsured ~ dnorm(0, 0.00001)
  phi_unemployment ~ dnorm(0, 0.00001)
  phi_crime ~ dnorm(0, 0.00001)
}



