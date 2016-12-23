model {

  for(i in 1:n_obs){
    hrc_votes[i] ~ dbin(p[i], n_votes[i])
    p[i] ~ dbeta(alpha[i], beta[i])
    
    # reparameterization of the beta distribution taken from this site
    # http://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
    
    alpha[i] <- (((1-mu[i]) / (sigma[i])^2 ) - 1/mu[i]) * mu[i]^2
    beta[i]  <- alpha[i] * ((1 / mu[i]) - 1)

    logit(mu[i]) <- 
      beta_white * white[i] +
      beta_age * age[i] +
      beta_diabetes * diabetes[i] + 
      beta_gini * gini[i] + 
      beta_income * income[i] +
      beta_high_school * high_school[i]+
      beta_uninsured * uninsured[i] +
      beta_unemployment * unemployment[i] +
      beta_crime * crime[i] 
    
    
    sigma[i] ~ dunif(0,.25)
  }

  beta_white ~ dnorm(0, 0.00001)
  beta_age ~ dnorm(0, 0.00001)
  beta_diabetes ~ dnorm(0, 0.00001)
  beta_gini ~ dnorm(0, 0.00001)
  beta_income ~ dnorm(0, 0.00001)
  beta_high_school ~ dnorm(0, 0.00001)
  beta_uninsured ~ dnorm(0, 0.00001)
  beta_unemployment ~ dnorm(0, 0.00001)
  beta_crime ~ dnorm(0, 0.00001)
}



