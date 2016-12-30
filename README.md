# This is a regression of the US presidential election results

Data for the regression is available [here](https://github.com/Deleetdk/USA.county.data)

The structure of the regression is roughly as follows:

+ The number of votes for Hillary is distributed `binomial` in the average propensity to vote for her in that county and the number of votes cast in total
+ The average propensity to vote for her is distributed `beta` in a reparameterization of the normal `alpha` and `beta` parameters - [see this link](http://stats.stackexchange.com/questions/41536/how-can-i-model-a-proportion-with-bugs-jags-stan)
+ The reparameterization is as follows:
  + `alpha = mu * phi`
  + `beta  = (1-mu) * phi`
  + `mu` is expected value of the beta distribution and can be interpreted as the average vote share for HRC
  + `phi` is a polarization parameter (low values imply low density of the beta distribution in the center of its support, high values the opposite), and can be interpreted as the polarization of the county
+ `mu` is a linear function of covariates with a logistic link function
+ `phi` is a linear function of covariates with an exponential link function
+ the coefficients for `mu` and `phi` are estimated from the data 
+ the following are the predictors (all at the county level):
  + the proportion of residents that are white
  + the median age
  + the prevalance of diabetes (a proxy for overall health levels)
  + the median income
  + the proportion of residents that have a high school diploma
  + the uninsurance rate
  + the unemployment rate
  + the rate of violent crime

