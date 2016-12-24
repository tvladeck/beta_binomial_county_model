# inspect model

inv_logit <- function(x) {exp(x)/(1+exp(x))}

coefficients <- 
  samples %>% 
  select(starts_with("beta")) %>% 
  get_intervals %>% 
  as.data.frame %>% 
  set_rownames(.$Coefficent) %>% 
  select(Estimate) %>% 
  as.vector

phi <- 
  samples %>% 
  select(starts_with("phi")) %>% 
  get_intervals %>% 
  as.data.frame %>% 
  set_rownames(.$Coefficent) %>% 
  select(Estimate) %>% 
  as.vector
  

val_data <- jags_data %>% 
  select(
    # needs to be in same order as coefficient colnames
    median_age, 
    Violent.crime, 
    Diabetes,
    Gini.Coefficient,
    At.Least.High.School.Diploma,
    Median.Earnings.2010.dollars, 
    Unemployment, 
    Uninsured,
    White
  )


logit_mu <- as.matrix(val_data) %*% as.matrix(coefficients)
mu <- inv_logit(logit_mu)
alpha <- mu * phi$Estimate
beta <- (1-mu) * phi$Estimate
est_props <- alpha / (alpha + beta)
act_props <- jags_data$hrc_votes / jags_data$n_votes

est_votes <- jags_data$n_votes * est_props
act_votes <- jags_data$hrc_votes

plot(est_votes, act_votes)
abline(a = 0, b  = 1)


