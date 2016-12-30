# inspect model

inv_logit <- function(x) {exp(x)/(1+exp(x))}

betas <- 
  samples %>% 
  dplyr::select(starts_with("beta")) %>% 
  # dplyr::select(-beta_sigma) %>% 
  get_intervals %>% 
  as.data.frame %>% 
  set_rownames(.$Coefficent) %>% 
  dplyr::select(Estimate) %>% 
  as.vector

mu_inter <-
  samples %>%
  dplyr::select(mu_inter) %>%
  get_intervals %>%
  as.data.frame %>%
  set_rownames(.$Coefficent) %>%
  dplyr::select(Estimate) %>%
  as.vector


phis <- 
  samples %>% 
  dplyr::select(starts_with("phi")) %>% 
  # dplyr::select(-phi_sigma) %>% 
  dplyr::select(-phi_inter) %>% 
  get_intervals %>% 
  as.data.frame %>% 
  magrittr::set_rownames(.$Coefficent) %>% 
  dplyr::select(Estimate) %>% 
  as.vector

phi_inter <- 
  samples %>% 
  dplyr::select(phi_inter) %>% 
  get_intervals %>% 
  as.data.frame %>% 
  set_rownames(.$Coefficent) %>% 
  dplyr::select(Estimate) %>% 
  as.vector
  

val_data <- jags_data %>% 
  dplyr::select(
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


logit_mu <- as.matrix(val_data) %*% as.matrix(betas)
mu <- inv_logit(
  mu_inter$Estimate + 
  logit_mu
)

log_phi <- as.matrix(val_data) %*% as.matrix(phis)
phi <- exp(phi_inter$Estimate + log_phi)

alpha <- mu * phi
beta <- (1-mu) * phi

est_props <- alpha / (alpha + beta)
act_props <- jags_data$hrc_votes / jags_data$n_votes

est_votes <- jags_data$n_votes * est_props
act_votes <- jags_data$hrc_votes


plot(log(act_votes),log(est_votes),
ylab = "log estimated votes", xlab = "log actual votes", pch = 3, ylim = c(10, 15), xlim = c(10, 15))
abline(a = 0, b = 1)

plot(act_votes,est_votes)
abline(a=0, b = 1)

county_desc <- 
  cbind.data.frame(
    log(alpha + beta),
    mu,
    jags_data$n_votes
  ) %>% 
  set_colnames(c("uniformity", "propensity", "size"))

ggplot(
  county_desc, 
  aes(x = uniformity, y = propensity, size = size)
) +
  geom_point(shape = 21, position = "jitter") + 
  theme(legend.position = "none")

a <- 
  sapply(1:length(alpha), function(x) dbeta(0:100/100, alpha[x], beta[x])) %>% 
  as.data.frame %>% 
  mutate(idx = 0:100/100) %>% 
  gather(idx, density) %>% 
  set_colnames(c("idx", "county", "density")) %>% 
  filter(county %in% sample(.$county, 100))

p <- ggplot(a, aes(x = idx, y = density, group = county)) + geom_line() + theme(legend.position = "NULL")

