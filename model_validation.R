# inspect model

source("bugs_plots.R")

inv_logit <- function(x) {exp(x)/(1+exp(x))}

mus <- 
  samples %>% 
  dplyr::select(starts_with("mu"), -mu_inter) %>% 
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


logit_mu <- as.matrix(val_data) %*% as.matrix(mus)
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

par(mfrow = c(2,2))

plot(act_votes,est_votes, pch = 3, 
     xlab = "actual votes", 
     ylab = "estimated votes",
     main = "Actual vs. Estimated Votes")
abline(a=0, b = 1)

plot(log(act_votes),log(est_votes),
  ylab = "log estimated votes", 
  xlab = "log actual votes", 
  pch = 3, 
  main = "Log Actual vs. Estimated")
abline(a = 0, b = 1)

plot(log(act_votes),log(est_votes),
  ylab = "log estimated votes", 
  xlab = "log actual votes", 
  pch = 3, 
  ylim = c(10, 15), xlim = c(10, 15),
  main = "Log Actual vs. Estimated",
  sub = "(Counties w/ more than 20k Votes)")
abline(a = 0, b = 1)

par(mfrow=c(1,1))

#### trace plots ####

mu_trace <- 
  samples %>% 
  select(starts_with("mu")) %>% 
  plot_trace + 
  theme(panel.background = element_blank()) + 
  scale_x_continuous(name = "iteration") + 
  scale_y_continuous(name = "", breaks = NULL) + 
  theme(strip.text.y = element_text(angle = 0, hjust = 0), 
        strip.background = element_blank())

phi_trace <- 
  samples %>% 
  select(starts_with("phi")) %>% 
  plot_trace + 
  theme(panel.background = element_blank()) + 
  scale_x_continuous(name = "iteration") + 
  scale_y_continuous(name = "", breaks = NULL) + 
  theme(strip.text.y = element_text(angle = 0, hjust = 0), 
        strip.background = element_blank())

county_desc <- 
  cbind.data.frame(
    log(phi),
    mu,
    jags_data$n_votes
  ) %>% 
  set_colnames(c("uniformity", "propensity", "size"))

ggplot(
  county_desc, 
  aes(x = uniformity, y = propensity, size = size)
) +
  geom_point(shape = 21, position = "jitter") + 
  theme(legend.position = "none", panel.background = element_blank()) +
  scale_x_continuous(breaks = NULL, name = "") + 
  scale_y_continuous(breaks = c(0.2, 0.8), 
                     labels = c("80% Trump", "80% Clinton"), name = "",
                     position = "right") + 
  annotate("segment", 
           x = min(county_desc$uniformity), 
           xend = min(county_desc$uniformity), 
           y = min(county_desc$propensity), 
           yend = min(county_desc$propensity) + .15, 
           arrow = arrow(angle = 10, length = unit(0.4, "cm"))) +
  annotate("segment", 
           x = min(county_desc$uniformity), 
           xend = min(county_desc$uniformity) + 1, 
           y = min(county_desc$propensity), 
           yend = min(county_desc$propensity), 
           arrow = arrow(angle = 10, length = unit(0.4, "cm"))) +
  annotate("text", 
           label = "Propensity", 
           x = min(county_desc$uniformity) + 0.1, 
           y = min(county_desc$propensity) + 0.05, 
           angle = 90,
           hjust = 0) +
    annotate("text", 
           label = "Uniformity", 
           x = min(county_desc$uniformity) + 0.45, 
           y = min(county_desc$propensity) + 0.025, 
           angle = 0,
           hjust = 0)

mu_coefs_plot <- 
  samples %>% 
  select(starts_with("mu")) %>% 
  plot_coef_boxplots() + 
  scale_x_discrete(
    breaks = c("mu_inter", "mu_white", "mu_uninsured", "mu_diabetes",
               "mu_income", "mu_crime", "mu_age", "mu_gini", 
               "mu_high_school", "mu_unemployment"),
    labels = c("intercept", "white", "uninsured", "diabetes", "income", 
               "crime", "age", "gini", "high school", "unemployment"),
    name = "") +
  scale_y_continuous(name = "<<< Trump    |    HRC >>>") +
  coord_flip() + 
  theme(panel.background = element_blank())

phi_coefs_plot <- 
  samples %>% 
  select(phi_inter, starts_with("phi")) %>% 
  plot_coef_boxplots() + 
  scale_x_discrete(
    breaks = c("phi_gini", "phi_high_school", "phi_age",
               "phi_uninsured", "phi_unemployment",
               "phi_crime", "phi_income", "phi_white",
               "phi_diabetes", "phi_inter"),
    labels = c("gini", "high school", "age",
               "uninsured", "unemployment", "crime",
               "income", "white", "diabetes", "intercept"),
    name = "") +
  scale_y_continuous(name = "<<< Less uniformity    |    More uniformity >>>") +
  coord_flip() + 
  theme(panel.background = element_blank())
  

dbeta(0:100/100, 10, 1) %>% 
  as.data.frame %>% 
  set_colnames(c("density"))  %>% 
  ggplot(aes(y = density, x = 0:100/100)) + 
  geom_line() + 
  scale_y_continuous(breaks = NULL) + 
  scale_x_continuous(name = "") + 
  theme(panel.background = element_blank())



beta_dists_plot <-
  sapply(1:length(alpha), function(x) dbeta(0:100/100, alpha[x], beta[x])) %>% 
  as.data.frame %>% 
  mutate(idx = 0:100/100) %>% 
  gather(idx, density) %>% 
  set_colnames(c("idx", "county", "density")) %>% 
  filter(county %in% sample(.$county, 100)) %>% 
  ggplot(aes(x = idx, y = density, group = county)) + geom_line() + theme(legend.position = "NULL")

