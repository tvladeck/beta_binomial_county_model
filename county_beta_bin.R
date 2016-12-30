setwd("~/Dropbox/Gradient/Research/Election Analysis/Other")

# devtools::install_github("deleetdk/USA.county.data")

library(USA.county.data)
data(USA_county_data)
library(rjags)
library(readr)
library(dplyr)
library(coda)
library(ggplot2)
library(tidyr)
library(randomForest)
library(magrittr)


hrc_votes <- USA_county_data$results.clintonh
n_votes   <- USA_county_data$votes
state     <- USA_county_data$State


jags_data <- USA_county_data %>% 
  dplyr::select(
    White,
    SIRE_homogeneity,
    median_age,
    Diabetes,
    Gini.Coefficient,
    Median.Earnings.2010.dollars,
    At.Least.High.School.Diploma,
    Uninsured,
    Unemployment,
    Violent.crime
  ) %>% 
  scale %>% 
  as.data.frame %>% 
  na.roughfix

jags_data <- 
  cbind.data.frame(
    hrc_votes,
    n_votes,
    state,
    jags_data
  ) 

jags_data <- 
  jags_data[which(!(is.na(hrc_votes) | is.na(n_votes) | is.na(state))), ]

jags_data$hrc_votes <- 
  as.integer(as.character(jags_data$hrc_votes))

jags_data$n_votes <- 
  as.integer(as.character(jags_data$n_votes))



bug_file <- "county_binom_model.bugs.R"

jags <- jags.model(bug_file, data = list(
  'hrc_votes'     = jags_data$hrc_votes,
  'n_votes'       = jags_data$n_votes,
  'n_obs'         = nrow(jags_data),
  'white'         = jags_data$White,
  'age'           = jags_data$median_age,
  'diabetes'      = jags_data$Diabetes,
  'gini'          = jags_data$Gini.Coefficient,
  'income'        = jags_data$Median.Earnings.2010.dollars,
  'high_school'   = jags_data$At.Least.High.School.Diploma,
  'uninsured'     = jags_data$Uninsured,
  'unemployment'  = jags_data$Unemployment,
  'crime'         = jags_data$Violent.crime
))


samples <- coda.samples(
  jags,
  c(
    'mu_inter',
    'beta_white',
    'beta_age',
    'beta_gini',
    'beta_income',
    'beta_diabetes',
    'beta_high_school',
    'beta_uninsured',
    'beta_unemployment',
    'beta_crime',
    'phi_inter',
    'phi_white',
    'phi_age',
    'phi_gini',
    'phi_income',
    'phi_diabetes',
    'phi_high_school',
    'phi_uninsured',
    'phi_unemployment',
    'phi_crime'
  ),
  10000
)

# without lognormal
# saveRDS(samples, "samples_1.rds")

# with lognormal + model variance
# saveRDS(samples, "samples_2.rds")

# without lognormal + beta and phi variance
# saveRDS(samples, "samples_3.rds")

# without lognormal, with beta intercept
saveRDS(samples, "samples_4.rds")

# samples_mcmc <- readRDS("samples_1.rds")
# samples <- as.data.frame(samples_mcmc[[1]])

