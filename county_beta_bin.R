setwd("~/Dropbox/Gradient/Research/Election Analysis/Other")
library(rjags)
library(readr)
library(dplyr)
library(coda)
library(ggplot2)
library(tidyr)
library(randomForest)

tidy_data <- read.csv("tidy_data.csv")

hrc_votes <- tidy_data$results.clintonh
n_votes   <- tidy_data$votes

jags_data <- tidy_data %>% 
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
    jags_data
  ) 

jags_data <- 
  jags_data[which(!(is.na(hrc_votes) | is.na(n_votes))), ]

bug_file <- "county_binom_model.bugs.R"

jags <- jags.model(bug_file, data = list(
  'hrc_votes'     = hrc_votes,
  'n_votes'       = n_votes,
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
    'p',
#    'alpha',
#    'beta',
#    'inter',
    'beta_white',
    'beta_age',
    'beta_gini',
    'beta_income',
    'beta_diabetes',
    'beta_high_school',
    'beta_uninsured',
    'beta_unemployment',
    'beta_crime'
  ),
  10000
)



# saveRDS(samples, "beta_bin_samples.rds")

samples <- readRDS("beta_bin_samples.rds")
samples <- as.data.frame(samples[[1]])

# samples <- 
#   samples %>% 
#   select(starts_with("p"), inter, starts_with("beta_"))

samples %>% 
  dplyr::select(inter, starts_with("beta_")) %>%  
  gather(coef, value) %>% 
  (function(df){
    df_o <- 
      df %>% 
      group_by(coef) %>% 
      summarize(average = mean(value)) %>% 
      arrange(desc(average)) %>% 
      mutate(order = 1:nrow(.))
    
    df <- 
      df %>% 
      left_join(df_o, by = "coef") %>% 
      mutate(coef = ordered::order_by(coef, order)) %>% 
      dplyr::select(-average, -order)
    
    return(df)
  }) %>% 
  ggplot(aes(y = value, x = coef)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_hline(yintercept=0)

samples %>% 
  # dplyr::select(starts_with("state_predictor")) %>%  
  gather(coef, value) %>% 
  (function(df){
    df_o <- 
      df %>% 
      group_by(coef) %>% 
      summarize(average = mean(value)) %>% 
      arrange(desc(average)) %>% 
      mutate(order = 1:nrow(.))
    
    df <- 
      df %>% 
      left_join(df_o, by = "coef") %>% 
      mutate(coef = ordered::order_by(coef, order)) %>% 
      dplyr::select(-average, -order)
    
    return(df)
  }) %>% 
  ggplot(aes(x = value, fill = coef)) + 
    geom_density(alpha = 0.7) + 
    theme(legend.position = "none") + 
    scale_fill_hue()