plot_trace <- function(samples){
  samples %>%
    gather(coef, value) %>%
    group_by(coef) %>%
    mutate(row_number = row_number()) %>%
  ggplot +
    aes(x = row_number, y = value) +
    geom_line() +
    facet_grid(coef~., scales = 'free')
}

get_intervals <- function(samples){
  samples %>%
  gather(coef, value) %>%
  group_by(coef) %>%
  summarise_all(funs(
    estimate = median,
    upper = quantile(., 0.95),
    lower = quantile(., 0.05)
  )) %>%
    rename(Coefficent = coef,
           Estimate = estimate,
           `Lower CI` = lower,
           `Upper CI` = upper)
}

plot_coef_boxplots <- function(samples)
{
  samples %>% 
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
    geom_boxplot(
      coef = 1.58,
      outlier.shape = NA,
      width = 0.1,
      position = position_dodge(width=0.5)
    ) + 
    geom_hline(yintercept=0)
}

plot_coef_densityplots <- function(samples)
{
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
}




