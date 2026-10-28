
library(tidyverse)
library(truncnorm)
library(GGally)
library(pmxcopula)

# Function to calculate summary statistics
summ <- function(dat){
  
  dat_name <- deparse(substitute(dat))
  
  dat <- dat %>% dplyr::select(-SEX)
  
  means <- dat %>% 
    summarise(across(everything(), mean)) %>% 
    mutate(across(everything(), ~round(.x, digits=2))) %>% 
    pivot_longer(cols=everything(), names_to="name", values_to="mean")
  
  sds <- dat %>% 
    summarise(across(everything(), sd)) %>% 
    mutate(across(everything(), ~round(.x, digits=2))) %>% 
    pivot_longer(cols=everything(), names_to="name", values_to="sd")
  
  sum <- left_join(means, sds) %>% 
    mutate(value=paste0(mean, " (", sd, ")")) %>% 
    dplyr::select(-mean, -sd) %>% 
    rename("{dat_name}":=value)
  
  return(sum)
  
}

# Function to simulate from truncated normal distribution
sim_from_md <- function(num, x, data){
  rtruncnorm(num, a=0, b=Inf, mean=mean(data[[x]]), sd=sd(data[[x]]))
}

# Function to generate virtual populations using marginal distribution method
sim_vp_from_md <- function(pop, nsub, sex, seed, nsim = 1) {
  withr::with_seed(seed, {
    map_dfr(seq_len(nsim), function(i) {
      
      map_dfc(
        set_names(cont_cov),
        ~ sim_from_md(nsub, .x, pop)
      ) %>%
        mutate(
          SEX = sex,
          simulation_nr = i
        )
    })
  })
}

# Function to generate virtual population from multivariate normal distribution
sim_vp_from_mvn <- function(pop, nsub, sex, seed, nsim = 1) {
  withr::with_seed(seed, {
    pop_cov <- pop %>% dplyr::select(all_of(cont_cov))
    map_dfr(seq_len(nsim), function(i) {
      MASS::mvrnorm(
        n = nsub,
        mu = colMeans(pop_cov),
        Sigma = cov(pop_cov)
      ) %>%
        as_tibble(.name_repair = ~ cont_cov) %>%
        mutate(SEX = sex, 
               simulation_nr = i)
    })
  })
}

# Function for combined marginal and dependency plot
plot_vp <- function(sim_data, obs_data) {
  
  sim <- sim_data %>%
    dplyr::select(all_of(cont_cov), simulation_nr)
  
  obs <- obs_data %>%
    dplyr::select(all_of(cont_cov))
  
  p1 <- plot_1dist(sim_data = sim, obs_data = obs, sim_nr = 100, var = cont_cov)
  
  p2 <- donutVPC(sim_data = sim, obs_data = obs, sim_nr = 100, cores=6)
  
  p1 / p2
}


