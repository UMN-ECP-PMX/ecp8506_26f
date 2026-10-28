
# Library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(GGally)
library(pmxcopula)
library(rvinecopulib)
library(patchwork)
library(cowplot)

source(here("wk11", "script", "function-helper.R"))

# Load population data ----------------------------------------------------

# Load data for analysis 
dataDir <- here("wk11/data")
pop <- read.csv(file.path(dataDir, "pop.csv")) %>% dplyr::select(-BMI)

# Separate the dataset based on SEX
pop_male    <- pop %>% filter(SEX == "male")
pop_female  <- pop %>% filter(SEX == "female")

# Check population
summ(pop) %>% knitr::kable()

cont_cov <- c("AGE", "WT", "HT", "EGFR", "ALB")
cat_cov <- c("SEX")

png(here("wk11", "pics", "pop.png"), height = 720, width = 720)
p <- ggpairs(pop, columns=cont_cov, 
              aes(color=SEX, alpha=0.5))
print(p)
dev.off()

# Marginal distributions --------------------------------------------------

#' `Virtual population generation method evaluation`

## Simulation virtual pop with n=100 simulation replicates
vp1 <- bind_rows(
  sim_vp_from_md(pop_male, nrow(pop_male), "male", 12315, nsim=100), 
  sim_vp_from_md(pop_female, nrow(pop_female), "female", 53124, nsim=100)
)

## Evaluation of virtual pop generation method
png(here("wk11", "pics", "md1.png"), height = 1440, width = 1440)
p1a <- plot_vp(vp1, pop)
print(p1a)
dev.off()

#' `Virtual population generation`

## Generate virtual populations using marginal distribution method
vp1 <- bind_rows(
  sim_vp_from_md(pop_male, nrow(pop_male), "male", 12315, nsim=1), 
  sim_vp_from_md(pop_female, nrow(pop_female), "female", 53124, nsim=1)
)

## Check summary statistics
left_join(summ(pop), summ(vp1)) %>% knitr::kable()

pop %>% count(SEX)
vp1 %>% count(SEX)

## Simulation versus observation distribution comparison
png(here("wk11", "pics", "md2.png"), height = 720, width = 1440)
p1b <- ggpairs(vp1, columns=cont_cov, 
               aes(color=SEX, alpha=0.5))
p1b <- plot_grid(
  ggmatrix_gtable(p), 
  ggmatrix_gtable(p1b), 
  ncol = 2
)
print(p1b)
dev.off()

# Multivariate normal distributions ---------------------------------------

#' `Virtual population generation method evaluation`

## Simulation virtual pop with n=100 simulation replicates
vp2 <- bind_rows(
  sim_vp_from_mvn(pop_male, nrow(pop_male), "male", 12315, nsim=100),
  sim_vp_from_mvn(pop_female, nrow(pop_female), "female", 35671, nsim=100)
)

## Evaluation of virtual pop generation method
png(here("wk11", "pics", "mvnd1.png"), height = 1440, width = 1440)
p2a <- plot_vp(vp2, pop)
print(p2a)
dev.off()

#' `Virtual population generation`

## Generate virtual populations using marginal distribution method
vp2 <- bind_rows(
  sim_vp_from_mvn(pop_male, nrow(pop_male), "male", 12315, nsim=1),
  sim_vp_from_mvn(pop_female, nrow(pop_female), "female", 35671, nsim=1)
)

## Check summary statistics
left_join(summ(pop), summ(vp2)) %>% knitr::kable()

pop %>% count(SEX)
vp2 %>% count(SEX)

## Simulation versus observation distribution 1 replicate comparison
png(here("wk11", "pics", "mvnd2.png"), height = 720, width = 1440)
p2b <- ggpairs(vp2, columns=cont_cov, 
               aes(color=SEX, alpha=0.5))
p2b <- plot_grid(
  ggmatrix_gtable(p), 
  ggmatrix_gtable(p2b), 
  ncol = 2
)
print(p2b)
dev.off()

# Copula ------------------------------------------------------------------

#' `Estimate copulas`
copula_male <- vine(pop_male %>% dplyr::select(all_of(cont_cov)), 
                    margins_controls=list(mult=1), 
                    copula_controls=list(family_set="parametric"))

copula_female <- vine(pop_female %>% dplyr::select(all_of(cont_cov)), 
                      margins_controls=list(mult=1), 
                      copula_controls=list(family_set="parametric"))

#' `Virtual population generation method evaluation`

## Simulation virtual pop with n=100 simulation replicates
vp3 <- bind_rows(
  rcopula(copula_male, sim_nr = 100) %>% mutate(SEX="male"), 
  rcopula(copula_female, sim_nr = 100) %>% mutate(SEX="female")
)

## Evaluation of virtual pop generation method
png(here("wk11", "pics", "copula1.png"), height = 1440, width = 1440)
p3a <- plot_vp(vp3, pop)
print(p3a)
dev.off()

#' `Virtual population generation`
vp3 <- withr::with_seed(seed=1234, 
                        bind_rows(
                          rscopula(copula_male, n_sim = nrow(pop_male),
                                   var_types = c("c", "c", "c", "c", "c")) %>% 
                            mutate(SEX="male"), 
                          rscopula(copula_female, n_sim = nrow(pop_female),
                                   var_types = c("c", "c", "c", "c", "c")) %>% 
                            mutate(SEX="female")
                          ))

# Check summary statistics
left_join(summ(pop), summ(vp3)) %>% knitr::kable()

# Check actual distribution
png(here("wk11", "pics", "copula2.png"), height = 720, width = 1440)
p3b <- ggpairs(vp3, columns=cont_cov, 
               aes(color=SEX, alpha=0.5))
p3b <- plot_grid(
  ggmatrix_gtable(p), 
  ggmatrix_gtable(p3b), 
  ncol = 2
)
print(p3b)
dev.off()
