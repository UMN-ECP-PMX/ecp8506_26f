
# Library
library(tidyverse)
library(nhanesA)

# Download demographic data (DEMO_D), and select/rename variables
dem <- nhanes('DEMO_D') %>% 
    select(ID = SEQN, AGE = RIDAGEYR, 
           SEX = RIAGENDR, RACE = RIDRETH1) 

# Download body measure examination data (BMX_D), and select/rename variables
wt <- nhanes('BMX_D') %>% 
    select(ID = SEQN, WT = BMXWT) 

# Merge data
covar <- inner_join(dem, wt, by="ID")

# Remove NAs
covar <- filter(covar, !is.na(WT) & !is.na(AGE) & !is.na(SEX) & !is.na(RACE) )
anyNA(covar)    

# Create virtual population (n=1000) via resampling with replacement
vp <- covar %>% sample_n(size = 1000,replace = T)
