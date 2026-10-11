
library(mrgsolve)
library(here)
library(tidyverse)

mod <- mread(here("wk10/model/hwwk6.cpp"))

see(mod)

mod %>% ev(amt=100, evid=1, cmt=1) %>% mrgsim() %>% plot()

mod %>% ev(amt=100, evid=1, cmt=1) %>% mrgsim(nid=10) %>% plot()


# PTA: 
# Toxicity target is 15
# Target attain if an individual has cmax < 15

threshold <- 15

out <- mod %>% ev(amt=200, evid=1, cmt=1) %>% 
  mrgsim_df(nid=100, obsonly=TRUE)

ggplot(out, aes(x=time, y=IPRED, group=ID))+geom_line()+
  geom_hline(aes(yintercept=threshold), linetype=2)

sum <- out %>% group_by(ID) %>% 
  summarise(cmax = max(IPRED)) %>% ungroup()
sum

ggplot(sum)+geom_boxplot(aes(y=cmax))+
  geom_hline(aes(yintercept=threshold), linetype=2)

sum <- sum %>% mutate(gt_thres = cmax>threshold)
sum

sum(sum$gt_thres)/nrow(sum)

pta <- function(dose){
  
  out <- mod %>% ev(amt=dose, evid=1, cmt=1) %>% 
    mrgsim_df(nid=100, obsonly=TRUE)
  
  sum <- out %>% group_by(ID) %>% 
    summarise(cmax = max(IPRED)) %>% ungroup() %>% 
    mutate(gt_thres = cmax>threshold)
  
  frac <- sum(sum$gt_thres)/nrow(sum) 
  
  output <- data.frame(dose=dose, frac=frac)
  
  return(output)
  
}

xx <- map(c(25, 50, 100, 200), pta) %>% bind_rows()

ggplot(xx, aes(x=dose, y=frac))+geom_point()+geom_line()+
  geom_hline(aes(yintercept=0.9), linetype=2)


## ECDF

out <- mod %>% ev(amt=200, evid=1, cmt=1) %>% 
  mrgsim_df(nid=100, obsonly=TRUE) 

out2 <- mod2 %>% param(THETA1 = 0.15) %>% 
  ev(amt=200, evid=1, cmt=1) %>% 
  mrgsim_df(nid=100, obsonly=TRUE) 

sum <- out %>% group_by(ID) %>% 
  summarise(cmax = max(IPRED)) %>% ungroup()

sum2 <- out2 %>% group_by(ID) %>% 
  summarise(cmax = max(IPRED)) %>% ungroup()

sum %>% ggplot()+
  geom_histogram(data=sum, aes(x=cmax, after_stat(density)), 
                 color="blue", fill=NA)+
  geom_histogram(data=sum2, aes(x=cmax, after_stat(density)), 
                 color="red", fill=NA)

ggplot()+
  stat_ecdf(data=sum , aes(x=cmax), color="blue")+
  stat_ecdf(data=sum2, aes(x=cmax), color="red")





