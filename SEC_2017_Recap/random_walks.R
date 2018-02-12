### Re-create random walks model from Jim Albert book in Statistical Methods and Analyses in Sports
## Note for NCAA - recalculate weights (per SEC or per NCAA?)
## https://gist.github.com/keberwein/e8782a7203b819bdb19e7ac6322dcf45

library(Lahman)
library(tidyverse)
library(baseballr)

wts <- fg_guts() %>% filter(season==2014)

dat <- Batting %>% filter(yearID == 2014)

dat2 <- dat %>% mutate(
  PA = AB + BB + HBP + SH + SF, 
  X1B = H - X2B - X3B - HR
) %>% filter(PA>400) 

drop_cls <- paste0("-",colnames(wts))
raw_dat2 <- cbind(dat2,wts)

dat3 <- raw_dat2 %>% mutate(
  wOBA = (wBB * BB + wHBP * HBP + w1B * X1B + w2B * X2B + w3B * X3B + wHR * HR)/(AB + BB - IBB + SF + HBP)
) %>% select_(.dots = drop_cls)


## Random Effects here we come

library(lme4)
library(LearnBayes)
corn1.lmer = glmer(wOBA ~ PA + (1 | playerID),data=dat3,family='binomial')


laplace.fit <- laplace(betabinexch,
                       0,
                       dat3$wOBA)

               