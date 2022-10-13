## ----setup, include=FALSE--------
knitr::opts_chunk$set(echo = TRUE, results = 'hide', fig.keep = 'none')


## ----message=FALSE, warning=FALSE, paged.print=FALSE----
# Add names of group members HERE
library(tidyverse)
library(broom)
library(wooldridge)
library(car)
library(lmtest)
library(estimatr)
library(magrittr)
library(modelsummary)


## --------------------------------
df <- as_tibble(gpa3)


## --------------------------------
datasummary_skim(df)


## ----include=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE----
df %<>% filter(spring==1)


## --------------------------------
df %<>% select(cumgpa,sat,hsperc,tothrs,female,black,white)


## --------------------------------
est <- lm(cumgpa ~ sat + hsperc + tothrs + female + black + white, data=df)
modelsummary(est, stars = T)

est.rob <- lm_robust(cumgpa ~ sat + hsperc + tothrs + female + black + white, data=df)
modelsummary(list(est,est.rob), stars = TRUE)


## --------------------------------
glance(est)
linearHypothesis(est.rob, c('sat','hsperc','tothrs','female','black','white'))


## --------------------------------
# Restricted model
restr <- lm(cumgpa ~ 1, data=df)
LMreg <- lm(resid(restr) ~ sat + hsperc + tothrs + female + black + white, data=df)
LM    <- nobs(LMreg)*glance(LMreg)$r.squared
pval  <- 1-pchisq(LM,6)


## ----message=FALSE, warning=FALSE, paged.print=FALSE----
df.auto <- read_csv('https://tyleransom.github.io/teaching/MetricsLabs/auto.csv')


## --------------------------------
df.auto %<>% mutate(log.price = log(price), foreign = as.factor(foreign))
est.auto <- lm(log.price ~ weight + foreign, data=df.auto)


## --------------------------------
modelsummary(est.auto)


## --------------------------------
est.rob.auto <- lm_robust(log.price ~ weight + foreign, data=df.auto)
modelsummary(list(est.auto,est.rob.auto))


## --------------------------------
est.clust.auto <- lm_robust(log.price ~ weight + foreign, data=df.auto, 
                            clusters=df.auto$manufacturer)
modelsummary(list(est.auto,est.rob.auto,est.clust.auto))


## --------------------------------
linearHypothesis(est.clust.auto,c("weight=0","foreignForeign=0"))

