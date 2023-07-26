## adding comment
library(tidyverse)


##### trial parameters
pi = 0.5 # probability of being randomized to the treatment arm
n = 500 # trial sample size
effect_size = 50 # increase in 10 units compared to control

##### distribution of the covariates as random variables
# Each covariate is considered a random variable. We know this, since a trial dataset is always a "sample" of the full population.
# When we're simulating a dataset, we are sampling from a distribution
# This distribution can be anything, but let's assume we have one continuous covariate, one binary covariate
age_distribution = c(
  age_mean = 50,
  age_sd = 4
)
prob_hypertension = 0.8 # assume 80% of subjects had hypertension at baseline
digital_twin_distribution = c(
  dt_mean = 10,
  dt_sd = 3
)
residual_sd = 3 # variance of the residual errors

##### data-generating model
## this is the model that we will use to generate the outcomes, based on the values we simulate for individual covariates
## true beta coefficients
beta_vec = c(
  beta_0 = 3, # intercept
  beta_arm = effect_size, # treatment effect
  beta_age = 0.1, # age
  beta_hypertension = 3, # presence (yes/no) of baseline hypertension
  beta_dt = 0.5 # digital twin (prognostic score)
)

############################################################
############# SIMULATE A DATASET USING INPUTS ##############
############################################################

############## covariates
## sample n ages from the distribution we specified from normal distribution
age = rnorm(
  n,
  mean = age_distribution['age_mean'],
  sd = age_distribution['age_sd']
)

## sample n yes/no for baseline hypertension from binomial distribution
hypertension = rbinom(
  n, 1, 
  prob = prob_hypertension # probability of having hypertension at baseline
)

## sample prognostic scores from normal distribution
dt = rnorm(
  n,
  mean = digital_twin_distribution['dt_mean'],
  sd = digital_twin_distribution['dt_sd']
)

## treatment arm assignment from binomial distribution
arm = rbinom(
  n, 1, 
  prob = pi # randomization ratio
)

## residual values. we assume that the residual are always centered around zero. this adds some random error
residual = rnorm(
  n,
  mean = 0,
  sd = residual_sd
)


############## generate outcomes
y = beta_vec['beta_0'] + beta_vec['beta_arm']*arm + beta_vec['beta_age']*age + 
  beta_vec['beta_hypertension']*hypertension + beta_vec['beta_dt']*dt + residual


############## put it all together
dat = data.frame(
  y = y,
  arm = arm,
  age = age,
  hypertension = hypertension,
  dt = dt,
  residual = residual
)
skimr::skim(dat)

############## analysis
procova = lm(y ~ 1 + arm + age + hypertension + dt, data = dat) %>% summary
unadj = lm(y ~ 1 + arm + age + hypertension, data = dat) %>% summary
1 - (procova$coefficients[2, 2]^2)/(unadj$coefficients[2, 2]^2)
dat %>%
  filter(arm == 0) %>%
  summarise(
    r2 = cor(dt, y)^2
  )
