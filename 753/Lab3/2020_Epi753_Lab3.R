# Known Issues
# - Need better function for effect measures with M-H adjustment
# - Need robust variance estimation for regression models

######### Epidemiologic Methods 3 ##############################################

# Lab 3: Causal Evaluation

######### Load Packages and Data ###############################################

# Clear Objects from Global Environment
# rm(list = ls())

# Load Packages
library(tidyverse)
library(haven)
library(Epi)
library(survey)
library(epiR)

# Import Data
dataset <- read_dta("2020_Epi753_Lab3.dta")

######### Questions 2-4 (At-Home)  #############################################

# Table of treatment failure by class of initial ART
addmargins(table(dataset$pi_regimen, dataset$tx_failure))

# Calculate simple odds ratio (similar to `cc` in Stata)
effx(response = tx_failure, 
    exposure = pi_regimen, 
    type = "binary", 
    eff = "OR", 
    data = dataset)

# Calculate simple risk ratio (similar to `cs` in Stata)
effx(response = tx_failure, 
    exposure = pi_regimen, 
    type = "binary", 
    eff = "RR", 
    data = dataset)

######### Question 5 (At-Home)  ################################################

# Stratified analysis (by low CD4 cell count)

# Dichotomize CD4 cell count
dataset <- dataset %>% mutate(lowCD4 = ifelse(cd4 < 200, 1, 0))
mx1 <- table(dataset$tx_failure, dataset$pi_regimen, dataset$lowCD4)
addmargins(mx1)

# Calculate adjusted risk ratio (using glm() for adjustment)
effx(response = tx_failure, 
    exposure = pi_regimen,
    control = lowCD4,
    type = "binary", 
    eff = "OR", 
    data = dataset)

# Calculate adjusted odds ratio (using M-H adjustment)
# Note: epi.2by2() is slow and returns a 2x2 table with the wrong cell frequencies.
# However, the odds ratios, confidence intervals, and test of homogeneity match
# the Stata output.
# epi.2by2(mx1, method = "case.control")

# Calculate adjusted risk ratio (using glm() for adjustment)
effx(response = tx_failure, 
    exposure = pi_regimen,
    control = lowCD4,
    type = "binary", 
    eff = "RR", 
    data = dataset)

# Calculate adjusted risk ratio (using M-H adjustment)
# Note: See above.

######### Questions 6-7 (at-home), Questions 5 (in-class) ######################

# Linear Model 1
# Note: This model does use robust variance estimation.
summary(lm(tx_failure ~ pi_regimen, data = dataset))

# Linear Model 2
# Note: This model does use robust variance estimation.
summary(lm(tx_failure ~ pi_regimen + lowCD4, data = dataset))

# Log Binomial Model 3
glm(tx_failure ~ pi_regimen, 
    family = binomial(link = log), data = dataset)
exp(coef(glm(tx_failure ~ pi_regimen, 
    family = binomial(link = log), data = dataset)))

# Log Binomial Model 4
glm(tx_failure ~ pi_regimen + lowCD4, 
    family = binomial(link = log), data = dataset)
exp(coef(glm(tx_failure ~ pi_regimen + lowCD4, 
    family = binomial(link = "log"), data = dataset)))

# Log Binomial Model 5
glm(tx_failure ~ pi_regimen + cd4, 
    family = binomial(link = "log"), data = dataset)
exp(coef((glm(tx_failure ~ pi_regimen + cd4, 
    family = binomial(link = "log"), data = dataset))))

# Note: This model does use robust variance estimation.
glm(tx_failure ~ pi_regimen + cd4, 
    family = poisson(link = "log"), data = dataset)
exp(coef((glm(tx_failure ~ pi_regimen + cd4, 
    family = poisson(link = "log"), data = dataset))))

# Log Binomial (and Poisson) Model 6
dataset <- dataset %>% mutate(
    cd4s = cd4 ^ 2, 
    cd4c = cd4 ^ 3)

# The log-binomial model (commented out below) fails to converge; consider uncommenting 
# this model and running it so you can see what that looks like.

# glm(tx_failure ~ pi_regimen + cd4 + cd4s + cd4c, 
#   family = binomial(link = log), data = dataset)

glm(tx_failure ~ pi_regimen + cd4 + cd4s + cd4c, 
    family = poisson(link = "log"), data = dataset)
exp(coef(glm(tx_failure ~ pi_regimen + cd4 + cd4s + cd4c, 
    family = poisson(link = "log"), data = dataset)))

# Logistic Model 7
glm(tx_failure ~ pi_regimen, 
    family = binomial(link = logit), data = dataset)
exp(coef(glm(tx_failure ~ pi_regimen, 
    family = binomial(link = logit), data = dataset)))

# Logistic Model 8
glm(tx_failure ~ pi_regimen + lowCD4, 
    family = binomial(link = logit), data = dataset)
exp(coef(glm(tx_failure ~ pi_regimen + lowCD4, 
    family = binomial(link = logit), data = dataset)))

# Logistic Model 9
glm(tx_failure ~ pi_regimen + cd4, 
    family = binomial(link = logit), data = dataset)
exp(coef(glm(tx_failure ~ pi_regimen + cd4, 
    family = binomial(link = logit), data = dataset)))

# Logistic Model 10
glm(tx_failure ~ pi_regimen + cd4 + cd4s + cd4c, 
    family = binomial(link = logit), data = dataset)
exp(coef(glm(tx_failure ~ pi_regimen + cd4 + cd4s + cd4c, 
    family = binomial(link = logit), data = dataset)))

######### Question 8 (At-Home) #################################################

# Table of initial ART regimen (exposure)
dataset <- dataset %>% 
    mutate(ge50yrs = ifelse(age >= 50, 1, 0))
addmargins(table(dataset$tx_failure, dataset$pi_regimen, dataset$ge50yrs))

# Code to calculate RR within strata of age (and get 95% CI)
# Note: Variable passed to strata argument below must be a factor.
dataset <- dataset %>% mutate(ge50yrs_f = ge50yrs)
dataset$ge50yrs_f <- factor(dataset$ge50yrs_f, levels = c(0, 1))

effx(response = tx_failure, 
    exposure = pi_regimen,
    strata = ge50yrs_f,
    type = "binary", 
    eff = "RR", 
    data = dataset)

######### Questions 10-11 (At-Home); Question 7 (In-Class) #####################

# Create Product Variable
dataset <- dataset %>% 
    mutate(pi_ge50 = pi_regimen * ge50yrs)

# Linear Model 11
# Note: This model does use robust variance estimation.
m11 <- lm(tx_failure ~ pi_regimen + ge50yrs + pi_ge50, 
    data = dataset)

coef(m11)
# Note: svycontrast() is similar to `lincom` in Stata.
svycontrast(m11, c("pi_regimen"=1, "pi_ge50"=1))

# Log-Binomial Model 12
m12 <- glm(tx_failure ~ pi_regimen + ge50yrs + pi_ge50, 
    family = binomial(link = log), data = dataset)

exp(coef(m12))
exp(svycontrast(m12, c("pi_regimen"=1, "pi_ge50"=1)))

# Logistic Model 13
m13 <- glm(tx_failure ~ pi_regimen + ge50yrs + pi_ge50, 
    family = binomial(link = logit), data = dataset)

exp(coef(m13))
exp(svycontrast(m13, c("pi_regimen"=1, "pi_ge50"=1)))
