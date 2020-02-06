# Known Issues
# - Add equivalent to `lsens` and `estat clas` postestimation
#   commands in Stata

### Epidemiologic Methods 3 ####################################################

# Lab 4: Prediction

### Load Packages and Data #####################################################

# Clear Global Environment
# rm(list = ls())

# Load Packages
library(tidyverse)
library(haven)
library(ROCR)
library(lattice)
library(ResourceSelection)

# Import Data
df <- read_dta("2020_Epi753_Lab4.dta")

# Summarize Missing Data
data.frame(sapply(df, function(x) sum(is.na(x))))

### Define Function rocafella to Generate ROC Plots ############################

# Note: This program uses the ROCR package to plot ROC curves. The plots require
# several steps, and I have defined a new function called rocafella() to
# simplify this process.

# Note: Below, assign the results of a logistic regression model to an object,
# then pass the object to rocafella to obtain the AUC and an ROC curve. For
# example: m1 <- glm(...); rocafella(m1).
rocafella <- function(model){
    ftv <- matrix(model$fitted.values)
    lab <- matrix(model$y)
    prb <- prediction(ftv, lab)
    per <- performance(prb, "tpr", "fpr")
    auc <- performance(prb, "auc")
    auc <- paste0("AUC = ", round(auc@y.values[[1]], digits = 4))
    plot(per, 
        main = "ROC Curve", 
        ylab = "Sensitivity", 
        xlab = "1 - Specificity",
        sub = auc,
        lwd = 7)
    x <- seq(0, 1); y <- seq(0, 1)
    lines(x, y, lwd = 1)
}

### Model 1: Simple Model of the Association between CHD and High DBP ##########

### Questions 1-3 (At-Home) ####################################################

# Define High DBP as DBP >= 90
df <- df %>% mutate(highDBP = ifelse(diabp >= 90, 1, 0))

# Separate Training and Validation Data
train <- filter(df, training == 1)
valid <- filter(df, training == 0)

# Make 2x2 Table of High DBP and CHD in Training Data
addmargins(table(train$highDBP, train$inc_chd))
# Note: In chisq.test(), set correct = FALSE to match Stata output
chisq.test(train$highDBP, train$inc_chd, correct = FALSE)

### Questions 4-5 (At-Home) and Question 2 (In-Class) ##########################

# Logistic Regression Model 1
m1 <- glm(inc_chd ~ highDBP, family = binomial(link = logit), data = train)
summary(m1)

# Create an ROC Curve for a Single Cutoff (DBP > 90)
rocafella(m1)

### Model 2: Model of the Association between CHD and BP (Continuous) ##########

### Question 6 (At-home) and Question 3 (In-class) #############################

# Check to See if DBP Is Normally Distributed
histogram(df$diabp)

# Univariate logistic regression model
m2 <- glm(inc_chd ~ diabp, family = binomial(link = logit), data = train)
summary(m2)

# Create an ROC Curve
rocafella(m2)

# Make a Comparison between the ROC Curves

### Models 3A-3E ###############################################################

# Logistic Model 3A
m3a <- glm(inc_chd ~ highDBP + age + sex01, 
    family = binomial(link = logit), data = train)
summary(m3a); hoslem.test(m3a$y, fitted(m3a), g = 10)
rocafella(m3a)

# Logistic Model 3B
m3b <- glm(inc_chd ~ highDBP + age + sex01 + totchol, 
    family = binomial(link = logit), data = train)
summary(m3b); hoslem.test(m3b$y, fitted(m3b), g = 10)
rocafella(m3b)

# Logistic Model 3C
m3c <- glm(inc_chd ~ highDBP + age + sex01 + totchol + cursmoke, 
    family = binomial(link = logit), data = train)
summary(m3c); hoslem.test(m3c$y, fitted(m3c), g = 10)
rocafella(m3c)

# Logistic Model 3D
m3d <- glm(inc_chd ~ highDBP + age + sex01 + totchol + bmi, 
    family = binomial(link = logit), data = train)
# Note: The Hosmer-Lemeshow test of Model 3D obtains slightly different results
# than Stata's, but the inference is unchanged.
summary(m3d); hoslem.test(m3d$y, fitted(m3d), g = 10)
rocafella(m3d)

# Logistic Model 3E
m3e <- glm(inc_chd ~ highDBP + age + sex01 + totchol + heartrte, 
    family = binomial(link = logit), data = train)
summary(m3e); hoslem.test(m3e$y, fitted(m3e), g = 10)
rocafella(m3e)

### Question 5 (In-Class): Test Prediction Model ###############################

# Predict Validation Set Using Estimates from the Training Set
valid$pr <- predict(m3d, valid, type = "response")

### Question 6 (In-Class) and Question 7 (At-Home) ##############################
### Predict the Probability of CHD for studyid 11263 using Model 3D #############

# Get Values for studyid = 11263
valid %>% 
    filter(studyid == 11263) %>% 
    select(highDBP, age, sex01, totchol, bmi)

# Summarize p to Determine Level of Risk
valid %>%
    summarize(
        min = min(pr, na.rm = TRUE),
        median = max(pr, na.rm = TRUE),
        max = max(pr, na.rm = TRUE),
        mean = mean(pr, na.rm = TRUE),
        sd = sd(pr, na.rm = TRUE))

# Report Predicted Probability and Determine if Individual 11263 Had Event
valid %>% 
    filter(studyid == 11263) %>% 
    select(pr, inc_chd)

### Question 7 (In-Class) #

# Part A
glm(inc_chd ~ highDBP + age + sex01 + totchol + bmi, 
    family = binomial(link = logit), data = train)

# Part B
