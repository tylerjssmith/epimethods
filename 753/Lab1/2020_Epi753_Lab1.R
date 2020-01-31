################################################################################
##### Lab Exercise 1: Life Expectancy and Household Income in Baltimore ########
################################################################################

##### Load Packages and Data ###################################################

# Load packages
library(tidyverse)
library(haven)

# Import Data
epi_data <- read_dta("2020_Epi753_Lab1.dta")

# Remove extraneous rows at the end of the imported data set
epi_data <- head(epi_data, 56)

##### Background ###############################################################

# Summarize Life Expectancy
# Note: See ?summarize for help file on how to include additional statistics.
summarize(epi_data, obs = n(),
                    mean = mean(LifeExpectancy),
                    sd = sd(LifeExpectancy),
                    min = min(LifeExpectancy),
                    max = max(LifeExpectancy))

# Summarize Median Income
summarize(epi_data, obs = n(),
                    mean = mean(MedianIncome),
                    sd = sd(MedianIncome),
                    min = min(MedianIncome),
                    max = max(MedianIncome))

# View Select Variables
epi_data2 <- select(epi_data, CommunityStatisticalArea, LifeExpectancy, MedianIncome)
View(epi_data2)

##### Question 1 (in-class) ####################################################

# Create a scatter plot with lowess curve of median income and life expectancy
g <- ggplot(aes(MedianIncome, LifeExpectancy), data = epi_data)

g + geom_point() +
    geom_smooth(method = "loess", se = FALSE, col = "orange") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Neighborhood Median Income and Life Expectancy in Baltimore",
         x = "Median Income (USD)", 
         y = "Life Expectancy (years)")

##### Questions 1-2 (at-home) ##################################################

# Model 1
epi_data <- epi_data %>% mutate(inclt35_bin = ifelse(MedianIncome < 35000, 1, 0))
summary(lm(LifeExpectancy ~ inclt35_bin, data = epi_data))

##### Questions 3-4 (at-home) ##################################################

# Model 2
epi_data$income_ind <- ntile(epi_data$MedianIncome, 5)

epi_data$income_ind <- factor(epi_data$income_ind, levels = 1:5)

summary(lm(LifeExpectancy ~ income_ind, data = epi_data))

##### Question 5 (at-home) #####################################################

# Model 3
epi_data$income_qt1 <- ntile(epi_data$MedianIncome, 5)

m3 <- summary(lm(LifeExpectancy ~ income_qt1, data = epi_data))

##### Question 5 (at-home) #####################################################

##### Question 6-7 (at-home) ###################################################

# Model 4
summary(lm(LifeExpectancy ~ MedianIncome, data = epi_data))

##### Question 8-9 (at-home) ###################################################

# Model 5
summarize(epi_data, 
    mean = mean(MedianIncome), 
    sd = sd(MedianIncome))

epi_data <- mutate(epi_data, income_c_s = (MedianIncome - 44608) / 20340)

m5 <- summary(lm(LifeExpectancy ~ income_c_s, data = epi_data))
m5

##### Question 8 (in-class) ####################################################

h <- ggplot(aes(x, y), data = epi_data3)

h + geom_point() 


+
    geom_point(aes(x = x, y = y)) +
    geom_smooth(method = "loess", se = FALSE, col = "orange") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Neighborhood Median Income and Life Expectancy in Baltimore",
        x = "Median Income (USD)", 
        y = "Life Expectancy (years)")

