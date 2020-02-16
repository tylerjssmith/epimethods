### Epidemiologic Methods: Lab 5 ###############################################

### Preliminaries ##############################################################

# Load Packages
library(tidyverse)
library(haven)
library(survival)
library(survminer)

# Clear Global Environment (if desired)
# rm(list = ls())

# Import Data
df <- read_dta("2020_Epi753_Lab5.dta")

# Check for Missing Data
data.frame(sapply(df, function(x) sum(is.na(x))))

### Analysis 1: Time from Enrollment ###########################################

# Entry, Exit, and Follow-up Time
df <- df %>% mutate(entry1 = 0)
df <- df %>% mutate(exit1 = lastyear - enrollyear)
df <- df %>% mutate(fup1 = exit1 - entry1)

# Summarize
sum(df$fup1)
sum(df$statusatlast)

### Analysis 2: Time from Age 18 ###############################################

# Entry, Exit, and Follow-up Time
df <- df %>% mutate(entry2 = enrollyear - age18year)
df <- df %>% mutate(exit2 = lastyear - age18year)
df <- df %>% mutate(fup2 = exit2 - entry2)

# Summarize
sum(df$fup2)
sum(df$statusatlast)

### Analysis 3: Time from HAART Initiation #####################################

# Entry, Exit, and Follow-up Time
df <- df %>% mutate(entry3 = 
    ifelse(artyear < 2100 & artyear <= enrollyear, enrollyear - artyear,
    ifelse(artyear < 2100 & artyear > enrollyear, 0, NA)))

df <- df %>% mutate(exit3 = ifelse(artyear < 2100, lastyear - artyear, NA))
df <- df %>% mutate(fup3 = ifelse(artyear < 2100, exit3 - entry3, NA))

# Summarize
df %>% 
    filter(artyear < 2100) %>% 
        summarize(fup3 = sum(fup3), statusatlast = sum(statusatlast))

### Analysis 4: Inclusion of Immortal Person Time (Incorrect Analysis) #########

# Entry, Exit, and Follow-up Time
df <- df %>% mutate(entry4 = ifelse(artyear < 2100, 0, NA))
df <- df %>% mutate(exit4 = ifelse(artyear < 2100, lastyear - artyear, NA))
df <- df %>% mutate(fup4 = ifelse(artyear < 2100, exit4 - entry4, NA))

# Summarize    
df %>% 
    filter(artyear < 2100) %>% 
        summarize(fup4 = sum(fup4), statusatlast = sum(statusatlast))

### Analysis 5 (In-Class): Incorrect Risk Set Alignment ########################

# Entry, Exit, and Follow-up Time
df <- df %>% mutate(entry5 = ifelse(artyear < 2100, 0, NA))
df <- df %>% mutate(exit5 = 
    ifelse(artyear < 2100 & artyear <= enrollyear, lastyear - enrollyear,
    ifelse(artyear < 2100 & artyear >  enrollyear, lastyear - artyear, NA)))
df <- df %>% mutate(fup5 = ifelse(artyear < 2100, exit5 - entry5, NA))

# Summarize    
df %>% 
    filter(artyear < 2100) %>% 
        summarize(fup5 = sum(fup5), statusatlast = sum(statusatlast))

### Question 7C (In-Class) #####################################################

# Kaplan-Meier Plot of Analysis 3
png(filename = "analysis3.png")
ggsurvplot(
    fit = survfit(Surv(fup3, statusatlast) ~ 1, data = df),
    title = "Analysis 3",
    xlab = "Years", 
    ylab = "Survival")
dev.off()

# Kaplan-Meier Plot of Analysis 5
png(filename = "analysis5.png")   
ggsurvplot(
    fit = survfit(Surv(fup5, statusatlast) ~ 1, data = df),
    title = "Analysis 5",
    xlab = "Years", 
    ylab = "Survival")
dev.off()
