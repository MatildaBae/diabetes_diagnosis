#################################################
## LOADING IN, PROCESSING AND SUBSAMPLING DATA ##
#################################################

# Loading in Libraries
rm(list = ls())
library(tidyverse)
library(tidymodels)
library(ranger)
library(janitor)

# Setting seed for reproducibility
set.seed(123)

# Loading in Data
url <- 'https://github.com/MatildaBae/dsci-100-2023W1-group45/raw/main/data/diabetes_binary_5050split_health_indicators_BRFSS2015.csv'
diab_data <- read_csv(url)


# Formatting column classes appropriately
age_levels <- c('[18,24]', 
                '[25,29]', 
                '[30,34]', 
                '[35,39]', 
                '[40,44]', 
                '[45,49]', 
                '[50,54]', 
                '[55,59]', 
                '[60,64]', 
                '[65,69]', 
                '[70,74]', 
                '[75,79]', 
                '[79,∞)')

diab_data <- diab_data %>% 
        mutate(across(everything(), as_factor)) %>%
        mutate(Diabetes_binary = fct_recode(Diabetes_binary, 'Case' = '1', 'Control' = '0')) %>%
        mutate(Diabetes_binary = factor(Diabetes_binary, levels = c('Case', 'Control'))) %>%
        mutate(Sex = fct_recode(Sex, 'Female' = '0', 'Male' = '1')) %>%
        mutate(Sex = factor(Sex, levels = c('Male', 'Female'))) %>%
        mutate(Age = fct_recode(Age, 
                                '[18,24]' = '1',
                                '[25,29]' = '2',
                                '[30,34]' = '3',
                                '[35,39]' = '4',
                                '[40,44]' = '5',
                                '[45,49]' = '6',
                                '[50,54]' = '7',
                                '[55,59]' = '8',
                                '[60,64]' = '9',
                                '[65,69]' = '10',
                                '[70,74]' = '11',
                                '[75,79]' = '12',
                                '[79,∞)' = '13')) %>%
        mutate(Age = factor(Age, levels = age_levels)) %>%
        mutate(BMI = as.numeric(BMI),
               MentHlth = as.numeric(MentHlth),
               GenHlth = as.numeric(GenHlth)) %>%
        clean_names()

# Subsampling data to 1000 observations (500 from each class)
diab_samp <- diab_data %>% 
        group_by(diabetes_binary) %>%
        slice_sample(n = 500) %>%
        ungroup()

write_csv(file = 'data/diabetes_subsampled.csv', diab_samp)
