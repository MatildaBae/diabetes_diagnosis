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

# Exploring BMI data
summary_bmi <- diab_samp %>% 
  select(diabetes_binary, bmi) %>%
  group_by(diabetes_binary) %>%
  summarize(mean_bmi = mean(bmi),
            sd_bmi = sd(bmi))
summary_bmi

mean_bmi_case <- summary_bmi %>% 
  filter(diabetes_binary == 'Case') %>% 
  pull(mean_bmi)
mean_bmi_control <- summary_bmi %>% 
  filter(diabetes_binary == 'Control') %>% 
  pull(mean_bmi)

# Plotting BMI frequency
bmi_plot <- diab_samp %>%
  ggplot(aes(x = bmi, fill = diabetes_binary)) +
  geom_histogram(bins = 30, 
                 color = 'white',
                 linewidth = 0.5,
                 alpha = 0.5,
                 position = 'identity') +
  geom_vline(xintercept = mean_bmi_case, color = 'darkgreen', linewidth = 1) +
  geom_vline(xintercept = mean_bmi_control, color = 'red', linewidth = 1) +
  scale_x_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  labs(x = 'Body Mass Index (BMI)', 
       y = 'Count',
       fill = '') +
  ggtitle('BMI distribution across cases-controls') +
  scale_fill_brewer(palette = 'Set2') +
  theme_classic() +
  theme(text = element_text(size = 12))
bmi_plot

# General and Mental Health scores
diab_samp %>% select(diabetes_binary, gen_hlth, ment_hlth) %>%
  group_by(diabetes_binary) %>%
  summarize(across(gen_hlth:ment_hlth, mean, .names = 'mean_{.col}'),
            across(gen_hlth:ment_hlth, sd, .names = 'sd_{.col}'))

# Plotting age data
diab_samp %>% 
  select(diabetes_binary, age) %>%
  count(diabetes_binary, age) 

age_plot <- diab_samp %>%
  select(diabetes_binary, age) %>%
  ggplot(aes(x = age, fill = diabetes_binary)) +
  geom_bar(stat = 'count', 
           position = 'identity',
           color = 'white',
           alpha = 0.5,
           width = 1) +
  labs(x = 'Age bracket',
       y = 'Count',
       fill = '') +
  ggtitle('Age distribution across cases-controls') +
  scale_fill_brewer(palette = 'Set2') +
  theme_classic() +
  theme(text = element_text(size = 12),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
age_plot

# Population plot
diab_samp %>% 
  count(diabetes_binary, 
        age, 
        sex) %>%
  mutate(n = ifelse(sex == 'Male', 
                    n * -1, 
                    n)) %>%
  ggplot(aes(x = age, 
             y = n, 
             fill = diabetes_binary)) +
  geom_bar(stat = 'identity', 
           position = 'dodge',
           color = 'white',
           width = 1) +
  labs(x = 'Age bracket', 
       y = 'Count',
       fill = '') +
  ggtitle('Age and gender distribution across case-controls') +
  coord_flip() +
  scale_fill_brewer(palette = 'Set2') +
  theme_classic() +
  scale_y_continuous(labels = abs,
                     expand = c(0,0)) +
  facet_wrap(~sex, 
             strip.position = 'bottom', 
             scale = 'free_x') +
  theme(text = element_text(size = 12), 
        panel.spacing.x = unit(0, 'pt'))


# Sex data
diab_samp %>% 
  count(diabetes_binary, sex) %>%
  ggplot(aes(x = diabetes_binary,
             y = n,
             fill = sex)) +
  geom_bar(stat = 'identity', 
           position = 'fill') +
  labs(x = '',
       y = 'Proportion',
       fill = '') +
  ggtitle('Sex distribution across cases-controls') +
  scale_fill_brewer(palette = 'Set2') +
  theme_classic()


# Looking at binary survey data
diab_summary <- diab_samp %>% 
  select(-sex) %>%
  group_by(diabetes_binary) %>% 
  summarize(across(where(~ is.factor(.x) && length(unique(.x)) == 2), ~ sum(. == "1") / n(), .names = '{.col}'))

survey_plot <- diab_summary %>%
  pivot_longer(!diabetes_binary) %>%
  ggplot(aes(x = name, y = value, fill = diabetes_binary)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7) +
  labs(x = '', y = 'Proportion answered "Yes"', fill = '') +
  ggtitle("Survey results across cases-controls") +
  scale_fill_brewer(palette = 'Set2') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
survey_plot

col <- c("diabetes_binary","diff_walk", "heart_diseaseor_attack", "high_bp", "high_chol", "phys_activity", "bmi", "gen_hlth", "ment_hlth", "age", "smoker", "stroke") 

diab_sel <- diab_samp %>%
  select(col)
