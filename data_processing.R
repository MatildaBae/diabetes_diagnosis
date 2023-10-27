# Loading in Libraries
library(tidyverse)
library(RColorBrewer)


# Loading in Data
url <- 'https://github.com/MatildaBae/dsci-100-2023W1-group45/raw/main/diabetes_binary_5050split_health_indicators_BRFSS2015.csv'
diab_data <- read_csv(url)


# Inspecting data
dim(diab_data)
str(diab_data)


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
               GenHlth = as.numeric(GenHlth))


# Making sure that classes are balanced
diab_data %>% count(Diabetes_binary)


# Exploring BMI data
summary_bmi <- diab_data %>% 
        select(Diabetes_binary, BMI) %>%
        group_by(Diabetes_binary) %>%
        summarize(mean_BMI = mean(BMI),
                  sd_BMI = sd(BMI))
summary_bmi

mean_bmi_case <- summary_bmi %>% 
        filter(Diabetes_binary == 'Case') %>% 
        pull(mean_BMI)
mean_bmi_control <- summary_bmi %>% 
        filter(Diabetes_binary == 'Control') %>% 
        pull(mean_BMI)

# Plotting BMI frequency
bmi_plot <- diab_data %>%
        ggplot(aes(x = BMI, fill = Diabetes_binary)) +
        geom_histogram(bins = 30, 
                       color = 'white',
                       size = 0.5,
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
diab_data %>% select(Diabetes_binary, GenHlth, MentHlth) %>%
        group_by(Diabetes_binary) %>%
        summarize(across(GenHlth:MentHlth, mean, .names = 'mean_{.col}'),
                  across(GenHlth:MentHlth, sd, .names = 'sd_{.col}'))


# Plotting age data
diab_data %>% 
        select(Diabetes_binary, Age) %>%
        count(Diabetes_binary, Age) 

age_plot <- diab_data %>%
        select(Diabetes_binary, Age) %>%
        ggplot(aes(x = Age, fill = Diabetes_binary)) +
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
diab_data %>% 
        count(Diabetes_binary, 
              Age, 
              Sex) %>%
        mutate(n = ifelse(Sex == 'Male', 
                          n * -1, 
                          n)) %>%
        ggplot(aes(x = Age, 
                   y = n, 
                   fill = Diabetes_binary)) +
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
        facet_wrap(~Sex, 
                   strip.position = 'bottom', 
                   scale = 'free_x') +
        theme(text = element_text(size = 12), 
              panel.spacing.x = unit(0, 'pt'))
                

# Sex data
diab_data %>% 
        count(Diabetes_binary, Sex) %>%
        ggplot(aes(x = Diabetes_binary,
                   y = n,
                   fill = Sex)) +
        geom_bar(stat = 'identity', 
                 position = 'fill') +
        labs(x = '',
             y = 'Proportion',
             fill = '') +
        ggtitle('Sex distribution across cases-controls') +
        scale_fill_brewer(palette = 'Set2') +
        theme_classic()


# Looking at binary survey data
diab_summary <- diab_data %>% 
        select(-Sex) %>%
        group_by(Diabetes_binary) %>% 
        summarize(across(where(~ is.factor(.x) && length(unique(.x)) == 2), ~ sum(. == "1") / n(), .names = '{.col}'))
survey_plot <- diab_summary %>%
        pivot_longer(!Diabetes_binary) %>%
        ggplot(aes(x = name, y = value, fill = Diabetes_binary)) +
        geom_bar(stat = 'identity', position = 'dodge', width = 0.7) +
        labs(x = '', y = 'Proportion answered "Yes"', fill = '') +
        ggtitle("Survey results across cases-controls") +
        scale_fill_brewer(palette = 'Set2') +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
survey_plot