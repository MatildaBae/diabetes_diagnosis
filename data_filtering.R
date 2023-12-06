#################################################
## LOADING IN, PROCESSING AND SUBSAMPLING DATA ##
#################################################

# Loading in Libraries
rm(list = ls())
library(tidyverse)
library(tidymodels)
library(ranger)
library(janitor)
library(vip)

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

# Before selecting variables, visualize classification

# Change factor variables into numeric for visualization
factor_cols_all <- sapply(diab_samp, is.factor)

diab_vis_all <- diab_samp %>%
  mutate_if(factor_cols_all, as.integer)

selected_numeric_all <- diab_vis_all[, factor_cols_all]
selected_numeric_all <- selected_numeric_all - 1

diab_vis_all[, factor_cols_all] <- selected_numeric_all

# Perform PCA
pca_all <- prcomp(diab_vis_all[, -which(names(diab_vis_all) == 'diabetes_binary')], 
                  scale. = TRUE)

# Extract the first two principal components
pca_data_all <- as.data.frame(pca_all$x[, 1:2])
colnames(pca_data_all) <- c("PC1", "PC2")

# Add target variable for coloring
pca_data_all$diabetes_binary <- diab_samp$diabetes_binary

# Plot using ggplot
pca_vis_all <- ggplot(aes(x=PC1, y=PC2, color=diabetes_binary), 
                           data = pca_data_all) +
  geom_point() +
  ggtitle("PCA Results including every variables") +
  labs(color= "Diabetes")

pca_vis_all

# Select 9 variables by observing plots from each variables
col <- c("diabetes_binary","diff_walk", "heart_diseaseor_attack", "high_bp", "high_chol", "phys_activity", "bmi", "gen_hlth", "ment_hlth", "age") 

diab_sel <- diab_samp %>%
  select(col)


# Change factor variables into numeric for visualization
factor_cols <- sapply(diab_sel, is.factor)

diab_vis <- diab_sel %>%
  mutate_if(factor_cols, as.integer)

selected_numeric <- diab_vis[, factor_cols]
selected_numeric <- selected_numeric - 1

diab_vis[, factor_cols] <- selected_numeric

# Visualize data in 2-dimensions plot
# Perform PCA
pca <- prcomp(diab_vis[, -which(names(diab_vis) == 'diabetes_binary')], scale. = TRUE)

# Extract the first two principal components
pca_data <- as.data.frame(pca$x[, 1:2])
colnames(pca_data) <- c("PC1", "PC2")

# Add target variable for coloring
pca_data$diabetes_binary <- diab_samp$diabetes_binary

# Plot using ggplot
pca_vis <- ggplot(aes(x=PC1, y=PC2, color=diabetes_binary), data = pca_data) +
  geom_point() +
  ggtitle("PCA Results using 9 selected variables") +
  labs(color= "Diabetes")

pca_vis

# Create a data frame for plotting the loading vectors
loading_plot <- data.frame(
  Variable = colnames(pca$rotation),
  PC1 = pca$rotation[, 1],  # PC1 loadings
  PC2 = pca$rotation[, 2]   # PC2 loadings
)
loading_plot$Variable <- rownames(loading_plot)

# Plot the PCA loading plot with arrows
ggplot(loading_plot, aes(x = 0, y = 0, xend = PC1, yend = PC2)) +
  geom_segment(arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "orange") +
  geom_text(aes(label = Variable, x = PC1 * 1.2, y = PC2 * 1.2), 
            hjust = ifelse(loading_plot$PC1 > 0, 0, 1),  # Adjust horizontal position
            vjust = ifelse(loading_plot$PC2 > 0, 0, 1)) +
  xlim(-1, 1) + ylim(-1, 1) +
  labs(x = "PC1", y = "PC2", title = "PCA Loading Plot with Arrows")



################################
# RANDOM FOREST CLASSIFICATION #
################################

# Splitting data into training-testing subsets
diab_split <- diab_sel %>%
  initial_split(prop = 0.75,
                strata = diabetes_binary)
diab_train <- diab_split %>%
  training()
diab_test <- diab_split %>%
  testing()


# Setting up folds for cross-validation
diab_cv <- diab_samp %>% 
  vfold_cv(v = 10, strata = diabetes_binary)

# Setting up recipe
diab_recipe <- recipe(diabetes_binary ~ ., diab_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_factor_predictors())


# Setting up model specification
cv_spec <- rand_forest(mtry = tune(), 
                       min_n = tune(),
                       trees = 1001) %>%
  set_engine('ranger') %>%
  set_mode('classification')

# Setting up workflow object
cv_workflow <- workflow() %>%
  add_recipe(diab_recipe) %>%
  add_model(cv_spec)



# 5-fold cross-validation, 
cv_results <- cv_workflow %>% 
  tune_grid(resamples = diab_cv, grid = 20) %>%
  collect_metrics()


# Inspecting cross-validation results
cv_results %>% 
  filter(.metric == 'accuracy') %>% 
  select(mtry, min_n, mean) %>%
  arrange(desc(mean))


# mtry_param <- cv_results %>% filter(.metric == 'accuracy') %>%
#         arrange(desc(mean)) %>%
#         slice_head(n = 1) %>%
#         select(mtry) %>%
#         pull()
mtry_param <- 2

# min_n_param <- cv_results %>% filter(.metric == 'accuracy') %>%
#         arrange(desc(mean)) %>%
#         slice_head(n = 1) %>%
#         select(min_n) %>%
#         pull()
min_n_param <- 36



# Setting up final model specification 
rf_spec <- rand_forest(mtry = mtry_param, 
                       min_n = min_n_param, 
                       trees = 1001) %>% 
  set_engine('ranger',
             importance = 'impurity') %>%
  set_mode('classification')

rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(diab_recipe)

rf_predict <- rf_wf %>%
  fit(diab_train)

rf_predict %>% predict(diab_test) %>% bind_cols(diab_test) %>% 
  metrics(truth = diabetes_binary, .pred_class)

rf_predict %>% predict(diab_test, type = 'prob') %>% bind_cols(diab_test) %>%
  roc_curve(truth = diabetes_binary, .pred_Case) %>%
  autoplot()

rf_predict %>% predict(diab_test) %>% bind_cols(diab_test) %>%
  conf_mat(truth = diabetes_binary, estimate = .pred_class)

rf_predict %>% predict(diab_test) %>% bind_cols(diab_test) %>%
  f_meas(truth = diabetes_binary, estimate = .pred_class)

rf_predict %>% predict(diab_test) %>% bind_cols(diab_test) %>%
  spec(truth = diabetes_binary, estimate = .pred_class)

rf_predict %>% predict(diab_test) %>% bind_cols(diab_test) %>%
  sens(truth = diabetes_binary, estimate = .pred_class)

# Variable importance scores
rf_predict %>% extract_fit_parsnip() %>% vip()



#########################
## LOGISTIC REGRESSION ##
#########################

lr_spec <- logistic_reg() %>%
  set_engine('glm') %>%
  set_mode('classification')

lr_wf <- workflow() %>%
  add_recipe(diab_recipe) %>%
  add_model(lr_spec)

lr_fit <- lr_wf %>%
  fit(diab_train)

lr_predict <- lr_fit %>%
  predict(diab_test)

lr_predict %>% bind_cols(diab_test) %>% metrics(truth = diabetes_binary, estimate = .pred_class)
