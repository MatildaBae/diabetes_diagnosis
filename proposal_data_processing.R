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

col1 <- c("Diabetes_binary","DiffWalk", "HeartDiseaseorAttack", "HighBP", "HighChol", "PhysActivity", "BMI", "GenHlth", "MentHlth", "Age") 

diab_1 <- diab_data %>%
  select(col1)

library(tidymodels)
library(dplyr)
library(caret)

# Identify numeric and factor variables
numeric_cols <- sapply(diab_1, is.numeric)
factor_cols <- sapply(diab_1, is.factor)

# Scale numeric variables
data_scaled <- diab_1
data_scaled[, numeric_cols] <- scale(diab_1[, numeric_cols])

# Combine scaled numeric variables and factor variables
diab_2 <- cbind(data_scaled[, numeric_cols], diab_1[, factor_cols])

# Split data into 75% for training and 25% for testing
set.seed(4321)
diab_split <- diab_2 %>%
  initial_split(prop = 0.75)

# Extract the data in each split
diab_train <- training(diab_split)
diab_test <- testing(diab_split)

# Print the number of cases in each split
cat("Training cases: ", nrow(diab_train), "\n",
    "Testing cases: ", nrow(diab_test), sep="")
# Training cases: 53019
# Testing cases: 17673

# Run random forest model
library(randomForest)
library(datasets)

set.seed(4321)
rf <- randomForest(Diabetes_binary ~., data = diab_train, proximity = FALSE)

# Print model
print(rf)
# Out of bag error is 25.57%, so the train data set model accuracy is around 75%..

# Prediction & Confusion Matrix - Train Data
p1 <- predict(rf, diab_train)
confusionMatrix(p1, diab_train$Diabetes_binary) # Accuracy 80%

# Prediction & Confusion Matrix - Test Data
p2 <- predict(rf, diab_test)
confusionMatrix(p2, diab_test$Diabetes_binary) # Accuracy 74%

# Plot rf
plot(rf)

# Default mtry : 3, ntrees : 500
# Tune the model for higher accuracy
# Tune hyperparameter 'mtry'

# Create training control
train_control <- trainControl(method = "cv", number = 5)

# Define the hyperparameter grid
hyperparameters  <- expand.grid(
  mtry = c(1:6)
)

# Train the Random Forest model with hyperparameter tuning
set.seed(4321)
model <- train(
  Diabetes_binary ~ .,
  data = diab_2,
  method = "rf",
  trControl = train_control,
  tuneGrid = hyperparameters
)

# Get the best model
best_model <- model$bestTune
best_model # mtry 3 default...

# Tune hyperparameter 'ntrees'
# Separate predictors from the target variable
predictors <- diab_2[, -which(names(diab_2) == 'Diabetes_binary')]
target <- diab_2$Diabetes_binary

# Create a sequence of ntree values to try
ntree_values <- seq(from = 50, to = 500, by = 50)

# Initialize vectors to store results
oob_errors <- numeric(length(ntree_values))

# Loop through ntree values and train Random Forest models
for (i in seq_along(ntree_values)) {
  rf_model <- randomForest(predictors, target, ntree = ntree_values[i], 
                          importance = TRUE)
  oob_errors[i] <- rf_model$err.rate[ntree_values[i]]
}

# Plot OOB error rates against ntree values
plot(ntree_values, oob_errors, type = "b", 
     xlab = "Number of Trees (ntree)", 
     ylab = "Out-of-Bag (OOB) Error")

# Find the ntree value with the lowest OOB error
optimal_ntree <- ntree_values[which.min(oob_errors)]
optimal_ntree # ntrees 450

# Train the final model using the best hyperparameters
final_model <- randomForest(
  Diabetes_binary ~.,
  data = diab_2,
  mtry = best_model$mtry,
  ntree = optimal_ntree
)
print(final_model)
# Out of bag error is 25.5%, model accuracy is still around 75%

# Prediction & Confusion Matrix - Train Data
final_p1 <- predict(final_model, diab_train)
confusionMatrix(final_p1, diab_train$Diabetes_binary) # Accuracy 79.34%

# Prediction & Confusion Matrix - Test Data
final_p2 <- predict(final_model, diab_test)
confusionMatrix(final_p2, diab_test$Diabetes_binary) # Accuracy 79.07%

# Plot rf
plot(final_model)

# Tuning the parameter, test accuracy raise in 5%

# Find out which variable decreases the model accuracy
var_importance <- importance(final_model)
print(var_importance)
# 1.GenHlth 2.BMI

# Show it to 2 dimension plot, but data has many factor variables, how?
# PCA is a linear method, retain the most important info
# t-SNE is a nonlinear method, preserving local structures
# Maybe use t-SNE because it has only 2 classifiers

