# Set working directory
setwd("C:/RProj/SpaceTitanic")

# Install packages if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}

if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

if (!requireNamespace("xgboost", quietly = TRUE)) {
  install.packages("xgboost")
}

if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}



library(tidyverse)
library(rpart)
library(randomForest)
library(xgboost)
library(caret)

set.seed(1000)


# Load the datasets
if (file.exists(train_file_path) && file.exists(test_file_path)) {
  df.train <- read.csv(train_file_path)
  df.test <- read.csv(test_file_path)}



df.test$Transported <- NA
combi <- rbind(df.train,df.test)
combi$SES <- combi$Spa + combi$RoomService + combi$ShoppingMall + combi$VRDeck + combi$FoodCourt
combi$Deck <- sapply(combi$Cabin, FUN=function(x){strsplit(x, split="[/]")[[1]][1]})
df.train <- combi[1:8693,]
df.test <- combi[8694:12970,]
str(combi)

# Check the structure of df.train
str(df.train)

# Ensure that 'Transported' is a factor
df.train$Transported <- as.factor(df.train$Transported)

# Check the structure again
str(df.train)

# Check for missing values
sum(is.na(df.train))

# Impute missing values
df.train <- na.omit(df.train)

# Impute missing values in 'VRDeck'
df.train$VRDeck <- ifelse(is.na(df.train$VRDeck), mean(df.train$VRDeck, na.rm = TRUE), df.train$VRDeck)

# Train the random forest model
fit <- randomForest(Transported ~ HomePlanet + CryoSleep + Destination + Age +
                      VIP + RoomService + FoodCourt + ShoppingMall + Spa + VRDeck + Deck,
                    data = df.train,
                    importance = TRUE,
                    ntree = 2000)

# Visualize variable importance
varImpPlot(fit)

#First PRediction
# Check for missing values in the test set
if (any(is.na(df.test))) {
  # Impute missing values in the test set (using the same strategy as in the training set)
  df.test$VRDeck <- ifelse(is.na(df.test$VRDeck), mean(df.train$VRDeck, na.rm = TRUE), df.test$VRDeck)
}

# Make predictions on the test set
Prediction <- predict(fit, df.test)

# Create a submission data frame
submit <- data.frame(PassengerId = df.test$PassengerId, Transported = Prediction)

# Write the submission file
write.csv(submit, file = "1-forest.csv", row.names = FALSE)


# Second Prediction
# Assuming 'df.train' is your training dataset
df.train <- drop_na(df.train)

# Train the random forest model with cross-validation
fit_cv <- randomForest(Transported ~ HomePlanet + CryoSleep + Destination + Age +
                         VIP + RoomService + FoodCourt + ShoppingMall + Spa + VRDeck + Deck,
                       data = df.train,
                       importance = TRUE,
                       ntree = 2000)

# Visualize variable importance
varImpPlot(fit_cv)

# Prediction 
Prediction <- predict(fit, df.test)
submit <- data.frame(PassengerId = df.test$PassengerId, Transported = Prediction)
write.csv(submit, file = "2-forest.csv", row.names = FALSE)

#Random Forest
# GEts results of .68856

# Make predictions on the test set using the cross-validated model
Prediction_cv <- predict(fit_cv, df.test)

# Create a submission data frame
submit_cv <- data.frame(PassengerId = df.test$PassengerId, Transported = Prediction_cv)

# Write the submission file
write.csv(submit_cv, file = "rf-forest.csv", row.names = FALSE)

# Let us now try K-fold Cross Validation 
library(randomForest)
library(caret)

# Set the number of folds (e.g., 5 for 5-fold cross-validation)
num_folds <- 5

# Create a data frame for cross-validation with 'Transported' as a factor
df.train$Transported <- as.factor(df.train$Transported)

# Set up k-fold cross-validation
folds <- createFolds(df.train$Transported, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize an empty vector to store predictions
all_predictions <- vector("list", length = num_folds)




# Perform k-fold cross-validation
# Gets results of .6902

for (i in 1:num_folds) {
  # Create training and test sets for the current fold
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  
  train_data <- df.train[train_indices, ]
  test_data <- df.train[test_indices, ]
  
  # Train the random forest model
  fit <- randomForest(Transported ~ HomePlanet + CryoSleep + Destination + Age +
                        VIP + RoomService + FoodCourt + ShoppingMall + Spa + VRDeck + Deck,
                      data = train_data,
                      importance = TRUE,
                      ntree = 2000)
  
  # Make predictions on the test set
  predictions <- predict(fit, test_data)
  
  # Store predictions for evaluation
  all_predictions[[i]] <- data.frame(PassengerId = test_data$PassengerId, Transported = predictions)
}

# Combine predictions from all folds
final_predictions <- do.call(rbind, all_predictions)

# Evaluate the overall performance, e.g., calculate accuracy, precision, recall, etc.
# You can use the confusionMatrix function from the caret package
conf_matrix <- confusionMatrix(final_predictions$Transported, df.train$Transported)
print(conf_matrix)

# Make predictions on the test set
Prediction_final <- predict(fit_final, df.test)

# Create a submission data frame
submit_final <- data.frame(PassengerId = df.test$PassengerId, Transported = Prediction_final)

# Write the submission file
write.csv(submit_final, file = "k-fold.csv", row.names = FALSE)


