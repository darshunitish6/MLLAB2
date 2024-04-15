# Load necessary libraries
library(caret)
library(rpart)
library(rpart.plot)  # for visualization

# Read data from the provided dataset
file_path <- "C:/Users/MSI/OneDrive/Desktop/ML/oulad-assessments.csv" 
# Update with the actual file path
data <- read.csv(file_path)
data <- na.omit(data)

# First, split the data into a training set and a testing set
set.seed(123)  # for reproducibility
training_indices <- createDataPartition(data$score, p = 0.8, list = FALSE)  # Assuming 'score' is the target variable
training_set <- data[training_indices, ]
testing_set <- data[-training_indices, ]

# Train the model
model <- rpart(score ~ ., data = training_set)

# Make predictions
predictions <- predict(model, testing_set)

# Convert predictions to factor
predictions <- as.factor(ifelse(predictions > 0.5, 1, 0))  # Assuming 'score' is a binary variable

# Convert testing_set$score to factor if it's not already
testing_set$score <- as.factor(testing_set$score)

# Ensure the levels of 'predictions' and 'testing_set$score' are in the same order
levels(predictions) <- levels(testing_set$score)

# Evaluate the model
confusionMatrix(predictions, testing_set$score)

# Visualize the decision tree
rpart.plot(model)