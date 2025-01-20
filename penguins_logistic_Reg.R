library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(mlbench)
theme_set(theme_bw())

url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/inst/extdata/penguins.csv"
penguins <- read.csv(url)

head(penguins)
summary(penguins)

# Drop rows with missing values
penguins <- na.omit(penguins)

# Select numeric columns 
numeric_data <- penguins[, 3:6]  

# k-means clustering
set.seed(123)
kmeans.result <- kmeans(numeric_data, centers = 3)

penguins$Cluster <- as.factor(kmeans.result$cluster)
head(penguins)

# cluster centers
kmeans.result$centers
# cluster IDs
kmeans.result$cluster

# calculate distances between objects and cluster centers
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((numeric_data - centers)^2))

outliers <- order(distances, decreasing=T)[1:10]
print(outliers)
print(numeric_data[outliers,])


# Plot clusters using ggplot2
ggplot(penguins, aes(x = bill_depth_mm, y = body_mass_g, color = Cluster)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_point(data = penguins[outliers, ], aes(x = bill_depth_mm, y = body_mass_g),
             color = "black", shape = 8, size = 9) +
  geom_point(data = as.data.frame(kmeans.result$centers), 
             aes(x = bill_depth_mm, y = body_mass_g), 
             color = "purple", shape = 4, size = 9, stroke = 1.5) +
  labs(title = "Penguin Clusters with K-means",
       x = "Bill Depth (mm)",
       y = "Body Mass(g)") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme(legend.position = "bottom")

# Remove outliers from the dataset
penguins_no_outliers <- penguins[-outliers, ]

head(penguins_no_outliers)
summary(penguins_no_outliers)
nrow(penguins) - nrow(penguins_no_outliers)

# Convert 'sex' 
penguins_no_outliers$sex <- factor(penguins_no_outliers$sex, levels = c("male", "female"))

# Split the data into training and testing sets
set.seed(123)
training.samples <- penguins_no_outliers$sex %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data <- penguins_no_outliers[training.samples, ]
test.data <- penguins_no_outliers[-training.samples, ]

# Define a function to fit a logistic regression model
fit_and_evaluate_model <- function(formula, train_data, test_data) {
  # Fit the logistic regression model
  model <- glm(formula, data = train_data, family = binomial)

  cat("\nModel Summary for:", as.character(formula), "\n")
  print(summary(model))
  
  # Evaluate model on test data
  probabilities <- predict(model, newdata = test_data, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "female", "male")
  
  # Accuracy
  accuracy <- mean(predicted.classes == test_data$sex)
  cat("\nModel Accuracy:", accuracy, "\n")
  
  return(model)
}

# evaluate models with different combinations of predictors
model_1 <- fit_and_evaluate_model(sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, train.data, test.data)
model_2 <- fit_and_evaluate_model(sex ~ bill_length_mm + body_mass_g, train.data, test.data)
model_3 <- fit_and_evaluate_model(sex ~ bill_depth_mm + body_mass_g, train.data, test.data)
model_4 <- fit_and_evaluate_model(sex ~ flipper_length_mm + body_mass_g, train.data, test.data)

# Visualize the effect of predictors
train.data %>%
  mutate(prob = ifelse(sex == "female", 1, 0)) %>%
  ggplot(aes(bill_depth_mm, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model",
    x = "Bill Depth (mm)",
    y = "Probability of being Female"
  )

train.data %>%
  mutate(prob = ifelse(sex == "female", 1, 0)) %>%
  ggplot(aes(body_mass_g, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model",
    x = "Body Mass (g)",
    y = "Probability of being Female"
  )

# Predict on data
newdata <- data.frame(
  bill_length_mm = c(40, 50),
  bill_depth_mm = c(15, 18),
  flipper_length_mm = c(200, 210),
  body_mass_g = c(4000, 4500)
)

probabilities <- predict(model_1, newdata = newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "female", "male")

cat("\nPredictions for New Data:\n")
newdata <- newdata %>%
  mutate(probability = probabilities, predicted_sex = predicted.classes)
print(newdata)







