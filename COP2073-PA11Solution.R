# COP2073C Programming Assignment 11
# R. Williams
# 11/21/2026
# Analyze vehicle weight using linear models

install.packages("tidyverse")
install.packages("modelr")

library(tidyverse)
library(modelr)

# Step 1: Create a tibble
cars_data <- mtcars %>%
  select(cyl, wt) %>%
  mutate(cyl = factor(cyl))

print(cars_data)

# Step 2: Fit a linear model
cars_model <- lm(wt ~ cyl, data = cars_data)

summary(cars_model)

# Step 3: Create a prediction grid
cars_predictions <- cars_data %>%
  data_grid(cyl) %>%
  add_predictions(cars_model)

print(cars_predictions)

# Step 4: Plot original data and predictions
ggplot(cars_data, aes(x = cyl, y = wt)) +
  geom_point() +
  geom_point(
    data = cars_predictions,
    aes(y = pred),
    color = "red",
    size = 4
  ) +
  labs(
    title = "Vehicle Weight by Number of Cylinders",
    x = "Number of Cylinders",
    y = "Weight (1000 lbs)"
  )

# Step 5: Analysis of results
# The linear model shows a relationship between the
# number of cylinders and vehicle weight.
# Vehicles with 4 cylinders have the lowest predicted
# average weight, while 8-cylinder vehicles have the
# highest predicted average weight.
# The red points represent the model's predicted
# weights for each cylinder category.
# Overall, vehicles with more cylinders tend to
# weigh more in the mtcars dataset.