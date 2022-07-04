#### Assignment Week 6
# Import libraries
# whole tidyverse package
library(tidyverse)
# Useful for importing data
library(readr) 
#Useful for data wrangling
library(dplyr) 
#Useful for data wrangling
library(tidyr) 
# Useful for creating tidy tables
library(knitr) 
# useful for working with vectors and functions
library(purrr)
# useful to create insightful summaries of data set
library(skimr)
# useful to create insightful reports on data set
library(DataExplorer)
# install ggiraphExtra
install.packages('ggiraphExtra')
# import ggiraphExtra
library(ggiraphExtra)

# 1. Import data
sales <- read.csv(file.choose(), header = TRUE)

# 2. Sense check data
dim(sales)

str(sales)

as_tibble(sales)

View(sales)

# As expected from the previous EDA there are 16598 observations and 9 variables

# 3. Subset the data to create a dataset that contains only the variables that we will be using for the MLR model

# a) Subset the df 
sales_mlr <- select(sales, c(NA_Sales, EU_Sales, Global_Sales))

# view the head of the df
head(sales_mlr)

# Following the EDA in task 5 we confirmed the correlation and linearity between our predictor variables (NA_Sales, EU_Sales)
# and the variable that will be predicted (Global_Sales). Now we will build the MLR model.

#4. Build the Multiple Linear Regression model
model_mlr = lm(Global_Sales ~ NA_Sales + EU_Sales, data = sales_mlr)
 
# Summary of the model
summary(model_mlr) 

# Notes: The Adjusted R-Squared shows that  96.4%. of the observed values can be explained by the model. This suggests that the model is highly accurate
# The significance of the explanatory variables in the coefficients table is also very high with both variables scoring 3 stars. We can conclude this is a very strong model.

# 4.1 Evaluating the Model

# The residual standard error is the average distance that the observed values fall from the regression line. In our case
# the residual error of 0.2927 is very low.

# Histogram plot of the residual standard error
ggplot(data = model_mlr, aes(x = model_mlr$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black', bins = 20) +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

#Notes:
#From the histogram we can see that the residual errors are normally distributed which 
# supports the assumption of multiple linear regression models that the error values are random.

# This code plots 4 charts: Risuduals vs Fitted, Normal Q-Q, Scale-Location and Risiduals vs Leverage.
plot(model_mlr)

#Notes:
#Residuals vs Fitted: is used to check the assumptions of linearity. The plot shows that the residuals are spread roughly equally around the horizontal line 
#without distinct patterns (red line is approximately horizontal at zero), that is a good indication of having a linear relationship.

#Normal Q-Q: is used to check the normality of residuals assumption. The majority of the residuals follow the 
#straight dashed line, so the assumption is fulfilled.

#Scale-Location: is used to check the homoscedasticity of residuals (equal variance of residuals). As the residuals are spread randomly 
#around the horizontal line with randomly spread points, we can be confident the assumption is fulfilled.



# 5. Visualise the MLR model

pred_plt <- ggPredict(model_mlr, terms = c("EU_Sales", "NA_Sales"))

#Plot the MLR model
plot(pred_plt) + 
  labs(
    x = "North America Sales", 
    y = "Global Sales (units)", 
    title = "Predicted Global Sales (units)"
  )


# 6. Use model to predict sales figures for next year

# I will create a copy of the data frame to store the predicted values
sales_predictions <- data.frame(sales_mlr)


# Predict the next years sales for all products using our model in the predict function and store the predict values in a new column in the data frame
sales_predictions$Predicted_Sales <- predict(lm(model_mlr), newdata = sales_mlr)


# display                    
View(sales_predictions)        

# Notes: From the results in the predict column we can see that the model has successfully predicted unit sales for each product
# in the next financial year.
# The number 1 ranked product is predicted to increase in units sold from 82.74 million units this year to 86.96 in the next year.




