###### Week 5: Exploratory Data Analysis of the Games Sales Data

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

# 1. Import data
sales <- read.csv(file.choose(), header = TRUE)


# 2. Sense check data

dim(sales)

str(sales)

as_tibble(sales)

View(sales)
# There are 16598 obseravtions and 9 columns in the data frame.


# 3. Check for missing values
sum(is.na(sales))

# Notes: There no missing values in the data

# descriptive summary of data
summary(sales)
skim(sales)

# Notes:
#From the summary we can see that the range in the values for the NA_Sales, EU_Sales and Global_Sales are
# widely distributed suggesting that the data might not be normally distributed.


# 4. Transform the dataset using string manipulation

# a) Convert all data in Genre to lower case:
sales$Genre <- str_to_lower(sales$Genre)

# view df
head(sales)

# b) Merge genre and platform columns

sales$Genre_Platform <- str_c(sales$Genre, sales$Platform, sep = " ")

# c) drop Genre and platform columns
sales1 <- select(sales, -c(Genre, Platform))

# view df
head(sales1)


# 5. Visualise the Data


# a) Plot Boxplots for the sales variables

# Boxplots of the sales variables:
boxplot(sales$Global_Sales) 
boxplot(sales$NA_Sales) 
boxplot(sales$EU_Sales) 

# Notes:
#The boxplots for all 3 sales variables show there to be many outliers. These outliers however are not due to errors in data collection but 
# are legitmate sales values as such I have taken the decision to keep the outliers.

# a) Plot Histogram to check the distribution and skewness of the sales data

# Global_Sales Histogram
ggplot(data = sales1,
       mapping = aes(Global_Sales)) +
  geom_histogram(fill = "red",
                 color = "black",
                 bins = 20) +
  labs(title = 'Global Sales distribution',
       y = 'Frequency',
       x = 'Global Sales') +
  theme_classic()

# EU_Sales Histogram
ggplot(data = sales1,
       mapping = aes(EU_Sales)) +
  geom_histogram(fill = "red",
                 color = "black",
                 bins = 20) +
  labs(title = 'EU Sales distribution',
       y = 'Frequency',
       x = 'EU Sales') +
  theme_classic()


# NA_Sales Histogram
   ggplot(data = sales1,
       mapping = aes(NA_Sales)) +
  geom_histogram(fill = "red",
                 color = "black",
                 bins = 20) +
  labs(title = 'NA Sales distribution',
       y = 'Frequency',
       x = 'NA Sales') +
  theme_classic()

# #Notes: 
   #The histogram for the sales data variable (Global_Sales,EU_Sales, NA_Sales) suggests that the data is extremely skewed to the right and thus confirms that it's not normally distributed.
   # As Multiple linear regression doesn't assume that variables are normally distributed there isn't a need to normalise the data.
   


# 6.Which variables will you use for studying the correlation? 
   
   
# To predict Global Sales I will be using the NA_Sales and EU_Sales variables as it is expected that these variables will be positively correlated with 
# Global_Sales. To test this I will plot the correlation with a scatter plot.
   
# a) Check the correlation of Global_Sales with the two variables that will be used to predict it.
   
# correlation between NA sales and global sales
cor(sales1$Global_Sales, sales1$NA_Sales)

# correlation between EU sales and global sales
cor(sales$Global_Sales, sales$EU_Sales)

# Notes:
    # At 0.941 Global sales is strongly positively correlated with both NA sales. There is also a strong a positive
    # correlation with EU_Sales

# correlation between NA sales and EU sales
cor(sales$NA_Sales, sales$EU_Sales)

# Notes:
      # Although not as strong, the two predictor variables are also strongly correlated at 0.767



# b) Scatter plot of the two variables that will be used to predict Global_sales

# Scatter plot of global sales and North America sales
  ggplot(data = sales1,
         mapping = aes(x = Global_Sales, 
             y = jitter(NA_Sales,))) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', 
              lwd = 0.5, 
              color = 'red')+
  labs(title = 'Global Sales vs North America sales',
       x = 'Global Sales',
       y = 'NA Sales')+
  theme_light()

# Scatter plot of global sales and EU sales

  ggplot(data = sales1,
         mapping = aes(x = Global_Sales, 
             y = EU_Sales)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', 
              lwd = 0.5, 
              color = 'red')+
  labs(title = 'Global sales vs EU sales',
       x = 'Global Sales',
       y = 'EU Sales')+
  theme_light()

# Notes:
     # The two scatter plots above show that there is linear relationship between Global_Sales and the two variables. As one of the assumptions of linear models is linearity between
     # the x and y variables we can use NA_Sales and EU_Sales to build an MLR model to predict Global_Sales.

# 7.  Save and export tranformed sales dataframe to csv as sales1
write.csv(sales1, file = '/Users/hamdihassan/Desktop/Data Analysis training/Course_3_Adavnced_Analytics/Assignment 3/sales_transformed.csv')
