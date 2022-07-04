# import packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(purrr)

# 1. Load and sense check data

# read data
lego <- read.csv(file.choose(), header = TRUE)

# View data frame
View(lego)

# check df type
typeof(lego)

# check class
class(lego)

# check df dimensions
dim(lego)

# check for missing values
sum(is.na(lego))

# 2. Visualise the data to find insights related to the following questions.

# 2.1. Which age group submits the most reviews?

# a)Use the aggregate function to sum the number of reviews by age
ages_reviews <- aggregate(num_reviews~ages, lego, sum)


# b) Plot the total reviews by age

ggplot(data = ages_reviews, 
       mapping = aes(x=ages, 
                     y=num_reviews,
                     fill=factor(ages))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = num_reviews), vjust = 0) +
  labs(title = 'Total reviews by age ',
       x= 'Age ',
       y = 'Number of reviews')+
  theme_classic()

# From the visualisation we can see that ages 8 and 9 years of age have by far the highest reviews. However the chart is ver
# messy and difficult to identify the ages clearly. With this in mind I will create 3 age groups and replot the chart.


# c) Create 3 age groups and store in a new column in the data frame
lego <- lego %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      ages <= 10            ~ "0-10",
      ages > 10 & ages <= 20 ~ "11-20",
      ages > 20 & ages <= 30 ~ "21-30",
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-10", "11-20","21-30")))

# view data
head(lego)


# d) Use aggregate function to sum ages by num_reviews and store in a new object 
age_group_reviews <- aggregate(num_reviews~age_group, lego, sum)


# e) Visualise the age groups by num_reviews

ggplot(data = age_group_reviews, 
       mapping = aes(x=age_group, 
                     y=num_reviews)) +
  geom_bar(stat = 'identity', fill = (c('dark green','light blue',' yellow'))) +
  geom_text(aes(label = num_reviews), vjust = 0) +
  labs(title = 'Total reviews by age groups',
       x= 'Age groups',
       y = 'Number of reviews')+
  theme_classic()

# As we can see from the visualisation age group 0-10 accounts for more than 50% of all reviews.
# Children at these ages are unlikely to be completing reviews online so this suggests that parents and carers
# who buy lego products for their children are much more likley to leave a review than than those between 11 and 30 years of age.



# 3. What is the most expensive Lego set purchased by customers who are at least 25 years old (>25 years)?

# a) Create a new data frame that contains only data for customers 25 years or older
lego_age25 <- round(lego[lego$ages>=25,],2)

# sense check new data frame
str(lego_age25)
# Note: new data frame has 8 columns as expected and 2313 observations compared 12261 in the original dataset

# b)Count the frequency of age and group by list_price using the groupby() and summarise() functions 
#to get a total purchase per price list

age_purchases <- lego_age25 %>%
  group_by(list_price) %>%
  dplyr::summarise(ages = n()) %>%
  arrange(round(desc(ages), 2))

# View the grouped data frame
head(age_purchases)


# c) Arrange the data frame by descending order of list_price and slice the top 10 values

# Use arrange() function order values in descending order and then slice top 10 and store in a new object
expensive <- age_purchases %>%
  arrange(desc(list_price)) %>%
  slice(1:10) 

# d) Use max() function to determine the most expensive price with an associated purchase
max(age_count$list_price)

# Notes:
#The the most expensive purchase recorded within the target age group is $259.87 which answers the question.
# The get an idea of how often this product and others in the top 10 most expensive are purchase I will visualise it in a plot.

# e) Plot the top 10 list price by the number of purchases and highlight the most expensive list price.
ggplot(data = expensive, 
       mapping = aes(x=factor(list_price), 
                     y=ages,
                     fill = ifelse(list_price == "259.87", "Highlighted",'Normal'))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ages), vjust = 0) +
  labs(title = 'Most Expensive Product purchased by over 25s',
       x= 'List Price',
       y = 'Number of Purchaes')+
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill="light grey", 
                                         size=0.5, linetype='blank'))+
  scale_fill_discrete(name = "List Price", 
                      labels = c("Most Expensive", "Others"))

  
# Notes: The most expensive lego set purchased by customers 25 and over is $259.87.
# This product has been purchased on 1 occasion by that particular age group. 

# f) Most frequent price of purchases for customers 25 and over

# Use arrange() function order values in descending order and then slice top 10 and then store in a new object
popular_price <- age_purchases %>%
  arrange(desc(ages)) %>%
  slice(1:10) 

# g) Visualize most frequent price for purchase for customers 25 or older
ggplot(data = popular_price, 
       mapping = aes(x=factor(list_price), 
                     y=ages,
                     fill = factor(list_price))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ages), vjust = 0) +
  labs(title = 'Most Expensive Product purchased by over 25s',
       x= 'List Price',
       y = 'Number of Purchaes')
  

# 3.1 most popular products by piece_count for customers 25 and older.

# a) Count the frequency of age and group bypiece_count using the groupby() and 
#summarise() functions.
age_product <- lego_age25 %>%
  group_by(piece_count) %>%
  dplyr::summarise(ages = n()) %>%
  arrange(desc(ages))

# View the grouped data frame
head(age_product)

#  b) Use arrange() function order values in descending order and then slice top 10 and store in a new object
popular_product <- age_product %>%
  arrange(desc(ages)) %>%
  slice(1:10)

# c) Visualize most frequently product by piece_count for customers 25 or older
ggplot(data = popular_product, 
       mapping = aes(x=factor(piece_count), 
                     y=ages,
                     fill = factor(piece_count))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ages), vjust = 0) +
  labs(title = 'Most Popular piece count purchased by over 25s',
       x= 'Peice Count',
       y = 'Number of Purchaes')

# Notes:
#The visualisation shows that the most popular piece count purchased by customers in th eage group
# 25 years and older is 494 peices. 

