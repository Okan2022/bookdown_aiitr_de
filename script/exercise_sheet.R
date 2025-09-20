#Exercise Sheet AIITR 
#Okan Sarioglu
#21-07-2025

#===============================================================================

## Exercise 1: Standard Descriptive Statistics

#In this exercise we will work with the built-in iris package in R:

# a. Calculate the mode, mean and the median for the iris$Sepal.Length variable

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# b. Calculate the interquartile range, variance and the standard deviation 
#    for iris$Sepal.Length

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# c. Calculate all five measures at once by using a function that does so 
#   (Choose by yourself, which one you want to use)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# Exercise 2: Contingency Tables and Correlations
# 
# a. Make a Contingency Table for esoph$agegp and esoph$alcgp

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# b. Cut down the iris dataset to Sepal.Length, Sepal.Width, Petal.Length and 
#    Petal.Width and save it in an object called iris_numeric.

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# c. Make a correlation matrix with iris_numeric

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# d. Make the correlation matrix prettyChapter 4: Exploratory Data Analysis

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# Exercise 3: Working with packages
# 
# a. Use a function to get an overview of the dataset mtcars

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# b. Have a look at the structure of the missing values in mtcars

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# c. Make an automatized EDA report for mtcars

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

## Exercise Section

# To understand linear regression, we do not have to load any complicated data. 
# Let us assume, you are a market analyst and your customer is the production 
# company of a series called "Breaking Thrones". The production company wants to 
# know how the viewers of the series judge the final episode. You conduct a 
# survey and ask people on how satisfied they were with the season final and 
# some social demographics. Here is your codebook:

#  | Variable     | Description                                                                                                                                                    |
#  |------------------|------------------------------------------------------|
#  | id           | The id of the respondent                                                                                                                                       |
#  | satisfaction | The answer to the question 
#                 | "How satisfied were you with the final episode of Breaking 
#                 | Throne?", where 0 is completely dissatisfied and 10 
#                 | completely satisfied |
#  | age          | The age of the respondent                                                                                                                                      |
#  | female       | The Gender of the respondent, where 0 = Male, 1 = Female                                                                                                       |


### Exercise 1: Linear Regression with two variables

#You want to know if age has an impact on the satisfaction with the last episode. 
#You want to conduct a linear regression.

#a\. Calculate $\beta_0$ and $\beta_1$ by hand

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#b\. Calculate $\beta_0$ and $\beta_1$ automatically with R

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#c\. Interpret all quantities of your result: Standard Error, t-statistic, 
#    p-value, confidence intervals and the $R^2$.

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#d\. Check for influential outliers

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

### Exercise 2: Multivariate Regression

#a\. Add the variable `female` to your regression

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#b\. Interpret the Output. What has changed? What stays the same?

#c\. Make an interaction effect between age and gender and interpret it!

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#d\. Plot the interaction and make the plot nice

#-------------------------------------------------------------------------------

#-#-------------------------------------------------------------------------------

## Exercise Section

### Exercise 1: Writing a loop

# Write a `for` loop that prints the square of each number from 1 to 10

#-------------------------------------------------------------------------------

#Assigning an object for a better workflow
number <- 10

#The Loop 

#-------------------------------------------------------------------------------

### Exercise 2: Writing a function

# Write a function that takes the input x and squares it:

#-------------------------------------------------------------------------------

#Defining a function for squaring

sq <- function (x) {
  
  
  
}

#-------------------------------------------------------------------------------

#Defining a vector containing a vector from 1 to 10 
numbers <- c(1:10) 

#Applying the number 
sq(numbers)

#-------------------------------------------------------------------------------

### Exercise 3: The midnight Formula

#This is the midnight formula separated in two equations:

#$x_{1,2} = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$

# Make **one function** for the midnight formula, so the output are $x_1$ and 
# $x_2$. Test it with a = 2, b = -6, c = -8

# **Hint**: You need two split up the formula into two equations with two outputs.

#-------------------------------------------------------------------------------

mnf <- 
  
  mnf(2, 6, 8)

#-------------------------------------------------------------------------------



























