# Pei-Yu Jheng, 11/20/23, ALY6010 Week 3

#the environment reset code 
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

#Load any standard packages might need
library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(pacman)

#Spotify Top 10000 Streamed Songs
data <- read.csv("C:\\Users\\user\\Desktop\\2023 Fall\\ALY6010\\Module 3\\Project 3\\Spotify_final_dataset.csv")
class(data) #The class() function is used to show the object's category
glimpse(data) 

#Prepare data.frame for analysis
library("janitor")
data <- clean_names(data) #turn variables name to lower case
glimpse(data) #this function is used to see every column in a data frame.

#Remove any rows that contain NAs
df <- data %>% drop_na()

#Correcting data types
str(df)
#The data types are all correct, so no need to change

#Removing columns
df = subset(df, select = -c(position))

#Reorganizing the data
#Use regex to remove "(x10)" and convert to integer while retaining 0 in peak_position_x_times column
df$peak_position_x_times <- as.integer(gsub("\\(x(\\d+)\\)", "\\1", df$peak_position_x_times))
#The gsub function and regular expressions are used here to replace "(x10)" with an empty string 
#while retaining the numeric part. 
#Finally, use the as.integer function to convert the character vector to an integer vector 
#while retaining the 0s. 
#This way, you get a vector of integers where "(x10)" is removed and 0 is retained.

glimpse(df)

##Produce descriptive statistics
#Determine descriptive statistics for interesting variables
#max, min, mean, median, mode. standard deviation, variance, and range.
summary(df) #returns the minimum, maximum, mean, median, and 1st and 3rd quartiles for a numerical vector.

#Load summarytools package
library(summarytools)

#Generate descriptive statistics table
descr <- descr(df)

descr

# Use describe() to obtain descriptive statistics
library(psych)

description <- describe(df)

# Print the result
print(description)

# Generate descriptive statistics tables by group
grouped_stats <- df %>%
  group_by(df$artist_name) %>%
  summarise_all(list(mean = mean, sd = sd, min = min, max = max))

#Alternatively, use summarytools package for more detailed tables
grouped_stats_summarytools <- dfSummary(df)
print(grouped_stats_summarytools)

##
max(df$top_10_x_times)
df[df$top_10_x_times == 302, ]

#Select rows with "Peak Position" <= 10
top_10 <- subset(df, peak_position <= 10, select = c(peak_position, total_streams))

print(top_10)
#means the songs' peak position has been top 10

##Part I – task: Conduct a one-sample t-tests for mean using an appropriate variable from the data set.
#conduct a one-sample t-test on the variable "Total Streams" from top_10 subset in Spotify dataset.
#The null hypothesis (H0) for a one-sample t-test is that 
#there is no significant difference between the sample mean and a known or hypothesized population mean. 
#The alternative hypothesis (H1) is that there is a significant difference.

#My hypotheses
#Null Hypothesis (H0): The mean total streams of the peak position has been top 10 songs on Spotify is equal to a hypothesized value (μ0).
#H0:μ=μ0
#Alternative Hypothesis (H1): The mean total streams of the peak position has been top 10 songs on Spotify is not equal to the hypothesized value.
#H1:μ not equal to μ0

#I want to test whether the mean total streams are significantly different from 90,000,000

# Set hypothesized mean
hypothesized_mean <- 90000000

# Perform one-sample t-test
t_test_result <- t.test(top_10$total_streams, mu = hypothesized_mean)

# Print the test result
print(t_test_result)

#If the p-value is less than your chosen significance level (e.g., 0.05), 
#you would reject the null hypothesis and conclude that there is a significant difference.
#If the p-value is greater than your chosen significance level, you would fail to reject the null hypothesis
#, suggesting no significant difference.

##Part II – task: Conduct hypothesis testing for p-value using an appropriate variable from the data sets.
#conduct a hypothesis test for the p-value using the variable "Total Streams" from Spotify dataset
#conducting a hypothesis test for the proportion of songs with "Total Streams" greater than 500,000 in the Spotify dataset.

df$binary_streams <- ifelse(df$total_streams > 500000, 1, 0)

# Set your hypothesized proportion
hypothesized_proportion <- 0.5  # Assuming 50% as a starting point

# Perform hypothesis test for the p-value
prop_test_result <- prop.test(sum(df$binary_streams), n = length(df$binary_streams), p = hypothesized_proportion)

# Print the test result
print(prop_test_result)

#conducting a hypothesis test for the proportion of songs with "Total Streams" greater than 500,000 in the top 10 songs. 

top_10$binary_streams <- ifelse(top_10$total_streams > 500000, 1, 0)

# Set your hypothesized proportion
hypothesized_proportion <- 0.5  # Assuming 50% as a starting point

# Perform hypothesis test for the p-value
prop_test_result1 <- prop.test(sum(top_10$binary_streams), n = length(top_10$binary_streams), p = hypothesized_proportion)

# Print the test result
print(prop_test_result1)


#If the p-value is less than your chosen significance level (e.g., 0.05), you would reject the null hypothesis
#, suggesting a significant difference in the mean total streams from the hypothesized value of 500,000. 
#If the p-value is greater, you would fail to reject the null hypothesis, suggesting no significant difference.

#SECOND TEST
peak_df <- subset(df, peak_position <= 10, select = c(peak_position, peak_streams))
hypothesized_mean1 <- 1500000

# Perform one-sample t-test
t_test_result1 <- t.test(peak_df$peak_streams, mu = hypothesized_mean1)

# Print the test result
print(t_test_result1)

#p-value
df$binary_streams <- ifelse(df$peak_streams > 1500000, 1, 0)

# Set your hypothesized proportion
hypothesized_proportion <- 0.5  # Assuming 50% as a starting point

# Perform hypothesis test for the p-value
prop_test_result2 <- prop.test(sum(df$binary_streams), n = length(df$binary_streams), p = hypothesized_proportion)

# Print the test result
print(prop_test_result2)


