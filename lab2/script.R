# INST314
# Section 0201
# Spring 2021, Week 3
#
# Alec K. Mattu
# 02-14-2021

# Step 0
# Import the CDC dataset from openintro.org
# The data is imported to the variable cdc
source("http://www.openintro.org/stat/data/cdc.R")

# Step 1
# Create a scatterplot of weight vs. desired weight
# x = Actual weight
# y = Desired weight
plot(cdc$weight, cdc$wtdesire, xlab = "Weight", ylab = "Weight (Desired)")

# Step 2
# Create a new variable (wdiff) which contains the difference
# between weight and desired weight.
# By using examples of the in-class lab/demo video, we can
# simply assign the evaluation of weight-wtdesire to a new var
wdiff <- (cdc$weight - cdc$wtdesire)

# Step 3
# Find the data type of wdiff
typeof(wdiff) # integer

# Step 4
# Use graphs and functions to determine the 
# center, shape, and spread of wdiff.
summary(wdiff) # Gather numerical data about the dataset
plot(density(wdiff)) # Visualize the dataset
hist(wdiff) # Visualize the datasets

# Step 5
# Use numerical summaries and box-plots to determine
# if men tend to view weight differently than women
# We need to split the data from men/women into their
# own variables to get numerical data from it
# Then we will use the summary function and build a boxplot
men_wdiff <- subset(data.frame(wdiff, cdc$gender), cdc.gender == "m")
wom_wdiff <- subset(data.frame(wdiff, cdc$gender), cdc.gender == "f")
summary(men_wdiff)
summary(wom_wdiff)
plot(x = gender, y = wdiff, data = cdc, geom = "boxplot", names = c("Men", "Women"))