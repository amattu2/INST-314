# Load the CSV file from downloads
present <- read.csv("~/Downloads/present.csv")

# Question 2a. Find the number of years
present$year
min(present$year)
max(present$year)

# Question 3a. Dimensions and column names
dim(present)
names(present)

# Question 4a. Boy to girl ratio
plot(x = present$year, y = present$boys, col="blue", type = 'l')
lines(x = present$year, y = present$girls, col="pink", type = "l")

# Question 5a. Maximum births
max(present$boys + present$girls)

