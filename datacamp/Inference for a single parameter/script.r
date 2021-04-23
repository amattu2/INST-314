# Print gss data
print(gss)

# Load dplyr
library("dplyr")

gss2016 <- gss %>%
  filter(year == 2016)

######################################

# Previous code
library(dplyr)
gss2016 <- gss %>%
  filter(year == 2016)

# Print gss2016 data
print(gss2016)

# Load ggplot2
library(ggplot2)

# Plot distribution of consci
ggplot(gss2016, aes(x = consci)) +
  # Add a bar layer
  geom_bar()

######################################

# From previous steps
library(dplyr)
gss2016 <- gss %>%
  filter(year == 2016)
library(ggplot2)
ggplot(gss2016, aes(x = consci)) +
  geom_bar()

# Compute proportion of high conf
p_hat <- gss2016 %>%
  summarize(prop_high = mean(consci == "High")) %>%
  pull()

######################################

# Load the infer package
library(infer)

# Create single bootstrap data set
boot1 <- gss2016 %>%
  # Specify the response
  specify(response = consci, success = "High") %>%
  # Generate one bootstrap replicate
  generate(reps = 1, type = "bootstrap")

# See the result
boot1

######################################

# From previous step
library(infer)
boot1 <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 1, type = "bootstrap")

# Using boot1, plot consci
ggplot(boot1, aes(x = consci)) +
  # Add bar layer
  geom_bar()

######################################

# From previous step
library(infer)
boot1 <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 1, type = "bootstrap")

# Compute proportion with high conf
boot1 %>%
  summarize(prop_high = mean(consci == "High")) %>%
  pull()

######################################

# Create bootstrap distribution for proportion with High conf
boot_dist <- gss2016 %>%
  # Specify the response and success
  specify(response = consci, success = "High") %>%
  # Generate 500 bootstrap reps
  generate(reps = 500, type = "bootstrap") %>%
  # Calculate proportions
  calculate(stat = "prop")

# See the result
boot_dist

######################################

# From previous step
boot_dist <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")

# Plot bootstrap distribution of stat
ggplot(boot_dist, aes(x = stat)) +
  # Add density layer
  geom_density()

######################################

# From previous steps
boot_dist <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")
ggplot(boot_dist, aes(x = stat)) +
  geom_density()

# Compute estimate of SE
SE <- boot_dist %>%
  summarize(se = sd(stat)) %>%
  pull()

######################################

# From previous steps
boot_dist <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")
ggplot(boot_dist, aes(x = stat)) +
  geom_density()
SE <- boot_dist %>%
  summarize(se = sd(stat)) %>%
  pull()

# Create CI
c(p_hat - 2 * SE, p_hat + 2 * SE)

######################################

# Create bootstrap distribution for proportion
boot_dist_small <- gss2016_small %>%
  # Specify the variable and success
  specify(response = consci, success = "High") %>%
  # Generate 500 bootstrap reps
  generate(reps = 500, type="bootstrap") %>%
  # Calculate the statistic
  calculate(stat = "prop")

# See the result
glimpse(boot_dist_small)

######################################

# From previous steps
boot_dist_small <- gss2016_small %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")
SE_small_n <- boot_dist_small %>%
  summarize(se = sd(stat)) %>%
  pull()
boot_dist_smaller <- gss2016_smaller %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")

# Compute and save estimate of second SE
SE_smaller_n <- boot_dist_smaller %>%
  summarize(se = sd(stat)) %>%
  pull()

# Compare the results for each dataset size
message("gss2016_small has ", nrow(gss2016_small), " rows and standard error ", SE_small_n)
message("gss2016_smaller has ", nrow(gss2016_smaller), " rows and standard error ", SE_smaller_n)

######################################

# Using gss2016, plot meta_region
ggplot(gss2016, aes(x = meta_region)) +
  geom_bar()

######################################

# From previous step
ggplot(gss2016, aes(x = meta_region)) +
  geom_bar()

# Specify the response for the bootstrap distribution
boot_dist <- gss2016 %>%
  specify(response = meta_region, success = "pacific") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")

######################################

# From previous steps
ggplot(gss2016, aes(x = meta_region)) +
  geom_bar()
boot_dist <- gss2016 %>%
  specify(response = meta_region, success = "pacific") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")

# Calculate std error
SE_low_p <- boot_dist %>%
  summarize(se = sd(stat)) %>%
  pull()

# Compare SEs
c(SE_low_p, SE)

######################################

# Calculate n as the number of rows
n <- nrow(gss2016)

# Calculate p_hat as the proportion in pacific meta region
p_hat <- gss2016 %>%
  summarize(prop_pacific = mean(meta_region == "pacific")) %>%
  pull()

# See the result
p_hat

######################################

# From previous step
n <- nrow(gss2016)
p_hat <- gss2016 %>%
  summarize(prop_pacific = mean(meta_region == "pacific")) %>%
  pull()

# Check conditions
n * p_hat >= 10
n * (1 - p_hat) >= 10

# Calculate SE
SE_approx <- sqrt(p_hat * (1 - p_hat) / n)

# Form 95% CI
c(p_hat - 2 * SE_approx, p_hat + 2 * SE_approx)
