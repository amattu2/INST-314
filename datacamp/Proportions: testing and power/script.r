# Using `gss2016`, plot postlife
ggplot(gss2016, aes(x = postlife)) +
  # Add bar layer
  geom_bar()

######################################

# From previous step
ggplot(gss2016, aes(x = postlife)) +
  geom_bar()

# Calculate and save proportion that believe
p_hat <- gss2016 %>%
  summarize(prop_yes = mean(postlife == "YES")) %>%
  pull()

# See the result
p_hat

######################################

# Generate one data set under H0
sim1 <- gss2016 %>%
  # Specify the response and success
  specify(response = postlife, success = "YES") %>%
  # Hypothesize the null value of p
  hypothesize(null = "point", p = 0.75) %>%
  # Generate a single simulated dataset
  generate(reps = 1, type = "simulate")

# See the result
sim1

######################################

# From previous step
sim1 <- gss2016 %>%
  specify(response = postlife, success = "YES") %>%
  hypothesize(null = "point", p = 0.75) %>%
  generate(reps = 1, type = "simulate")

# Using sim1, plot postlife
ggplot(sim1, aes(x = postlife)) +
  # Add bar layer
  geom_bar()

######################################

# From previous steps
sim1 <- gss2016 %>%
  specify(response = postlife, success = "YES") %>%
  hypothesize(null = "point", p = 0.75) %>%
  generate(reps = 1, type = "simulate")

# Compute proportion that believe
sim1 %>%
  summarize(prop_yes = mean(postlife == "YES")) %>%
  pull()

######################################

# Generate null distribution
null <- gss2016 %>%
  specify(response = postlife, success = "YES") %>%
  hypothesize(null = "point", p = 0.75) %>%
  generate(reps = 500, type = "simulate") %>%
  # Calculate proportions
  calculate(stat = "prop")

######################################

# From previous step
null <- gss2016 %>%
  specify(response = postlife, success = "YES") %>%
  hypothesize(null = "point", p = 0.75) %>%
  generate(reps = 500, type = "simulate") %>%
  calculate(stat = "prop")

# Visualize null distribution
ggplot(null, aes(x = stat)) +
  # Add density layer
  geom_density() +
  # Add line at observed
  geom_vline(xintercept = p_hat, color = "red")

######################################

# From previous step
null <- gss2016 %>%
  specify(response = postlife, success = "YES") %>%
  hypothesize(null = "point", p = 0.75) %>%
  generate(reps = 500, type = "simulate") %>%
  calculate(stat = "prop")

null %>%
  summarize(
    # Compute the one-tailed p-value
    one_tailed_pval = mean(stat >= p_hat),
    # Compute the two-tailed p-value
    two_tailed_pval = 2 * one_tailed_pval
  ) %>%
  pull(two_tailed_pval)

######################################

# Plot distribution of sex filled by cappun
ggplot(gss2016, aes(x = sex, fill = cappun)) +
  # Add bar layer
  geom_bar(position = "fill")

######################################

# Create null distribution
null <- gss2016 %>%
  # specify the response and explanatory as well as the success
  specify(cappun ~ sex, success = "FAVOR") %>%
  # set up null hypothesis
  hypothesize(null = "independence") %>%
  # generate 500 permuted reps
  generate(reps = 500, type = "permute") %>%
  # calculate the statistics
  calculate(stat = "diff in props", order = c("FEMALE", "MALE"))

######################################

# Create the bootstrap distribution
boot <- gss2016 %>%
   specify(cappun ~ sex, success = "FAVOR") %>%
   generate(reps = 500, type = "bootstrap") %>%
   calculate(stat = "diff in props", order = c("FEMALE", "MALE"))

######################################

# From previous step
boot <- gss2016 %>%
  specify(cappun ~ sex, success = "FAVOR") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("FEMALE", "MALE"))

# Compute the standard error
SE <- boot %>%
  summarize(se = sd(stat)) %>%
  pull()

# Form the CI (lower, upper)
c(d_hat - 2 * SE, d_hat + 2 * SE)

######################################

# Inspect coinflip
gssmod %>%
  select(coinflip)

# Compute two proportions
p_hats <- gssmod %>%
  group_by(coinflip) %>%
  summarize(prop_favor = mean(cappun == "FAVOR")) %>%
  pull()

# See the result
p_hats

######################################

# From previous step
p_hats <- gssmod %>%
  group_by(coinflip) %>%
  summarize(prop_favor = mean(cappun == "FAVOR")) %>%
  pull()

# Compute difference in proportions
d_hat <- diff(p_hats)

######################################

# From previous steps
p_hats <- gssmod %>%
  group_by(coinflip) %>%
  summarize(prop_favor = mean(cappun == "FAVOR")) %>%
  pull()
d_hat <- diff(p_hats)

# Form null distribution
null <- gssmod %>%
  # Specify the response and explanatory var and success
  specify(cappun ~ coinflip, success = "FAVOR") %>%
  # Set up the null hypothesis
  hypothesize(null = "independence") %>%
  # Generate 500 permuted data sets
  generate(reps = 500, type = "permute") %>%
  # Calculate statistics
  calculate(stat = "diff in props", order = c("heads", "tails"))

######################################

# From previous steps
p_hats <- gssmod %>%
  group_by(coinflip) %>%
  summarize(prop_favor = mean(cappun == "FAVOR")) %>%
  pull()
d_hat <- diff(p_hats)
null <- gssmod %>%
  specify(cappun ~ coinflip, success = "FAVOR") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "diff in props", order = c("heads", "tails"))

# Visualize null
ggplot(null, aes(x = stat)) +
  # Add density layer
  geom_density() +
  # Add vertical red line at obs stat
  geom_vline(xintercept = d_hat, color = "red")

######################################

# Set alpha
alpha <- 0.05

# Find cutoffs
lower <- null %>%
  summarize(l = quantile(stat, probs = alpha / 2)) %>%
  pull()
upper <- null %>%
  summarize(u = quantile(stat, probs = 1 - alpha / 2)) %>%
  pull()

# Is d_hat inside cutoffs?
d_hat %>%
  between(lower, upper)

######################################

# From previous step
alpha <- 0.05
upper <- null %>%
  summarize(u = quantile(stat, probs = 1 - alpha / 2)) %>%
  pull()
lower <- null %>%
  summarize(l = quantile(stat, probs = alpha / 2)) %>%
  pull()

# Visualize cutoffs
ggplot(null, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = d_hat, color = "red") +
  # Add vertical blue line for lower cutoff
  geom_vline(xintercept = lower, color = "blue") +
  # Add vertical blue line for upper cutoff
  geom_vline(xintercept = upper, color = "blue")

######################################

# Subset data
gss_party <- gss2016 %>%
  # Filter out the "Oth"
  filter(party != "Oth")

######################################

# From previous step
gss_party <- gss2016 %>%
  filter(party != "Oth")

# Visualize distribution take 1
gss_party %>%
  ggplot(aes(x = party, fill = natspac)) +
  # Add bar layer of proportions
  geom_bar(position = "fill")

######################################

# From previous step
gss_party <- gss2016 %>%
  filter(party != "Oth")

# Visualize distribution take 2
gss_party %>%
  ggplot(aes(x = party, fill = natspac)) +
  # Add bar layer of counts
  geom_bar()
