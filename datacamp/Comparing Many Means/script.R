# Using gss, plot wordsum
ggplot(gss, mapping = aes(x = wordsum)) +
  # Add a histogram layer
  geom_histogram(binwidth = 1) +
  # Facet by class
  facet_wrap(~class)
  
# Run an analysis of variance on wordsum vs. class
aov_wordsum_class <- aov(wordsum ~ class, gss)

# Tidy the model
tidy(aov_wordsum_class)

gss %>%
  # Group by class
  group_by(class) %>%
  # Calculate the std dev of wordsum as std_dev_wordsum
  summarise(std_dev_wordsum = sd(wordsum))
  
  
# Run a pairwise t-test on wordsum and class, without adjustment
t_test_results <- pairwise.t.test(gss$wordsum, gss$class, p.adjust.method = "none")

# Tidy the result
tidy(t_test_results)



