#library(tidyverse)
library(ggplot2)
source("Lec15DemoFunctions.R")
#source("Lec15DemoShinyApp.R")

# ---- 1. Sampling from a population ----

# ---- 1a. Creating the population ----
# Create a set of 250 thousand entries, where 88% of them are "support"
# and 12% are "not".
pop_size <- 250000
pop_proportion <- 0.88
population <- create_population(N=pop_size, p=pop_proportion)
head(population)
sum(population == "support") / pop_size  # Should be 0.88
# Sanity check: visualize the population with a chart
ggplot(data=data.frame(population), aes(x=population)) +
  geom_bar(color=4, fill=4)


# ---- 1b. Take samples ----
samples_10 <- sample(population, size=10)
samples_100 <- sample(population, size=100)
samples_1000 <- sample(population, size=1000)
samples_10000 <- sample(population, size=10000)
head(samples_1000)

# Q: Do you think a chart of the samples will look like the chart of the 
# population values? 
ggplot(data=data.frame(samples_10), aes(x=samples_10)) +
  geom_bar(color=4, fill=4)
ggplot(data=data.frame(samples_100), aes(x=samples_100)) +
  geom_bar(color=4, fill=4)


# ---- 1c. Computing p-hat ----
# Q: How would you compute p-hat from the samples?
# Q: What happens to p-hat as the sample size increases?

phat_10 <- sum(samples_10 == "support") / 10
phat_100 <- sum(samples_100 == "support") / 100
phat_1000 <- sum(samples_1000 == "support") / 1000
phat_10000 <- sum(samples_10000 == "support") / 10000

# Display table:
phats <- matrix(c(phat_10, phat_100, phat_1000, phat_10000))
rownames(phats) <- c("10 samples", "100 samples", "1000 samples", 
                     "10000 samples")
colnames(phats) <- c("p-hat")
phats <- as.table(phats)
phats


# ---- 2. Modeling a population with distributions ----
# Now take samples directly from a distribution function, in this case Binomial
# Q: What do each of the parameters mean? What are their analogues in the 
# population sampling case? What do you think the samples will look like?
samples_b10 <- rbinom(n=10, size=250000, prob=0.88)
head(samples_b10)


# ---- 3. Central Limit Theorem ---- 
# Take many samples from the population (simulation) to construct a dataset. 
# Then observe the result of the Central Limit Theorem.

# Q: What determines where the center or mean of the sampling distribution will
# fall?  
K <- 1000  # Simulation size
simulation_100 <- replicate(K, sample_get_phat_fn(population, n=100))

ggplot(data=data.frame(simulation_100), aes(x=simulation_100)) +
  geom_vline(aes(xintercept=pop_proportion), color="red") +
  geom_histogram(bins=100, alpha=0.5, color=4, fill="white") +
  labs(title="Histogram of p-hat values from experiment with 1000x100 samples",
       x="Sample proportion", y="Frequency") +
  xlim(c(0,1))

# Q: Why have we set the xlimits of the graph to be (0,1)?
# Q: Where do the parameters for the Normal distribution approximating p-hat 
# come from?

# Sample from a Normal distribution with computed parameters:
SE_phat100 <- sqrt((pop_proportion*(1-pop_proportion))/100)
normal_dist <- rnorm(K, mean=pop_proportion, sd=SE_phat100)

# For plotting ease, create a new dataframe with the simulation values and the
# sampled normal values, plus a categorical variable distinguishing them
comparison <- create_comparison_data(simulation_100, normal_dist, K)

# Plot the densities using the new dataframe:
ggplot(data=comparison, aes(x=values, color=source, fill=source)) +
  geom_density(lwd=1, alpha=0.25) +
  xlim(c(0,1))


# 4. ---- Repeat the experiment ---- 
# Now construct datasets from samples of different sizes and repeat the above. 

# Q: What happens to the distributions as sample size increases?

# Run Rshiny app
shinyApp(ui = ui, server = server)

# TODO: add sample & normal densities to app
# TODO: demonstrate something abt standard error

# TODO: test - sweep all variables and rerun
