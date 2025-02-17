library(ggplot2)
source("Lec15DemoFunctions.R")

# ---- 1. Sampling from a population ----

# ---- 1a. Creating the population ----
# Create a set of 250 thousand entries, where 88% of them are "support"
# and 12% are "not".
pop_size <- 25000
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


# ---- 1c. Computing p-hat, sampling variability ----
# Q: How would you compute p-hat from the samples?
# Q: What happens to p-hat as the sample size increases? Which value of p-hat 
# do you think will be closer to the "true" (population) value?

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
samples_b10 <- rbinom(n=10, size=pop_size, prob=pop_proportion)
head(samples_b10)

# TODO: rethink this section - check lecture slides
