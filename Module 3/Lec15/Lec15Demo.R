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

# Q: if we sample from the population multiple times, do you think the samples
# will all be the same, or different?

# Run these lines more than once and see what happens:
samples_10 <- sample(population, size=10)
ggplot(data=data.frame(samples_10), aes(x=samples_10)) +
  geom_bar(color=4, fill=4)

# Now take more samples of different sizes:
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

# ---- 2a. First simulation ----
# Q: What determines where the center or mean of the sampling distribution will
# fall?  
K <- 1000  # Simulation size
n <- 100   # Sample size
for(index in 1:K) {
  sampled_entries <- rbinom(n=n, size=pop_size, prob=pop_proportion) 
  phats_simulation <- sampled_entries / pop_size
}

ggplot(data=data.frame(phats_simulation), aes(x=phats_simulation)) +
  geom_vline(aes(xintercept=pop_proportion), color="red") +
  geom_histogram(bins=100, alpha=0.5, color=4, fill="white") +
  labs(title="Histogram of p-hat values from experiment with 1000x100 samples",
       x="Sample proportion", y="Frequency") +
  xlim(c(0,1))

# TODO: should we still be sampling the original population, or the Binomial?
