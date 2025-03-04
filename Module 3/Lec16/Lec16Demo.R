library(ggplot2)

# Testing a cross-platform working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
#print(getSourceEditorContext()$path)
setwd(dirname(getSourceEditorContext()$path))

source("Lec16DemoFunctions.R")
source("Lec16ShinyApp.R")

# ---- 1. From last time: sampling from a population, simulation ----

pop_size <- 250000
pop_proportion <- 0.88
population <- create_population(N=pop_size, p=pop_proportion)

sample_size <- 10
samples <- sample(population, size=sample_size)
phat <- sum(samples == "support") / sample_size

K <- 1000  # Simulation size
simulation <- replicate(K, sample_get_phat_fn(population, n=sample_size))
title <- sprintf("Histogram of p-hat values from experiment with %sx%s samples", 
                 K, sample_size)  # in case we change the sample/simulation size

ggplot(data=data.frame(simulation), aes(x=simulation)) +
  geom_vline(aes(xintercept=pop_proportion), color="red") +
  geom_histogram(bins=100, alpha=0.5, color=4, fill="white") +
  labs(title=title, x="Sample proportion", y="Frequency") +
  xlim(c(0,1))


# ---- 2. Normal approximation ----
# Q: Where do the parameters for the Normal distribution approximating p-hat 
# come from?

# Sample from a Normal distribution with computed parameters:
SE_phat <- sqrt((pop_proportion*(1-pop_proportion))/100)
normal_dist <- rnorm(K, mean=pop_proportion, sd=SE_phat)

# ---- 3. Comparing our simulation to the Normal ----
# For plotting ease, create a new dataframe with the simulation values and the
# sampled normal values, plus a categorical variable distinguishing them
comparison <- create_comparison_data(simulation, normal_dist, K)

# TODO: help with normalizing histogram
# Plot the densities using the new dataframe:
#ggplot(data=comparison, aes(x=values, color=source, fill=source)) +
  #geom_vline(aes(xintercept=pop_proportion), color="red") +
  #geom_histogram(data=comparison[comparison$source == "Simulation",], 
  #               aes(y=stat(count / sum(count))),
  #               bins=100, alpha=0.5, color=4, fill="white") +
  #geom_histogram(data=comparison[comparison$source == "Simulation",], 
  #               bins=100, alpha=0.5, color=4, fill="white") +
  #geom_density(data=comparison[comparison$source == "Normal",], 
  #             lwd=1, alpha=0.25) +
  #stat_function(fun=dnorm, args=list(mean=pop_proportion, sd=SE_phat)) +
  #xlim(c(0,1))

ggplot(data=as.data.frame(simulation), aes(x=simulation)) +
  geom_histogram(aes(y=after_stat(density)), bins=100, 
                 alpha=0.5, color=4, fill="white") +
  stat_function(fun=dnorm, args=list(mean=pop_proportion, sd=SE_phat), 
                lwd=1, alpha=0.5, color=2) +
  labs(title=title, x="Sample proportion", y="Density") +
  xlim(c(0,1))


# 3. ---- Repeat the experiment for different sample sizes ---- 
# Now construct datasets from samples of different sizes and repeat the above. 

# Q: What happens to the distributions as sample size increases?

# Run Rshiny app
shinyApp(ui = ui, server = server)
# TODO: one plot! same as above

