# Load necessary libraries
library(rstan)
library(readr)
library(dplyr)

# Set rstan options for better performance
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Read your CSV data
data_df <- read_csv("Shmuelli_2005.csv")

# Inspect the data
print(data_df)

# Prepare data for Stan
# Ensure counts are sorted
data_df <- data_df %>%
  arrange(count)

# Extract counts and frequencies
counts <- data_df$count
frequencies <- data_df$frequency

# Define maximum y for normalization (ensure it's sufficiently large)
Z_max <- max(counts) + 20  # Adjust as needed

# Prepare the list of data for Stan
stan_data <- list(
  N = length(counts),
  y = counts,
  freq = frequencies,
  Z_max = Z_max
)

# Compile the Stan model
stan_model <- stan_model(file = "compoisson_model_mu_nu.stan")

# Fit the model using MCMC
fit <- sampling(
  object = stan_model,
  data = stan_data,
  iter = 2000,            # Number of iterations
  warmup = 1000,          # Number of warmup (burn-in) iterations
  chains = 4,             # Number of chains
  seed = 123,             # Seed for reproducibility
  control = list(adapt_delta = 0.95, max_treedepth = 15)  # Control parameters
)

# Print a summary of the results
print(fit, pars = c("mu", "nu"))

# Trace plots to assess convergence
traceplot(fit, pars = c("mu", "nu"))

# Posterior distributions
library(bayesplot)
mcmc_areas(as.array(fit), pars = c("mu", "nu"))

# Posterior predictive checks
generated_freq <- extract(fit)$freq_rep

# Plot observed vs. replicated frequencies
observed <- frequencies
replicated <- apply(generated_freq, 2, mean)

plot(counts, observed, pch = 16, col = "blue",
     xlab = "Count", ylab = "Frequency",
     main = "Observed vs. Predicted Frequencies")
points(counts, replicated, pch = 16, col = "red")
legend("topright", legend = c("Observed", "Predicted"),
       col = c("blue", "red"), pch = 16)
