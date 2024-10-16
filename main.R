# Load necessary libraries
library(rstan)
library(readr)
library(dplyr)
library(ggplot2)

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
  iter = 500,            # Number of iterations
  warmup = 200,          # Number of warmup (burn-in) iterations
  chains = 4,             # Number of chains
  seed = 123,             # Seed for reproducibility
  control = list(adapt_delta = 0.95, max_treedepth = 15)  # Control parameters
)

# Print a summary of the results
print(fit, pars = c("mu", "nu"))

# Extract generated quantities (predicted frequencies)
generated_freq <- extract(fit)$freq_rep

# Get the mean predicted frequency for each count
replicated <- apply(generated_freq, 2, mean)

# Create a data frame for the plot
plot_data <- data.frame(
  count = counts,
  observed = frequencies,
  predicted = replicated
)

# Plot the observed data (bars) and predicted data (line)
ggplot(plot_data, aes(x = count)) +
  geom_bar(aes(y = observed), stat = "identity", fill = "blue", alpha = 0.6, width = 0.8) +  # Bars for observed
  geom_line(aes(y = predicted), color = "red", size = 1.2) +                                # Line for predicted
  geom_point(aes(y = predicted), color = "red", size = 2) +                                 # Points for predicted
  labs(
    title = "Observed vs. Predicted Frequencies",
    x = "Count",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )