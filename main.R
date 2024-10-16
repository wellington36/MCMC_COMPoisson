# Load necessary libraries
library(rstan)
library(readr)
library(dplyr)
library(ggplot2)

iterations = 500

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

# Prepare the list of data for Stan
stan_data <- list(
  N = length(counts),
  y = counts,
  freq = frequencies,
  ITER = 100
)

# Compile the Stan model
stan_model <- stan_model(file = "compoisson_fixed.stan")

# Fit the model using MCMC
fit <- sampling(
  object = stan_model,
  data = stan_data,
  iter = iterations,               # Number of iterations
  warmup = floor(iterations/2),    # Number of warmup (burn-in) iterations
  chains = 4,                      # Number of chains
  seed = 123,                      # Seed for reproducibility
  control = list(adapt_delta = 0.90, max_treedepth = 12)  # Control parameters
)

# Print a summary of the results
print(fit, pars = c("mu", "nu"))

summary_fit <- summary(fit, pars = c("mu", "nu"))

# Convert the summary output to a data frame
posterior_stats <- as.data.frame(summary_fit$summary)

# Print the column names to identify the correct columns
print(colnames(posterior_stats))

# Subset the rows for mu and nu
posterior_mu_nu <- posterior_stats[c("mu", "nu"), ]

# Check if the correct column for ESS exists, adjust column names accordingly
if ("ess_bulk" %in% colnames(posterior_mu_nu)) {
  ess_column <- "ess_bulk"
} else if ("n_eff" %in% colnames(posterior_mu_nu)) {
  ess_column <- "n_eff"
} else {
  stop("Effective sample size (ESS) column not found.")
}

# Create a summary table for mu and nu
summary_table <- data.frame(
  Parameter = c("mu", "nu"),
  Mean = posterior_stats$mean,
  Median = posterior_stats$`50%`,
  `95% BCI` = paste0("[", round(posterior_mu_nu$`2.5%`, 3), ", ", round(posterior_mu_nu$`97.5%`, 3), "]"),
  `Posterior SD` = posterior_stats$sd,
  MCSE = posterior_stats$se_mean,
  `ESS/minute` = posterior_stats$n_eff
)

# Display the summary table
print(summary_table)

# Optional: Format the table for display
library(knitr)
library(kableExtra)

summary_table %>%
  kable("html", col.names = c("Parameter", "Mean", "Median", "90% BCI", "Posterior SD", "MCSE", "ESS/minute")) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
