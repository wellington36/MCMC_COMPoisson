library(rstan)
library(readr)
library(dplyr)
library(ggplot2)

iterations = 500
FIXED = 100

# Set rstan options for better performance
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Read your CSV data
data_df <- read_csv("Shmuelli_2005.csv")

# Inspect the data
print(data_df)

# Prepare data for Stan
data_df <- data_df %>%
  arrange(count)

counts <- data_df$count
frequencies <- data_df$frequency

# Prepare the list of data for Stan
stan_data <- list(
  N = length(counts),
  y = counts,
  freq = frequencies,
  FIXED = FIXED
)

# Compile the Stan model
stan_model <- stan_model(file = "stan/compoisson_fixed.stan")

# Fit the model using MCMC
fit <- sampling(
  object = stan_model,
  data = stan_data,
  refresh = floor(iterations/5),
  iter = iterations,               # Number of iterations
  warmup = floor(iterations / 2),    # Number of warmup (burn-in) iterations
  chains = 4,                      # Number of chains
  control = list(adapt_delta = 0.90, max_treedepth = 12)  # Control parameters
)

# Print a summary of the results
print(fit, pars = c("mu", "nu"))

summary_fit <- summary(fit, pars = c("mu", "nu"))

# Convert the summary output to a data frame
posterior_stats <- as.data.frame(summary_fit$summary)

# Get elapsed time for each chain
chain_times <- get_elapsed_time(fit)

# Calculate the average time in minutes across all chains
avg_time_min <- mean(rowSums(chain_times)) / 60

# Check the column for effective sample size (ESS)
ess_column <- "n_eff"  # n_eff was identified earlier

# Calculate ESS/minute by dividing n_eff by the average time in minutes
ess_per_minute <- posterior_stats$n_eff / avg_time_min

# Create a summary table for mu and nu
summary_table <- data.frame(
  Parameter = c("mu", "nu"),
  Mean = posterior_stats$mean,
  Median = posterior_stats$`50%`,
  `95% BCI` = paste0("[", round(posterior_stats$`2.5%`, 3), ", ", round(posterior_stats$`97.5%`, 3), "]"),
  `Posterior SD` = posterior_stats$sd,
  MCSE = posterior_stats$se_mean,
  `ESS/minute` = ess_per_minute
)

# Display the summary table
print(summary_table)

# Optional: Format the table for display
library(knitr)
library(kableExtra)

summary_table %>%
  kable("html", col.names = c("Parameter", "Mean", "Median", "95% BCI", "Posterior SD", "MCSE", "ESS/minute")) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
