library(rstan)
library(readr)
library(dplyr)
library(ggplot2)

iterations = 5000
eps = 2**(-52)

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
  eps = eps,
  MAX_ITERS = 10**4
)

# Compile the Stan model
stan_model <- stan_model(file = "stan/compoisson_bounding.stan")

# Fit the model using MCMC
fit <- sampling(
  object = stan_model,
  data = stan_data,
  refresh = floor(iterations/5),
  iter = 2*iterations,               # Number of iterations
  warmup = iterations,    # Number of warmup (burn-in) iterations
  chains = 4,                      # Number of chains
  cores = 8,
  control = list(adapt_delta = 0.90, max_treedepth = 12)  # Control parameters
)

# Print a summary of the results
print(fit, pars = c("mu", "phi", "n"))

summary_fit <- summary(fit, pars = c("mu", "phi", "n"))

# Convert the summary output to a data frame
posterior_stats <- as.data.frame(summary_fit$summary)

# Get elapsed time for each chain
chain_times <- get_elapsed_time(fit)

# Calculate the average time in minutes across all chains
avg_time_min <- mean(rowSums(chain_times)) / 60

# Calculate ESS/minute by dividing n_eff by the average time in minutes
ess_per_minute <- posterior_stats$n_eff / avg_time_min


# Create a summary table for mu and phi
summary_table <- data.frame(
  Parameter = c("mu", "phi", "n"),
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

library(bayesplot)

# --- 1. Trace plots: did chains mix? ---
print(traceplot(fit, pars = c("mu", "phi")))

# --- 2. Extract posterior draws ---
post <- rstan::extract(fit, pars = c("mu", "phi"))
mu_hat  <- mean(post$mu)
phi_hat <- mean(post$phi)

# --- 3. Compute fitted (predicted) probabilities at posterior mean ---
y_max <- max(data_df$count)
logFunction_r <- function(mu, phi, n) {
  -n + n*log(n) - lgamma(n + 1) + phi*n + phi*n*log(mu) - phi*n*log(n)
}
log_terms <- sapply(0:y_max, function(n) {
  if (n == 0) return(0)
  logFunction_r(mu_hat, phi_hat, n)
})
logZ_hat <- log(sum(exp(log_terms - max(log_terms)))) + max(log_terms)  # stable log-sum-exp
probs <- exp(log_terms - logZ_hat)

total_n <- sum(data_df$frequency)
fitted_df <- data.frame(
  count = 0:y_max,
  fitted = probs * total_n
)

# --- 4. Overlay observed vs fitted counts ---
plot_df <- merge(data_df, fitted_df, by = "count", all = TRUE)
plot_df[is.na(plot_df)] <- 0

ggplot(plot_df, aes(x = count)) +
  geom_col(aes(y = frequency), fill = "steelblue", alpha = 0.6) +
  geom_point(aes(y = fitted), color = "firebrick", size = 2) +
  geom_line(aes(y = fitted), color = "firebrick") +
  labs(title = "Observed (bars) vs Fitted (red) frequencies",
       x = "Count", y = "Frequency") +
  theme_minimal()