library(rstan)
library(dplyr)
# Optional packages used for formatted output.
library(knitr)
library(kableExtra)
library(ggplot2)

# MCMC settings.
iterations <- 5000
max_iters <- 10^4
eps <- 2^(-52)

# Configure rstan for better performance.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ---- Load and prepare the fish$count data ----
fish <- read.csv("https://paul-buerkner.github.io/data/fish.csv")
y_obs <- fish$count

# Convert raw counts into grouped count/frequency data, as required
# by the zicompoisson_bounding.stan model.
count_table <- table(y_obs)
data_df <- data.frame(
  count = as.integer(names(count_table)),
  frequency = as.integer(count_table)
) %>%
  arrange(count)

print(data_df)

# Validate the required columns and their values.
stopifnot(all(c("count", "frequency") %in% names(data_df)))
stopifnot(all(data_df$count >= 0))
stopifnot(all(data_df$frequency >= 0))

# Prepare the data for Stan.
counts <- as.integer(data_df$count)
frequencies <- as.integer(data_df$frequency)
stan_data <- list(
  N = length(counts),
  y = counts,
  freq = frequencies,
  eps = eps,
  MAX_ITERS = max_iters
)

# Compile the ZI-COM-Poisson Stan model.
stan_model <- stan_model(file = "stan/zicompoisson_bounding.stan")

# Fit the model using MCMC.
fit <- sampling(
  object = stan_model,
  data = stan_data,
  refresh = floor(iterations / 5),
  iter = 2 * iterations,
  warmup = iterations,
  chains = 4,
  cores = min(4, parallel::detectCores()),
  control = list(
    adapt_delta = 0.90,
    max_treedepth = 12
  )
)

# Print the posterior summary for all model parameters and generated quantities.
parameters <- c("mu", "nu", "zi", "n")
print(fit, pars = parameters)
summary_fit <- summary(fit, pars = parameters)
posterior_stats <- as.data.frame(summary_fit$summary)

# Calculate the average elapsed time across chains, in minutes.
chain_times <- get_elapsed_time(fit)
avg_time_min <- mean(rowSums(chain_times)) / 60

# Calculate effective sample size per minute.
ess_per_minute <- posterior_stats$n_eff / avg_time_min

# Create a compact posterior summary table.
summary_table <- data.frame(
  Parameter = parameters,
  Mean = posterior_stats$mean,
  Median = posterior_stats$`50%`,
  `95% BCI` = paste0(
    "[",
    round(posterior_stats$`2.5%`, 3),
    ", ",
    round(posterior_stats$`97.5%`, 3),
    "]"
  ),
  `Posterior SD` = posterior_stats$sd,
  MCSE = posterior_stats$se_mean,
  `ESS/minute` = ess_per_minute,
  check.names = FALSE
)
print(summary_table)

# Optional HTML rendering of the summary table.
summary_table %>%
  kable(
    format = "html",
    col.names = c(
      "Parameter", "Mean", "Median", "95% BCI",
      "Posterior SD", "MCSE", "ESS/minute"
    )
  ) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover")
  )

# ---- Compute fitted frequencies from posterior draws ----

# Extract posterior draws (log_lambda and log_Z are transformed parameters,
# saved automatically alongside mu, nu, zi).
draws <- rstan::extract(fit, pars = c("log_lambda", "log_Z", "nu", "zi"))

total_n <- sum(data_df$frequency)
counts_vec <- data_df$count
n_counts <- length(counts_vec)
n_draws <- length(draws$nu)

# Matrix of fitted probabilities: rows = posterior draws, cols = count bins.
fitted_prob_mat <- matrix(NA_real_, nrow = n_draws, ncol = n_counts)

for (j in seq_len(n_counts)) {
  y_j <- counts_vec[j]
  log_p_com <- y_j * draws$log_lambda - draws$nu * lgamma(y_j + 1) - draws$log_Z
  p_com <- exp(log_p_com)

  if (y_j == 0) {
    fitted_prob_mat[, j] <- draws$zi + (1 - draws$zi) * p_com
  } else {
    fitted_prob_mat[, j] <- (1 - draws$zi) * p_com
  }
}

# Posterior mean probability per bin, converted to expected frequency.
fitted_prob_mean <- colMeans(fitted_prob_mat)
fitted_freq <- fitted_prob_mean * total_n

plot_df <- data.frame(
  count = data_df$count,
  Observed = data_df$frequency,
  Fitted = fitted_freq
)

ggplot(plot_df, aes(x = count)) +
  geom_col(aes(y = Observed), fill = "steelblue", alpha = 0.8) +
  geom_line(aes(y = Fitted), color = "red", linewidth = 1) +
  geom_point(aes(y = Fitted), color = "red", size = 2) +
  scale_x_continuous(breaks = plot_df$count) +
  labs(
    title = "Observed vs. Fitted Frequency Distribution: fish$count",
    subtitle = "Bars = observed frequencies, red line = posterior mean fitted values",
    x = "Count",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13)