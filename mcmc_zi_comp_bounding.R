library(rstan)
library(readr)
library(dplyr)

# Optional packages used for formatted output.
library(knitr)
library(kableExtra)

# MCMC settings.
iterations <- 5000
max_iters <- 10^4
eps <- 2^(-52)

# Configure rstan for better performance.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Read and inspect the grouped count data.
data_df <- read_csv("Shmuelli_2005.csv", show_col_types = FALSE) %>%
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

library(bayesplot)

# --- 1. Trace plots: did chains mix? ---
print(traceplot(fit, pars = c("mu", "nu")))

# --- 2. Extract posterior draws ---
post <- rstan::extract(fit, pars = c("mu", "nu"))
mu_hat  <- mean(post$mu)
nu_hat <- mean(post$nu)

# --- 3. Compute fitted (predicted) probabilities at posterior mean ---
y_max <- max(data_df$count)
logFunction_r <- function(mu, nu, n) {
  -n + n*log(n) - lgamma(n + 1) + nu*n + nu*n*log(mu) - nu*n*log(n)
}
log_terms <- sapply(0:y_max, function(n) {
  if (n == 0) return(0)
  logFunction_r(mu_hat, nu_hat, n)
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