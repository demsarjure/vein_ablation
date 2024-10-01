# function for bootstrapped ci
boot_ci <- function(x, n = 10000) {
  boot_mean <- numeric(n)
  for (i in 1:n) {
    boot_sample <- sample(x, length(x), replace = TRUE)
    boot_mean[i] <- mean(boot_sample)
  }

  conf_level <- 0.95
  ci_l <- quantile(boot_mean, (1 - conf_level) / 2)
  ci_u <- quantile(boot_mean, 1 - (1 - conf_level) / 2)

  return(c(ci_l, ci_u))
}


# function for bootstrapped sd of sums
boot_ci_sum <- function(x, n = 10000) {
  boot_sum <- numeric(n)
  for (i in 1:n) {
    boot_sample <- sample(x, length(x), replace = TRUE)
    boot_sum[i] <- sum(boot_sample)
  }

  conf_level <- 0.95
  ci_l <- quantile(boot_sum, (1 - conf_level) / 2)
  ci_u <- quantile(boot_sum, 1 - (1 - conf_level) / 2)

  return(c(ci_l, ci_u))
}


# function for bootstrapped sd of proportions
boot_ci_prop <- function(x, n = 10000) {
  boot_prop <- numeric(n)
  for (i in 1:n) {
    boot_sample <- sample(x, length(x), replace = TRUE)
    boot_prop[i] <- sum(boot_sample) / length(x)
  }

  conf_level <- 0.95
  ci_l <- quantile(boot_prop, (1 - conf_level) / 2)
  ci_u <- quantile(boot_prop, 1 - (1 - conf_level) / 2)

  return(c(ci_l, ci_u))
}


# report mean and ci
report_mean_ci <- function(x) {
  mean_x <- mean(x)

  ci <- boot_ci(x)
  ci_l <- ci[1]
  ci_u <- ci[2]

  cat(sprintf("%s [%s, %s]\n", round(mean_x, 2), round(ci_l, 2), round(ci_u, 2)))
}


# report mean and ci for proportions
report_mean_ci_prop <- function(x) {
  mean_x <- mean(x) * 100

  ci <- boot_ci(x)
  ci_l <- ci[1] * 100
  ci_u <- ci[2] * 100

  cat(sprintf("%s%% [%s%%, %s%%]\n", round(mean_x, 2), round(ci_l, 2), round(ci_u, 2)))
}


# report mean and ci for binary data
report_mean_ci_binary <- function(x) {
  mean_x <- (sum(x) / length(x)) * 100

  ci <- boot_ci(x)
  ci_l <- ci[1] * 100
  ci_u <- ci[2] * 100

  cat(sprintf("%s%% [%s%%, %s%%]\n", round(mean_x, 2), round(ci_l, 2), round(ci_u, 2)))
}


# report sum and ci
report_sum_ci <- function(x) {
  sum_x <- sum(x)

  ci <- boot_ci_sum(x)
  ci_l <- ci[1]
  ci_u <- ci[2]

  cat(sprintf("%s [%s, %s]\n", round(sum_x, 2), round(ci_l, 2), round(ci_u, 2)))
}


# report proportion and ci for binary data
report_prop_ci <- function(x) {
  prop_x <- (sum(x) / length(x)) * 100

  ci <- boot_ci_prop(x)
  ci_l <- ci[1] * 100
  ci_u <- ci[2] * 100

  cat(sprintf("%s%% [%s%%, %s%%]\n", round(prop_x, 2), round(ci_l, 2), round(ci_u, 2)))
}
