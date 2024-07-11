# function for bootstrapped sd
boot_sd <- function(x, n = 10000) {
  boot_mean <- numeric(n)
  for (i in 1:n) {
    boot_sample <- sample(x, length(x), replace = TRUE)
    boot_mean[i] <- mean(boot_sample)
  }
  sd(boot_mean)
}

# function for bootstrapped sd of sums
boot_sd_sum <- function(x, n = 10000) {
  boot_sum <- numeric(n)
  for (i in 1:n) {
    boot_sample <- sample(x, length(x), replace = TRUE)
    boot_sum[i] <- sum(boot_sample)
  }
  sd(boot_sum)
}


# function for bootstrapped sd of proportions
boot_sd_prop <- function(x, n = 10000) {
  boot_prop <- numeric(n)
  for (i in 1:n) {
    boot_sample <- sample(x, length(x), replace = TRUE)
    boot_prop[i] <- sum(boot_sample) / length(x)
  }
  sd(boot_prop)
}
