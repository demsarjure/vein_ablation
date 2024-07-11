# function for bootstrapped sd
boot_sd <- function(x, n = 10000) {
  boot_mean <- numeric(n)
  for (i in 1:n) {
    boot_sample <- sample(x, length(x), replace = TRUE)
    boot_mean[i] <- mean(boot_sample)
  }
  sd(boot_mean)
}
