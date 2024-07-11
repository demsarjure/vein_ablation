library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv", sep = ";")

# r
df_all$dormant_rspv_rr <- as.numeric(df_all$dormant_rspv_rr)
df_all$dormant_rspv_ra <- as.numeric(df_all$dormant_rspv_ra)
df_all$dormant_rspv_rp <- as.numeric(df_all$dormant_rspv_rp)
df_all$dormant_ripv_ra <- as.numeric(df_all$dormant_ripv_ra)
df_all$dormant_ripv_rp <- as.numeric(df_all$dormant_ripv_rp)
df_all$dormant_ripv_ri <- as.numeric(df_all$dormant_ripv_ri)

# l
df_all$dormant_lspv_lr <- as.numeric(df_all$dormant_lspv_lr)
df_all$dormant_lspv_lrg <- as.numeric(df_all$dormant_lspv_lrg)
df_all$dormant_lspv_lp <- as.numeric(df_all$dormant_lspv_lp)
df_all$dormant_lipv_la <- as.numeric(df_all$dormant_lipv_la)
df_all$dormant_lipv_li <- as.numeric(df_all$dormant_lipv_li)
df_all$dormant_lipv_lp <- as.numeric(df_all$dormant_lipv_lp)

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# rspv_rr ----------------------------------------------------------------------
# close: 0.21 +/- 0.07, high_density: 0.03 +/- 0.03
close <- na.omit(df_close$dormant_rspv_rr)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_rspv_rr)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# rspv_ra ----------------------------------------------------------------------
# close: 0.11 +/- 0.06, high_density: 0.08 +/- 0.05
close <- na.omit(df_close$dormant_rspv_ra)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_rspv_ra)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# rspv_rp ----------------------------------------------------------------------
# close: 0.14 +/- 0.07, high_density: 0.03 +/- 0.03
close <- na.omit(df_close$dormant_rspv_rp)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_rspv_rp)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 0.39
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_ra ----------------------------------------------------------------------
# close: 0, high_density: 0
close <- na.omit(df_close$dormant_ripv_ra)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_ripv_ra)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_rp ----------------------------------------------------------------------
# close: 0.14 +/- 0.07, high_density: 0.08 +/- 0.05
close <- na.omit(df_close$dormant_ripv_rp)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_ripv_rp)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 0.73
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_ri ----------------------------------------------------------------------
# close: 0, high_density: 0
close <- na.omit(df_close$dormant_ripv_ri)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_ripv_ri)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lr ----------------------------------------------------------------------
# close: 0.03 +/- 0.03, high_density: 0
close <- na.omit(df_close$dormant_lspv_lr)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_lspv_lr)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 0.73
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lrg ---------------------------------------------------------------------
# close: 0.14 +/- 0.07, high_density: 0.03 +/- 0.03
close <- na.omit(df_close$dormant_lspv_lrg)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_lspv_lrg)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 0.73
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lp ----------------------------------------------------------------------
# close: 0.14 +/- 0.08, high_density: 0
close <- na.omit(df_close$dormant_lspv_lp)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_lspv_lp)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 0.13
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv_la ----------------------------------------------------------------------
# close: 0.11 +/- 0.06, high_density: 0
close <- na.omit(df_close$dormant_lipv_la)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_lipv_la)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 0.26
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv_li ----------------------------------------------------------------------
# close: 0, high_density: 0
close <- na.omit(df_close$dormant_lipv_li)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_lipv_li)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv_lp ----------------------------------------------------------------------
# close: 0.1 +/- 0.06, high_density: 0
close <- na.omit(df_close$dormant_lipv_lp)
sum(close) / length(close)
boot_sd(close)

high_density <- na.omit(df_high_density$dormant_lipv_lp)
sum(high_density) / length(high_density)
boot_sd(high_density)

# proportions test, p = 0.26
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)
