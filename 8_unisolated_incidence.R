library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv", sep = ";")

# r
df_all$rspv_rr_isolation <- as.numeric(df_all$rspv_rr_isolation)
df_all$rspv_ra_isolation <- as.numeric(df_all$rspv_ra_isolation)
df_all$rspv_rp_isolation <- as.numeric(df_all$rspv_rp_isolation)
df_all$ripv_ra_isolation <- as.numeric(df_all$ripv_ra_isolation)
df_all$ripv_rp_isolation <- as.numeric(df_all$ripv_rp_isolation)
df_all$ripv_ri_isolation <- as.numeric(df_all$ripv_ri_isolation)

# l
df_all$lspv_lr_isolation <- as.numeric(df_all$lspv_lr_isolation)
df_all$lspv_lrg_isolation <- as.numeric(df_all$lspv_lrg_isolation)
df_all$lspv_lp_isolation <- as.numeric(df_all$lspv_lp_isolation)
df_all$lipv_la_isolation <- as.numeric(df_all$lipv_la_isolation)
df_all$lipv_li_isolation <- as.numeric(df_all$lipv_li_isolation)
df_all$lipv_lp_isolation <- as.numeric(df_all$lipv_lp_isolation)

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# rspv_rr ----------------------------------------------------------------------
# close: 0.1 +/- 0.05, high_density: 0
close <- na.omit(df_close$rspv_rr_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$rspv_rr_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 0.27
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# rspv_ra ----------------------------------------------------------------------
# close: 0.14 +/- 0.06, high_density: 0.07 +/- 0.05
close <- na.omit(df_close$rspv_ra_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$rspv_ra_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 0.77
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# rspv_rp ----------------------------------------------------------------------
# close: 0.1 +/- 0.06, high_density: 0.08 +/- 0.05
close <- na.omit(df_close$rspv_rp_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$rspv_rp_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_ra ----------------------------------------------------------------------
# close: 0.03 +/- 0.03, high_density: 0
close <- na.omit(df_close$ripv_ra_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$ripv_ra_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_rp ----------------------------------------------------------------------
# close: 0.1 +/- 0.05, high_density: 0
close <- na.omit(df_close$ripv_rp_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$ripv_rp_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 0.27
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_ri ----------------------------------------------------------------------
# close: 0, high_density: 0
close <- na.omit(df_close$ripv_ri_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$ripv_ri_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lr ----------------------------------------------------------------------
# close: 0.03 +/- 0.03, high_density: 0.03 +/- 0.03
close <- na.omit(df_close$lspv_lr_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$lspv_lr_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lrg ---------------------------------------------------------------------
# close: 0.03 +/- 0.03, high_density: 0.03 +/- 0.03
close <- na.omit(df_close$lspv_lrg_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$lspv_lrg_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lp ----------------------------------------------------------------------
# close: 0.07 +/- 0.05, high_density: 0.03 +/- 0.03
close <- na.omit(df_close$lspv_lp_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$lspv_lp_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv_la ----------------------------------------------------------------------
# close: 0.07 +/- 0.05, high_density: 0
close <- na.omit(df_close$lipv_la_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$lipv_la_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv_li ----------------------------------------------------------------------
# close: 0, high_density: 0.03 +/- 0.03
close <- na.omit(df_close$lipv_li_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$lipv_li_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv_lp ----------------------------------------------------------------------
# close: 0.03 +/- 0.03, high_density: 0.03 +/- 0.03
close <- na.omit(df_close$lipv_lp_isolation)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$lipv_lp_isolation)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)
