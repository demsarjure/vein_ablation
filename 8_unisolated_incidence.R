library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv")

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
# close: 10.34 +/- 5.62, high_density: 0
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

# 3, 5.45
all <- na.omit(df_all$rspv_rr_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


# rspv_ra ----------------------------------------------------------------------
# close: 13.79 +/- 6.46, high_density: 7.69 +/- 5.2
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

# 6, 10.91
all <- na.omit(df_all$rspv_ra_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


# rspv_rp ----------------------------------------------------------------------
# close: 10.34 +/- 5.65, high_density: 7.69 +/- 5.17
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

# 5, 9.09
all <- na.omit(df_all$rspv_rp_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


# right_carina_rspv ------------------------------------------------------------
# close: 10.34 +/- 5.63, high_density: 0
close <- na.omit(df_close$right_carina_rspv)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$right_carina_rspv)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)

# 3, 5.45
all <- na.omit(df_all$right_carina_rspv)
length(all) - sum(all)
1 - (sum(all) / length(all))


# ripv_ra ----------------------------------------------------------------------
# close: 3.45 +/- 3.41, high_density: 0
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

# 1, 1.82
all <- na.omit(df_all$ripv_ra_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


# ripv_rp ----------------------------------------------------------------------
# close: 10.34 +/- 5.64, high_density: 0
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

# 3, 5.45
all <- na.omit(df_all$ripv_rp_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))

t
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

# 0, 0
all <- na.omit(df_all$ripv_ri_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


# right_carina_ripv ------------------------------------------------------------
# close: 10.34 +/- 5.63, high_density: 0
close <- na.omit(df_close$right_carina_ripv)
1 - (sum(close) / length(close))
boot_sd(close)

high_density <- na.omit(df_high_density$right_carina_ripv)
1 - (sum(high_density) / length(high_density))
boot_sd(high_density)

# proportions test, p = 1
x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)

# 3, 5.45
all <- na.omit(df_all$right_carina_ripv)
length(all) - sum(all)
1 - (sum(all) / length(all))


# lspv_lr ----------------------------------------------------------------------
# close: 3.45 +/- 3.41, high_density: 3.85 +/- 3.79
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

# 2, 3.64
all <- na.omit(df_all$lspv_lr_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


# lspv_lrg ---------------------------------------------------------------------
# close: 3.45 +/- 3.34, high_density: 3.85 +/- 3.76
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

# 2, 3.64
all <- na.omit(df_all$lspv_lrg_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


# lspv_lp ----------------------------------------------------------------------
# close: 6.9 +/- 4.67, high_density: 3.85 +/- 3.75
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

# 3, 5.45
all <- na.omit(df_all$lspv_lp_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


# lipv_la ----------------------------------------------------------------------
# close: 6.9 +/- 4.67, high_density: 0
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

# 2, 3.64
all <- na.omit(df_all$lipv_la_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


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

# 1, 1.82
all <- na.omit(df_all$lipv_li_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))


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

# 2, 3.64
all <- na.omit(df_all$lipv_lp_isolation)
length(all) - sum(all)
1 - (sum(all) / length(all))

