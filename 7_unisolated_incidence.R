library(readxl)
library(tidyverse)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# r
df_all$rspv_rr_isolation <- as.numeric(df_all$rspv_rr_isolation)
df_all$rspv_ra_isolation <- as.numeric(df_all$rspv_ra_isolation)
df_all$rspv_rp_isolation <- as.numeric(df_all$rspv_rp_isolation)
df_all$right_carina_rspv <- as.numeric(df_all$right_carina_rspv)
df_all$ripv_ra_isolation <- as.numeric(df_all$ripv_ra_isolation)
df_all$ripv_rp_isolation <- as.numeric(df_all$ripv_rp_isolation)
df_all$ripv_ri_isolation <- as.numeric(df_all$ripv_ri_isolation)
df_all$right_carina_ripv <- as.numeric(df_all$right_carina_ripv)

# l
df_all$lspv_lr_isolation <- as.numeric(df_all$lspv_lr_isolation)
df_all$lspv_lrg_isolation <- as.numeric(df_all$lspv_lrg_isolation)
df_all$lspv_lp_isolation <- as.numeric(df_all$lspv_lp_isolation)
df_all$left_carina_lspv <- 1
df_all$lipv_la_isolation <- as.numeric(df_all$lipv_la_isolation)
df_all$lipv_li_isolation <- as.numeric(df_all$lipv_li_isolation)
df_all$lipv_lp_isolation <- as.numeric(df_all$lipv_lp_isolation)
df_all$left_carina_lipv <- 1

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# rspv_rr ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$rspv_rr_isolation)
high_density <- 1 - na.omit(df_high_density$rspv_rr_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# rspv_ra ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$rspv_ra_isolation)
high_density <- 1 - na.omit(df_high_density$rspv_ra_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# rspv_rp ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$rspv_rp_isolation)
high_density <- 1 - na.omit(df_high_density$rspv_rp_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# right_carina_rspv ------------------------------------------------------------
close <- 1 - na.omit(df_close$right_carina_rspv)
high_density <- 1 - na.omit(df_high_density$right_carina_rspv)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_ra ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$ripv_ra_isolation)
high_density <- 1 - na.omit(df_high_density$ripv_ra_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_rp ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$ripv_rp_isolation)
high_density <- 1 - na.omit(df_high_density$ripv_rp_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv_ri ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$ripv_ri_isolation)
high_density <- 1 - na.omit(df_high_density$ripv_ri_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# right_carina_ripv ------------------------------------------------------------
close <- 1 - na.omit(df_close$right_carina_ripv)
high_density <- 1 - na.omit(df_high_density$right_carina_ripv)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lr ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$lspv_lr_isolation)
high_density <- 1 - na.omit(df_high_density$lspv_lr_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lrg ---------------------------------------------------------------------
close <- 1 - na.omit(df_close$lspv_lrg_isolation)
high_density <- 1 - na.omit(df_high_density$lspv_lrg_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv_lp ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$lspv_lp_isolation)
high_density <- 1 - na.omit(df_high_density$lspv_lp_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# left_carina_lspv -------------------------------------------------------------
0


# lipv_la ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$lipv_la_isolation)
high_density <- 1 - na.omit(df_high_density$lipv_la_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv_li ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$lipv_li_isolation)
high_density <- 1 - na.omit(df_high_density$lipv_li_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv_lp ----------------------------------------------------------------------
close <- 1 - na.omit(df_close$lipv_lp_isolation)
high_density <- 1 - na.omit(df_high_density$lipv_lp_isolation)

report_mean_ci_binary(close)
report_mean_ci_binary(high_density)

report_sum_ci(close)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# left_carina_lipv -------------------------------------------------------------
0


# rspv -------------------------------------------------------------------------
close <- c(
  df_close$rspv_rr_isolation,
  df_close$rspv_ra_isolation,
  df_close$rspv_rp_isolation,
  df_close$right_carina_rspv
)
close <- 1 - close
report_mean_ci_binary(close)
report_sum_ci(close)

high_density <- c(
  df_high_density$rspv_rr_isolation,
  df_high_density$rspv_ra_isolation,
  df_high_density$rspv_rp_isolation,
  df_high_density$right_carina_rspv
)
high_density <- 1 - high_density
report_mean_ci_binary(high_density)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# ripv -------------------------------------------------------------------------
close <- c(
  df_close$ripv_ra_isolation,
  df_close$ripv_rp_isolation,
  df_close$ripv_ri_isolation,
  df_close$right_carina_ripv
)
close <- 1 - close
report_mean_ci_binary(close)
report_sum_ci(close)

high_density <- c(
  df_high_density$ripv_ra_isolation,
  df_high_density$ripv_rp_isolation,
  df_high_density$ripv_ri_isolation,
  df_high_density$right_carina_ripv
)
high_density <- 1 - high_density
report_mean_ci_binary(high_density)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lspv -------------------------------------------------------------------------
close <- c(
  df_close$lspv_lr_isolation,
  df_close$lspv_lrg_isolation,
  df_close$lspv_lp_isolation,
  df_close$left_carina_lspv
)
close <- 1 - close
report_mean_ci_binary(close)
report_sum_ci(close)

high_density <- c(
  df_high_density$lspv_lr_isolation,
  df_high_density$lspv_lrg_isolation,
  df_high_density$lspv_lp_isolation,
  df_high_density$left_carina_lspv
)
high_density <- 1 - high_density
report_mean_ci_binary(high_density)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)


# lipv -------------------------------------------------------------------------
close <- c(
  df_close$lipv_la_isolation,
  df_close$lipv_li_isolation,
  df_close$lipv_lp_isolation,
  df_close$left_carina_lipv
)
close <- 1 - close
report_mean_ci_binary(close)
report_sum_ci(close)

high_density <- c(
  df_high_density$lipv_la_isolation,
  df_high_density$lipv_li_isolation,
  df_high_density$lipv_lp_isolation,
  df_high_density$left_carina_lipv
)
high_density <- 1 - high_density
report_mean_ci_binary(high_density)
report_sum_ci(high_density)

x <- c(length(close) - sum(close), length(high_density) - sum(high_density))
prop.test(
  x = x,
  n = c(length(close), length(high_density))
)
