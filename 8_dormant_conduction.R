library(readxl)
library(tidyverse)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

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
close <- na.omit(df_close$dormant_rspv_rr)
high_density <- na.omit(df_high_density$dormant_rspv_rr)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# rspv_ra ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_rspv_ra)
high_density <- na.omit(df_high_density$dormant_rspv_ra)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# rspv_rp ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_rspv_rp)
high_density <- na.omit(df_high_density$dormant_rspv_rp)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# ripv_ra ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_ripv_ra)
high_density <- na.omit(df_high_density$dormant_ripv_ra)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# ripv_rp ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_ripv_rp)
high_density <- na.omit(df_high_density$dormant_ripv_rp)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# ripv_ri ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_ripv_ri)
high_density <- na.omit(df_high_density$dormant_ripv_ri)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# lspv_lr ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_lspv_lr)
high_density <- na.omit(df_high_density$dormant_lspv_lr)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# lspv_lrg ---------------------------------------------------------------------
close <- na.omit(df_close$dormant_lspv_lrg)
high_density <- na.omit(df_high_density$dormant_lspv_lrg)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# lspv_lp ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_lspv_lp)
high_density <- na.omit(df_high_density$dormant_lspv_lp)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# lipv_la ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_lipv_la)
high_density <- na.omit(df_high_density$dormant_lipv_la)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# lipv_li ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_lipv_li)
high_density <- na.omit(df_high_density$dormant_lipv_li)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# lipv_lp ----------------------------------------------------------------------
close <- na.omit(df_close$dormant_lipv_lp)
high_density <- na.omit(df_high_density$dormant_lipv_lp)

report_sum_ci(close)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# group comparisons ------------------------------------------------------------
close_all <- c(
  na.omit(df_close$dormant_rspv_rr) +
    na.omit(df_close$dormant_rspv_ra) +
    na.omit(df_close$dormant_rspv_rp) +
    na.omit(df_close$dormant_ripv_rp) +
    na.omit(df_close$dormant_lspv_lr) +
    na.omit(df_close$dormant_lspv_lrg) +
    na.omit(df_close$dormant_lspv_lp) +
    na.omit(df_close$dormant_lipv_la) +
    na.omit(df_close$dormant_lipv_li) +
    na.omit(df_close$dormant_lipv_lp)
)

high_density_all <- c(
  na.omit(df_high_density$dormant_rspv_rr) +
    na.omit(df_high_density$dormant_rspv_ra) +
    na.omit(df_high_density$dormant_rspv_rp) +
    na.omit(df_high_density$dormant_ripv_rp) +
    na.omit(df_high_density$dormant_lspv_lr) +
    na.omit(df_high_density$dormant_lspv_lrg) +
    na.omit(df_high_density$dormant_lspv_lp) +
    na.omit(df_high_density$dormant_lipv_la) +
    na.omit(df_high_density$dormant_lipv_li) +
    na.omit(df_high_density$dormant_lipv_lp)
)

report_sum_ci(close_all)
report_sum_ci(high_density_all)

wilcox.test(close_all, high_density_all)


# rspv -------------------------------------------------------------------------
close <- c(
  na.omit(df_close$dormant_rspv_rr),
  na.omit(df_close$dormant_rspv_ra),
  na.omit(df_close$dormant_rspv_rp)
)
report_sum_ci(close)

high_density <- c(
  na.omit(df_high_density$dormant_rspv_rr),
  na.omit(df_high_density$dormant_rspv_ra),
  na.omit(df_high_density$dormant_rspv_rp)
)
report_sum_ci(high_density)

wilcox.test(close, high_density)

# ripv -------------------------------------------------------------------------
# close: 4 ± 1.86, high_density: 2 ± 1.4
close <- c(
  na.omit(df_close$dormant_ripv_rp),
  na.omit(df_close$dormant_ripv_ra),
  na.omit(df_close$dormant_ripv_ri)
)
report_sum_ci(close)

high_density <- c(
  na.omit(df_high_density$dormant_ripv_rp),
  na.omit(df_high_density$dormant_ripv_ra),
  na.omit(df_high_density$dormant_ripv_ri)
)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# lspv -------------------------------------------------------------------------
# close: 10 ± 3.31, high_density: 1 ± 0.98
close <- c(
  na.omit(df_close$dormant_lspv_lr),
  na.omit(df_close$dormant_lspv_lrg),
  na.omit(df_close$dormant_lspv_lp)
)
report_sum_ci(close)

high_density <- c(
  na.omit(df_high_density$dormant_lspv_lr),
  na.omit(df_high_density$dormant_lspv_lrg),
  na.omit(df_high_density$dormant_lspv_lp)
)
report_sum_ci(high_density)

wilcox.test(close, high_density)


# lipv -------------------------------------------------------------------------
# close: 7 ± 2.56, high_density: 0 ± 0
close <- c(
  na.omit(df_close$dormant_lipv_la),
  na.omit(df_close$dormant_lipv_li),
  na.omit(df_close$dormant_lipv_lp)
)
report_sum_ci(close)

high_density <- c(
  na.omit(df_high_density$dormant_lipv_la),
  na.omit(df_high_density$dormant_lipv_li),
  na.omit(df_high_density$dormant_lipv_lp)
)
report_sum_ci(high_density)

wilcox.test(close, high_density)
