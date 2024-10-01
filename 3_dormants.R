library(tidyverse)
library(readxl)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")
df_high_density <- df_all %>% filter(procedure_type == "high_density")

# r
df_high_density$rspv_rr <- as.numeric(df_high_density$rspv_rr)
df_high_density$rspv_ra <- as.numeric(df_high_density$rspv_ra)
df_high_density$rspv_rp <- as.numeric(df_high_density$rspv_rp)
df_high_density$ripv_ra <- as.numeric(df_high_density$ripv_ra)
df_high_density$ripv_rp <- as.numeric(df_high_density$ripv_rp)
df_high_density$ripv_ri <- as.numeric(df_high_density$ripv_ri)

# l
df_high_density$lspv_lr <- as.numeric(df_high_density$lspv_lr)
df_high_density$lspv_lrg <- as.numeric(df_high_density$lspv_lrg)
df_high_density$lspv_lp <- as.numeric(df_high_density$lspv_lp)
df_high_density$lipv_la <- as.numeric(df_high_density$lipv_la)
df_high_density$lipv_li <- as.numeric(df_high_density$lipv_li)
df_high_density$lipv_lp <- as.numeric(df_high_density$lipv_lp)


# RSPV -------------------------------------------------------------------------
report_sum_ci(na.omit(df_high_density$rspv_rr))
report_sum_ci(na.omit(df_high_density$rspv_ra))
report_sum_ci(na.omit(df_high_density$rspv_rp))

report_prop_ci(na.omit(df_high_density$rspv_rr), length(na.omit(df_high_density$rspv_rr)))
report_prop_ci(na.omit(df_high_density$rspv_ra), length(na.omit(df_high_density$rspv_ra)))
report_prop_ci(na.omit(df_high_density$rspv_rp), length(na.omit(df_high_density$rspv_rp)))


# RIPV -------------------------------------------------------------------------
report_sum_ci(na.omit(df_high_density$ripv_ra))
report_sum_ci(na.omit(df_high_density$ripv_rp))
report_sum_ci(na.omit(df_high_density$ripv_ri))

report_prop_ci(na.omit(df_high_density$ripv_ra), length(na.omit(df_high_density$ripv_ra)))
report_prop_ci(na.omit(df_high_density$ripv_rp), length(na.omit(df_high_density$ripv_rp)))
report_prop_ci(na.omit(df_high_density$ripv_ri), length(na.omit(df_high_density$ripv_ri)))


# LSPV -------------------------------------------------------------------------
report_sum_ci(na.omit(df_high_density$lspv_lr))
report_sum_ci(na.omit(df_high_density$lspv_lrg))
report_sum_ci(na.omit(df_high_density$lspv_lp))

report_prop_ci(na.omit(df_high_density$lspv_lr), length(na.omit(df_high_density$lspv_lr)))
report_prop_ci(na.omit(df_high_density$lspv_lrg), length(na.omit(df_high_density$lspv_lrg)))
report_prop_ci(na.omit(df_high_density$lspv_lp), length(na.omit(df_high_density$lspv_lp)))


# LIPV -------------------------------------------------------------------------
report_sum_ci(na.omit(df_high_density$lipv_la))
report_sum_ci(na.omit(df_high_density$lipv_li))
report_sum_ci(na.omit(df_high_density$lipv_lp))

report_prop_ci(na.omit(df_high_density$lipv_la), length(na.omit(df_high_density$lipv_la)))
report_prop_ci(na.omit(df_high_density$lipv_li), length(na.omit(df_high_density$lipv_li)))
report_prop_ci(na.omit(df_high_density$lipv_lp), length(na.omit(df_high_density$lipv_lp)))
