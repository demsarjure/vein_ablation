library(tidyverse)
library(readxl)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# skin_skin_time ---------------------------------------------------------------
report_mean_ci(df_close$skin_skin_time)
report_mean_ci(df_high_density$skin_skin_time)

wilcox.test(df_close$skin_skin_time, df_high_density$skin_skin_time)


# la_dwell_time ----------------------------------------------------------------
report_mean_ci(df_close$la_dwell_time)
report_mean_ci(df_high_density$la_dwell_time)

wilcox.test(df_close$la_dwell_time, df_high_density$la_dwell_time)


# ablation_time ----------------------------------------------------------------
report_mean_ci(df_close$ablation_time)
report_mean_ci(df_high_density$ablation_time)

wilcox.test(df_close$ablation_time, df_high_density$ablation_time)


# ablation_time_hd -------------------------------------------------------------
df_high_density$ablation_time_hd <- as.numeric(df_high_density$ablation_time_hd)
report_mean_ci(df_high_density$ablation_time_hd)


# hd_map_time ------------------------------------------------------------------
df_high_density$hd_map_time <- as.numeric(df_high_density$hd_map_time)
report_mean_ci(df_high_density$hd_map_time)


# number_of_rf_lesions_pvi -----------------------------------------------------
report_mean_ci(df_close$number_of_rf_lesions_pvi)
report_mean_ci(df_high_density$number_of_rf_lesions_pvi)

wilcox.test(
  df_close$number_of_rf_lesions_pvi,
  df_high_density$number_of_rf_lesions_pvi
)


# additional_lesions_hd --------------------------------------------------------
df_high_density$additional_lesions_hd <-
  as.numeric(df_high_density$additional_lesions_hd)
report_mean_ci(df_high_density$additional_lesions_hd)


# first_pass_rspv --------------------------------------------------------------
report_mean_ci_binary(df_close$first_pass_rspv)
report_mean_ci_binary(df_high_density$first_pass_rspv)

prop.test(
  x = c(sum(df_close$first_pass_rspv), sum(df_high_density$first_pass_rspv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_ripv --------------------------------------------------------------
report_mean_ci_binary(df_close$first_pass_ripv)
report_mean_ci_binary(df_high_density$first_pass_ripv)

prop.test(
  x = c(sum(df_close$first_pass_ripv), sum(df_high_density$first_pass_ripv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_lspv --------------------------------------------------------------
report_mean_ci_binary(df_close$first_pass_lspv)
report_mean_ci_binary(df_high_density$first_pass_lspv)

prop.test(
  x = c(sum(df_close$first_pass_lspv), sum(df_high_density$first_pass_lspv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_lipv --------------------------------------------------------------
report_mean_ci_binary(df_close$first_pass_lipv)
report_mean_ci_binary(df_high_density$first_pass_lipv)

prop.test(
  x = c(sum(df_close$first_pass_lipv), sum(df_high_density$first_pass_lipv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_per_patient -------------------------------------------------------
report_mean_ci_binary(df_close$first_pass_per_patient)
report_mean_ci_binary(df_high_density$first_pass_per_patient)

prop.test(
  x = c(
    sum(df_close$first_pass_per_patient),
    sum(df_high_density$first_pass_per_patient)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)


# cti ablation, close 2 patients, high_density 4 patient -----------------------
n_cti_close <- 2
cti_close <- c(rep(1, n_cti_close), rep(0, nrow(df_close) - n_cti_close))

n_cti_high_density <- 4
cti_high_density <- c(
  rep(1, n_cti_high_density),
  rep(0, nrow(df_high_density) - n_cti_high_density)
)

report_mean_ci_binary(cti_close)
report_mean_ci_binary(cti_high_density)

prop.test(
  x = c(
    sum(cti_close),
    sum(cti_high_density)
  ),
  n = c(length(cti_close), length(cti_high_density))
)
