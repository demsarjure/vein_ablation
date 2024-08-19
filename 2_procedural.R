library(tidyverse)
library(readxl)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# skin_skin_time ---------------------------------------------------------------
# close 131.47 ± 27.06
mean(df_close$skin_skin_time)
sd(df_close$skin_skin_time)

# high_density 144.1 ± 23.96
mean(df_high_density$skin_skin_time)
sd(df_high_density$skin_skin_time)

# test p = 0.08
wilcox.test(df_close$skin_skin_time, df_high_density$skin_skin_time)


# la_dwell_time ----------------------------------------------------------------
# close 114.47 ± 22.63
mean(df_close$la_dwell_time)
sd(df_close$la_dwell_time)

# high_density 124.79 ± 20.38
mean(df_high_density$la_dwell_time)
sd(df_high_density$la_dwell_time)

# test p = 0.06
wilcox.test(df_close$la_dwell_time, df_high_density$la_dwell_time)


# ablation_time ----------------------------------------------------------------
# close 29.44 ± 7.9
mean(df_close$ablation_time)
sd(df_close$ablation_time)

# high_density 31.41 ± 8.75
mean(df_high_density$ablation_time)
sd(df_high_density$ablation_time)

# test p = 0.46
wilcox.test(df_close$ablation_time, df_high_density$ablation_time)


# ablation_time_hd -------------------------------------------------------------
df_high_density$ablation_time_hd <- as.numeric(df_high_density$ablation_time_hd)

# high_density 2.27 ± 2.17
mean(df_high_density$ablation_time_hd)
sd(df_high_density$ablation_time_hd)


# hd_map_time ------------------------------------------------------------------
df_high_density$hd_map_time <- as.numeric(df_high_density$hd_map_time)

# high_density 20.28 ± 7.21
mean(df_high_density$hd_map_time)
sd(df_high_density$hd_map_time)


# number_of_rf_lesions_pvi -----------------------------------------------------
# close 89.7 ± 20.26
mean(df_close$number_of_rf_lesions_pvi)
sd(df_close$number_of_rf_lesions_pvi)

# high_density 89.45 ± 20.16
mean(df_high_density$number_of_rf_lesions_pvi)
sd(df_high_density$number_of_rf_lesions_pvi)

# test p = 0.96
wilcox.test(
  df_close$number_of_rf_lesions_pvi,
  df_high_density$number_of_rf_lesions_pvi
)


# additional_lesions_hd --------------------------------------------------------
df_high_density$additional_lesions_hd <-
  as.numeric(df_high_density$additional_lesions_hd)

# high_density 6.86 ± 6.91
mean(df_high_density$additional_lesions_hd)
sd(df_high_density$additional_lesions_hd)


# first_pass_rspv --------------------------------------------------------------
# close: 76.67 ± 7.76%, high_density: 86.21 ± 6.4%
sum(df_close$first_pass_rspv) / nrow(df_close)
boot_sd(df_close$first_pass_rspv)

sum(df_high_density$first_pass_rspv) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_rspv)

# proportions test, p = 0.54
prop.test(
  x = c(sum(df_close$first_pass_rspv), sum(df_high_density$first_pass_rspv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_ripv --------------------------------------------------------------
# close: 83.33 ± 6.78%, high_density: 89.66 ± 5.7%
sum(df_close$first_pass_ripv) / nrow(df_close)
boot_sd(df_close$first_pass_ripv)

sum(df_high_density$first_pass_ripv) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_ripv)

# proportions test, p = 0.74
prop.test(
  x = c(sum(df_close$first_pass_ripv), sum(df_high_density$first_pass_ripv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_lspv --------------------------------------------------------------
# close: 83.33 ± 6.79%, high_density: 96.55 ± 3.39%
sum(df_close$first_pass_lspv) / nrow(df_close)
boot_sd(df_close$first_pass_lspv)

sum(df_high_density$first_pass_lspv) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_lspv)

# proportions test, p = 0.21
prop.test(
  x = c(sum(df_close$first_pass_lspv), sum(df_high_density$first_pass_lspv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_lipv --------------------------------------------------------------
# close: 100%, high_density: 97.67 ± 96.55 ± 3.39%
sum(df_close$first_pass_lipv) / nrow(df_close)
boot_sd(df_close$first_pass_lipv)

sum(df_high_density$first_pass_lipv) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_lipv)

# proportions test, p = 0.99
prop.test(
  x = c(sum(df_close$first_pass_lipv), sum(df_high_density$first_pass_lipv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_per_patient -------------------------------------------------------
# close: 60 ± 8.88%, high_density: 79.31 ± 7.54%
sum(df_close$first_pass_per_patient) / nrow(df_close)
boot_sd(df_close$first_pass_per_patient)

sum(df_high_density$first_pass_per_patient) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_per_patient)

# proportions test, p = 0.18
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
cti_high_density <- c(rep(1, n_cti_high_density), rep(0, nrow(df_high_density) - n_cti_high_density))

# 6.67 ± 4.52%
sum(cti_close / length(cti_close))
boot_sd(cti_close)

# 13.33 ± 6.15%
sum(cti_high_density / length(cti_high_density))
boot_sd(cti_high_density)

# proportions test, p = 0.67
prop.test(
  x = c(
    sum(cti_close),
    sum(cti_high_density)
  ),
  n = c(length(cti_close), length(cti_high_density))
)
