library(tidyverse)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv")

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# skin_skin_time ---------------------------------------------------------------
# close 131.47 +/- 27.06
mean(df_close$skin_skin_time)
sd(df_close$skin_skin_time)

# high_density 144.87 +/- 23.91
mean(df_high_density$skin_skin_time)
sd(df_high_density$skin_skin_time)

# test p = 0.05
wilcox.test(df_close$skin_skin_time, df_high_density$skin_skin_time)


# la_dwell_time ----------------------------------------------------------------
# close 114.47 +/- 22.63
mean(df_close$la_dwell_time)
sd(df_close$la_dwell_time)

# high_density 125.8 +/- 20.77
mean(df_high_density$la_dwell_time)
sd(df_high_density$la_dwell_time)

# test p = 0.04
wilcox.test(df_close$la_dwell_time, df_high_density$la_dwell_time)


# ablation_time ----------------------------------------------------------------
# close 29.44 +/- 7.9
mean(df_close$ablation_time)
sd(df_close$ablation_time)

# high_density 31.47 +/- 8.6
mean(df_high_density$ablation_time)
sd(df_high_density$ablation_time)

# test p = 0.44
wilcox.test(df_close$ablation_time, df_high_density$ablation_time)


# ablation_time_hd -------------------------------------------------------------
df_high_density$ablation_time_hd <- as.numeric(df_high_density$ablation_time_hd)

# high_density 2.34 +/- 2.18
mean(df_high_density$ablation_time_hd)
sd(df_high_density$ablation_time_hd)


# hd_map_time ------------------------------------------------------------------
df_high_density$hd_map_time <- as.numeric(df_high_density$hd_map_time)

# high_density 20.23 +/- 7.09
mean(df_high_density$hd_map_time)
sd(df_high_density$hd_map_time)


# number_of_rf_lesions_pvi -----------------------------------------------------
# close 89.7 +/- 20.26
mean(df_close$number_of_rf_lesions_pvi)
sd(df_close$number_of_rf_lesions_pvi)

# high_density 89.07 +/- 19.92
mean(df_high_density$number_of_rf_lesions_pvi)
sd(df_high_density$number_of_rf_lesions_pvi)

# test p = 0.89
wilcox.test(
  df_close$number_of_rf_lesions_pvi,
  df_high_density$number_of_rf_lesions_pvi
)


# additional_lesions_hd --------------------------------------------------------
df_high_density$additional_lesions_hd <-
  as.numeric(df_high_density$additional_lesions_hd)

# high_density 7 +/- 6.83
mean(df_high_density$additional_lesions_hd)
sd(df_high_density$additional_lesions_hd)


# first_pass_rspv --------------------------------------------------------------
# close: 76.67 +/- 7.76, high_density: 86.67 +/- 6.19
sum(df_close$first_pass_rspv) / nrow(df_close)
boot_sd(df_close$first_pass_rspv)

sum(df_high_density$first_pass_rspv) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_rspv)

# proportions test, p = 0.5
prop.test(
  x = c(sum(df_close$first_pass_rspv), sum(df_high_density$first_pass_rspv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_ripv --------------------------------------------------------------
# close: 83.33 +/- 6.78, high_density: 90 +/- 5.4
sum(df_close$first_pass_ripv) / nrow(df_close)
boot_sd(df_close$first_pass_ripv)

sum(df_high_density$first_pass_ripv) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_ripv)

# proportions test, p = 0.7
prop.test(
  x = c(sum(df_close$first_pass_ripv), sum(df_high_density$first_pass_ripv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_lspv --------------------------------------------------------------
# close: 83.33 +/- 6.79, high_density: 96.67 +/- 3.27
sum(df_close$first_pass_lspv) / nrow(df_close)
boot_sd(df_close$first_pass_lspv)

sum(df_high_density$first_pass_lspv) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_lspv)

# proportions test, p = 0.19
prop.test(
  x = c(sum(df_close$first_pass_lspv), sum(df_high_density$first_pass_lspv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_lipv --------------------------------------------------------------
# close: 1, high_density: 0.97 +/- 0.03
sum(df_close$first_pass_lipv) / nrow(df_close)
boot_sd(df_close$first_pass_lipv)

sum(df_high_density$first_pass_lipv) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_lipv)

# proportions test, p = 1
prop.test(
  x = c(sum(df_close$first_pass_lipv), sum(df_high_density$first_pass_lipv)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# first_pass_per_patient -------------------------------------------------------
# close: 60 +/- 8.88, high_density: 80 +/- 7.37
sum(df_close$first_pass_per_patient) / nrow(df_close)
boot_sd(df_close$first_pass_per_patient)

sum(df_high_density$first_pass_per_patient) / nrow(df_high_density)
boot_sd(df_high_density$first_pass_per_patient)

# proportions test, p = 0.16
prop.test(
  x = c(
    sum(df_close$first_pass_per_patient),
    sum(df_high_density$first_pass_per_patient)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)
