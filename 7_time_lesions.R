library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv")

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# skin_skin_time2 --------------------------------------------------------------
# close 80.93 +/- 34.71
mean(df_close$skin_skin_time2)
sd(df_close$skin_skin_time2)

# high_density 62.8 +/- 39.96
mean(df_high_density$skin_skin_time2)
sd(df_high_density$skin_skin_time2)

# test p = 0.09
wilcox.test(df_close$skin_skin_time2, df_high_density$skin_skin_time2)


# ablate_reisolization_time ----------------------------------------------------
# close 1.95 +/- 2.68
mean(df_close$ablate_reisolization_time, na.rm = TRUE)
sd(df_close$ablate_reisolization_time, na.rm = TRUE)

# high_density 0.81 +/- 1.78
mean(df_high_density$ablate_reisolization_time, na.rm = TRUE)
sd(df_high_density$ablate_reisolization_time, na.rm = TRUE)

# test p = 0.03
wilcox.test(
  df_close$ablate_reisolization_time,
  df_high_density$ablate_reisolization_time,
  na.rm = TRUE
)


# ablate_removal_time_dormant --------------------------------------------------
# close 2.56 +/- 2.21
mean(df_close$ablate_removal_time_dormant, na.rm = TRUE)
sd(df_close$ablate_removal_time_dormant, na.rm = TRUE)

# high_density 1.99 +/- 2.82
mean(df_high_density$ablate_removal_time_dormant, na.rm = TRUE)
sd(df_high_density$ablate_removal_time_dormant, na.rm = TRUE)

# test p = 0.07
wilcox.test(
  df_close$ablate_removal_time_dormant,
  df_high_density$ablate_removal_time_dormant,
  na.rm = TRUE
)


# rf_lesion_number_isolation ---------------------------------------------------
# close 7.45 +/- 8.57
mean(df_close$rf_lesion_number_isolation, na.rm = TRUE)
sd(df_close$rf_lesion_number_isolation, na.rm = TRUE)

# high_density 2.42 +/- 5.21
mean(df_high_density$rf_lesion_number_isolation, na.rm = TRUE)
sd(df_high_density$rf_lesion_number_isolation, na.rm = TRUE)

# test p = 0.01
wilcox.test(
  df_close$rf_lesion_number_isolation,
  df_high_density$rf_lesion_number_isolation,
  na.rm = TRUE
)


# rf_lesion_number_gap ---------------------------------------------------------
# close 9.79 +/- 8.94
mean(df_close$rf_lesion_number_gap, na.rm = TRUE)
sd(df_close$rf_lesion_number_gap, na.rm = TRUE)

# high_density 2.73 +/- 5.46
mean(df_high_density$rf_lesion_number_gap, na.rm = TRUE)
sd(df_high_density$rf_lesion_number_gap, na.rm = TRUE)

# test p = 0.0007
wilcox.test(
  df_close$rf_lesion_number_gap,
  df_high_density$rf_lesion_number_gap,
  na.rm = TRUE
)
