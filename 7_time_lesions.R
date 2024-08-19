library(readxl)
library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# skin_skin_time2 --------------------------------------------------------------
# close 83.97 ± 31.19
mean(df_close$skin_skin_time2)
sd(df_close$skin_skin_time2)

# high_density 69.55 ± 39.19
mean(df_high_density$skin_skin_time2)
sd(df_high_density$skin_skin_time2)

# test p = 0.1
wilcox.test(df_close$skin_skin_time2, df_high_density$skin_skin_time2)


# ablate_reisolization_time ----------------------------------------------------
# close 1.88 ± 2.65
mean(df_close$ablate_reisolization_time, na.rm = TRUE)
sd(df_close$ablate_reisolization_time, na.rm = TRUE)

# high_density 0.73 ± 1.7
mean(df_high_density$ablate_reisolization_time, na.rm = TRUE)
sd(df_high_density$ablate_reisolization_time, na.rm = TRUE)

# test p = 0.02
wilcox.test(
  df_close$ablate_reisolization_time,
  df_high_density$ablate_reisolization_time,
  na.rm = TRUE
)


# ablate_removal_time_dormant --------------------------------------------------
# close 2.71 ± 2.26
mean(df_close$ablate_removal_time_dormant, na.rm = TRUE)
sd(df_close$ablate_removal_time_dormant, na.rm = TRUE)

# high_density 1.79 ± 2.74
mean(df_high_density$ablate_removal_time_dormant, na.rm = TRUE)
sd(df_high_density$ablate_removal_time_dormant, na.rm = TRUE)

# test p = 0.02
wilcox.test(
  df_close$ablate_removal_time_dormant,
  df_high_density$ablate_removal_time_dormant,
  na.rm = TRUE
)


# rf_lesion_number_isolation ---------------------------------------------------
# close 7.2 ± 8.53
mean(df_close$rf_lesion_number_isolation, na.rm = TRUE)
sd(df_close$rf_lesion_number_isolation, na.rm = TRUE)

# high_density 2.17 ± 4.99
mean(df_high_density$rf_lesion_number_isolation, na.rm = TRUE)
sd(df_high_density$rf_lesion_number_isolation, na.rm = TRUE)

# test p = 0.01
wilcox.test(
  df_close$rf_lesion_number_isolation,
  df_high_density$rf_lesion_number_isolation,
  na.rm = TRUE
)


# rf_lesion_number_gap ---------------------------------------------------------
# close 9.79 ± 8.84
mean(df_close$rf_lesion_number_gap, na.rm = TRUE)
sd(df_close$rf_lesion_number_gap, na.rm = TRUE)

# high_density 2.45 ± 5.23
mean(df_high_density$rf_lesion_number_gap, na.rm = TRUE)
sd(df_high_density$rf_lesion_number_gap, na.rm = TRUE)

# test p = 0.0001
wilcox.test(
  df_close$rf_lesion_number_gap,
  df_high_density$rf_lesion_number_gap,
  na.rm = TRUE
)
