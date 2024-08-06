library(tidyverse)
library(readxl)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx", stringsAsFactors = TRUE)

# gender to 0/1
df_all$gender <- as.factor(df_all$gender)
df_all$gender_numeric <- as.numeric(df_all$gender) - 1

# anticoagulation to 0/1
df_all$anticoagulant <- as.factor(df_all$gender)
df_all$anticoagulant_numeric <- as.numeric(df_all$anticoagulant) - 1

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# age --------------------------------------------------------------------------
# close 63.05 ± 11.25
mean(df_close$age)
sd(df_close$age)

# high_density 61.56 ± 9.6
mean(df_high_density$age)
sd(df_high_density$age)

# test p = 0.44
wilcox.test(df_close$age, df_high_density$age)


# gender -----------------------------------------------------------------------
# 0 = F, 1 = M, close: 50 ± 9.2% male, high_density: 67.67 ± 8.67% male
sum(df_close$gender_numeric) / nrow(df_close)
boot_sd(df_close$gender_numeric)

sum(df_high_density$gender_numeric) / nrow(df_high_density)
boot_sd(df_high_density$gender_numeric)

# proportions test, p = 0.29
prop.test(
  x = c(sum(df_close$gender_numeric), sum(df_high_density$gender_numeric)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# bmi --------------------------------------------------------------------------
# close 29.69 ± 5.74
mean(df_close$bmi)
sd(df_close$bmi)

# high_density 27.88 ± 4.4
mean(df_high_density$bmi)
sd(df_high_density$bmi)

# test p = 0.4
wilcox.test(df_close$bmi, df_high_density$bmi)


# la_volume_index --------------------------------------------------------------
# close 39.87 ± 11.4
mean(df_close$la_volume_index)
sd(df_close$la_volume_index)

# high_density 39.1 ± 10.21
mean(df_high_density$la_volume_index)
sd(df_high_density$la_volume_index)

# test p = 0.54
wilcox.test(df_close$la_volume_index, df_high_density$la_volume_index)


# la_size ----------------------------------------------------------------------
# close 41.1 ± 4.13
mean(df_close$la_size)
sd(df_close$la_size)

# high_density 41.2 ± 6.05
mean(df_high_density$la_size)
sd(df_high_density$la_size)

# test p = 0.92
wilcox.test(df_close$la_size, df_high_density$la_size)


# lvedvi -----------------------------------------------------------------------
# close 60.6 ± 7.68
mean(df_close$lvedvi)
sd(df_close$lvedvi)

# high_density 58.89 ± 8.26
# drop NA
high_density_lvedvi <- df_high_density$lvedvi[!is.na(df_high_density$lvedvi)]
mean(high_density_lvedvi)
sd(high_density_lvedvi)

# test p = 0.71
wilcox.test(df_close$lvedvi, high_density_lvedvi)


# class_III_drugs --------------------------------------------------------------
# close: 33.33 ± 8.59%, high_density: 26.67 ± 8.11%
sum(df_close$class_III_drugs) / nrow(df_close)
boot_sd(df_close$class_III_drugs)

sum(df_high_density$class_III_drugs) / nrow(df_high_density)
boot_sd(df_high_density$class_III_drugs)

# proportions test, p = 0.78
prop.test(
  x = c(sum(df_close$class_III_drugs), sum(df_high_density$class_III_drugs)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# class_I_drugs --------------------------------------------------------------
# close: 43.33 ± 8.91%, high_density: 33.33 ± 8.63%
sum(df_close$class_I_drugs) / nrow(df_close)
boot_sd(df_close$class_I_drugs)

sum(df_high_density$class_I_drugs) / nrow(df_high_density)
boot_sd(df_high_density$class_I_drugs)

# proportions test, p = 0.6
prop.test(
  x = c(sum(df_close$class_I_drugs), sum(df_high_density$class_I_drugs)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# beta_blockers --------------------------------------------------------------
# close: 56.67 ± 8.97%, high_density: 53.33 ± 9.08%
sum(df_close$beta_blockers) / nrow(df_close)
boot_sd(df_close$beta_blockers)

sum(df_high_density$beta_blockers) / nrow(df_high_density)
boot_sd(df_high_density$beta_blockers)

# proportions test, p = 1
prop.test(
  x = c(sum(df_close$beta_blockers), sum(df_high_density$beta_blockers)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# anticoagulant ----------------------------------------------------------------
# close: 83.33 ± 6.83%, high_density: 80 ± 7.3%
sum(df_close$anticoagulant_numeric) / nrow(df_close)
boot_sd(df_close$anticoagulant_numeric)

sum(df_high_density$anticoagulant_numeric) / nrow(df_high_density)
boot_sd(df_high_density$anticoagulant_numeric)

# proportions test, p = 1
prop.test(
  x = c(
    sum(df_close$anticoagulant_numeric),
    sum(df_high_density$anticoagulant_numeric)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)


# probnp -----------------------------------------------------------------------
# close 255.21 ± 169.69
close_probnp <- df_close$probnp[!is.na(df_close$probnp)]
mean(close_probnp)
sd(close_probnp)

# high_density 313.9 ± 494.2
# drop NA
high_density_probnp <- df_high_density$probnp[!is.na(df_high_density$probnp)]
mean(high_density_probnp)
sd(high_density_probnp)

# test p = 0.27
wilcox.test(close_probnp, high_density_probnp)


# chf --------------------------------------------------------------------------
# close: 3.33 ± 3.27%, high_density: 3.33 ± 3.3%
sum(df_close$chf) / nrow(df_close)
boot_sd(df_close$chf)

sum(df_high_density$chf) / nrow(df_high_density)
boot_sd(df_high_density$chf)

# proportions test, p = 1
prop.test(
  x = c(sum(df_close$chf), sum(df_high_density$chf)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# hypertension_history ---------------------------------------------------------
# close: 56.67 ± 8.97%, high_density: 43.33 ± 9.05%
sum(df_close$hypertension_history) / nrow(df_close)
boot_sd(df_close$hypertension_history)

sum(df_high_density$hypertension_history) / nrow(df_high_density)
boot_sd(df_high_density$hypertension_history)

# proportions test, p = 0.44
prop.test(
  x = c(sum(df_close$hypertension_history), sum(df_high_density$hypertension_history)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# age_75 -----------------------------------------------------------------------
# close: 13.33 ± 6.21%, high_density: 3.33 ± 3.24%
sum(df_close$age_75) / nrow(df_close)
boot_sd(df_close$age_75)

sum(df_high_density$age_75) / nrow(df_high_density)
boot_sd(df_high_density$age_75)

# proportions test, p = 0.35
prop.test(
  x = c(sum(df_close$age_75), sum(df_high_density$age_75)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# diabetes_history -------------------------------------------------------------
# close: 13.33 ± 6.2%, high_density: 10 ± 5.49%
sum(df_close$diabetes_history) / nrow(df_close)
boot_sd(df_close$diabetes_history)

sum(df_high_density$diabetes_history) / nrow(df_high_density)
boot_sd(df_high_density$diabetes_history)

# proportions test, p = 1
prop.test(
  x = c(sum(df_close$diabetes_history), sum(df_high_density$diabetes_history)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# vascular_disease -------------------------------------------------------------
# close: 10 ± 5.45%, high_density: 3.33 ± 3.25%
sum(df_close$vascular_disease) / nrow(df_close)
boot_sd(df_close$vascular_disease)

sum(df_high_density$vascular_disease) / nrow(df_high_density)
boot_sd(df_high_density$vascular_disease)

# proportions test, p = 0.6
prop.test(
  x = c(sum(df_close$vascular_disease), sum(df_high_density$vascular_disease)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# age_65_74 --------------------------------------------------------------------
# close: 50 ± 9.22%, high_density: 36.67 ± 8.7%
sum(df_close$age_65_74) / nrow(df_close)
boot_sd(df_close$age_65_74)

sum(df_high_density$age_65_74) / nrow(df_high_density)
boot_sd(df_high_density$age_65_74)

# proportions test, p = 0.43
prop.test(
  x = c(sum(df_close$age_65_74), sum(df_high_density$age_65_74)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# cha2ds2vasc ------------------------------------------------------------------
# close 2 ± 1.46
mean(df_close$cha2ds2vasc)
sd(df_close$cha2ds2vasc)

# high_density 1.3 ± 1.05
mean(df_high_density$cha2ds2vasc)
sd(df_high_density$cha2ds2vasc)

# test p = 0.05
wilcox.test(df_close$cha2ds2vasc, df_high_density$cha2ds2vasc)
