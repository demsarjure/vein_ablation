library(tidyverse)
library(readxl)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# gender to 0/1
df_all$gender <- as.factor(df_all$gender)
df_all$gender_numeric <- as.numeric(df_all$gender) - 1

# anticoagulation to 0/1
df_all$anticoagulant <- as.factor(df_all$anticoagulant)
df_all$anticoagulant_numeric <- as.numeric(df_all$anticoagulant) - 1

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# age --------------------------------------------------------------------------
# close 63.05 ± 11.25
mean(df_close$age)
sd(df_close$age)

# high_density 62.61 ± 7.83
mean(df_high_density$age)
sd(df_high_density$age)

# test p = 0.57
wilcox.test(df_close$age, df_high_density$age)


# gender -----------------------------------------------------------------------
# 0 = F, 1 = M, close: 50 ± 9.2% male, high_density: 65.52 ± 8.81% male
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

# high_density 27.98 ± 4.44
mean(df_high_density$bmi)
sd(df_high_density$bmi)

# test p = 0.46
wilcox.test(df_close$bmi, df_high_density$bmi)


# la_volume_index --------------------------------------------------------------
# close 39.87 ± 11.4
mean(df_close$la_volume_index)
sd(df_close$la_volume_index)

# high_density 39.38 ± 10.28
mean(df_high_density$la_volume_index)
sd(df_high_density$la_volume_index)

# test p = 0.62
wilcox.test(df_close$la_volume_index, df_high_density$la_volume_index)


# la_size ----------------------------------------------------------------------
# close 41.1 ± 4.13
mean(df_close$la_size)
sd(df_close$la_size)

# high_density 41.38 ± 6.08
mean(df_high_density$la_size)
sd(df_high_density$la_size)

# test p = 0.95
wilcox.test(df_close$la_size, df_high_density$la_size)


# lvedvi -----------------------------------------------------------------------
# close 60.6 ± 7.68
mean(df_close$lvedvi)
sd(df_close$lvedvi)

# high_density 58.78 ± 8.4
# drop NA
high_density_lvedvi <- df_high_density$lvedvi[!is.na(df_high_density$lvedvi)]
mean(high_density_lvedvi)
sd(high_density_lvedvi)

# test p = 0.64
wilcox.test(df_close$lvedvi, high_density_lvedvi)


# lvef -------------------------------------------------------------------------
# close 66.57 ± 5.91
mean(df_close$lvef)
sd(df_close$lvef)

# high_density 65.22 ± 7.01
high_density_lvef <- df_high_density$lvef[!is.na(df_high_density$lvef)]
mean(high_density_lvef)
sd(high_density_lvef)

# test p = 0.47
wilcox.test(df_close$lvef, high_density_lvef)


# class_III_drugs --------------------------------------------------------------
# close: 33.33 ± 8.59%, high_density: 27.59 ± 8.3%
sum(df_close$class_III_drugs) / nrow(df_close)
boot_sd(df_close$class_III_drugs)

sum(df_high_density$class_III_drugs) / nrow(df_high_density)
boot_sd(df_high_density$class_III_drugs)

# proportions test, p = 0.84
prop.test(
  x = c(sum(df_close$class_III_drugs), sum(df_high_density$class_III_drugs)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# class_I_drugs --------------------------------------------------------------
# close: 43.33 ± 8.91%, high_density: 31.03 ± 8.49%
sum(df_close$class_I_drugs) / nrow(df_close)
boot_sd(df_close$class_I_drugs)

sum(df_high_density$class_I_drugs) / nrow(df_high_density)
boot_sd(df_high_density$class_I_drugs)

# proportions test, p = 0.48
prop.test(
  x = c(sum(df_close$class_I_drugs), sum(df_high_density$class_I_drugs)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# beta_blockers --------------------------------------------------------------
# close: 56.67 ± 8.97%, high_density: 55.17 ± 9.25%
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
# close: 83.33 ± 6.87%, high_density: 82.76 ± 6.94%
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
close_probnp <- na.omit(as.numeric(df_close$probnp))
mean(close_probnp)
sd(close_probnp)

# high_density 323.3 ± 501.06
# drop NA
high_density_probnp <- na.omit(as.numeric(df_high_density$probnp))
mean(high_density_probnp)
sd(high_density_probnp)

# test p = 0.27
wilcox.test(close_probnp, high_density_probnp)


# chf --------------------------------------------------------------------------
# close: 3.33 ± 3.27%, high_density: 3.45 ± 3.37%
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
# close: 56.67 ± 8.97%, high_density: 41.38 ± 9.08%
sum(df_close$hypertension_history) / nrow(df_close)
boot_sd(df_close$hypertension_history)

sum(df_high_density$hypertension_history) / nrow(df_high_density)
boot_sd(df_high_density$hypertension_history)

# proportions test, p = 0.36
prop.test(
  x = c(sum(df_close$hypertension_history), sum(df_high_density$hypertension_history)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# age_75 -----------------------------------------------------------------------
# close: 13.33 ± 6.21%, high_density: 3.45 ± 3.37%
sum(df_close$age_75) / nrow(df_close)
boot_sd(df_close$age_75)

sum(df_high_density$age_75) / nrow(df_high_density)
boot_sd(df_high_density$age_75)

# proportions test, p = 0.37
prop.test(
  x = c(sum(df_close$age_75), sum(df_high_density$age_75)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# diabetes_history -------------------------------------------------------------
# close: 13.33 ± 6.2%, high_density: 10.34 ± 5.63%
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
# close: 10 ± 5.45%, high_density: 3.45 ± 3.42%
sum(df_close$vascular_disease) / nrow(df_close)
boot_sd(df_close$vascular_disease)

sum(df_high_density$vascular_disease) / nrow(df_high_density)
boot_sd(df_high_density$vascular_disease)

# proportions test, p = 0.63
prop.test(
  x = c(sum(df_close$vascular_disease), sum(df_high_density$vascular_disease)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# age_65_74 --------------------------------------------------------------------
# close: 50 ± 9.22%, high_density: 37.93 ± 9.14%
sum(df_close$age_65_74) / nrow(df_close)
boot_sd(df_close$age_65_74)

sum(df_high_density$age_65_74) / nrow(df_high_density)
boot_sd(df_high_density$age_65_74)

# proportions test, p = 0.5
prop.test(
  x = c(sum(df_close$age_65_74), sum(df_high_density$age_65_74)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# cha2ds2vasc ------------------------------------------------------------------
# close 2 ± 1.46
mean(df_close$cha2ds2vasc)
sd(df_close$cha2ds2vasc)

# high_density 1.31 ± 1.07
mean(df_high_density$cha2ds2vasc)
sd(df_high_density$cha2ds2vasc)

# test p = 0.06
wilcox.test(df_close$cha2ds2vasc, df_high_density$cha2ds2vasc)
