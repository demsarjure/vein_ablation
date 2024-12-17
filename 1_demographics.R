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
report_mean_ci(df_close$age)
report_mean_ci(df_high_density$age)

wilcox.test(df_close$age, df_high_density$age)


# gender -----------------------------------------------------------------------
report_mean_ci_binary(df_close$gender_numeric)
report_mean_ci_binary(df_high_density$gender_numeric)

prop.test(
  x = c(sum(df_close$gender_numeric), sum(df_high_density$gender_numeric)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# bmi --------------------------------------------------------------------------
report_mean_ci(df_close$bmi)
report_mean_ci(df_high_density$bmi)

wilcox.test(df_close$bmi, df_high_density$bmi)


# la_volume_index --------------------------------------------------------------
report_mean_ci(df_close$la_volume_index)
report_mean_ci(df_high_density$la_volume_index)

wilcox.test(df_close$la_volume_index, df_high_density$la_volume_index)


# la_size ----------------------------------------------------------------------
report_mean_ci(df_close$la_size)
report_mean_ci(df_high_density$la_size)

wilcox.test(df_close$la_size, df_high_density$la_size)


# lvedvi -----------------------------------------------------------------------
report_mean_ci(df_close$lvedvi)
report_mean_ci(na.omit(as.numeric(df_high_density$lvedvi)))

wilcox.test(df_close$lvedvi, high_density_lvedvi)


# lvef -------------------------------------------------------------------------
report_mean_ci(df_close$lvef)
report_mean_ci(na.omit(as.numeric(df_high_density$lvef)))

wilcox.test(df_close$lvef, high_density_lvef)


# class_III_drugs --------------------------------------------------------------
report_mean_ci_binary(df_close$class_III_drugs)
report_mean_ci_binary(df_high_density$class_III_drugs)

prop.test(
  x = c(sum(df_close$class_III_drugs), sum(df_high_density$class_III_drugs)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# class_I_drugs --------------------------------------------------------------
report_mean_ci_binary(df_close$class_I_drugs)
report_mean_ci_binary(df_high_density$class_I_drugs)

prop.test(
  x = c(sum(df_close$class_I_drugs), sum(df_high_density$class_I_drugs)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# beta_blockers --------------------------------------------------------------
report_mean_ci_binary(df_close$beta_blockers)
report_mean_ci_binary(df_high_density$beta_blockers)

prop.test(
  x = c(sum(df_close$beta_blockers), sum(df_high_density$beta_blockers)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# anticoagulant ----------------------------------------------------------------
report_mean_ci_binary(df_close$anticoagulant_numeric)
report_mean_ci_binary(df_high_density$anticoagulant_numeric)

# proportions test, p = 1
prop.test(
  x = c(
    sum(df_close$anticoagulant_numeric),
    sum(df_high_density$anticoagulant_numeric)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)


# probnp -----------------------------------------------------------------------
close_probnp <- na.omit(as.numeric(df_close$probnp))
report_mean_ci(close_probnp)
high_density_probnp <- na.omit(as.numeric(df_high_density$probnp))
report_mean_ci(high_density_probnp)

wilcox.test(close_probnp, high_density_probnp)


# chf --------------------------------------------------------------------------
report_mean_ci_binary(df_close$chf)
report_mean_ci_binary(df_high_density$chf)

prop.test(
  x = c(sum(df_close$chf), sum(df_high_density$chf)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# hypertension_history ---------------------------------------------------------
report_mean_ci_binary(df_close$hypertension_history)
report_mean_ci_binary(df_high_density$hypertension_history)

prop.test(
  x = c(
    sum(df_close$hypertension_history),
    sum(df_high_density$hypertension_history)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)


# age_75 -----------------------------------------------------------------------
report_mean_ci_binary(df_close$age_75)
report_mean_ci_binary(df_high_density$age_75)

prop.test(
  x = c(sum(df_close$age_75), sum(df_high_density$age_75)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# diabetes_history -------------------------------------------------------------
report_mean_ci_binary(df_close$diabetes_history)
report_mean_ci_binary(df_high_density$diabetes_history)

prop.test(
  x = c(sum(df_close$diabetes_history), sum(df_high_density$diabetes_history)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# vascular_disease -------------------------------------------------------------
report_mean_ci_binary(df_close$vascular_disease)
report_mean_ci_binary(df_high_density$vascular_disease)

prop.test(
  x = c(sum(df_close$vascular_disease), sum(df_high_density$vascular_disease)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# age_65_74 --------------------------------------------------------------------
report_mean_ci_binary(df_close$age_65_74)
report_mean_ci_binary(df_high_density$age_65_74)

prop.test(
  x = c(sum(df_close$age_65_74), sum(df_high_density$age_65_74)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# cha2ds2vasc ------------------------------------------------------------------
report_mean_ci(df_close$cha2ds2vasc)
report_mean_ci(df_high_density$cha2ds2vasc)

wilcox.test(df_close$cha2ds2vasc, df_high_density$cha2ds2vasc)


# cied -------------------------------------------------------------------------
cied_close <- c(1, rep(0, nrow(df_close) - 1))
cied_hd <- c(1, rep(0, nrow(df_high_density) - 1))
report_mean_ci_binary(cied_close)
report_mean_ci_binary(cied_hd)

prop.test(
  x = c(sum(cied_close), sum(cied_hd)),
  n = c(length(cied_close), length(cied_hd))
)
