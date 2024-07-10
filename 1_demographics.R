library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv", sep = ";", stringsAsFactors = TRUE)

# gender to 0/1
df_all$gender_numeric <- as.numeric(df_all$gender) - 1

# anticoagulation to 0/1
df_all$anticoagulant_numeric <- as.numeric(df_all$anticoagulant) - 1

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# age --------------------------------------------------------------------------
# close 63.05 +/- 11.25
mean(df_close$age)
sd(df_close$age)

# high_density 61.56 +/- 9.6
mean(df_high_density$age)
sd(df_high_density$age)

# test p = 0.44
wilcox.test(df_close$age, df_high_density$age)


# gender -----------------------------------------------------------------------
# 0 = F, 1 = M, close: 0.5 male, high_density: 0.67 male
sum(df_close$gender_numeric) / nrow(df_close)
sum(df_high_density$gender_numeric) / nrow(df_high_density)

# proportions test, p = 0.29
prop.test(
    x = c(sum(df_close$gender_numeric), sum(df_high_density$gender_numeric)),
    n = c(nrow(df_close), nrow(df_high_density))
)


# bmi --------------------------------------------------------------------------
# close 29.69 +/- 5.74
mean(df_close$bmi)
sd(df_close$bmi)

# high_density 27.88 +/- 4.4
mean(df_high_density$bmi)
sd(df_high_density$bmi)

# test p = 0.4
wilcox.test(df_close$bmi, df_high_density$bmi)


# la_volume_index --------------------------------------------------------------
# close 39.87 +/- 11.4
mean(df_close$la_volume_index)
sd(df_close$la_volume_index)

# high_density 39.1 +/- 10.21
mean(df_high_density$la_volume_index)
sd(df_high_density$la_volume_index)

# test p = 0.54
wilcox.test(df_close$la_volume_index, df_high_density$la_volume_index)


# la_size ----------------------------------------------------------------------
# close 41.1 +/- 4.13
mean(df_close$la_size)
sd(df_close$la_size)

# high_density 41.2 +/- 6.05
mean(df_high_density$la_size)
sd(df_high_density$la_size)

# test p = 0.92
wilcox.test(df_close$la_size, df_high_density$la_size)


# lvedvi -----------------------------------------------------------------------
# close 60.6 +/- 7.68
mean(df_close$lvedvi)
sd(df_close$lvedvi)

# high_density 58.89 +/- 8.26
# drop NA
high_density_lvedvi <- df_high_density$lvedvi[!is.na(df_high_density$lvedvi)]
mean(high_density_lvedvi)
sd(high_density_lvedvi)

# test p = 0.71
wilcox.test(df_close$lvedvi, high_density_lvedvi)


# anticoagulant ----------------------------------------------------------------
# 0 = No, 1 = Yes, close: 0.83 yes, high_density: 0.8 yes
sum(df_close$anticoagulant_numeric) / nrow(df_close)
sum(df_high_density$anticoagulant_numeric) / nrow(df_high_density)

# proportions test, p = 1
prop.test(
    x = c(sum(df_close$anticoagulant_numeric), sum(df_high_density$anticoagulant_numeric)),
    n = c(nrow(df_close), nrow(df_high_density))
)


# probnp -----------------------------------------------------------------------
# close 255.21 +/- 169.69
close_probnp <- df_close$probnp[!is.na(df_close$probnp)]
mean(close_probnp)
sd(close_probnp)

# high_density 313.9 +/- 494.2
# drop NA
high_density_probnp <- df_high_density$probnp[!is.na(df_high_density$probnp)]
mean(high_density_probnp)
sd(high_density_probnp)

# test p = 0.27
wilcox.test(close_probnp, high_density_probnp)


# chf --------------------------------------------------------------------------
# close: 0.03, high_density: 0.03
sum(df_close$chf) / nrow(df_close)
sum(df_high_density$chf) / nrow(df_high_density)

# proportions test, p = 1
prop.test(
    x = c(sum(df_close$chf), sum(df_high_density$chf)),
    n = c(nrow(df_close), nrow(df_high_density))
)


# hypertension_history ---------------------------------------------------------
# close: 0.57, high_density: 0.43
sum(df_close$hypertension_history) / nrow(df_close)
sum(df_high_density$hypertension_history) / nrow(df_high_density)

# proportions test, p = 0.44
prop.test(
    x = c(sum(df_close$hypertension_history), sum(df_high_density$hypertension_history)),
    n = c(nrow(df_close), nrow(df_high_density))
)


# age_75 -----------------------------------------------------------------------
# close: 0.13, high_density: 0.03
sum(df_close$age_75) / nrow(df_close)
sum(df_high_density$age_75) / nrow(df_high_density)

# proportions test, p = 0.35
prop.test(
    x = c(sum(df_close$age_75), sum(df_high_density$age_75)),
    n = c(nrow(df_close), nrow(df_high_density))
)


# diabetes_history -------------------------------------------------------------
# close: 0.13, high_density: 0.1
sum(df_close$diabetes_history) / nrow(df_close)
sum(df_high_density$diabetes_history) / nrow(df_high_density)

# proportions test, p = 1
prop.test(
    x = c(sum(df_close$diabetes_history), sum(df_high_density$diabetes_history)),
    n = c(nrow(df_close), nrow(df_high_density))
)


# stroke -----------------------------------------------------------------------
# close: 0.03, high_density: 0
sum(df_close$stroke) / nrow(df_close)
sum(df_high_density$stroke) / nrow(df_high_density)

# proportions test, p = 1
prop.test(
    x = c(sum(df_close$stroke), sum(df_high_density$stroke)),
    n = c(nrow(df_close), nrow(df_high_density))
)
# 1 patient has stroke, maybe remove?


# vascular_disease -------------------------------------------------------------
# close: 0.1, high_density: 0.03
sum(df_close$vascular_disease) / nrow(df_close)
sum(df_high_density$vascular_disease) / nrow(df_high_density)

# proportions test, p = 0.6
prop.test(
    x = c(sum(df_close$vascular_disease), sum(df_high_density$vascular_disease)),
    n = c(nrow(df_close), nrow(df_high_density))
)


# age_65_74 --------------------------------------------------------------------
# close: 0.5, high_density: 0.37
sum(df_close$age_65_74) / nrow(df_close)
sum(df_high_density$age_65_74) / nrow(df_high_density)

# proportions test, p = 0.43
prop.test(
    x = c(sum(df_close$age_65_74), sum(df_high_density$age_65_74)),
    n = c(nrow(df_close), nrow(df_high_density))
)


# cha2ds2vasc ------------------------------------------------------------------
# close 2 +/- 1.46
mean(df_close$cha2ds2vasc)
sd(df_close$cha2ds2vasc)

# high_density 1.3 +/- 1.05
mean(df_high_density$cha2ds2vasc)
sd(df_high_density$cha2ds2vasc)

# test p = 0.05
wilcox.test(df_close$cha2ds2vasc, df_high_density$cha2ds2vasc)
