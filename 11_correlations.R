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


## demographic data ------------------------------------------------------------
# age
# gender
# bmi
# la_volume_index
# la_size
# lvedvi
# anticoagulant
# probnp
# chf
# hypertension_history
# age_75
# diabetes_history
# vascular_disease
# age_65_74
# cha2ds2vasc

# select only demographic data columns and the column
df_demographic <- df_all %>% select(
  age,
  gender_numeric,
  bmi,
  la_volume_index,
  la_size,
  lvedvi,
  anticoagulant_numeric,
  probnp,
  chf,
  hypertension_history,
  age_75,
  diabetes_history,
  vascular_disease,
  age_65_74,
  cha2ds2vasc,
  all_4_veins_isolated
)

# drop na
df_demographic <- drop_na(df_demographic)

# reconnected
reconnected <- as.numeric(!df_demographic$all_4_veins_isolated)

# remove target
df_demographic <- df_demographic %>% select(-all_4_veins_isolated)

# linear regression df_demographic vs reconnected
lm_demographic <- lm(reconnected ~ ., data = df_demographic)
summary(lm_demographic)


## procedural data -------------------------------------------------------------
# skin_skin_time
# la_dwell_time
# ablation_time
# ablation_time_hd
# hd_map_time
# number_of_rf_lesions_pvi
# additional_lesions_hd
# first_pass_rspv
# first_pass_ripv
# first_pass_lspv
# first_pass_lipv
# first_pass_per_patient

# select only procedural data columns
df_procedural <- df_all %>% select(
  skin_skin_time,
  la_dwell_time,
  ablation_time,
  number_of_rf_lesions_pvi,
  first_pass_rspv,
  first_pass_ripv,
  first_pass_lspv,
  first_pass_lipv,
  first_pass_per_patient,
  all_4_veins_isolated
)

# drop na
df_procedural <- drop_na(df_procedural)

# reconnected
reconnected <- as.numeric(!df_procedural$all_4_veins_isolated)

# remove target
df_procedural <- df_procedural %>% select(-all_4_veins_isolated)

# linear regression df_procedural vs reconnected
lm_demographic <- lm(reconnected ~ ., data = df_procedural)
summary(lm_demographic)
