library(tidyverse)
library(readxl)
library(survival)
library(survminer)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# subset
df_all <- df_all %>%
  select(procedure_type, procedure_date, reintervention_date)

# to date, format is dd/mm/yyyy
df_all$procedure_date <- as.Date(df_all$procedure_date, format = "%d/%m/%Y")
df_all$reintervention_date <-
  as.Date(df_all$reintervention_date, format = "%d/%m/%Y")

# drop missing dates
df_all <- drop_na(df_all)

# diff
df_all$diff <- df_all$reintervention_date - df_all$procedure_date

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# summary statistics -----------------------------------------------------------
report_mean_ci(df_all$diff)
report_mean_ci(df_high_density$diff)

wilcox.test(as.numeric(df_close$diff), as.numeric(df_high_density$diff))


# survival analysis ------------------------------------------------------------
# create the survival object
df_all$procedure_type <- factor(df_all$procedure_type)
df_all$status <- rep(1, nrow(df_all))
surv_obj <- Surv(time = df_all$diff, event = df_all$status)

# fit the Kaplan-Meier model
km_fit <- survfit(surv_obj ~ procedure_type, data = df_all)

# plot the Kaplan-Meier survival curves
ggsurvplot(km_fit,
  data = df_all, pval = TRUE,
  conf.int = TRUE,
  legend.labs = c("Close", "High density")
)
