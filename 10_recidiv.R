library(tidyverse)
library(survival)
library(survminer)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv", sep = ";")

# subset
df_all <- df_all %>%
  select(procedure_type, procedure_date, recidiv)

# to date, format is dd/mm/yyyy
df_all$procedure_date <- as.Date(df_all$procedure_date, format = "%d/%m/%Y")
df_all$recidiv <-
  as.Date(df_all$recidiv, format = "%d/%m/%Y")

# drop missing dates
df_all <- drop_na(df_all)

# diff
df_all$diff <- df_all$recidiv - df_all$procedure_date

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# duration between the procedure and the recidiv -------------------------------
# close 224.6 +/- 101.14
mean(df_close$diff)
sd(df_close$diff)

# high_density 149.29 +/- 69.37
mean(df_high_density$diff)
sd(df_high_density$diff)


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
  legend.labs = c("close", "high density")
)

# save as a png
ggsave(
  "figs/recidiv.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px"
)
