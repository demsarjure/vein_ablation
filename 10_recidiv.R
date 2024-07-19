library(tidyverse)
library(survival)
library(survminer)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv")

# subset
df_all <- df_all %>%
  select(procedure_type, procedure_date, recidiv, number_of_isolated_veins)

# recidiv true/false
df_all$had_recidiv <- ifelse(df_all$recidiv == "", 0, 1)

# reconnected veins
df_all$number_of_reconnected_veins <- 4 - df_all$number_of_isolated_veins

# to date, format is dd/mm/yyyy
df_all$procedure_date <- as.Date(df_all$procedure_date, format = "%d/%m/%Y")
df_all$recidiv <-
  as.Date(df_all$recidiv, format = "%d/%m/%Y")

# only those that had recidiv
df_recidiv <- drop_na(df_all)

# diff
df_recidiv$diff <- df_recidiv$recidiv - df_recidiv$procedure_date

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")

df_recidiv_close <- df_recidiv %>% filter(procedure_type == "close")
df_recidiv_high_density <- df_recidiv %>% filter(procedure_type == "high_density")


# percentage/number of recidivs between groups ---------------------------------
# close: 10 +/- 2.57, high_density: 7 +/- 2.32
sum(df_close$recidiv)
boot_sd_sum(df_close$recidiv)

sum(df_high_density$recidiv)
boot_sd_sum(df_high_density$recidiv)

# test p = 0.4
wilcox.test(df_close$recidiv, df_high_density$recidiv)

# close: 33.33 +/- 8.49, high_density: 23.33 +/- 7.67
sum(df_close$recidiv) / nrow(df_close)
boot_sd(df_close$recidiv)

sum(df_close$recidiv) / nrow(df_high_density)
boot_sd(df_close$recidiv)

# proportions test, p = 0.5
prop.test(
  x = c(
    sum(df_close$recidiv),
    sum(df_close$recidiv)
  ),
  n = c(nrow(df_close), nrow(df_close))
)


# duration between the procedure and the recidiv -------------------------------
# close 224.6 +/- 101.14
mean(df_recidiv_close$diff)
sd(df_recidiv_close$diff)

# high_density 149.29 +/- 69.37
mean(df_recidiv_high_density$diff)
sd(df_recidiv_high_density$diff)


# survival analysis ------------------------------------------------------------
# create the survival object
df_recidiv$procedure_type <- factor(df_recidiv$procedure_type)
df_recidiv$status <- rep(1, nrow(df_recidiv))
surv_obj <- Surv(time = df_recidiv$diff, event = df_recidiv$status)

# fit the Kaplan-Meier model
km_fit <- survfit(surv_obj ~ procedure_type, data = df_recidiv)

# plot the Kaplan-Meier survival curves
ggsurvplot(km_fit,
  data = df_recidiv, pval = TRUE,
  conf.int = TRUE,
  legend.title = "",
  legend.labs = c("close", "high density"),
  xlab = "time (days)",
  ylab = "survival probability",
)

# save as a png
ggsave(
  "figs/recidiv.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px"
)


# is recidiv positively correlated with the number of reconnected veins --------
df_reconnected <- df_all %>% 
  select(c("procedure_type", "number_of_reconnected_veins", "had_recidiv"))
df_reconnected <- drop_na(df_reconnected)
df_recidiv_reconnected <- df_reconnected %>% filter(had_recidiv == 1)
df_no_recidiv_reconnected <- df_reconnected %>% filter(had_recidiv == 0)

# close: 23 +/- 5.05, high_density: 9 +/- 0.98
recidiv <- df_recidiv_reconnected$number_of_reconnected_veins
mean(recidiv)
sd(recidiv)

no_recidiv <- df_no_recidiv_reconnected$number_of_reconnected_veins
mean(no_recidiv)
sd(no_recidiv)

# test p = 0.4
wilcox.test(recidiv, no_recidiv)
