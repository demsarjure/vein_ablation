library(readxl)
library(tidyverse)
library(survival)
library(survminer)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# subset
df_all <- df_all %>%
  select(procedure_type, procedure_date, recidiv, number_of_isolated_veins, dormant_rspv_rr, dormant_rspv_ra, dormant_rspv_rp, dormant_ripv_ra, dormant_ripv_rp, dormant_ripv_ri, dormant_lspv_lr, dormant_lspv_lrg, dormant_lspv_lp, dormant_lipv_la, dormant_lipv_li, dormant_lipv_lp)

# recidiv true/false
df_all$had_recidiv <- ifelse(is.na(df_all$recidiv), 0, 1)

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
# close: 10 ± 2.57, high_density: 7 ± 2.32
sum(df_close$had_recidiv)
boot_sd_sum(df_close$had_recidiv)

sum(df_high_density$had_recidiv)
boot_sd_sum(df_high_density$had_recidiv)

# test p = 0.4
wilcox.test(df_close$had_recidiv, df_high_density$had_recidiv)

# close: 33.33 ± 8.49, high_density: 23.33 ± 7.67
sum(df_close$had_recidiv) / nrow(df_close)
boot_sd(df_close$had_recidiv)

sum(df_high_density$had_recidiv) / nrow(df_high_density)
boot_sd(df_close$had_recidiv)

# proportions test, p = 0.5
prop.test(
  x = c(
    sum(df_close$had_recidiv),
    sum(df_high_density$had_recidiv)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)


# duration between the procedure and the recidiv -------------------------------
# close 224.6 ± 101.14
mean(df_recidiv_close$diff)
sd(df_recidiv_close$diff)

# high_density 149.29 ± 69.37
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
  ylab = "recidiv probability",
  fun = "event"
)

# save as a png
ggsave(
  "figs/recidiv.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px"
)


# is recidiv correlated with the number of reconnected veins -------------------
df_reconnected <- df_all %>% 
  select(procedure_type, number_of_reconnected_veins, had_recidiv))
df_reconnected <- drop_na(df_reconnected)
df_recidiv_reconnected <- df_reconnected %>% filter(had_recidiv == 1)
df_no_recidiv_reconnected <- df_reconnected %>% filter(had_recidiv == 0)

# close: 23 ± 5.05, high_density: 9 ± 0.98
recidiv <- df_recidiv_reconnected$number_of_reconnected_veins
mean(recidiv)
sd(recidiv)

no_recidiv <- df_no_recidiv_reconnected$number_of_reconnected_veins
mean(no_recidiv)
sd(no_recidiv)

# test p = 0.4
wilcox.test(recidiv, no_recidiv)


# is recidiv correlated with the number of dormants ----------------------------
df_dormant <- df_all %>% 
  select(procedure_type, had_recidiv, dormant_rspv_rr, dormant_rspv_ra, dormant_rspv_rp, dormant_ripv_ra, dormant_ripv_rp, dormant_ripv_ri, dormant_lspv_lr, dormant_lspv_lrg, dormant_lspv_lp, dormant_lipv_la, dormant_lipv_li, dormant_lipv_lp)

df_dormant <- drop_na(df_dormant)
df_recidiv_dormant <- df_dormant %>% filter(had_recidiv == 1)
df_no_recidiv_dormant <- df_dormant %>% filter(had_recidiv == 0)

# close: 23 ± 5.05, high_density: 9 ± 0.98
recidiv <- c(
  df_recidiv_dormant$dormant_rspv_rr,
  df_recidiv_dormant$dormant_rspv_ra,
  df_recidiv_dormant$dormant_rspv_rp,
  df_recidiv_dormant$dormant_ripv_ra,
  df_recidiv_dormant$dormant_ripv_rp,
  df_recidiv_dormant$dormant_ripv_ri,
  df_recidiv_dormant$dormant_lspv_lr,
  df_recidiv_dormant$dormant_lspv_lrg,
  df_recidiv_dormant$dormant_lspv_lp,
  df_recidiv_dormant$dormant_lipv_la,
  df_recidiv_dormant$dormant_lipv_li,
  df_recidiv_dormant$dormant_lipv_lp
)
mean(recidiv)
sd(recidiv)

no_recidiv <- c(
  df_no_recidiv_dormant$dormant_rspv_rr,
  df_no_recidiv_dormant$dormant_rspv_ra,
  df_no_recidiv_dormant$dormant_rspv_rp,
  df_no_recidiv_dormant$dormant_ripv_ra,
  df_no_recidiv_dormant$dormant_ripv_rp,
  df_no_recidiv_dormant$dormant_ripv_ri,
  df_no_recidiv_dormant$dormant_lspv_lr,
  df_no_recidiv_dormant$dormant_lspv_lrg,
  df_no_recidiv_dormant$dormant_lspv_lp,
  df_no_recidiv_dormant$dormant_lipv_la,
  df_no_recidiv_dormant$dormant_lipv_li,
  df_no_recidiv_dormant$dormant_lipv_lp
)
mean(no_recidiv)
sd(no_recidiv)

# test p = 0.4
wilcox.test(recidiv, no_recidiv)
