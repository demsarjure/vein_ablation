library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv", sep = ";")
df_high_density <- df_all %>% filter(procedure_type == "high_density")

# r
df_high_density$rspv_rr <- as.numeric(df_high_density$rspv_rr)
df_high_density$rspv_ra <- as.numeric(df_high_density$rspv_ra)
df_high_density$rspv_rp <- as.numeric(df_high_density$rspv_rp)
df_high_density$ripv_ra <- as.numeric(df_high_density$ripv_ra)
df_high_density$ripv_rp <- as.numeric(df_high_density$ripv_rp)
df_high_density$ripv_ri <- as.numeric(df_high_density$ripv_ri)

# l
df_high_density$lspv_lr <- as.numeric(df_high_density$lspv_lr)
df_high_density$lspv_lrg <- as.numeric(df_high_density$lspv_lrg)
df_high_density$lspv_lp <- as.numeric(df_high_density$lspv_lp)
df_high_density$lipv_la <- as.numeric(df_high_density$lipv_la)
df_high_density$lipv_li <- as.numeric(df_high_density$lipv_li)
df_high_density$lipv_lp <- as.numeric(df_high_density$lipv_lp)

# set NA to 0
df_high_density$ripv_ra <- ifelse(is.na(df_high_density$ripv_ra), 0, df_high_density$ripv_ra)
df_high_density$lipv_li <- ifelse(is.na(df_high_density$lipv_li), 0, df_high_density$lipv_li)

# RSPV -------------------------------------------------------------------------
sum(df_high_density$rspv_rr)  # 15 - 1
sum(df_high_density$rspv_ra)  # 14 - 0.93
sum(df_high_density$rspv_rp)  # 9 - 0.6

# RIPV -------------------------------------------------------------------------
sum(df_high_density$ripv_ra)  # 2 - 0.13
sum(df_high_density$ripv_rp)  # 5 - 0.33
sum(df_high_density$ripv_ri)  # 1 - 0.07

# LSPV -------------------------------------------------------------------------
sum(df_high_density$lspv_lr)  # 2 - 0.13
sum(df_high_density$lspv_lrg) # 3 - 0.2
sum(df_high_density$lspv_lp)  # 4 - 0.27

# LIPV -------------------------------------------------------------------------
sum(df_high_density$lipv_la)  # 6 - 0.4
sum(df_high_density$lipv_li)  # 3 - 0.2
sum(df_high_density$lipv_lp)  # 4 - 0.27
