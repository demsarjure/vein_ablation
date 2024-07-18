library(tidyverse)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv")
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


# RSPV -------------------------------------------------------------------------
# rspv_rr
rspv_rr <- na.omit(df_high_density$rspv_rr)
sum(rspv_rr)                   # 15 +/- 5.47
boot_sd_sum(rspv_rr)

sum(rspv_rr) / length(rspv_rr) # 50 +/- 17.92
boot_sd_prop(rspv_rr)

# rspv_ra
rspv_ra <- na.omit(df_high_density$rspv_ra)
sum(rspv_ra)                   # 14 +/- 5.42
boot_sd_sum(rspv_ra)

sum(rspv_ra) / length(rspv_ra) # 46.67 +/- 17.77
boot_sd_prop(rspv_ra)

# rspv_rp
rspv_rp <- na.omit(df_high_density$rspv_rp)
sum(rspv_rp)                   # 9 +- 4.07
boot_sd_sum(rspv_rp)

sum(rspv_rp) / length(rspv_rp) # 30 +/- 13.54
boot_sd_prop(rspv_rp)


# RIPV -------------------------------------------------------------------------
# ripv_ra
ripv_ra <- na.omit(df_high_density$ripv_ra)
sum(ripv_ra)                   # 2 +/- 1.4
boot_sd_sum(ripv_ra)

sum(ripv_ra) / length(ripv_ra) # 6.9 +/- 4.72
boot_sd_prop(ripv_ra)

# ripv_rp
ripv_rp <- na.omit(df_high_density$ripv_rp)
sum(ripv_rp)                   # 5 +/- 3.46
boot_sd_sum(ripv_rp)

sum(ripv_rp) / length(ripv_rp) # 16.67 +/- 11.59
boot_sd_prop(ripv_rp)

# ripv_ri
ripv_ri <- na.omit(df_high_density$ripv_ri)
sum(ripv_ri)                   # 1 +/- 0.98
boot_sd_sum(ripv_ri)

sum(ripv_ri) / length(ripv_ri) # 3.33 +/- 3.27
boot_sd_prop(ripv_ri)


# LSPV -------------------------------------------------------------------------
# lspv_lr
lspv_lr <- na.omit(df_high_density$lspv_lr)
sum(lspv_lr)                   # 2 +/- 1.37
boot_sd_sum(lspv_lr)

sum(lspv_lr) / length(lspv_lr) # 6.67 +/- 4.5
boot_sd_prop(lspv_lr)

# lspv_lrg
lspv_lrg <- na.omit(df_high_density$lspv_lrg)
sum(lspv_lrg)                    # 3 +/- 1.66
boot_sd_sum(lspv_lrg)

sum(lspv_lrg) / length(lspv_lrg) # 10 +/- 5.51
boot_sd_prop(lspv_lrg)

# lspv_lp
lspv_lp <- na.omit(df_high_density$lspv_lp)
sum(lspv_lp)                   # 4 +/- 1.87
boot_sd_sum(lspv_lp)

sum(lspv_lp) / length(lspv_lp) # 13.33 +/- 6.22
boot_sd_prop(lspv_lp)


# LIPV -------------------------------------------------------------------------
# lipv_la
lipv_la <- na.omit(df_high_density$lipv_la)
sum(lipv_la)                   # 6 +/- 4.3
boot_sd_sum(lipv_la)

sum(lipv_la) / length(lipv_la) # 20 +/- 14.35
boot_sd_prop(lipv_la)

# lipv_li
lipv_li <- na.omit(df_high_density$lipv_li)
sum(lipv_li)                   # 3 +/- 2.89
boot_sd_sum(lipv_li)

sum(lipv_li) / length(lipv_li) # 10.34 +/- 10.22
boot_sd_prop(lipv_li)

# lipv_lp
lipv_lp <- na.omit(df_high_density$lipv_lp)
sum(lipv_lp)                   # 4 +/- 3.92
boot_sd_sum(lipv_lp)

sum(lipv_lp) / length(lipv_lp) # 13.33 +/- 13.15
boot_sd_prop(lipv_lp)
