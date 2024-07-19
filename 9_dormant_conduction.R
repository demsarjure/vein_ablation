library(tidyverse)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv")

# r
df_all$dormant_rspv_rr <- as.numeric(df_all$dormant_rspv_rr)
df_all$dormant_rspv_ra <- as.numeric(df_all$dormant_rspv_ra)
df_all$dormant_rspv_rp <- as.numeric(df_all$dormant_rspv_rp)
df_all$dormant_ripv_ra <- as.numeric(df_all$dormant_ripv_ra)
df_all$dormant_ripv_rp <- as.numeric(df_all$dormant_ripv_rp)
df_all$dormant_ripv_ri <- as.numeric(df_all$dormant_ripv_ri)

# l
df_all$dormant_lspv_lr <- as.numeric(df_all$dormant_lspv_lr)
df_all$dormant_lspv_lrg <- as.numeric(df_all$dormant_lspv_lrg)
df_all$dormant_lspv_lp <- as.numeric(df_all$dormant_lspv_lp)
df_all$dormant_lipv_la <- as.numeric(df_all$dormant_lipv_la)
df_all$dormant_lipv_li <- as.numeric(df_all$dormant_lipv_li)
df_all$dormant_lipv_lp <- as.numeric(df_all$dormant_lipv_lp)

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# rspv_rr ----------------------------------------------------------------------
# close: 6 +/- 2.15, high_density: 1 +/- 0.98
close <- na.omit(df_close$dormant_rspv_rr)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 21.43%


high_density <- na.omit(df_high_density$dormant_rspv_rr)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 3.85%

# test, p = 0.06
wilcox.test(close, high_density)


# rspv_ra ----------------------------------------------------------------------
# close: 3 +/- 1.65, high_density: 2 +/- 1.35
close <- na.omit(df_close$dormant_rspv_ra)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 10.71%

high_density <- na.omit(df_high_density$dormant_rspv_ra)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 7.69%

# test, p = 0.72
wilcox.test(close, high_density)


# rspv_rp ----------------------------------------------------------------------
# close: 4 +/- 1.85, high_density: 1 +/- 0.98
close <- na.omit(df_close$dormant_rspv_rp)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 14.29%

high_density <- na.omit(df_high_density$dormant_rspv_rp)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 3.85%

# test, p = 0.19
wilcox.test(close, high_density)


# ripv_ra ----------------------------------------------------------------------
# close: 0, high_density: 0
close <- na.omit(df_close$dormant_ripv_ra)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 0%

high_density <- na.omit(df_high_density$dormant_ripv_ra)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 0%

# test, p = 1
wilcox.test(close, high_density)


# ripv_rp ----------------------------------------------------------------------
# close: 4 +/- 1.86, high_density: 2 +/- 1.35
close <- na.omit(df_close$dormant_ripv_rp)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 14.29%

high_density <- na.omit(df_high_density$dormant_ripv_rp)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 7.69%

# test, p = 0.45
wilcox.test(close, high_density)


# ripv_ri ----------------------------------------------------------------------
# close: 0, high_density: 0
close <- na.omit(df_close$dormant_ripv_ri)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 0%

high_density <- na.omit(df_high_density$dormant_ripv_ri)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 0%

# test, p = 1
wilcox.test(close, high_density)


# lspv_lr ----------------------------------------------------------------------
# close: 1 +/- 0.98, high_density: 0
close <- na.omit(df_close$dormant_lspv_lr)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 3.57%

high_density <- na.omit(df_high_density$dormant_lspv_lr)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 0%

# test, p = 0.35
wilcox.test(close, high_density)


# lspv_lrg ---------------------------------------------------------------------
# close: 4 +/- 1.86, high_density: 1 +/- 0.98
close <- na.omit(df_close$dormant_lspv_lrg)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 14.29%

high_density <- na.omit(df_high_density$dormant_lspv_lrg)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 3.85%

# test, p = 0.19
wilcox.test(close, high_density)


# lspv_lp ----------------------------------------------------------------------
# close: 4 +/- 2.33, high_density: 0
close <- na.omit(df_close$dormant_lspv_lp)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 14.29%

high_density <- na.omit(df_high_density$dormant_lspv_lp)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 0%

# test, p = 0.09
wilcox.test(close, high_density)


# lipv_la ----------------------------------------------------------------------
# close: 3 +/- 1.62, high_density: 0
close <- na.omit(df_close$dormant_lipv_la)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 10.71%

high_density <- na.omit(df_high_density$dormant_lipv_la)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 0%

# test, p = 0.26
wilcox.test(close, high_density)


# lipv_li ----------------------------------------------------------------------
# close: 0, high_density: 0
close <- na.omit(df_close$dormant_lipv_li)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 0%

high_density <- na.omit(df_high_density$dormant_lipv_li)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) 

# test, p = 1
wilcox.test(close, high_density)


# lipv_lp ----------------------------------------------------------------------
# close: 3 +/- 1.64, high_density: 0
close <- na.omit(df_close$dormant_lipv_lp)
sum(close)
boot_sd_sum(close)
sum(close) / length(close) # 10.71%

high_density <- na.omit(df_high_density$dormant_lipv_lp)
sum(high_density)
boot_sd_sum(high_density)
sum(high_density) / length(high_density) # 0

# test, p = 0.09
wilcox.test(close, high_density)
