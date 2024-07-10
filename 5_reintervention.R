library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv", sep = ";")

# was reintenvention required
df_all$reintervention <- ifelse(df_all$reintervention_date == "", 0, 1)

# split before NA removal
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")

# subset
df_all <- df_all %>%
  select(procedure_type, procedure_date, reintervention_date)

# to date, format is dd/mm/yyyy
df_all$procedure_date <- as.Date(df_all$procedure_date, format = "%d/%m/%Y")
df_all$reintervention_date <-
  as.Date(df_all$reintervention_date, format = "%d/%m/%Y")

# diff
df_all$diff <- df_all$reintervention_date - df_all$procedure_date

# drop missing dates
df_all <- drop_na(df_all)

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# 5 did not require reintervention ---------------------------------------------
# proprortions test between close and high_density for reintervention
prop.test(
  x = c(
    sum(df_close$reintervention == 1),
    sum(df_high_density$reintervention == 1)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)


# duration between procedure and reintervention ---------------------------------
# close 444.69 +/- 94.71
mean(df_close$diff)
sd(df_close$diff)

# high_density 381.69 +/- 103.71
mean(df_high_density$diff)
sd(df_high_density$diff)

# test p = 0.057
wilcox.test(as.numeric(df_close$diff), as.numeric(df_high_density$diff))
