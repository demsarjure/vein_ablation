library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv", sep = ";")

# subset
df_all <- df_all %>%
  select(procedure_type, number_of_isolated_veins)

# drop_na
df_all <- drop_na(df_all)

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# number_of_isolated_veins -----------------------------------------------------
# close 3.2 +/- 0.94
mean(df_close$number_of_isolated_veins)
sd(df_close$number_of_isolated_veins)

# high_density 3.65 +/- 0.75
mean(df_high_density$number_of_isolated_veins)
sd(df_high_density$number_of_isolated_veins)

# test p = 0.02
wilcox.test(
  df_close$number_of_isolated_veins,
  df_high_density$number_of_isolated_veins
)
