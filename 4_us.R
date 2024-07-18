library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv")
df_all <- drop_na(df_all)

# la_volume_index_12 vs  la_volume_index ---------------------------------------
# la_volume_index_12 41.87 +/- 11.45
mean(df_all$la_volume_index_12)
sd(df_all$la_volume_index_12)

# la_volume_index 39.74 +/- 10.4
mean(df_all$la_volume_index)
sd(df_all$la_volume_index)

# 2.1 +/- 9.4
diff <- df_all$la_volume_index_12 - df_all$la_volume_index
mean(diff)
sd(diff)

# paired wilcoxon between la_volume_index_12 and la_volume_index, p = 0.21
wilcox.test(df_all$la_volume_index_12, df_all$la_volume_index, paired = TRUE)
