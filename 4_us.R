library(tidyverse)
library(readxl)


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")


# la_volume_index_12 vs la_volume_index ----------------------------------------
# la_volume_index_12 41.15 ± 10.73
mean(df_all$la_volume_index_12)
sd(df_all$la_volume_index_12)

# la_volume_index 39.48 ± 10.74
mean(df_all$la_volume_index)
sd(df_all$la_volume_index)

# 1.67 ± 9.16
diff <- df_all$la_volume_index_12 - df_all$la_volume_index
mean(diff)
sd(diff)

# paired wilcoxon between la_volume_index_12 and la_volume_index, p = 0.23
wilcox.test(df_all$la_volume_index_12, df_all$la_volume_index, paired = TRUE)
