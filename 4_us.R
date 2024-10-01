library(tidyverse)
library(readxl)


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")


# la_volume_index_12 vs la_volume_index ----------------------------------------
report_mean_ci(df_all$la_volume_index_12)

report_mean_ci(df_all$la_volume_index)

diff <- df_all$la_volume_index_12 - df_all$la_volume_index
report_mean_ci(diff)

wilcox.test(df_all$la_volume_index_12, df_all$la_volume_index, paired = TRUE)
