library(readxl)
library(tidyverse)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# skin_skin_time2 --------------------------------------------------------------
close <- na.omit(as.numeric(df_close$skin_skin_time2))
report_mean_ci(close)
high_density <- na.omit(as.numeric(df_high_density$skin_skin_time2))
report_mean_ci(high_density)

wilcox.test(close, high_density)


# ablate_reisolization_time ----------------------------------------------------
close <- na.omit(as.numeric(df_close$ablate_reisolization_time))
report_mean_ci(close)
high_density <- na.omit(as.numeric(df_high_density$ablate_reisolization_time))
report_mean_ci(high_density)

wilcox.test(close, high_density)


# ablate_removal_time_dormant --------------------------------------------------
close <- na.omit(as.numeric(df_close$ablate_removal_time_dormant))
report_mean_ci(close)
high_density <- na.omit(as.numeric(df_high_density$ablate_removal_time_dormant))
report_mean_ci(high_density)

wilcox.test(close, high_density)


# rf_lesion_number_isolation ---------------------------------------------------
close <- na.omit(as.numeric(df_close$rf_lesion_number_isolation))
report_mean_ci(close)
high_density <- na.omit(as.numeric(df_high_density$rf_lesion_number_isolation))
report_mean_ci(high_density)

wilcox.test(close, high_density)


# rf_lesion_number_gap ---------------------------------------------------------
close <- na.omit(as.numeric(df_close$rf_lesion_number_gap))
report_mean_ci(close)
high_density <- na.omit(as.numeric(df_high_density$rf_lesion_number_gap))
report_mean_ci(high_density)

wilcox.test(close, high_density)
