library(readxl)
library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")


# skin_skin_time2 --------------------------------------------------------------
# close 83.97 ± 31.19
close <- na.omit(as.numeric(df_close$skin_skin_time2))
mean(close)
sd(close)

# high_density 72.04 ± 30.94
high_density <- na.omit(as.numeric(df_high_density$skin_skin_time2))
mean(high_density)
sd(high_density)

# test p = 0.15
wilcox.test(close, high_density)


# ablate_reisolization_time ----------------------------------------------------
# close 3.63 ± 2.7
close <- na.omit(as.numeric(df_close$ablate_reisolization_time))
mean(close)
sd(close)

# high_density 3.52 ± 2.09
high_density <- na.omit(as.numeric(df_high_density$ablate_reisolization_time))
mean(high_density)
sd(high_density)

# test p = 0.79
wilcox.test(close, high_density)


# ablate_removal_time_dormant --------------------------------------------------
# close 3.27 ± 2.07
close <- na.omit(as.numeric(df_close$ablate_removal_time_dormant))
mean(close)
sd(close)

# high_density 4 ± 2.82
high_density <- na.omit(as.numeric(df_high_density$ablate_removal_time_dormant))
mean(high_density)
sd(high_density)

# test p = 0.84
wilcox.test(close, high_density)

# rf_lesion_number_isolation ---------------------------------------------------
# close 13.5 ± 7.06
close <- na.omit(as.numeric(df_close$rf_lesion_number_isolation))
mean(close)
sd(close)

# high_density 10.5 ± 5.86
high_density <- na.omit(as.numeric(df_high_density$rf_lesion_number_isolation))
mean(high_density)
sd(high_density)

# test p = 0.39
wilcox.test(close, high_density)


# rf_lesion_number_gap ---------------------------------------------------------
# close 13.59 ± 7.5
close <- na.omit(as.numeric(df_close$rf_lesion_number_gap))
mean(close)
sd(close)

# high_density 10.14 ± 6.04
high_density <- na.omit(as.numeric(df_high_density$rf_lesion_number_gap))
mean(high_density)
sd(high_density)

# test p = 0.32
wilcox.test(close, high_density)

