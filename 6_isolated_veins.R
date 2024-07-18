library(tidyverse)
library(ggplot2)


# preprocessing ----------------------------------------------------------------
df_all <- read.csv("data/cleaned.csv")

# subset
df_all <- df_all %>%
  select(procedure_type, number_of_isolated_veins, all_4_veins_isolated)

# drop_na
df_all <- drop_na(df_all)

# percentage of isolated veins
df_all$percentage_of_isolated_veins <- df_all$number_of_isolated_veins / 4

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


# percentage of isolated veins -------------------------------------------------
# close 80.17 +/- 23.5
mean(df_close$percentage_of_isolated_veins)
sd(df_close$percentage_of_isolated_veins)

# high_density 91.35 +/- 18.63
mean(df_high_density$percentage_of_isolated_veins)
sd(df_high_density$percentage_of_isolated_veins)

# proportions test, p = 0.29
prop.test(
  x = c(sum(df_close$number_of_isolated_veins), sum (df_high_density$number_of_isolated_veins)),
  n = c(nrow(df_close) * 4, nrow(df_high_density) * 4)
)


# 4 isolated veins closed vs high density --------------------------------------
# close 44.83 +/- 9.22
sum(df_close$all_4_veins_isolated) / nrow(df_close)
boot_sd(df_close$all_4_veins_isolated)

# high_density 76.92 +/- 8.3
sum(df_high_density$all_4_veins_isolated) / nrow(df_high_density)
boot_sd(df_high_density$all_4_veins_isolated)

# proportions test, p = 0.03
prop.test(
  x = c(sum(df_close$all_4_veins_isolated), sum(df_high_density$all_4_veins_isolated)),
  n = c(nrow(df_close), nrow(df_high_density))
)


# plot number of isolated veins close vs high density --------------------------
# summarize mean + sd per group
df_cm_hd <- df_all %>%
  group_by(procedure_type) %>%
  summarize(
    mean = mean(number_of_isolated_veins),
    sd = sd(number_of_isolated_veins)
  )

# change high_density to high density
df_cm_hd$procedure_type <- c("close", "high density")

# plot per type
ggplot(df_cm_hd, aes(x = mean, y = procedure_type)) +
  geom_errorbarh(
    aes(xmin = mean - sd, xmax = 4),
    linewidth = 1,
    height = 0.2,
    color = "grey50"
  ) +
  geom_point(shape = 16, size = 3, color = "grey25") +
  xlim(0, 4) +
  ylab("") +
  xlab("number of isolated veins")

# save as a png
ggsave(
  "figs/isolated_veins.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px"
)


# plot counts of isolated veins per group --------------------------------------
# bootstrap 1000 times
df_cm_hd_sd <- data.frame(
  procedure_type = character(),
  number_of_isolated_veins = numeric(),
  n = numeric()
)
n_boot <- 1000
for (i in 1:n_boot) {
  boot_close <- sample(df_close$number_of_isolated_veins, replace = TRUE)
  boot_high_density <-
    sample(df_high_density$number_of_isolated_veins, replace = TRUE)

  # merge
  df_merged <- data.frame(
    procedure_type = c(rep("close", length(boot_close)), rep("high_density", length(boot_high_density))),
    number_of_isolated_veins = c(boot_close, boot_high_density)
  )

  # get mean
  df_cm_hd_sd_boot <- df_merged %>%
    group_by(procedure_type, number_of_isolated_veins) %>%
    summarize(n = n())

  # append to df_cm_hd_sd
  df_cm_hd_sd <- rbind(df_cm_hd_sd, df_cm_hd_sd_boot)
}

# get sd per group and number_isolated_veins
df_cm_hd_sd <- df_cm_hd_sd %>%
  group_by(procedure_type, number_of_isolated_veins) %>%
  summarize(sd = sd(n))

# counts
df_cm_hd_counts <- df_all %>%
  group_by(procedure_type, number_of_isolated_veins) %>%
  summarize(n = n())
df_cm_hd_counts$sd <- df_cm_hd_sd$sd
df_cm_hd_counts <- as.data.frame(df_cm_hd_counts)

# add missing rows
df_cm_hd_counts <- df_cm_hd_counts %>%
  complete(procedure_type, number_of_isolated_veins, fill = list(n = 0, sd = 0))

# plot per type per count
ggplot(
  df_cm_hd_counts,
  aes(x = number_of_isolated_veins, y = n, fill = procedure_type)
) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(
    position = position_dodge(0.8),
    aes(ymin = n - sd, ymax = n + sd),
    linewidth = 0.5,
    width = 0.5,
    color = "black"
  ) +
  scale_fill_manual(values = c("grey25", "grey75")) +
  ylab("patients") +
  xlab("number of isolated veins") +
  theme(legend.title = element_blank())

# save as a png
ggsave(
  "figs/isolated_veins_per_type.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px"
)
