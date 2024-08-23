library(ggplot2)
library(readxl)
library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

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
# close 3.23 ± 0.94
mean(df_close$number_of_isolated_veins)
sd(df_close$number_of_isolated_veins)

# high_density 3.69 ± 0.71
mean(df_high_density$number_of_isolated_veins)
sd(df_high_density$number_of_isolated_veins)

# test p = 0.01
wilcox.test(
  df_close$number_of_isolated_veins,
  df_high_density$number_of_isolated_veins
)


# percentage of isolated veins -------------------------------------------------
# close 80.83 ± 23.38%
mean(df_close$percentage_of_isolated_veins)
sd(df_close$percentage_of_isolated_veins)

# high_density 92.24 ± 17.81%
mean(df_high_density$percentage_of_isolated_veins)
sd(df_high_density$percentage_of_isolated_veins)

# proportions test, p = 0.02
prop.test(
  x = c(
    sum(df_close$number_of_isolated_veins),
    sum(df_high_density$number_of_isolated_veins)
  ),
  n = c(nrow(df_close) * 4, nrow(df_high_density) * 4)
)


# 4 isolated veins closed vs high density --------------------------------------
# close 46.67 ± 9.24%
sum(df_close$all_4_veins_isolated) / nrow(df_close)
boot_sd(df_close$all_4_veins_isolated)

# high_density 79.31 ± 7.57%
sum(df_high_density$all_4_veins_isolated) / nrow(df_high_density)
boot_sd(df_high_density$all_4_veins_isolated)

# proportions test, p = 0.02
prop.test(
  x = c(
    sum(df_close$all_4_veins_isolated),
    sum(df_high_density$all_4_veins_isolated)
  ),
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

# labels
df_cm_hd$procedure_type <- c("Close", "High density")

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
  xlab("Average number of durably isolated pulmonary veins") +
  theme_minimal()

# save as a png
ggsave(
  "figs/isolated_veins.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px",
  bg = "white"
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
    procedure_type = c(
      rep("close", length(boot_close)),
      rep("high_density", length(boot_high_density))
    ),
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

# labels, replace close with Close, high_density with High density
df_cm_hd_counts$procedure_type[df_cm_hd_counts$procedure_type == "close"] <- "Close"
df_cm_hd_counts$procedure_type[df_cm_hd_counts$procedure_type == "high_density"] <- "High density"

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
  ylab("Patients") +
  xlab("Number of durably isolated pulmonary veins") +
  theme(legend.title = element_blank()) +
  theme_minimal()

# save as a png
ggsave(
  "figs/isolated_veins_per_type.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px",
  bg = "white"
)


# durable isolation plot -------------------------------------------------------
# sum per group
df_veins <- df_cm_hd_counts %>%
  group_by(procedure_type) %>%
  summarize(veins = sum(n * number_of_isolated_veins))

# labels
df_veins$procedure_type[df_veins$procedure_type == "Close"] <- "CM group"
df_veins$procedure_type[df_veins$procedure_type == "High density"] <- "HD group"

# plot
ggplot(df_veins, aes(x = procedure_type, y = veins)) +
  geom_bar(stat = "identity", fill = c("grey25", "grey75")) +
  geom_text(aes(label = veins), vjust = -0.5) +
  ylab("Pulmonary veins") +
  xlab("") +
  ylim(0, 115) +
  theme_minimal()

# save as a png
ggsave(
  "figs/isolated_veins_sum.png",
  width = 720,
  height = 1080,
  dpi = 300,
  units = "px",
  bg = "white"
)
