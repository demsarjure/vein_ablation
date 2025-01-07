library(ggplot2)
library(readxl)
library(tidyverse)

source("utils.R")


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
report_mean_ci(df_close$number_of_isolated_veins)
report_mean_ci(df_high_density$number_of_isolated_veins)

wilcox.test(
  df_close$number_of_isolated_veins,
  df_high_density$number_of_isolated_veins
)


# percentage of isolated veins -------------------------------------------------
report_mean_ci_prop(df_close$percentage_of_isolated_veins)
report_mean_ci_prop(df_high_density$percentage_of_isolated_veins)

prop.test(
  x = c(
    sum(df_close$number_of_isolated_veins),
    sum(df_high_density$number_of_isolated_veins)
  ),
  n = c(nrow(df_close) * 4, nrow(df_high_density) * 4)
)


# 4 isolated veins closed vs high density --------------------------------------
report_mean_ci_prop(df_close$all_4_veins_isolated)
report_mean_ci_prop(df_high_density$all_4_veins_isolated)

prop.test(
  x = c(
    sum(df_close$all_4_veins_isolated),
    sum(df_high_density$all_4_veins_isolated)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)


# plot number of isolated veins close vs high density --------------------------
# close 3.23 [2.9, 3.53]
# hd 3.69 [3.41, 3.93]
# summarize mean + sd per group
df_isolated <- data.frame(
  procedure_type = c("Close", "High density"),
  mean = c(3.23, 3.69),
  lower_ci = c(2.9, 3.41),
  upper_ci = c(3.53, 3.93)
)

# plot per type
ggplot(df_isolated, aes(x = mean, y = procedure_type)) +
  geom_errorbarh(
    aes(xmin = lower_ci, xmax = upper_ci),
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
df_isolated_boot <- data.frame(
  procedure_type = character(),
  number_of_isolated_veins = numeric(),
  n = numeric()
)

n_boot <- 10
for (i in 1:n_boot) {
  # close
  boot_close <- sample(df_close$number_of_isolated_veins, replace = TRUE)

  # count distinct values in boot_close
  df_close_temp <- data.frame(
    number_of_isolated_veins = boot_close
  ) %>%
    group_by(number_of_isolated_veins) %>%
    summarize(n = n())

  # add empty rows for number_of_isolated_veins from 0 to 4 that are missing
  df_close_temp <- as.data.frame(df_close_temp)
  df_close_temp$number_of_isolated_veins <-
    factor(df_close_temp$number_of_isolated_veins, levels = 0:4)
  df_close_temp <- df_close_temp %>%
    complete(
      number_of_isolated_veins,
      fill = list(n = 0)
    )

  # hd
  boot_high_density <-
    sample(df_high_density$number_of_isolated_veins, replace = TRUE)

  # count distinct values in boot_close
  df_high_density_temp <- data.frame(
    number_of_isolated_veins = boot_high_density
  ) %>%
    group_by(number_of_isolated_veins) %>%
    summarize(n = n())

  # add empty rows for number_of_isolated_veins from 0 to 4 that are missing
  df_high_density_temp <- as.data.frame(df_high_density_temp)
  df_high_density_temp$number_of_isolated_veins <-
    factor(df_high_density_temp$number_of_isolated_veins, levels = 0:4)
  df_high_density_temp <- df_high_density_temp %>%
    complete(
      number_of_isolated_veins,
      fill = list(n = 0)
    )

  # merge
  df_isolated_boot <- rbind(
    df_isolated_boot,
    data.frame(
      procedure_type = "close",
      number_of_isolated_veins = df_close_temp$number_of_isolated_veins,
      n = df_close_temp$n
    ),
    data.frame(
      procedure_type = "high_density",
      number_of_isolated_veins = df_high_density_temp$number_of_isolated_veins,
      n = df_high_density_temp$n
    )
  )
}

# get CI per group and number_isolated_veins
df_isolated_ci <- df_isolated_boot %>%
  group_by(procedure_type, number_of_isolated_veins) %>%
  summarize(lower_ci = quantile(n, 0.025), upper_ci = quantile(n, 0.975))

# counts
df_isolated_counts <- df_all %>%
  group_by(procedure_type, number_of_isolated_veins) %>%
  summarize(n = n())

# add missing rows
df_isolated_counts <- as.data.frame(df_isolated_counts)
df_isolated_counts <- df_isolated_counts %>%
  complete(
    procedure_type,
    number_of_isolated_veins,
    fill = list(n = 0, lower_ci = 0, upper_ci = 0)
  )

# add CI
df_isolated <- df_isolated_counts
df_isolated$lower_ci <- df_isolated_ci$lower_ci
df_isolated$upper_ci <- df_isolated_ci$upper_ci

# labels, replace close with Close, high_density with High density
df_isolated$procedure_type[df_isolated$procedure_type == "close"] <- "Close"
df_isolated$procedure_type[df_isolated$procedure_type == "high_density"] <- "High density"

# calculate p-values
df_p_values <- NULL
for (i in 0:4) {
  df_n <- df_isolated %>%
    filter(number_of_isolated_veins == i)
  close_isolated <- (df_n %>% filter(procedure_type == "Close"))$n
  hd_isolated <- (df_n %>% filter(procedure_type == "High density"))$n

  # create vectors
  p <- prop.test(
    x = c(close_isolated, hd_isolated),
    n = c(nrow(df_close), nrow(df_high_density))
  )$p.value

  # round
  p <- round(p, 2)

  df_p_values <-
    rbind(df_p_values, data.frame(number_of_isolated_veins = i, p_value = p))
}

# max value for each number of isolated veins
df_max <- df_isolated %>%
  group_by(number_of_isolated_veins) %>%
  summarize(max_y = max(upper_ci) + 0.5)

# add max to p_values
df_p_values <- merge(df_p_values, df_max, by = "number_of_isolated_veins")

# change procedure_type Close to CM group, High density to HD group
df_isolated$procedure_type[df_isolated$procedure_type == "Close"] <- "CM group"
df_isolated$procedure_type[df_isolated$procedure_type == "High density"] <- "HD group"

# plot per type per count
ggplot(
  df_isolated,
  aes(x = number_of_isolated_veins, y = n, fill = procedure_type)
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    width = 0.8
  ) +
  geom_errorbar(
    position = position_dodge(0.8),
    aes(ymin = lower_ci, ymax = upper_ci),
    linewidth = 0.5,
    width = 0.5,
    color = "black"
  ) +
  geom_text(
    data = df_p_values,
    aes(
      x = number_of_isolated_veins,
      y = max_y,
      label = paste0("p = ", p_value),
      fill = NULL
    ),
    position = position_dodge(0.8),
    vjust = -0.5,
    size = 2.5
  ) +
  scale_fill_manual(values = c("grey25", "grey75")) +
  ylab("Patients") +
  xlab("Number of durably isolated pulmonary veins") +
  theme_minimal() +
  theme(legend.title = element_blank())

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
