library(readxl)
library(tidyverse)
library(survival)
library(survminer)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")

# subset
df_all <- df_all %>%
  select(
    procedure_type,
    procedure_date,
    recidiv,
    number_of_isolated_veins,
    dormant_rspv_rr,
    dormant_rspv_ra,
    dormant_rspv_rp,
    dormant_ripv_ra,
    dormant_ripv_rp,
    dormant_ripv_ri,
    dormant_lspv_lr,
    dormant_lspv_lrg,
    dormant_lspv_lp,
    dormant_lipv_la,
    dormant_lipv_li,
    dormant_lipv_lp
  )

# recidiv true/false
df_all$had_recidiv <- ifelse(is.na(df_all$recidiv), 0, 1)

# reconnected veins
df_all$number_of_reconnected_veins <- 4 - df_all$number_of_isolated_veins

# to date, format is dd/mm/yyyy
df_all$procedure_date <- as.Date(df_all$procedure_date, format = "%d/%m/%Y")
df_all$recidiv <-
  as.Date(df_all$recidiv, format = "%d/%m/%Y")

# sort out the type
df_all$procedure_type <- factor(df_all$procedure_type)

# split
df_close <- df_all %>% filter(procedure_type == "close")
df_high_density <- df_all %>% filter(procedure_type == "high_density")

# only those that had recidiv
df_recidiv <- drop_na(df_all)

# diff
df_recidiv$diff <- df_recidiv$recidiv - df_recidiv$procedure_date

# split
df_recidiv_close <- df_recidiv %>%
  filter(procedure_type == "close")
df_recidiv_high_density <- df_recidiv %>%
  filter(procedure_type == "high_density")


# percentage/number of recidivs between groups ---------------------------------
report_sum_ci(df_close$had_recidiv)
report_sum_ci(df_high_density$had_recidiv)
wilcox.test(df_close$had_recidiv, df_high_density$had_recidiv)

report_prop_ci(df_close$had_recidiv)
report_prop_ci(df_high_density$had_recidiv)
prop.test(
  x = c(
    sum(df_close$had_recidiv),
    sum(df_high_density$had_recidiv)
  ),
  n = c(nrow(df_close), nrow(df_high_density))
)


# duration between the procedure and the recidiv -------------------------------
report_mean_ci(df_recidiv_close$diff)
report_mean_ci(df_recidiv_high_density$diff)


# survival analysis ------------------------------------------------------------
# create the survival object
df_recidiv$status <- rep(1, nrow(df_recidiv))
surv_obj <- Surv(time = df_recidiv$diff, event = df_recidiv$status)

# fit the Kaplan-Meier model
km_fit <- survfit(surv_obj ~ procedure_type, data = df_recidiv)

# plot the Kaplan-Meier survival curves
surv_plot <- ggsurvplot(km_fit,
  data = df_recidiv,
  pval = TRUE,
  conf.int = TRUE,
  legend.title = "",
  legend.labs = c("CM group", "HD group"),
  xlab = "Time (days)",
  ylab = "Freedom from arrhythmia recurrence (%)",
  fun = "event"
)

# extract ggplot and flip the y-axis
surv_plot$plot +
  scale_y_reverse(
    breaks = c(0, 0.25, 0.5, 0.75, 1.00),
    labels = c(100, 75, 50, 25, 0)
  )

# save as a png
ggsave(
  "figs/km_recidiv.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px",
  bg = "white"
)


# KM for all -------------------------------------------------------------------
df_all$diff <- df_all$recidiv - df_all$procedure_date

df_km_all <- df_all %>%
  select(procedure_type, diff)

df_km_all$event <- ifelse(is.na(df_km_all$diff), 0, 1)

# use max time + 1 for censored cases
df_km_all$diff[is.na(df_km_all$diff)] <- max(df_km_all$diff, na.rm = TRUE) + 1

surv_obj_all <- Surv(time = df_km_all$diff, event = df_km_all$event)
km_all_fit <- survfit(surv_obj_all ~ procedure_type, data = df_km_all)

surv_plot_all <- ggsurvplot(
  km_all_fit,
  data = df,
  pval = TRUE,
  conf.int = TRUE,
  legend.title = "",
  legend.labs = c("CM group", "HD group"),
  xlab = "Time (days)",
  ylab = "Freedom from arrhythmia recurrence (%)",
  fun = "event"
)

# extract ggplot and flip the y-axis
surv_plot_all$plot +
  scale_y_reverse(
    breaks = c(0, 0.25, 0.5, 0.75, 1.00),
    labels = c(100, 75, 50, 25, 0),
    limits = c(1, 0)
  )

# save as a png
ggsave(
  "figs/km_all.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px",
  bg = "white"
)


# plot survivability through time ----------------------------------------------
n_close <- nrow(df_close)
n_high_density <- nrow(df_high_density)
n_recidiv <- nrow(df_recidiv)

# sort df recidiv by diff
df_recidiv <- df_recidiv[order(df_recidiv$diff), ]

df_survivability <- data.frame(
  time = c(0, 0),
  surv = c(100, 100),
  group = c("close", "high_density")
)

previous_close <- 100
previous_high_density <- 100
close_count <- 0
high_density_count <- 0
for (i in seq_len(n_recidiv)) {
  day <- df_recidiv$diff[i]
  group <- df_recidiv$procedure_type[i]

  if (group == "close") {
    close_count <- close_count + 1
    surv <- 100 - 100 * (close_count / n_close)
  } else {
    high_density_count <- high_density_count + 1
    surv <- 100 - 100 * (high_density_count / n_high_density)
  }

  if (group == "close") {
    df_survivability <- df_survivability %>%
      add_row(
        data.frame(
          time = as.numeric(day),
          surv = previous_close,
          group = group
        )
      )
  }
  else if (group == "high_density") {
    df_survivability <- df_survivability %>%
      add_row(
        data.frame(
          time = as.numeric(day),
          surv = previous_high_density,
          group = group
        )
      )
  }

  df_survivability <- df_survivability %>%
    add_row(
      data.frame(
        time = as.numeric(day),
        surv = surv,
        group = group
      )
    )

  if (group == "close") {
    previous_close <- surv
  } else {
    previous_high_density <- surv
  }
}

df_survivability <- df_survivability %>%
  add_row(
    data.frame(
      time = max(df_survivability$time),
      surv =
        min(df_survivability$surv[df_survivability$group == "high_density"]),
      group = "high_density"
    )
  )

# replace
df_survivability$group[df_survivability$group == "close"] <-
  "CM group"
df_survivability$group[df_survivability$group == "high_density"] <-
  "HD group"

# plot
ggplot(df_survivability, aes(x = time, y = surv, color = group)) +
  geom_line(linewidth = 1) +
  labs(x = "Time (days)", y = "Freedom from arrhythmia recurrence (%)") +
  scale_color_manual(values = c("grey25", "grey75")) +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text", x = 350, y = 90, label = "p = 0.56", size = 5)

# save as a png
ggsave(
  "figs/recidiv.png",
  width = 1920,
  height = 1080,
  dpi = 300,
  units = "px",
  bg = "white"
)


# is recidiv correlated with the number of reconnected veins -------------------
df_reconnected <- df_all %>%
  select(procedure_type, number_of_reconnected_veins, had_recidiv)
df_reconnected <- drop_na(df_reconnected)
df_recidiv_reconnected <- df_reconnected %>% filter(had_recidiv == 1)
df_no_recidiv_reconnected <- df_reconnected %>% filter(had_recidiv == 0)

recidiv <- df_recidiv_reconnected$number_of_reconnected_veins
report_mean_ci(recidiv)

no_recidiv <- df_no_recidiv_reconnected$number_of_reconnected_veins
report_mean_ci(no_recidiv)

wilcox.test(recidiv, no_recidiv)


# is recidiv correlated with the number of dormants ----------------------------
df_dormant <- df_all %>%
  select(
    procedure_type,
    had_recidiv,
    dormant_rspv_rr,
    dormant_rspv_ra,
    dormant_rspv_rp,
    dormant_ripv_ra,
    dormant_ripv_rp,
    dormant_ripv_ri,
    dormant_lspv_lr,
    dormant_lspv_lrg,
    dormant_lspv_lp,
    dormant_lipv_la,
    dormant_lipv_li,
    dormant_lipv_lp
  )

df_dormant <- drop_na(df_dormant)
df_recidiv_dormant <- df_dormant %>% filter(had_recidiv == 1)
df_no_recidiv_dormant <- df_dormant %>% filter(had_recidiv == 0)

recidiv <- c(
  df_recidiv_dormant$dormant_rspv_rr,
  df_recidiv_dormant$dormant_rspv_ra,
  df_recidiv_dormant$dormant_rspv_rp,
  df_recidiv_dormant$dormant_ripv_ra,
  df_recidiv_dormant$dormant_ripv_rp,
  df_recidiv_dormant$dormant_ripv_ri,
  df_recidiv_dormant$dormant_lspv_lr,
  df_recidiv_dormant$dormant_lspv_lrg,
  df_recidiv_dormant$dormant_lspv_lp,
  df_recidiv_dormant$dormant_lipv_la,
  df_recidiv_dormant$dormant_lipv_li,
  df_recidiv_dormant$dormant_lipv_lp
)
report_mean_ci(recidiv)

no_recidiv <- c(
  df_no_recidiv_dormant$dormant_rspv_rr,
  df_no_recidiv_dormant$dormant_rspv_ra,
  df_no_recidiv_dormant$dormant_rspv_rp,
  df_no_recidiv_dormant$dormant_ripv_ra,
  df_no_recidiv_dormant$dormant_ripv_rp,
  df_no_recidiv_dormant$dormant_ripv_ri,
  df_no_recidiv_dormant$dormant_lspv_lr,
  df_no_recidiv_dormant$dormant_lspv_lrg,
  df_no_recidiv_dormant$dormant_lspv_lp,
  df_no_recidiv_dormant$dormant_lipv_la,
  df_no_recidiv_dormant$dormant_lipv_li,
  df_no_recidiv_dormant$dormant_lipv_lp
)
report_mean_ci(no_recidiv)

wilcox.test(recidiv, no_recidiv)
