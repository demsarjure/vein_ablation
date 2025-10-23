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

# number of dormants
df_all$number_of_dormants <- rowSums(
  df_all[, c(
    "dormant_rspv_rr",
    "dormant_rspv_ra",
    "dormant_rspv_rp",
    "dormant_ripv_ra",
    "dormant_ripv_rp",
    "dormant_ripv_ri",
    "dormant_lspv_lr",
    "dormant_lspv_lrg",
    "dormant_lspv_lp",
    "dormant_lipv_la",
    "dormant_lipv_li",
    "dormant_lipv_lp"
  )],
  na.rm = TRUE
)

# drop indvidual dormant columns
df_all <- df_all %>%
  select(
    -c(
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
  )

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


# KM ---------------------------------------------------------------------------
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
  data = df_km_all,
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
  group = c("close", "high_density"),
  n_patients = c(n_close, n_high_density)
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
    n_patients <- n_close - close_count
  } else {
    high_density_count <- high_density_count + 1
    surv <- 100 - 100 * (high_density_count / n_high_density)
    n_patients <- n_high_density - high_density_count
  }

  if (group == "close") {
    df_survivability <- df_survivability %>%
      add_row(
        data.frame(
          time = as.numeric(day),
          surv = previous_close,
          group = group,
          n_patients = n_close - close_count + 1
        )
      )
  } else if (group == "high_density") {
    df_survivability <- df_survivability %>%
      add_row(
        data.frame(
          time = as.numeric(day),
          surv = previous_high_density,
          group = group,
          n_patients = n_high_density - high_density_count + 1
        )
      )
  }

  df_survivability <- df_survivability %>%
    add_row(
      data.frame(
        time = as.numeric(day),
        surv = surv,
        group = group,
        n_patients = n_patients
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
      group = "high_density",
      n_patients = n_high_density - high_density_count
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

# plot slovenian
df_survivability_si <- df_survivability %>%
  mutate(
    group = recode(
      group,
      "CM group" = "CM skupina",
      "HD group" = "HD skupina"
    )
  )

# create points at regular x intervals (every 20 from 0 to 440)
x_points <- seq(0, 440, by = 20)

# for missing values, use the last known n_patients (carry forward)
df_corners_si <- df_survivability_si %>%
  group_by(group) %>%
  arrange(time) %>%
  complete(time = x_points) %>%
  fill(n_patients, .direction = "down") %>%
  filter(time %in% x_points) %>%
  ungroup() %>%
  distinct(group, time, .keep_all = TRUE)

# add fixed y positions based on group
df_corners_si <- df_corners_si %>%
  mutate(
    y_pos = ifelse(group == "CM skupina", 58, 52)
  )

ggplot(df_survivability_si, aes(x = time, y = surv, color = group)) +
  geom_line(linewidth = 1) +
  geom_text(
    data = df_corners_si,
    aes(label = n_patients, x = time, y = y_pos),
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  labs(x = "Čas (dnevi)", y = "Odsotnost ponovitve aritmije (%)") +
  scale_color_manual(values = c("grey25", "grey75")) +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  annotate("text", x = 350, y = 90, label = "p = 0.56", size = 5) +
  annotate("text",
    x = -5, y = 46, label = "Bolniki brez ponovitve aritmije",
    hjust = 0, size = 3.5
  )

# save as a png
ggsave(
  "figs/recidiv_si.png",
  width = 1920,
  height = 1080,
  dpi = 200,
  units = "px",
  bg = "white"
)

# recidiv vs no recidiv --------------------------------------------------------
# is there a difference in the number of reconnected veins given recidiv -------
df_rv <- df_all %>%
  select(procedure_type, number_of_reconnected_veins, had_recidiv)
df_rv <- drop_na(df_rv)
df_recidiv_rv <- df_rv %>% filter(had_recidiv == 1)
df_no_recidiv_rv <- df_rv %>% filter(had_recidiv == 0)

recidiv_rv <- df_recidiv_rv$number_of_reconnected_veins
report_mean_ci(recidiv_rv)

no_recidiv_rv <- df_no_recidiv_rv$number_of_reconnected_veins
report_mean_ci(no_recidiv_rv)

wilcox.test(recidiv_rv, no_recidiv_rv)

# recidiv + number of reconnected veins (cm vs hd) -----------------------------
recidiv_rv_cm <- df_recidiv_rv %>%
  filter(procedure_type == "close")
recidiv_rv_cm <- recidiv_rv_cm$number_of_reconnected_veins
report_mean_ci(recidiv_rv_cm)

recidiv_rv_hd <- df_recidiv_rv %>%
  filter(procedure_type == "high_density")
recidiv_rv_hd <- recidiv_rv_hd$number_of_reconnected_veins
report_mean_ci(recidiv_rv_hd)

wilcox.test(recidiv_rv_cm, recidiv_rv_hd)

# is there a difference in the number of dormants given recidiv ----------------
df_d <- df_all %>%
  select(procedure_type, number_of_dormants, had_recidiv)
df_d <- drop_na(df_d)
df_recidiv_d <- df_d %>% filter(had_recidiv == 1)
df_no_recidiv_d <- df_d %>% filter(had_recidiv == 0)

recidiv_d <- df_recidiv_d$number_of_dormants
report_mean_ci(recidiv_d)

no_recidiv_d <- df_no_recidiv_d$number_of_dormants
report_mean_ci(no_recidiv_d)

wilcox.test(recidiv_d, no_recidiv_d)

# recidiv + number of dormants (cm vs hd) --------------------------------------
recidiv_d_cm <- df_recidiv_d %>%
  filter(procedure_type == "close")
recidiv_d_cm <- recidiv_d_cm$number_of_dormants
report_mean_ci(recidiv_d_cm)

recidiv_d_hd <- df_recidiv_d %>%
  filter(procedure_type == "high_density")
recidiv_d_hd <- recidiv_d_hd$number_of_dormants
report_mean_ci(recidiv_d_hd)

wilcox.test(recidiv_d_cm, recidiv_d_hd)

# is there a difference in reconnected veins + dormants given recidiv ----------
df_rv_d <- df_all %>%
  select(
    procedure_type,
    number_of_reconnected_veins,
    number_of_dormants,
    had_recidiv
  )
df_rv_d$rv_d <-
  df_rv_d$number_of_reconnected_veins +
  df_rv_d$number_of_dormants
df_rv_d <- df_rv_d %>%
  select(procedure_type, rv_d, had_recidiv)
df_rv_d <- drop_na(df_rv_d)
df_recidiv_rv_d <- df_rv_d %>% filter(had_recidiv == 1)
df_no_recidiv_rv_d <- df_rv_d %>% filter(had_recidiv == 0)

recidiv_rv_d <- df_recidiv_rv_d$rv_d
report_mean_ci(recidiv_rv_d)

no_recidiv_rv_d <- df_no_recidiv_rv_d$rv_d
report_mean_ci(no_recidiv_rv_d)

wilcox.test(recidiv_rv_d, no_recidiv_rv_d)

# recidiv + (number of reconnected veins + dormants) (cm vs hd) ----------------
recidiv_rv_d_cm <- df_recidiv_rv_d %>%
  filter(procedure_type == "close")
recidiv_rv_d_cm <- recidiv_rv_d_cm$rv_d
report_mean_ci(recidiv_rv_d_cm)

recidiv_rv_d_hd <- df_recidiv_rv_d %>%
  filter(procedure_type == "high_density")
recidiv_rv_d_hd <- recidiv_rv_d_hd$rv_d
report_mean_ci(recidiv_rv_d_hd)

wilcox.test(recidiv_rv_d_cm, recidiv_rv_d_hd)

# HD recidiv vs HD no recidiv rv -----------------------------------------------
hd_recidiv_rv <- df_recidiv_rv %>% filter(procedure_type == "high_density")
hd_recidiv_rv <- hd_recidiv_rv$number_of_reconnected_veins
report_mean_ci(hd_recidiv_rv)

hd_no_recidiv_rv <- df_no_recidiv_rv %>%
  filter(procedure_type == "high_density")
hd_no_recidiv_rv <- hd_no_recidiv_rv$number_of_reconnected_veins
report_mean_ci(hd_no_recidiv_rv)

wilcox.test(hd_recidiv_rv, hd_no_recidiv_rv)

# HD recidiv vs HD no recidiv d ------------------------------------------------
hd_recidiv_d <- df_recidiv_d %>%
  filter(procedure_type == "high_density")
hd_recidiv_d <- hd_recidiv_d$number_of_dormants
report_mean_ci(hd_recidiv_d)

hd_no_recidiv_d <- df_no_recidiv_d %>%
  filter(procedure_type == "high_density")
hd_no_recidiv_d <- hd_no_recidiv_d$number_of_dormants
report_mean_ci(hd_no_recidiv_d)

wilcox.test(hd_recidiv_d, hd_no_recidiv_d)

# HD recidiv vs HD no recidiv rv_d ---------------------------------------------
hd_recidiv_rv_d <- df_recidiv_rv_d %>%
  filter(procedure_type == "high_density")
hd_recidiv_rv_d <- hd_recidiv_rv_d$rv_d
report_mean_ci(hd_recidiv_rv_d)

hd_no_recidiv_rv_d <- df_no_recidiv_rv_d %>%
  filter(procedure_type == "high_density")
hd_no_recidiv_rv_d <- hd_no_recidiv_rv_d$rv_d
report_mean_ci(hd_no_recidiv_rv_d)

wilcox.test(hd_recidiv_rv_d, hd_no_recidiv_rv_d)

# CM recidiv vs CM no recidiv rv -----------------------------------------------
cm_recidiv_rv <- df_recidiv_rv %>% filter(procedure_type == "close")
cm_recidiv_rv <- cm_recidiv_rv$number_of_reconnected_veins
report_mean_ci(cm_recidiv_rv)

cm_no_recidiv_rv <- df_no_recidiv_rv %>%
  filter(procedure_type == "close")
cm_no_recidiv_rv <- cm_no_recidiv_rv$number_of_reconnected_veins
report_mean_ci(cm_no_recidiv_rv)

wilcox.test(cm_recidiv_rv, cm_no_recidiv_rv)

# CM recidiv vs CM no recidiv d ------------------------------------------------
cm_recidiv_d <- df_recidiv_d %>%
  filter(procedure_type == "close")
cm_recidiv_d <- cm_recidiv_d$number_of_dormants
report_mean_ci(cm_recidiv_d)

cm_no_recidiv_d <- df_no_recidiv_d %>%
  filter(procedure_type == "close")
cm_no_recidiv_d <- cm_no_recidiv_d$number_of_dormants
report_mean_ci(cm_no_recidiv_d)

wilcox.test(cm_recidiv_d, cm_no_recidiv_d)

# CM recidiv vs CM no recidiv rv_d ---------------------------------------------
cm_recidiv_rv_d <- df_recidiv_rv_d %>%
  filter(procedure_type == "close")
cm_recidiv_rv_d <- cm_recidiv_rv_d$rv_d
report_mean_ci(cm_recidiv_rv_d)

cm_no_recidiv_rv_d <- df_no_recidiv_rv_d %>%
  filter(procedure_type == "close")
cm_no_recidiv_rv_d <- cm_no_recidiv_rv_d$rv_d
report_mean_ci(cm_no_recidiv_rv_d)

wilcox.test(cm_recidiv_rv_d, cm_no_recidiv_rv_d)
