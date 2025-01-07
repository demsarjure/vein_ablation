library(readxl)
library(tidyverse)

source("utils.R")


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")
df_all <- df_all %>% filter(procedure_type == "high_density")


# locations --------------------------------------------------------------------
df_locations <- df_all %>% select(
  rspv_rr, rspv_ra, rspv_rp,
  ripv_ra, ripv_rp, ripv_ri,
  lspv_lr, lspv_lrg, lspv_lp,
  lipv_la, lipv_li, lipv_lp,
  dormant_rspv_rr, dormant_rspv_ra, dormant_rspv_rp,
  dormant_ripv_ra, dormant_ripv_rp, dormant_ripv_ri,
  dormant_lspv_lr, dormant_lspv_lrg, dormant_lspv_lp,
  dormant_lipv_la, dormant_lipv_li, dormant_lipv_lp
)
df_locations <- drop_na(df_locations)

# cast all to numeric
df_locations <- df_locations %>% mutate_all(as.numeric)

# 29 participants
n_participants <- nrow(df_locations)
n_participants

# 348 locations
n_participants * 12

# dormants pre
dormants_pre <- c(
  df_locations$rspv_rr,
  df_locations$rspv_ra,
  df_locations$rspv_rp,
  df_locations$ripv_ra,
  df_locations$ripv_rp,
  df_locations$ripv_ri,
  df_locations$lspv_lr,
  df_locations$lspv_lrg,
  df_locations$lspv_lp,
  df_locations$lipv_la,
  df_locations$lipv_li,
  df_locations$lipv_lp
)

# dormants post
dormants_post <- c(
  df_locations$dormant_rspv_rr,
  df_locations$dormant_rspv_ra,
  df_locations$dormant_rspv_rp,
  df_locations$dormant_ripv_ra,
  df_locations$dormant_ripv_rp,
  df_locations$dormant_ripv_ri,
  df_locations$dormant_lspv_lr,
  df_locations$dormant_lspv_lrg,
  df_locations$dormant_lspv_lp,
  df_locations$dormant_lipv_la,
  df_locations$dormant_lipv_li,
  df_locations$dormant_lipv_lp
)

# 34 dormants before
n_dormants_pre <- sum(dormants_pre >= 1)
n_dormants_pre

# 7 dormants post
n_dormants_post <- sum(dormants_post >= 1)
n_dormants_post

# iterate over all rows and look for matches
match_locations <- data.frame(
  one_one = 0,
  one_zero = 0,
  zero_one = 0,
  zero_zero = 0
)

for (i in seq_len(nrow(df_locations))) {
  # rspv_rr
  if (df_locations[i, "rspv_rr"] >= 1 &&df_locations[i, "dormant_rspv_rr"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "rspv_rr"] >= 1 && df_locations[i, "dormant_rspv_rr"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "rspv_rr"] == 0 && df_locations[i, "dormant_rspv_rr"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # rspv_ra
  if (df_locations[i, "rspv_ra"] >= 1 && df_locations[i, "dormant_rspv_ra"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "rspv_ra"] >= 1 && df_locations[i, "dormant_rspv_ra"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "rspv_ra"] == 0 && df_locations[i, "dormant_rspv_ra"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # rspv_rp
  if (df_locations[i, "rspv_rp"] >= 1 && df_locations[i, "dormant_rspv_rp"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "rspv_rp"] >= 1 && df_locations[i, "dormant_rspv_rp"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "rspv_rp"] == 0 && df_locations[i, "dormant_rspv_rp"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # ripv_ra
  if (df_locations[i, "ripv_ra"] >= 1 && df_locations[i, "dormant_ripv_ra"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "ripv_ra"] >= 1 && df_locations[i, "dormant_ripv_ra"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "ripv_ra"] == 0 && df_locations[i, "dormant_ripv_ra"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # ripv_rp
  if (df_locations[i, "ripv_rp"] >= 1 && df_locations[i, "dormant_ripv_rp"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "ripv_rp"] >= 1 && df_locations[i, "dormant_ripv_rp"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "ripv_rp"] == 0 && df_locations[i, "dormant_ripv_rp"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # ripv_ri
  if (df_locations[i, "ripv_ri"] >= 1 && df_locations[i, "dormant_ripv_ri"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "ripv_ri"] >= 1 && df_locations[i, "dormant_ripv_ri"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "ripv_ri"] == 0 && df_locations[i, "dormant_ripv_ri"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # lspv_lr
  if (df_locations[i, "lspv_lr"] >= 1 && df_locations[i, "dormant_lspv_lr"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "lspv_lr"] >= 1 && df_locations[i, "dormant_lspv_lr"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "lspv_lr"] == 0 && df_locations[i, "dormant_lspv_lr"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # lspv_lrg
  if (df_locations[i, "lspv_lrg"] >= 1 && df_locations[i, "dormant_lspv_lrg"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "lspv_lrg"] >= 1 && df_locations[i, "dormant_lspv_lrg"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "lspv_lrg"] == 0 && df_locations[i, "dormant_lspv_lrg"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # lspv_lp
  if (df_locations[i, "lspv_lp"] >= 1 && df_locations[i, "dormant_lspv_lp"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "lspv_lp"] >= 1 && df_locations[i, "dormant_lspv_lp"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "lspv_lp"] == 0 && df_locations[i, "dormant_lspv_lp"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # lipv_la
  if (df_locations[i, "lipv_la"] >= 1 && df_locations[i, "dormant_lipv_la"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "lipv_la"] >= 1 && df_locations[i, "dormant_lipv_la"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "lipv_la"] == 0 && df_locations[i, "dormant_lipv_la"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # lipv_li
  if (df_locations[i, "lipv_li"] >= 1 && df_locations[i, "dormant_lipv_li"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "lipv_li"] >= 1 && df_locations[i, "dormant_lipv_li"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "lipv_li"] == 0 && df_locations[i, "dormant_lipv_li"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }

  # lipv_lp
  if (df_locations[i, "lipv_lp"] >= 1 && df_locations[i, "dormant_lipv_lp"] >= 1) {
    match_locations$one_one <- match_locations$one_one + 1
  } else if (df_locations[i, "lipv_lp"] >= 1 && df_locations[i, "dormant_lipv_lp"] == 0) {
    match_locations$one_zero <- match_locations$one_zero + 1
  } else if (df_locations[i, "lipv_lp"] == 0 && df_locations[i, "dormant_lipv_lp"] >= 1) {
    match_locations$zero_one <- match_locations$zero_one + 1
  } else {
    match_locations$zero_zero <- match_locations$zero_zero + 1
  }
}

# p < 0.0001
prop.test(
  match_locations$one_one,
  match_locations$one_one + match_locations$one_zero,
  p = 0.5
)


# veins ------------------------------------------------------------------------
df_veins <- df_all %>% select(
  dormant_rspv_index,
  dormant_rspv_remap,
  dormant_ripv_index,
  dormant_ripv_remap,
  dormant_lspv_index,
  dormant_lspv_remap,
  dormant_lipv_index,
  dormant_lipv_remap
)
df_veins <- drop_na(df_veins)

# cast all to numeric
df_veins <- df_veins %>% mutate_all(as.numeric)

# 29 participants
n_participants <- nrow(df_veins)
n_participants

# 116 veins
n_participants * 4

# dormants pre
dormants_pre <- c(
  df_veins$dormant_rspv_index,
  df_veins$dormant_ripv_index,
  df_veins$dormant_lspv_index,
  df_veins$dormant_lipv_index
)

# dormants post
dormants_post <- c(
  df_veins$dormant_rspv_remap,
  df_veins$dormant_ripv_remap,
  df_veins$dormant_lspv_remap,
  df_veins$dormant_lipv_remap
)

# 30 dormants before
n_dormants_pre <- sum(dormants_pre >= 1)
n_dormants_pre

# 7 dormants post
n_dormants_post <- sum(dormants_post >= 1)
n_dormants_post

# iterate over all rows and look for matches
match_veins <- data.frame(
  one_one = 0,
  one_zero = 0,
  zero_one = 0,
  zero_zero = 0
)

for (i in seq_len(nrow(df_veins))) {
  # rspv
  if (df_veins[i, "dormant_rspv_index"] >= 1 && df_veins[i, "dormant_rspv_remap"] >= 1) {
    match_veins$one_one <- match_veins$one_one + 1
  } else if (df_veins[i, "dormant_rspv_index"] >= 1 && df_veins[i, "dormant_rspv_remap"] == 0) {
    match_veins$one_zero <- match_veins$one_zero + 1
  } else if (df_veins[i, "dormant_rspv_index"] == 0 && df_veins[i, "dormant_rspv_remap"] >= 1) {
    match_veins$zero_one <- match_veins$zero_one + 1
  } else {
    match_veins$zero_zero <- match_veins$zero_zero + 1
  }

  # ripv
  if (df_veins[i, "dormant_ripv_index"] >= 1 && df_veins[i, "dormant_ripv_remap"] >= 1) {
    match_veins$one_one <- match_veins$one_one + 1
  } else if (df_veins[i, "dormant_ripv_index"] >= 1 && df_veins[i, "dormant_ripv_remap"] == 0) {
    match_veins$one_zero <- match_veins$one_zero + 1
  } else if (df_veins[i, "dormant_ripv_index"] == 0 && df_veins[i, "dormant_ripv_remap"] >= 1) {
    match_veins$zero_one <- match_veins$zero_one + 1
  } else {
    match_veins$zero_zero <- match_veins$zero_zero + 1
  }

  # lspv
  if (df_veins[i, "dormant_lspv_index"] >= 1 && df_veins[i, "dormant_lspv_remap"] >= 1) {
    match_veins$one_one <- match_veins$one_one + 1
  } else if (df_veins[i, "dormant_lspv_index"] >= 1 && df_veins[i, "dormant_lspv_remap"] == 0) {
    match_veins$one_zero <- match_veins$one_zero + 1
  } else if (df_veins[i, "dormant_lspv_index"] == 0 && df_veins[i, "dormant_lspv_remap"] >= 1) {
    match_veins$zero_one <- match_veins$zero_one + 1
  } else {
    match_veins$zero_zero <- match_veins$zero_zero + 1
  }

  # lipv
  if (df_veins[i, "dormant_lipv_index"] >= 1 && df_veins[i, "dormant_lipv_remap"] >= 1) {
    match_veins$one_one <- match_veins$one_one + 1
  } else if (df_veins[i, "dormant_lipv_index"] >= 1 && df_veins[i, "dormant_lipv_remap"] == 0) {
    match_veins$one_zero <- match_veins$one_zero + 1
  } else if (df_veins[i, "dormant_lipv_index"] == 0 && df_veins[i, "dormant_lipv_remap"] >= 1) {
    match_veins$zero_one <- match_veins$zero_one + 1
  } else {
    match_veins$zero_zero <- match_veins$zero_zero + 1
  }
}

# proportions test that match is not significantly different from 0
prop.test(
  match_veins$one_one,
  match_veins$one_one + match_veins$zero_one + match_veins$one_zero,
  p = 0.5
)


# veins ------------------------------------------------------------------------
df_veins <- df_all %>% select(
  dormant_rspv_index,
  sum_dormant_unisolated_rspv,
  dormant_ripv_index,
  sum_dormant_unisolated_ripv,
  dormant_lspv_index,
  sum_dormant_unisolated_lspv,
  dormant_lipv_index,
  sum_dormant_unisolated_lipv
)
df_veins <- drop_na(df_veins)

# cast all to numeric
df_veins <- df_veins %>% mutate_all(as.numeric)

# 29 participants
n_participants <- nrow(df_veins)
n_participants

# 116 veins
n_participants * 4

# dormants pre
dormants_pre <- c(
  df_veins$dormant_rspv_index,
  df_veins$dormant_ripv_index,
  df_veins$dormant_lspv_index,
  df_veins$dormant_lipv_index
)

# dormants post
dormants_post <- c(
  df_veins$sum_dormant_unisolated_rspv,
  df_veins$sum_dormant_unisolated_ripv,
  df_veins$sum_dormant_unisolated_lspv,
  df_veins$sum_dormant_unisolated_lipv
)

# 30 dormants before
n_dormants_pre <- sum(dormants_pre >= 1)
n_dormants_pre

# 7 dormants post
dormants_unisolated <- c(
  df_veins$sum_dormant_unisolated_rspv,
  df_veins$sum_dormant_unisolated_ripv,
  df_veins$sum_dormant_unisolated_lspv,
  df_veins$sum_dormant_unisolated_lipv
)
n_dormants_post <- sum(dormants_unisolated >= 1)
n_dormants_post

# iterate over all rows and look for matches
match <- data.frame(
  one_one = 0,
  one_zero = 0,
  zero_one = 0,
  zero_zero = 0
)

for (i in seq_len(nrow(df_veins))) {
  # rspv
  if (df_veins[i, "dormant_rspv_index"] >= 1 && df_veins[i, "sum_dormant_unisolated_rspv"] >= 1) {
    match$one_one <- match$one_one + 1
  } else if (df_veins[i, "dormant_rspv_index"] >= 1 && df_veins[i, "sum_dormant_unisolated_rspv"] == 0) {
    match$one_zero <- match$one_zero + 1
  } else if (df_veins[i, "dormant_rspv_index"] == 0 && df_veins[i, "sum_dormant_unisolated_rspv"] >= 1) {
    match$zero_one <- match$zero_one + 1
  } else {
    match$zero_zero <- match$zero_zero + 1
  }

  # ripv
  if (df_veins[i, "dormant_ripv_index"] >= 1 && df_veins[i, "sum_dormant_unisolated_ripv"] >= 1) {
    match$one_one <- match$one_one + 1
  } else if (df_veins[i, "dormant_ripv_index"] >= 1 && df_veins[i, "sum_dormant_unisolated_ripv"] == 0) {
    match$one_zero <- match$one_zero + 1
  } else if (df_veins[i, "dormant_ripv_index"] == 0 && df_veins[i, "sum_dormant_unisolated_ripv"] >= 1) {
    match$zero_one <- match$zero_one + 1
  } else {
    match$zero_zero <- match$zero_zero + 1
  }

  # lspv
  if (df_veins[i, "dormant_lspv_index"] >= 1 && df_veins[i, "sum_dormant_unisolated_lspv"] >= 1) {
    match$one_one <- match$one_one + 1
  } else if (df_veins[i, "dormant_lspv_index"] >= 1 && df_veins[i, "sum_dormant_unisolated_lspv"] == 0) {
    match$one_zero <- match$one_zero + 1
  } else if (df_veins[i, "dormant_lspv_index"] == 0 && df_veins[i, "sum_dormant_unisolated_lspv"] >= 1) {
    match$zero_one <- match$zero_one + 1
  } else {
    match$zero_zero <- match$zero_zero + 1
  }

  # lipv
  if (df_veins[i, "dormant_lipv_index"] >= 1 && df_veins[i, "sum_dormant_unisolated_lipv"] >= 1) {
    match$one_one <- match$one_one + 1
  } else if (df_veins[i, "dormant_lipv_index"] >= 1 && df_veins[i, "sum_dormant_unisolated_lipv"] == 0) {
    match$one_zero <- match$one_zero + 1
  } else if (df_veins[i, "dormant_lipv_index"] == 0 && df_veins[i, "sum_dormant_unisolated_lipv"] >= 1) {
    match$zero_one <- match$zero_one + 1
  } else {
    match$zero_zero <- match$zero_zero + 1
  }
}

# proportions test that match is not significantly different from 0
prop.test(
  match$one_one,
  match$one_one + match$one_zero,
  p = 0.5
)
