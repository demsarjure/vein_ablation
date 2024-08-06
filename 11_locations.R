library(readxl)
library(tidyverse)


# preprocessing ----------------------------------------------------------------
df_all <- read_excel("data/cleaned.xlsx")
df_all <- df_all %>% filter(procedure_type == "high_density")


# locations --------------------------------------------------------------------
# only relevant columns
df_all <- df_all %>% select(
    rspv_rr, rspv_ra, rspv_rp,
    ripv_ra, ripv_rp, ripv_ri,
    lspv_lr, lspv_lrg, lspv_lp,
    lipv_la, lipv_li, lipv_lp,
    dormant_rspv_rr, dormant_rspv_ra, dormant_rspv_rp,
    dormant_ripv_ra, dormant_ripv_rp, dormant_ripv_ri,
    dormant_lspv_lr, dormant_lspv_lrg, dormant_lspv_lp,
    dormant_lipv_la, dormant_lipv_li, dormant_lipv_lp
)
df_all <- drop_na(df_all)

# cast all to numeric
df_all <- df_all %>% mutate_all(as.numeric)

# 29 participants
n_participants <- nrow(df_all)
n_participants

# 348 locations
n_participants * 12

# dormants pre
dormants_pre <- c(
    df_all$rspv_rr,
    df_all$rspv_ra,
    df_all$rspv_rp,
    df_all$ripv_ra,
    df_all$ripv_rp,
    df_all$ripv_ri,
    df_all$lspv_lr,
    df_all$lspv_lrg,
    df_all$lspv_lp,
    df_all$lipv_la,
    df_all$lipv_li,
    df_all$lipv_lp
)

# dormants post
dormants_post <- c(
    df_all$dormant_rspv_rr,
    df_all$dormant_rspv_ra,
    df_all$dormant_rspv_rp,
    df_all$dormant_ripv_ra,
    df_all$dormant_ripv_rp,
    df_all$dormant_ripv_ri,
    df_all$dormant_lspv_lr,
    df_all$dormant_lspv_lrg,
    df_all$dormant_lspv_lp,
    df_all$dormant_lipv_la,
    df_all$dormant_lipv_li,
    df_all$dormant_lipv_lp
)

# 35 dormants before
n_dormants_pre <- sum(dormants_pre)
n_dormants_pre

# 7 dormants post
n_dormants_post <- sum(dormants_post)
n_dormants_post

# iterate over all rows and look for matches
match <- NULL
j <- 1
for (i in 1:nrow(df_all)) {
    # rspv_rr
    if (df_all[i, "rspv_rr"] >= 1 && df_all[i, "dormant_rspv_rr"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # rspv_ra
    if (df_all[i, "rspv_ra"] >= 1 && df_all[i, "dormant_rspv_ra"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # rspv_rp
    if (df_all[i, "rspv_rp"] >= 1 && df_all[i, "dormant_rspv_rp"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # ripv_ra
    if (df_all[i, "ripv_ra"] >= 1 && df_all[i, "dormant_ripv_ra"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # ripv_rp
    if (df_all[i, "ripv_rp"] >= 1 && df_all[i, "dormant_ripv_rp"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # ripv_ri
    if (df_all[i, "ripv_ri"] >= 1 && df_all[i, "dormant_ripv_ri"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # lspv_lr
    if (df_all[i, "lspv_lr"] >= 1 && df_all[i, "dormant_lspv_lr"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # lspv_lrg
    if (df_all[i, "lspv_lrg"] >= 1 && df_all[i, "dormant_lspv_lrg"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # lspv_lp
    if (df_all[i, "lspv_lp"] >= 1 && df_all[i, "dormant_lspv_lp"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # lipv_la
    if (df_all[i, "lipv_la"] >= 1 && df_all[i, "dormant_lipv_la"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # lipv_li
    if (df_all[i, "lipv_li"] >= 1 && df_all[i, "dormant_lipv_li"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }

    # lipv_lp
    if (df_all[i, "lipv_lp"] >= 1 && df_all[i, "dormant_lipv_lp"] >= 1) {
        match[j] <- 1
        j <- j + 1
    } else {
        match[j] <- 0
        j <- j + 1
    }
}

# proportions test that match is not significantly different from 0
prop.test(sum(match), length(match), p = 0.5)
