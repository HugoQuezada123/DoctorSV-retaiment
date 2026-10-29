


#----------------------------------------------.
# 1. libraries ----
#----------------------------------------------.

library(arrow)
library(DescTools)
library(dplyr)
library(tidyr)
library(tibble)
library(flextable)
library(openxlsx)
library(stringr)
library(rdrobust)


# https://fews.net/el-salvador-fews-net-admin-boundaries-january-2025?utm_source=chatgpt.com



# ============================================================.
# 1. FRDisc ----
# ============================================================.
# FUZZY REGRESSION DISCONTINUITY

# Install/load packages
library(rdrobust)
library(ggplot2)
library(dplyr)
library(scales)

# df_filtered <- readRDS("df_30_60.rds")
df_filtered <- readRDS("df_ge30.rds")

Desc(df_filtered$id)


# checking ----
Desc(df_filtered$latest_bmi)

df_filtered %>% # df_filtered
  mutate(
    bmi_side = case_when(
      earliest_BMI <  30 ~ "BMI < 30",
      earliest_BMI >= 30 ~ "BMI >= 30",
      TRUE               ~ NA_character_
    )
  ) %>%
  count(cohort, bmi_side) %>%
  tidyr::pivot_wider(
    names_from  = bmi_side,
    values_from = n,
    values_fill = 0
  ) %>%
  # Row totals
  mutate(Total = `BMI < 30` + `BMI >= 30`) %>%
  # Column totals + grand total
  bind_rows(
    summarise(
      .,
      cohort     = "Total",
      `BMI < 30` = sum(`BMI < 30`),
      `BMI >= 30` = sum(`BMI >= 30`),
      Total      = sum(Total)
    )
  )


# ------------------------------------------------------------.
# Cross-tab: DoctorSV_NCD × BMI side
# ------------------------------------------------------------.

df_filtered %>%
  mutate(
    bmi_side = case_when(
      earliest_BMI < 30  ~ "BMI < 30",
      earliest_BMI >= 30 ~ "BMI >= 30",
      TRUE               ~ NA_character_
    )
  ) %>%
  filter(!is.na(bmi_side)) %>%
  count(cohort, bmi_side) %>%
  group_by(cohort) %>%
  mutate(
    pct = n / sum(n) * 100,
    value = sprintf("%d (%.1f%%)", n, pct)
  ) %>%
  ungroup() %>%
  select(cohort, bmi_side, value) %>%
  pivot_wider(
    names_from = bmi_side,
    values_from = value
  ) %>%
  # Row totals
  left_join(
    df_filtered %>%
      filter(!is.na(earliest_BMI)) %>%
      count(cohort, name = "Total") %>%
      mutate(Total = as.character(Total)),
    by = "cohort"
  )


# -----------------------------.
# 1.1. Prepare data----
# -----------------------------.
Desc(df_filtered$cohort)
Desc(df_rd$cohort)

df_rd <- df_filtered %>%
  filter(cohort %in% c("DoctorSV NCD", "DoctorSV Non-Chronic")) %>% # DoctorSV Non-Chronic
  droplevels()

Desc(df_rd$cohort)

df_rd <- df_rd %>%
  select(
    earliest_BMI,
    DoctorSV_NCD,
    med_retained,
    visit_retained,
    test_retained,
    referral_retained
  ) %>%
  filter(
    !is.na(earliest_BMI),
    !is.na(DoctorSV_NCD)
  ) %>%
  mutate(
    # --- running variable: make sure it is numeric ---
    earliest_BMI = as.numeric(as.character(earliest_BMI)),
    
    # --- enrollment (fuzzy treatment): 0/1 numeric ---
    treatment = as.numeric(as.character(DoctorSV_NCD)), # enrolled_ncd
    
    # --- side-of-cutoff indicator (descriptive/ITT only) ---
    treat = as.integer(earliest_BMI >= 30),
    
    # --- retention outcomes: 0/1 numeric ---
    med_retained      = case_when(as.character(med_retained)      == "yes"  ~ 1,
                                  as.character(med_retained)      == "no"   ~ 0,
                                  TRUE ~ NA_real_),
    visit_retained    = case_when(as.character(visit_retained)    == "yes"  ~ 1,
                                  as.character(visit_retained)    == "no"   ~ 0,
                                  TRUE ~ NA_real_),
    test_retained     = case_when(as.character(test_retained)     == "yes"  ~ 1,
                                  as.character(test_retained)     == "no"   ~ 0,
                                  TRUE ~ NA_real_),
    referral_retained = case_when(as.character(referral_retained) == "TRUE" ~ 1,
                                  as.character(referral_retained) == "FALSE" ~ 0,
                                  as.character(referral_retained) == "yes"  ~ 1,
                                  as.character(referral_retained) == "no"   ~ 0,
                                  TRUE ~ NA_real_)
  )

# -----------------------------.
# 1.2. First stage----
# -----------------------------.
# Does crossing BMI = 30 change the probability
# of being in DoctorSV NCD?
Desc(df_rd$earliest_BMI)

first_stage <- rdrobust(
  y = df_rd$treatment,
  x = df_rd$earliest_BMI,
  c = 30
)

summary(first_stage)


# -----------------------------.
# 1.3. FRD FOR EACH OUTCOME ----
# -----------------------------.

# -------------------------------------------------------------.
# Medication retention
# -------------------------------------------------------------.

df_med <- df_rd %>%
  filter(!is.na(med_retained))

reduced_form_med <- rdrobust(
  y = df_med$med_retained,
  x = df_med$earliest_BMI,
  c = 30
)

fuzzy_rd_med <- rdrobust(
  y = df_med$med_retained,
  x = df_med$earliest_BMI,
  c = 30,
  fuzzy = df_med$treatment
)

summary(reduced_form_med)
summary(fuzzy_rd_med)


# -------------------------------------------------------------.
# Visit retention
# -------------------------------------------------------------.

df_visit <- df_rd %>%
  filter(!is.na(visit_retained))

reduced_form_visit <- rdrobust(
  y = df_visit$visit_retained,
  x = df_visit$earliest_BMI,
  c = 30
)

fuzzy_rd_visit <- rdrobust(
  y = df_visit$visit_retained,
  x = df_visit$earliest_BMI,
  c = 30,
  fuzzy = df_visit$treatment
)

summary(reduced_form_visit)
summary(fuzzy_rd_visit)


# -------------------------------------------------------------.
# Test retention
# -------------------------------------------------------------.

df_test <- df_rd %>%
  filter(!is.na(test_retained))

reduced_form_test <- rdrobust(
  y = df_test$test_retained,
  x = df_test$earliest_BMI,
  c = 30
)

fuzzy_rd_test <- rdrobust(
  y = df_test$test_retained,
  x = df_test$earliest_BMI,
  c = 30,
  fuzzy = df_test$treatment
)

summary(reduced_form_test)
summary(fuzzy_rd_test)


# -------------------------------------------------------------.
# Referral retention
# -------------------------------------------------------------.

df_referral <- df_rd %>%
  filter(!is.na(referral_retained))

reduced_form_referral <- rdrobust(
  y = df_referral$referral_retained,
  x = df_referral$earliest_BMI,
  c = 30
)

fuzzy_rd_referral <- rdrobust(
  y = df_referral$referral_retained,
  x = df_referral$earliest_BMI,
  c = 30,
  fuzzy = df_referral$treatment
)

summary(reduced_form_referral)
summary(fuzzy_rd_referral)



# ============================================================.
# 1.4. Plots ----
# ============================================================.

# Function to create retention plot

make_rd_plot <- function(data, outcome, outcome_label) {
  
  df_plot <- data %>%
    filter(!is.na(.data[[outcome]])) %>%
    mutate(
      bmi_bin = floor(earliest_BMI * 2) / 2
    ) %>%
    group_by(bmi_bin) %>%
    summarise(
      percent_retained = mean(.data[[outcome]]) * 100,
      n = n(),
      .groups = "drop"
    )
  
  df_plot$bmi_bin <- as.numeric(as.character(df_plot$bmi_bin))
  
  x_max <- ceiling(
    max(df_plot$bmi_bin, na.rm = TRUE) / 5
  ) * 5
  
  p <- ggplot(
    df_plot,
    aes(x = bmi_bin, y = percent_retained)
  ) +
    
    geom_point(
      shape = 16,
      size = 1.6,
      alpha = 0.5,
      color = "grey30"
    ) +
    
    # Below BMI 30
    geom_smooth(
      data = subset(df_plot, bmi_bin < 30),
      method = "lm",
      se = TRUE,
      color = "#0072B2",
      fill = "#0072B2",
      alpha = 0.20,
      linewidth = 1
    ) +
    
    # BMI 30 or above
    geom_smooth(
      data = subset(df_plot, bmi_bin >= 30),
      method = "lm",
      se = TRUE,
      color = "#D55E00",
      fill = "#D55E00",
      alpha = 0.20,
      linewidth = 1
    ) +
    
    # BMI cutoff
    geom_vline(
      xintercept = 30,
      linetype = "dashed",
      color = "grey20",
      linewidth = 0.5
    ) +
    
    scale_x_continuous(
      breaks = seq(10, x_max, by = 10),
      limits = c(10, x_max),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    
    scale_y_continuous(
      labels = label_number(
        accuracy = 1,
        suffix = "%"
      ),
      expand = expansion(
        mult = c(0.02, 0.05)
      )
    ) +
    
    labs(
      title = paste0(
        outcome_label,
        " Around the BMI = 30 Eligibility Cutoff"
      ),
      x = "Earliest BMI (kg/m²)",
      y = paste0(outcome_label, " (%)")
    ) +
    
    theme_classic(base_size = 12) +
    
    theme(
      axis.title = element_text(
        face = "bold",
        size = 12
      ),
      axis.text = element_text(
        color = "black",
        size = 11
      ),
      axis.line = element_line(
        color = "black",
        linewidth = 0.4
      ),
      axis.ticks = element_line(
        color = "black",
        linewidth = 0.4
      ),
      legend.position = "none",
      plot.margin = margin(
        8, 12, 8, 8
      )
    )
  
  return(p)
}


# -----------------------------.
# Medication retention
# -----------------------------.

p_med <- make_rd_plot(
  df_rd,
  "med_retained",
  "Medication Retention"
)

p_med


# -----------------------------.
# Visit retention
# -----------------------------.

p_visit <- make_rd_plot(
  df_rd,
  "visit_retained",
  "Visit Retention"
)

p_visit


# -----------------------------.
# Test retention
# -----------------------------.

p_test <- make_rd_plot(
  df_rd,
  "test_retained",
  "Test Retention"
)

p_test


# -----------------------------.
# Referral retention
# -----------------------------.

p_referral <- make_rd_plot(
  df_rd,
  "referral_retained",
  "Referral Retention"
)

p_referral


# ============================================================.
# 1.5. Save plots ----
# ============================================================.

ggsave(
  "DoctorSV_med_ret_bmi30_NCD_noChronic.pdf", # _NCD_noChronic
  p_med,
  width = 8,
  height = 6,
  units = "in",
  device = cairo_pdf
)

ggsave(
  "DoctorSV_visit_ret_bmi30_NCD_noChronic.pdf",
  p_visit,
  width = 8,
  height = 6,
  units = "in",
  device = cairo_pdf
)

ggsave(
  "DoctorSV_test_ret_bmi30_NCD_noChronic.pdf",
  p_test,
  width = 8,
  height = 6,
  units = "in",
  device = cairo_pdf
)

ggsave(
  "DoctorSV_referral_ret_bmi30_NCD_noChronic.pdf",
  p_referral,
  width = 8,
  height = 6,
  units = "in",
  device = cairo_pdf
)

# Table 2 continuity table ----

# ==================================================================.
# RD BASELINE COVARIATE CONTINUITY ANALYSIS
# Two cohort comparisons: NCD vs Chronic, NCD vs Non-Chronic
# ==================================================================.

# ------------------------------------------------------------------.
# 0. Packages
# ------------------------------------------------------------------.
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(rdrobust)
library(writexl)

# ------------------------------------------------------------------.
# 1. Load raw data
# ------------------------------------------------------------------.
patient_clean  <- readRDS("patient_clean.rds")
chronic_clean  <- readRDS("chronic_clean.rds")
appoint_clean  <- readRDS("appoint_clean.rds")
medicin_clean  <- readRDS("medicin_clean.rds")
service_clean  <- readRDS("service_clean.rds")
referen_clean  <- readRDS("referen_clean.rds")

df_raw <- readRDS("df_30_60.rds")

Desc(df_raw$education_cat)

# ------------------------------------------------------------------.
# 2. Earliest BMI per patient (first visit with a BMI measurement)
# ------------------------------------------------------------------.
earliest_bmi <- appoint_clean %>%
  filter(!is.na(visit_bmi), !is.na(visit_date)) %>%
  group_by(id) %>%
  slice_min(visit_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(id, earliest_BMI = visit_bmi, first_visit_date = visit_date)

# ------------------------------------------------------------------.
# 3. Variable lists + labels
# ------------------------------------------------------------------.
baseline_vars <- c(
  "region", "age_at_enrollment", "Sex", "smoking",
  "occupation_cat", "education_cat", "latest_bmi", "first_visit_date"
)

baseline_labels <- c(
  region            = "Region",
  age_at_enrollment = "Age, years",
  Sex               = "Sex",
  smoking           = "Smoking",
  occupation_cat    = "Occupation",
  education_cat     = "Education",
  latest_bmi        = "BMI, kg/m²",
  first_visit_date  = "First visit date"
)

continuous_vars  <- c("age_at_enrollment", "latest_bmi", "first_visit_date")
categorical_vars <- c("region", "Sex", "smoking", "occupation_cat", "education_cat")

# ------------------------------------------------------------------.
# 4. Build RD baseline data for a given cohort pair
# ------------------------------------------------------------------.
build_rd_baseline <- function(cohort_pair) {
  df_raw %>%
    filter(cohort %in% cohort_pair) %>%
    droplevels() %>%
    select(-any_of(c("earliest_BMI", "first_visit_date",
                     "bmi_running", "bmi_eligible"))) %>%
    left_join(earliest_bmi, by = "id") %>%
    mutate(
      bmi_running  = earliest_BMI,
      bmi_eligible = if_else(earliest_BMI >= 30, 1, 0, missing = NA_real_)
    )
}

# ------------------------------------------------------------------.
# 5. RD continuity for a continuous variable
# ------------------------------------------------------------------.
run_rd_continuity <- function(data, outcome, outcome_label) {
  
  df <- data %>%
    filter(!is.na(earliest_BMI), !is.na(.data[[outcome]])) %>%
    mutate(rd_outcome = .data[[outcome]])
  
  # Dates -> numeric days since earliest observed date
  if (inherits(df$rd_outcome, "Date")) {
    date_origin <- min(df$rd_outcome, na.rm = TRUE)
    df <- df %>% mutate(rd_outcome = as.numeric(rd_outcome - date_origin))
  } else {
    date_origin <- NULL
  }
  
  n_below <- sum(df$earliest_BMI < 30)
  n_above <- sum(df$earliest_BMI >= 30)
  
  # Return NA row if cutoff not spanned on both sides
  if (n_below == 0 || n_above == 0) {
    return(tibble(
      variable        = outcome,
      characteristic  = outcome_label,
      predicted_below = NA_real_,
      predicted_above = NA_real_,
      estimate        = NA_real_,
      ci_low          = NA_real_,
      ci_high         = NA_real_,
      p_value         = NA_real_,
      bandwidth       = NA_real_,
      N               = nrow(df),
      date_origin     = date_origin
    ))
  }
  
  rd <- rdrobust(y = df$rd_outcome, x = df$earliest_BMI,
                 c = 30, kernel = "triangular")
  
  estimate  <- rd$coef[1]
  ci_low    <- rd$ci[1, 1]
  ci_high   <- rd$ci[1, 2]
  p_value   <- rd$pv[1]
  bandwidth <- rd$bws[1]
  n_total   <- rd$N_h[1] + rd$N_h[2]
  
  df_local <- df %>%
    filter(earliest_BMI >= 30 - bandwidth,
           earliest_BMI <= 30 + bandwidth)
  
  # Predicted value at cutoff, below side
  df_left <- df_local %>% filter(earliest_BMI < 30)
  predicted_below <- if (nrow(df_left) >= 3) {
    as.numeric(predict(
      lm(rd_outcome ~ I(earliest_BMI - 30), data = df_left),
      newdata = data.frame(earliest_BMI = 30)
    ))
  } else NA_real_
  
  # Predicted value at cutoff, above side
  df_right <- df_local %>% filter(earliest_BMI >= 30)
  predicted_above <- if (nrow(df_right) >= 3) {
    as.numeric(predict(
      lm(rd_outcome ~ I(earliest_BMI - 30), data = df_right),
      newdata = data.frame(earliest_BMI = 30)
    ))
  } else NA_real_
  
  tibble(
    variable        = outcome,
    characteristic  = outcome_label,
    predicted_below = predicted_below,
    predicted_above = predicted_above,
    estimate        = estimate,
    ci_low          = ci_low,
    ci_high         = ci_high,
    p_value         = p_value,
    bandwidth       = bandwidth,
    N               = n_total,
    date_origin     = date_origin
  )
}

# ------------------------------------------------------------------.
# 6. RD continuity for each level of a categorical variable
# ------------------------------------------------------------------.
run_categorical_rd <- function(data, variable) {
  
  categories <- data %>%
    filter(!is.na(.data[[variable]])) %>%
    distinct(.data[[variable]]) %>%
    pull(1) %>%
    as.character()
  
  map_dfr(categories, function(category) {
    
    y <- case_when(
      is.na(data[[variable]]) ~ NA_real_,
      as.character(data[[variable]]) == category ~ 1,
      TRUE ~ 0
    )
    
    keep <- !is.na(data$earliest_BMI) & !is.na(y)
    if (sum(keep) == 0) return(NULL)
    
    n_below <- sum(data$earliest_BMI[keep] < 30)
    n_above <- sum(data$earliest_BMI[keep] >= 30)
    if (n_below == 0 || n_above == 0) return(NULL)
    
    rd <- rdrobust(y = y[keep], x = data$earliest_BMI[keep],
                   c = 30, kernel = "triangular")
    
    estimate  <- rd$coef[1]
    ci_low    <- rd$ci[1, 1]
    ci_high   <- rd$ci[1, 2]
    p_value   <- rd$pv[1]
    bandwidth <- rd$bws[1]
    n_total   <- rd$N_h[1] + rd$N_h[2]
    
    df_temp <- tibble(
      earliest_BMI = data$earliest_BMI[keep],
      rd_outcome   = y[keep]
    ) %>%
      filter(earliest_BMI >= 30 - bandwidth,
             earliest_BMI <= 30 + bandwidth)
    
    df_left <- df_temp %>% filter(earliest_BMI < 30)
    predicted_below <- if (nrow(df_left) >= 3) {
      as.numeric(predict(
        lm(rd_outcome ~ I(earliest_BMI - 30), data = df_left),
        newdata = data.frame(earliest_BMI = 30)
      ))
    } else NA_real_
    
    df_right <- df_temp %>% filter(earliest_BMI >= 30)
    predicted_above <- if (nrow(df_right) >= 3) {
      as.numeric(predict(
        lm(rd_outcome ~ I(earliest_BMI - 30), data = df_right),
        newdata = data.frame(earliest_BMI = 30)
      ))
    } else NA_real_
    
    tibble(
      variable        = variable,
      characteristic  = paste0("  ", category),
      predicted_below = predicted_below,
      predicted_above = predicted_above,
      estimate        = estimate,
      ci_low          = ci_low,
      ci_high         = ci_high,
      p_value         = p_value,
      bandwidth       = bandwidth,
      N               = n_total,
      date_origin     = NULL
    )
  })
}

# ------------------------------------------------------------------.
# 7. Full pipeline for one cohort pair
# ------------------------------------------------------------------.
run_rd_for_pair <- function(cohort_pair, label) {
  
  df_rd_baseline <- build_rd_baseline(cohort_pair)
  
  continuous_results <- map_dfr(
    continuous_vars,
    ~ run_rd_continuity(
      data          = df_rd_baseline,
      outcome       = .x,
      outcome_label = baseline_labels[[.x]]
    )
  ) %>%
    mutate(section = "Continuous variables")
  
  categorical_results <- map_dfr(
    categorical_vars,
    ~ run_categorical_rd(data = df_rd_baseline, variable = .x)
  ) %>%
    mutate(section = "Categorical variables")
  
  rd_continuity_results <- bind_rows(
    continuous_results,
    categorical_results
  ) %>%
    select(
      section, variable, characteristic,
      predicted_below, predicted_above,
      estimate, ci_low, ci_high,
      p_value, bandwidth, N, date_origin
    )
  
  rd_continuity_table <- rd_continuity_results %>%
    mutate(
      `Predicted mean below BMI 30` = if_else(
        variable == "first_visit_date",
        as.character(as.Date(predicted_below, origin = date_origin)),
        format(round(predicted_below, 3), nsmall = 3)
      ),
      `Predicted mean above BMI 30` = if_else(
        variable == "first_visit_date",
        as.character(as.Date(predicted_above, origin = date_origin)),
        format(round(predicted_above, 3), nsmall = 3)
      ),
      `RD estimate` = if_else(
        variable == "first_visit_date",
        round(estimate, 1),
        round(estimate, 3)
      ),
      `95% CI` = paste0("(", round(ci_low, 3), ", ",
                        round(ci_high, 3), ")"),
      `p-value` = case_when(
        is.na(p_value) ~ NA_character_,
        p_value < 0.001 ~ "<0.001",
        TRUE ~ formatC(p_value, format = "f", digits = 3)
      ),
      Bandwidth = round(bandwidth, 2)
    ) %>%
    select(
      section, variable, characteristic,
      `Predicted mean below BMI 30`, `Predicted mean above BMI 30`,
      `RD estimate`, `95% CI`, `p-value`, Bandwidth, N
    )
  
  list(
    table    = rd_continuity_table,
    baseline = df_rd_baseline,
    label    = label
  )
}

# ------------------------------------------------------------------.
# 8. Run both comparisons
# ------------------------------------------------------------------.
comparisons <- list(
  "NCD vs Chronic"     = c("DoctorSV NCD", "DoctorSV Chronic"),
  "NCD vs Non-Chronic" = c("DoctorSV NCD", "DoctorSV Non-Chronic")
)

results <- imap(comparisons, ~ run_rd_for_pair(.x, .y))

# ------------------------------------------------------------------.
# 9. View
# ------------------------------------------------------------------.
results[["NCD vs Chronic"]]$table
results[["NCD vs Non-Chronic"]]$table

# ------------------------------------------------------------------.
# 10. Export to Excel (one sheet per comparison)
# ------------------------------------------------------------------.
write_xlsx(
  list(
    "NCD vs Chronic"     = results[["NCD vs Chronic"]]$table,
    "NCD vs Non-Chronic" = results[["NCD vs Non-Chronic"]]$table
  ),
  "RD_baseline_covariate_continuity.xlsx"
)

# ==================================================================.
# flowchart ----
# ==================================================================.

# .... Libraries

library(dplyr)
library(stringr)
library(tibble)


# .... Create visit retention flag

patient_flag <- df_disc %>%
  group_by(id) %>%
  summarise(
    visit_retained = {
      if (all(is.na(completed_visits))) {
        NA
      } else {
        any(completed_visits >= 2, na.rm = TRUE)
      }
    },
    .groups = "drop"
  ) %>%
  mutate(
    visit_retained = factor(
      ifelse(
        visit_retained,
        "yes",
        "no"
      ),
      levels = c("yes", "no")
    )
  )


# .... Create medication retention flag

medicin_flag <- medicin_clean %>%
  group_by(id) %>%
  summarise(
    med_retained = any(
      med_dispensed == "Yes",
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    med_retained = factor(
      ifelse(
        med_retained,
        "yes",
        "no"
      ),
      levels = c("yes", "no")
    )
  )


# .... Create test retention flag

service_flag <- service_clean %>%
  group_by(id) %>%
  summarise(
    test_retained = any(
      test_performed,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    test_retained = factor(
      ifelse(
        test_retained,
        "yes",
        "no"
      ),
      levels = c("yes", "no")
    )
  )


# .... Create referral retention flag

referen_flag <- referen_clean %>%
  group_by(id) %>%
  summarise(
    referral_retained = any(
      str_to_lower(
        as.character(referral_performed)
      ) == "yes",
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    referral_retained = factor(
      ifelse(
        referral_retained,
        "yes",
        "no"
      ),
      levels = c("yes", "no")
    )
  )


# .... Combine RD data with outcome flags

rd_analysis <- df_rd_baseline %>%
  select(
    id,
    earliest_BMI,
    bmi_eligible
  ) %>%
  distinct(id, .keep_all = TRUE) %>%
  left_join(
    patient_flag %>%
      select(
        id,
        visit_retained
      ),
    by = "id"
  ) %>%
  left_join(
    medicin_flag %>%
      select(
        id,
        med_retained
      ),
    by = "id"
  ) %>%
  left_join(
    service_flag %>%
      select(
        id,
        test_retained
      ),
    by = "id"
  ) %>%
  left_join(
    referen_flag %>%
      select(
        id,
        referral_retained
      ),
    by = "id"
  )


# .... Initial cohort

n_initial <- df_disc %>%
  summarise(
    N = n_distinct(id)
  ) %>%
  pull(N)


# .... Participants with available earliest BMI

n_bmi <- rd_analysis %>%
  filter(
    !is.na(earliest_BMI)
  ) %>%
  summarise(
    N = n_distinct(id)
  ) %>%
  pull(N)


# .... Participants without available earliest BMI

n_missing_bmi <- n_initial - n_bmi


# .... BMI groups

bmi_groups <- rd_analysis %>%
  filter(
    !is.na(earliest_BMI)
  ) %>%
  mutate(
    bmi_group = case_when(
      earliest_BMI < 30 ~ "BMI <30 kg/m²",
      earliest_BMI >= 30 ~ "BMI ≥30 kg/m²"
    )
  ) %>%
  count(
    bmi_group,
    name = "N"
  )


# .... Number below BMI 30

n_below_30 <- bmi_groups %>%
  filter(
    bmi_group == "BMI <30 kg/m²"
  ) %>%
  pull(N)


# .... Number at or above BMI 30

n_above_30 <- bmi_groups %>%
  filter(
    bmi_group == "BMI ≥30 kg/m²"
  ) %>%
  pull(N)


# .... Outcome-specific analytic samples

n_medication <- rd_analysis %>%
  filter(
    !is.na(earliest_BMI),
    !is.na(med_retained)
  ) %>%
  summarise(
    N = n_distinct(id)
  ) %>%
  pull(N)


n_visit <- rd_analysis %>%
  filter(
    !is.na(earliest_BMI),
    !is.na(visit_retained)
  ) %>%
  summarise(
    N = n_distinct(id)
  ) %>%
  pull(N)


n_test <- rd_analysis %>%
  filter(
    !is.na(earliest_BMI),
    !is.na(test_retained)
  ) %>%
  summarise(
    N = n_distinct(id)
  ) %>%
  pull(N)


n_referral <- rd_analysis %>%
  filter(
    !is.na(earliest_BMI),
    !is.na(referral_retained)
  ) %>%
  summarise(
    N = n_distinct(id)
  ) %>%
  pull(N)


# .... Summary of participant flow

# .... Final flow-chart numbers

flow_counts <- tibble(
  stage = c(
    "Initial cohort",
    "Available earliest BMI",
    "Missing earliest BMI",
    "BMI <30 kg/m²",
    "BMI ≥30 kg/m²",
    "Medication analytic sample",
    "Visit analytic sample",
    "Test analytic sample",
    "Referral analytic sample"
  ),
  N = c(
    n_initial,
    n_bmi,
    n_missing_bmi,
    n_below_30,
    n_above_30,
    n_medication,
    n_visit,
    n_test,
    n_referral
  )
)

flow_counts


# .... Follow-up time in days

df_rd_baseline <- df_rd_baseline %>%
  mutate(
    first_visit_date = as.Date(
      first_visit_date_numeric,
      origin = "1970-01-01"
    ),
    follow_up_days = as.numeric(
      as.Date("2026-09-14") - first_visit_date
    )
  )

Desc(df_rd_baseline$follow_up_days)

























