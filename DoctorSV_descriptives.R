
# libaries ----
library(arrow)
library(DescTools)
library(dplyr)
library(tidyr)
library(tibble)
library(flextable)
library(openxlsx)
library(stringr)
library(finalfit)


# open data ----
patient_clean <- readRDS("patient_clean.rds")
chronic_clean <- readRDS("chronic_clean.rds")
appoint_clean <- readRDS("appoint_clean.rds")
medicin_clean <- readRDS("medicin_clean.rds")
service_clean <- readRDS("service_clean.rds")
referen_clean <- readRDS("referen_clean.rds")

Desc(patient_clean$last_visit_date)


# -----------------------------------------------------------.
# TABLE 1 ----
# Three cohorts + pairwise p-values
# -----------------------------------------------------------.

df_filtered <- readRDS("df_30_60.rds")


df_filtered %>%
  count(cohort, education_level) %>%
  group_by(cohort) %>%
  mutate(percent = n / sum(n) * 100) %>% filter(education_level == "University complete")

# ............................................................
# 2. Variables
# ............................................................

# Total tests---- 
 
# test, referral, med counts from patient_clean data

tests_by_participante <- df_filtered %>%
  group_by(cohort) %>%
  summarise(
    total_tests_requested = sum(
      total_tests_requested,
      na.rm = TRUE
    ),
    referrals_generated = sum(
      referrals_generated,
      na.rm = TRUE
    ),
    distinct_prescriptions = sum(
      distinct_prescriptions,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = -cohort,
    names_to = "variable",
    values_to = "n"
  ) %>%
  tidyr::pivot_wider(
    names_from = cohort,
    values_from = n
  )

tests_by_participante

# test from service_clean data

id_cohort <- df_filtered[, c('id', 'cohort')]
service_clean2 <- left_join(id_cohort, service_clean, by = "id")

Desc(service_clean2$test_gen_type)

tests_by_participante2 <- service_clean2 %>%
  group_by(cohort) %>%
  summarise(
    total_tests = n(),
    laboratory_tests = sum(test_gen_type == "Laboratory", na.rm = TRUE),
    imaging_tests = sum(test_gen_type == "Imaging", na.rm = TRUE),
    procedure_tests = sum(test_gen_type == "Procedure", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = -cohort,
    names_to = "test_type",
    values_to = "n"
  ) %>%
  tidyr::pivot_wider(
    names_from = cohort,
    values_from = n
  )

tests_by_participante2

# other variables by cohort

# select variables 
explanatory <- c(
  "region",
  "age_at_enrollment",
  "Sex",
  "smoking",
  "occupation_cat",
  "education_cat"
)
   

dependent <- "cohort"

# Right-skewed count variables
count_vars <- c(
  "total_tests_requested",
  "referrals_generated",
  "prescription_lines"
)


# ............................................................
# 3. Descriptive table — all 3 cohorts
# ............................................................

table1 <- df_filtered %>%
  mutate(cohort = droplevels(factor(cohort))) %>%
  summary_factorlist(
    dependent,
    explanatory,
    p = FALSE,
    column = TRUE,
    add_dependent_label = TRUE,
    na_include = FALSE,
    total_col = FALSE,
    cont = "median",
    cont_range = TRUE,
    digits = c(1, 1, 3, 1, 1)
  )


# ............................................................
# 4. NCD vs Chronic p values 
# ............................................................

df_sub <- df_filtered %>%
  filter(cohort %in% c("DoctorSV NCD", "DoctorSV Chronic")) %>%
  mutate(cohort = droplevels(factor(cohort)))

# keep only explanatory vars with >1 level among the filtered data
keep <- explanatory[sapply(explanatory, function(v) {
  x <- df_sub[[v]]
  if (is.factor(x) || is.character(x)) dplyr::n_distinct(x, na.rm = TRUE) > 1
  else TRUE
})]

p_chronic <- df_sub %>%
  summary_factorlist(
    dependent, keep,
    p = TRUE, column = FALSE, add_dependent_label = FALSE,
    na_include = FALSE, total_col = FALSE,
    cont = "median", cont_range = TRUE,
    p_cont = "wilcox.test", p_cat = "chisq",
    digits = c(1, 1, 3, 1, 1)
  ) %>%
  pull(p)


# ............................................................
# 5. NCD vs Non-Chronic p values 
# ............................................................

rm(df_sub)

df_sub <- df_filtered %>%
  filter(cohort %in% c("DoctorSV NCD", "DoctorSV Non-Chronic")) %>%
  mutate(cohort = droplevels(factor(cohort)))

# keep only explanatory vars with >1 level among the filtered data
keep <- explanatory[sapply(explanatory, function(v) {
  x <- df_sub[[v]]
  if (is.factor(x) || is.character(x)) dplyr::n_distinct(x, na.rm = TRUE) > 1
  else TRUE
})]

p_nonchronic <- df_sub %>%
  summary_factorlist(
    dependent, keep,
    p = TRUE, column = FALSE, add_dependent_label = FALSE,
    na_include = FALSE, total_col = FALSE,
    cont = "median", cont_range = TRUE,
    p_cont = "wilcox.test", p_cat = "chisq",
    digits = c(1, 1, 3, 1, 1)
  ) %>%
  pull(p)

# ............................................................
# 6. Add pairwise p-values
# ............................................................

table1 <- table1 %>%
  mutate(
    p_NCD_vs_Chronic = p_chronic,
    p_NCD_vs_NonChronic = p_nonchronic
  )

View(table1)

# ............................................................
# Check other variables
# ............................................................

Desc(df_filtered$cohort)
Desc(df_filtered$total_tests_requested)
Desc(df_filtered$referrals_generated)
Desc(df_filtered$prescription_lines)

library(dplyr)

# Cohort frequencies
df_filtered %>% count(cohort, name = "n") %>%
  mutate(perc = round(100 * n / sum(n), 1))

df_filtered %>%
  group_by(cohort) %>%
  summarise(
    total_tests_requested = sum(total_tests_requested, na.rm = TRUE),
    referrals_generated   = sum(referrals_generated,   na.rm = TRUE),
    prescription_lines    = sum(prescription_lines,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    summarise(., cohort = "Total", across(where(is.numeric), sum))
  ) %>%
  tidyr::pivot_longer(-cohort, names_to = "variable", values_to = "total") %>%
  tidyr::pivot_wider(names_from = cohort, values_from = total)



Desc(df_filtered$total_tests_requested)
Desc(df_filtered$referrals_generated)
Desc(df_filtered$prescription_lines)

# ............................................................
# 8. Export to Excel
# ............................................................

write.xlsx(
  table1,
  file = paste0(Sys.Date(), "_Table1_cohorts.xlsx"),
  rowNames = FALSE)
  

# ===========================================================.
# STABLE 2 ----
# ===========================================================.
# missing values only

# ..........................................
# variables
# ..........................................

vars <- c(
  "region", "age_at_enrollment", "Sex", "smoking", "occupation_cat",
  "education_cat", "hypertension", "latest_bmi", "T2DM", "dyslipidemia", 
  "CKD", "asthma", "COPD", "obesity"
)


# ..........................................
# cohort sizes
# ..........................................
df_filtered <- readRDS("df_30_60.rds")

cohort_n <- df_filtered %>% count(cohort) %>% tibble::deframe()
total_n  <- nrow(df_filtered)

# ..........................................
# stable 2 
# ..........................................
# missing values: n (%) per cohort

table2 <- df_filtered %>%
  select(cohort, all_of(vars)) %>%
  group_by(cohort) %>%
  summarise(across(all_of(vars), ~ sum(is.na(.x))), .groups = "drop") %>%
  pivot_longer(-cohort, names_to = "variable", values_to = "n_missing") %>%
  mutate(
    pct  = 100 * n_missing / cohort_n[as.character(cohort)],
    # show "0" when no missing; otherwise "n (pct%)"
    cell = ifelse(
      n_missing == 0,
      "0",
      sprintf("%d (%.1f%%)", n_missing, pct)
    )
  ) %>%
  select(variable, cohort, cell) %>%
  pivot_wider(names_from = cohort, values_from = cell) %>%
  left_join(
    df_filtered %>%
      select(all_of(vars)) %>%
      summarise(across(all_of(vars), ~ sum(is.na(.x)))) %>%
      pivot_longer(everything(),
                   names_to  = "variable",
                   values_to = "n_missing_total") %>%
      mutate(
        pct_missing_total = ifelse(
          n_missing_total == 0,
          "0",
          sprintf("%.1f%%", 100 * n_missing_total / total_n)
        )
      ),
    by = "variable"
  ) %>%
  relocate(variable,
           n_missing_total, pct_missing_total,
           any_of(c("DoctorSV NCD", "DoctorSV Chronic", "DoctorSV Non-Chronic")))

table2

# ..........................................
# save to Excel
# ..........................................

write.xlsx(
  table2,
  file      = "table2_missing.xlsx",      
  sheetName = "Missing values",
  overwrite = TRUE
)




# ---------------------------------------------.
# maps ----
# ---------------------------------------------.
# retention maps by district


df_filtered <- readRDS("df_30_60.rds")


# ============================================================.
# Retention maps by district — El Salvador
# ============================================================.

library(dplyr)
library(sf)
library(stringi)
library(geojsonsf)
library(ggplot2)
library(patchwork)
library(viridis)

# ------------------------------------------------------------.
# 1. Read district boundaries
# ------------------------------------------------------------.
el_salvador_districts <- geojsonsf::geojson_sf(
  paste(readLines("El_Salvador_districts.geojson", warn = FALSE), collapse = "")
) %>%
  mutate(
    district_clean = name %>%
      stri_trans_general("Latin-ASCII") %>%
      tolower() %>%
      trimws()
  )

# ------------------------------------------------------------.
# 2. District-level retention percentages
# ------------------------------------------------------------.

df_filtered <- df_filtered %>%
  mutate(
    referral_retained = case_when(
      is.na(referral_retained)                              ~ NA_character_,
      tolower(referral_retained) == "true"                  ~ "yes",
      tolower(referral_retained) == "false"                 ~ "no",
      TRUE                                                  ~ NA_character_
    )
  )


retention_pct <- function(x, level = "yes") {
  if (all(is.na(x))) return(NA_real_)
  mean(x == level, na.rm = TRUE) * 100
}

district_retention <- df_filtered %>%
  filter(!is.na(district)) %>%
  group_by(district) %>%
  summarise(
    visit_pct    = retention_pct(visit_retained),
    med_pct      = retention_pct(med_retained),
    test_pct     = retention_pct(test_retained),
    referral_pct = retention_pct(referral_retained),
    n_patients   = n(),
    .groups = "drop"
  ) %>%
  mutate(
    district_clean = district %>%
      stri_trans_general("Latin-ASCII") %>%
      tolower() %>%
      trimws()
  )

# ------------------------------------------------------------.
# 3. Reconcile district-name mismatches (explicit aliases only)
# ------------------------------------------------------------.
district_aliases <- c(
  "san luis de la reyna" = "san luis de la reina"
  # "nahuilingo"            -> no polygon in GeoJSON, dropped below
  # "san antonio del mosco" -> no polygon in GeoJSON, dropped below
)

district_retention <- district_retention %>%
  mutate(
    district_clean = recode(district_clean, !!!district_aliases,
                            .default = district_clean)
  )

map_names <- el_salvador_districts %>%
  st_drop_geometry() %>%
  distinct(district_clean)

unmatched <- district_retention %>%
  anti_join(map_names, by = "district_clean")

if (nrow(unmatched) > 0) {
  message(sprintf(
    "Excluded %d district(s) with no polygon in GeoJSON: %s (%d patients)",
    nrow(unmatched),
    paste(unmatched$district_clean, collapse = ", "),
    sum(unmatched$n_patients)
  ))
}

district_retention <- district_retention %>%
  semi_join(map_names, by = "district_clean")

# ------------------------------------------------------------.
# 4. Join + dissolve duplicate polygons
# ------------------------------------------------------------.
district_map <- el_salvador_districts %>%
  left_join(district_retention, by = "district_clean") %>%
  group_by(district_clean) %>%
  summarise(
    name         = first(name),
    visit_pct    = first(visit_pct),
    med_pct      = first(med_pct),
    test_pct     = first(test_pct),
    referral_pct = first(referral_pct),
    n_patients   = first(n_patients),
    geometry     = st_union(geometry),
    .groups = "drop"
  )

cat("Districts in map:", nrow(district_map), "\n")
cat("With visit data:", sum(!is.na(district_map$visit_pct)), "\n")

# ------------------------------------------------------------.
# 5. Shared theme
# ------------------------------------------------------------.
map_theme <- theme_void(base_size = 11) +
  theme(
    plot.title        = element_text(size = 13, face = "bold", hjust = 0.5,
                                     margin = margin(b = 4)),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    legend.key.height = unit(0.9, "cm"),
    legend.key.width  = unit(0.35, "cm"),
    plot.margin       = margin(4, 4, 4, 4)
  )

# ------------------------------------------------------------.
# 6. Legend scales (limits from Desc() output)
# ------------------------------------------------------------.
ranges <- list(
  visit = c(60, 100),   # observed 62.5 – 100
  med   = c(25,  85),   # observed 28.6 – 84.5
  test  = c(45, 100)    # observed 50.0 – 100
)

make_scale <- function(lims, pal = "cividis") {
  scale_fill_viridis_c(
    option   = pal,
    name     = "Retention (%)",
    limits   = lims,
    breaks   = pretty(lims, n = 5),
    labels   = function(x) paste0(round(x), "%"),
    na.value = "grey90",
    guide    = guide_colorbar(reverse = TRUE)
  )
}

# ------------------------------------------------------------.
# 7. Build maps
# ------------------------------------------------------------.


make_scale <- function(lims, legend_title = "Retention (%)") {
  scale_fill_gradientn(
    colours  = rev(RColorBrewer::brewer.pal(9, "RdBu")),
    name     = legend_title,
    limits   = lims,
    oob      = scales::squish,
    breaks   = pretty(lims, n = 5),
    labels   = function(x) paste0(round(x), "%"),
    na.value = "grey90",
    guide    = guide_colorbar(
      reverse        = FALSE,        # low at bottom, high at top
      ticks.colour   = "grey30",
      frame.colour   = "grey30",
      barheight      = unit(3.5, "cm"),   # bar length in the panel
      barwidth       = unit(0.35, "cm"),  # bar thickness
      title.position = "top",             # title above the bar, not beside
      label.theme    = element_text(size = 6.5),
      title.theme    = element_text(size = 7.5, face = "bold")
    )
  )
}

make_map <- function(var, title, lims, legend_title = "Retention (%)") {
  ggplot(district_map) +
    geom_sf(aes(fill = .data[[var]]),
            color = "black", linewidth = 0.12) +
    make_scale(lims, legend_title) +
    coord_sf(crs = st_crs(district_map), datum = st_crs(district_map)) +
    labs(title = title) +
    map_theme
}

map_visit    <- make_map("visit_pct",    "Visit retention",      c(0,  100))
map_med      <- make_map("med_pct",      "Medication retention", c(0,  100))
map_test     <- make_map("test_pct",     "Test retention",       c(0,  100))
map_referral <- make_map("referral_pct", "Referral retention",   c(0,  100))

Desc(district_map$visit_pct)
Desc(district_map$med_pct)
Desc(district_map$test_pct)
Desc(district_map$referral_pct)


# ------------------------------------------------------------.
# 8. Combine + export
# ------------------------------------------------------------.
retention_maps <- (map_visit + map_med + map_test + map_referral) +
  plot_layout(ncol = 2)

ggsave("retention_by_district_El_Salvador.pdf",
       retention_maps, width = 12, height = 10, bg = "white")

# equiplot ----

df_filtered <- readRDS("df_30_60.rds")
# df_disc <- readRDS("df_disc.rds")

# ============================================================.
# FULL PIPELINE: from raw data to equiplot panels
# ============================================================.

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)

# ------------------------------------------------------------.
# STEP 1. Normalise retention columns to "yes" / "no"
# ------------------------------------------------------------.
to_yes_no <- function(x) {
  x_chr <- as.character(x)
  dplyr::case_when(
    is.na(x)                                                ~ NA_character_,
    tolower(trimws(x_chr)) %in% c("yes", "true",  "1", "y") ~ "yes",
    tolower(trimws(x_chr)) %in% c("no",  "false", "0", "n") ~ "no",
    TRUE                                                    ~ NA_character_
  )
}

df_filtered <- df_filtered %>%
  mutate(across(
    c(visit_retained, med_retained, test_retained, referral_retained),
    to_yes_no
  ))

# ------------------------------------------------------------.
# STEP 2. Build df_eq with stratifier variables
#          Education recoded to English
# ------------------------------------------------------------.
df_eq <- df_filtered %>%
  mutate(
    age_cat = cut(
      age_at_enrollment,
      breaks = c(-Inf, 35, 60, Inf),
      labels = c("<35", "35-60", "60+"),
      right  = FALSE
    ),
    occupation_cat3 = case_when(
      occupation_cat == "Employed"                            ~ "Employed",
      occupation_cat == "Homemaker"                           ~ "Homemaker",
      occupation_cat %in% c("Student/Retired", "Unemployed")  ~ "Unemployed",
      TRUE                                                    ~ NA_character_
    ) %>%
      factor(levels = c("Unemployed", "Homemaker", "Employed")),
    education_cat = case_when(
      education_cat == "Baja"  ~ "Low",
      education_cat == "Media" ~ "Medium",
      education_cat == "Alta"  ~ "High",
      TRUE                     ~ NA_character_
    ) %>%
      factor(levels = c("Low", "Medium", "High"))
  )

# ------------------------------------------------------------.
# STEP 3. Build all_retention
# ------------------------------------------------------------.
retention_long <- df_eq %>%
  select(
    education_cat,
    occupation_cat3,
    age_cat,
    visit_retained,
    med_retained,
    test_retained,
    referral_retained
  ) %>%
  pivot_longer(
    cols      = c(visit_retained, med_retained, test_retained, referral_retained),
    names_to  = "outcome",
    values_to = "retained"
  ) %>%
  mutate(
    outcome = recode(outcome,
                     visit_retained    = "Visit",
                     med_retained      = "Medication",
                     test_retained     = "Test",
                     referral_retained = "Referral")
  )

summarise_one <- function(data, strat_var) {
  data %>%
    filter(!is.na(.data[[strat_var]])) %>%
    group_by(category = .data[[strat_var]], outcome) %>%
    summarise(
      n_nonmissing = sum(!is.na(retained)),
      n_yes        = sum(retained == "yes", na.rm = TRUE),
      pct          = round(n_yes / n_nonmissing * 100, 1),
      .groups      = "drop"
    ) %>%
    mutate(stratifier = strat_var) %>%
    select(stratifier, category, outcome, n_nonmissing, n_yes, pct)
}

all_retention <- bind_rows(
  summarise_one(retention_long, "education_cat"),
  summarise_one(retention_long, "occupation_cat3"),
  summarise_one(retention_long, "age_cat")
) %>%
  arrange(stratifier, outcome, desc(pct))

print(all_retention, n = Inf)

# check min and max
all_retention %>%
  group_by(outcome) %>%
  summarise(
    n_strata    = n(),
    min_pct     = min(pct),
    max_pct     = max(pct),
    spread_pp   = max_pct - min_pct,
    min_stratum = paste(stratifier[which.min(pct)],
                        category[which.min(pct)], sep = ": "),
    max_stratum = paste(stratifier[which.max(pct)],
                        category[which.max(pct)], sep = ": "),
    .groups = "drop"
  )



# ------------------------------------------------------------.
# STEP 4. Prepare plotting data (English education levels)
# ------------------------------------------------------------.
plot_data <- all_retention %>%
  mutate(
    stratifier = factor(stratifier,
                        levels = c("education_cat",
                                   "occupation_cat3",
                                   "age_cat"),
                        labels = c("Education", "Occupation", "Age")),
    outcome = factor(outcome,
                     levels = c("Visit", "Medication", "Test", "Referral")),
    category = factor(category,
                      levels = c(
                        "Low", "Medium", "High",
                        "Unemployed", "Homemaker", "Employed",
                        "<35", "35-60", "60+"
                      ))
  )

# ------------------------------------------------------------.
# STEP 5. Palette (English education levels)
# ------------------------------------------------------------.
category_palette <- c(
  "Low"         = "#1b9e77",
  "Medium"      = "#66c2a5",
  "High"        = "#d9f0d3",
  "Unemployed"  = "#d95f02",
  "Homemaker"   = "#fdbf6f",
  "Employed"    = "#7f3b08",
  "<35"         = "#7570b3",
  "35-60"       = "#b2a2d1",
  "60+"         = "#e6e0f2"
)

# ------------------------------------------------------------.
# STEP 6. Global size breaks — 3 bubbles shared by all panels
# ------------------------------------------------------------.
size_breaks_global <- pretty(
  range(plot_data$n_nonmissing, na.rm = TRUE),
  n = 3
)
size_breaks_global <- size_breaks_global[
  size_breaks_global >= min(plot_data$n_nonmissing, na.rm = TRUE) &
    size_breaks_global <= max(plot_data$n_nonmissing, na.rm = TRUE)
]

if (length(size_breaks_global) < 3) {
  size_breaks_global <- round(
    seq(min(plot_data$n_nonmissing, na.rm = TRUE),
        max(plot_data$n_nonmissing, na.rm = TRUE),
        length.out = 3)
  )
}
size_breaks_global <- unique(size_breaks_global)

# ------------------------------------------------------------.
# STEP 7. Custom x-axis ranges
# ------------------------------------------------------------.
x_ranges_custom <- list(
  Visit      = c(78, 88),
  Medication = c(62, 68),
  Test       = c(83, 88),
  Referral   = c(26, 32)
)

x_breaks_n <- list(
  Visit      = 4,
  Medication = 4,
  Test       = 4,
  Referral   = 4
)

# ------------------------------------------------------------.
# STEP 8. Panel builder
# ------------------------------------------------------------.


make_panel <- function(outcome_name, panel_letter,
                       x_lim = NULL, x_breaks = NULL) {
  
  df    <- plot_data %>% filter(outcome == outcome_name)
  lims  <- if (!is.null(x_lim))    x_lim    else x_ranges_custom[[outcome_name]]
  nbrk  <- if (!is.null(x_breaks)) x_breaks else x_breaks_n[[outcome_name]]
  brks  <- pretty(lims, n = nbrk)
  
  ggplot(df, aes(x = pct, y = stratifier,
                 color = category, size = n_nonmissing)) +
    
    geom_line(aes(group = stratifier),
              color = "grey50", linewidth = 0.4) +
    
    geom_point(alpha = 0.9) +
    
    scale_color_manual(
      values = category_palette,
      name   = "Category",
      drop   = FALSE
    ) +
    scale_size_continuous(
      name   = "Patients",
      range  = c(2, 8),
      breaks = size_breaks_global,
      limits = range(plot_data$n_nonmissing, na.rm = TRUE),
      labels = comma
    ) +
    scale_x_continuous(
      limits = lims,
      breaks = brks,
      labels = function(x) paste0(x, "%"),
      oob    = scales::squish
    ) +
    scale_y_discrete(expand = expansion(add = c(0.3, 0.3))) +
    
    labs(
      title = sprintf("%s. %s retention", panel_letter, outcome_name),
      x     = "Retention (%)",
      y     = NULL
    ) +
    
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title         = element_text(face = "bold", size = 11, hjust = 0),
      axis.text.y        = element_text(size = 10),
      legend.position    = "bottom",
      legend.title       = element_text(size = 9, face = "bold"),
      legend.text        = element_text(size = 8),
      legend.key.size    = unit(0.4, "cm"),
      plot.margin        = margin(4, 6, 4, 4)
    )
}

# ------------------------------------------------------------.
# STEP 9. Build the four panels
# ------------------------------------------------------------.

p_visit    <- make_panel("Visit",      "A", x_lim = c(80, 95),  x_breaks = 4)
p_med      <- make_panel("Medication", "B", x_lim = c(10, 22),  x_breaks = 4) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
p_test     <- make_panel("Test",       "C", x_lim = c(90, 100), x_breaks = 4) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
p_referral <- make_panel("Referral",   "D", x_lim = c(12, 20),  x_breaks = 4) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


# ------------------------------------------------------------.
# STEP 10. Arrange horizontally with a single shared legend
# ------------------------------------------------------------.
figure <- (p_visit | p_med | p_test | p_referral) +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "bottom") &
  guides(
    color = guide_legend(
      order          = 1,
      nrow           = 3,
      byrow          = TRUE,
      title.position = "top",
      override.aes   = list(size = 4)
    ),
    size = guide_legend(
      order          = 2,
      nrow           = 1,
      byrow          = TRUE,
      title.position = "top"
    )
  )

figure <- figure +
  plot_annotation(
    title    = "Retention by education, occupation, and age",
    subtitle = "Each panel has its own custom x-axis range",
    caption  = "Dots at panel edges indicate values outside the displayed range.",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30"),
      plot.caption  = element_text(size = 8, color = "grey40", hjust = 0)
    )
  )

# ------------------------------------------------------------.
# STEP 11. Save
# ------------------------------------------------------------.
figure

ggsave("equiplot_retention_panels.pdf",
       figure, width = 18, height = 6, bg = "white")





