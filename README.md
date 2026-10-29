# DoctorSV NCD Program: Retention in Care Analysis

R code for the evaluation of early retention in care in DoctorSV, El Salvador's nationwide AI-assisted telemedicine platform, comparing patients enrolled in the noncommunicable disease (NCD) pathway with patients who did not enroll.

The analysis has three scripts, run in order:

| Order | Script | Purpose |
| --- | --- | --- |
| 1 | `DoctorSV_cleaning.R` | Cleans the six raw DoctorSV datasets, builds the running variable and retention outcomes, and creates the analytic samples |
| 2 | `DoctorSV_descriptives.R` | Table 1 (characteristics by cohort), missing-data table, retention maps by district, and equiplots |
| 3 | `DoctorSV_FRD.R` | Fuzzy regression discontinuity (FRD) at BMI 30 kg/m², covariate continuity table, RD plots, and flow-chart counts |

---

## Requirements

- **R** 4.6.1 or later
- **Packages:**
  - Data: `arrow`, `dplyr`, `tidyr`, `tibble`, `purrr`, `stringr`, `stringi`
  - Descriptives and tables: `DescTools`, `finalfit`, `flextable`, `openxlsx`, `writexl`
  - Regression discontinuity: `rdrobust`
  - Figures and maps: `ggplot2`, `patchwork`, `scales`, `viridis`, `RColorBrewer`, `sf`, `geojsonsf`

```r
install.packages(c("arrow", "dplyr", "tidyr", "tibble", "purrr", "stringr", "stringi",
                   "DescTools", "finalfit", "flextable", "openxlsx", "writexl",
                   "rdrobust", "ggplot2", "patchwork", "scales", "viridis",
                   "RColorBrewer", "sf", "geojsonsf"))
```

---

## Input data

Raw data are de-identified DoctorSV extracts in Parquet format, stored in `Box/AI Diagnostics/raw_data/`. Patients are linked across files by a pseudonymized ID (`paciente_seudonimo`, renamed `id`).

| File | Content | Unit |
| --- | --- | --- |
| `01_pacientes.parquet` | Demographics, cohort, chronic conditions, questionnaire, summary counts | One row per patient |
| `02_condiciones_cronicas.parquet` | Chronic condition diagnoses and dates | One row per patient-condition |
| `03_citas.parquet` | Visits, status, diagnoses, BMI at visit | One row per visit |
| `medicamentos` (Parquet) | Prescriptions and dispensing | One row per prescription line |
| `servicios_clinicos` (Parquet) | Laboratory tests, imaging, and procedures | One row per test order |
| `referencias` (Parquet) | Referrals and their status | One row per referral |

The district map in `DoctorSV_descriptives.R` also requires `El_Salvador_districts.geojson` in the working directory.

**Data extraction date:** 14 September 2026 (`cutoff_date`), used to calculate follow-up.

**Paths:** The scripts use absolute Windows paths (`C:/Users/hgq/Box/...`). Update these before running on another computer.

---

## 1. `DoctorSV_cleaning.R`

### Sections 2 to 6: clean each dataset
Each raw dataset is read, its variables are recoded from Spanish to English labels and appropriate types (factors, dates, numbers), and a cleaned subset is saved:

| Section | Output | Main variables |
| --- | --- | --- |
| 2. Patients | `patient_clean.rds` | `cohort`, `DoctorSV_NCD`, `Sex`, `age_at_enrollment`, `department`, `region`, `district`, chronic conditions (0/1), visit counts, BMI, questionnaire items, education (`education_cat`), occupation (`occupation_cat`), test/prescription/referral counts |
| 2. Chronic conditions | `chronic_clean.rds` | `condition`, `first_dx_date`, `last_dx_date`, `diagnosis_origin` |
| 3. Visits (citas) | `appoint_clean.rds` | `visit_date`, `visit_status`, `visit_number`, `visit_bmi`, diagnoses, `specialty` |
| 4. Medications | `medicin_clean.rds` | `med_date`, `med_dispensed`, `med_dispensing_date` |
| 5. Clinical services | `service_clean.rds` | `test_gen_type` (Laboratory/Imaging/Procedure), `test_gen_date`, `test_performed`, `test_perfomed_date` |
| 6. Referrals | `referen_clean.rds` | `referral_gen_date`, `referral_status`, `referral_performed` |

**Cohorts** (`cohort`, from the DoctorSV extract):
- **DoctorSV NCD:** enrolled in the NCD pathway
- **DoctorSV Chronic:** at least one prioritized chronic condition, not enrolled
- **DoctorSV Non-Chronic:** no prioritized condition, with a relevant measurement (reference level)

### Section "retention": running variable and outcomes
- **r2. Running variable:** earliest non-missing BMI per patient (`earliest_BMI`) and its date (`earliest_BMI_date`), from `appoint_clean`. This date is the index date.
- **r3. Retention outcomes** (one value per patient):

| Variable | Definition | Source |
| --- | --- | --- |
| `visit_retained` | 3 or more completed visits (`completed_visits` ≥ 3) | `patient_clean` |
| `med_retained` | At least one prescription dispensed | `medicin_clean` |
| `lab_retained` | 2 or more laboratory tests performed; `NA` if none ordered | `service_clean` |
| `imaging_retained` | 1 or more imaging studies performed; `NA` if none ordered | `service_clean` |
| `procedure_retained` | 1 or more procedures performed; `NA` if none ordered | `service_clean` |
| `referral_retained` | At least one referral completed; `NA` if all missing or "Not applicable" | `referen_clean` |

All outcomes are coded "yes"/"no". `NA` means the service was not ordered (not applicable), not missing data.

- **r4.** Joined dataset saved as `df_disc.rds` (one row per patient).

### Section "filters": analytic samples
Sequential exclusions from `df_disc`, logged in `removal_log` for the flow chart:

1. Exclude patients with any prioritized chronic condition other than obesity (diabetes, hypertension, CKD, dyslipidemia, asthma, COPD, other chronic respiratory disease)
2. Exclude patients younger than 18 years (or missing age)
3. Exclude patients missing earliest BMI or its date
4. Keep plausible BMI values (12 < BMI < 70 kg/m²)
5. Follow-up from the index date to 14 September 2026:
   - **5a.** ≥ 30 days → `df_ge30.rds`
   - **5b.** 30 to 60 days → `df_30_60.rds`

---

## 2. `DoctorSV_descriptives.R`

Uses `df_30_60.rds` and the cleaned event files.

| Section | Output | Description |
| --- | --- | --- |
| Table 1 | `YYYY-MM-DD_Table1_cohorts.xlsx` | Characteristics by cohort (medians and IQR, n and %), with pairwise p-values for NCD vs. Chronic and NCD vs. Non-Chronic (Wilcoxon rank-sum and χ² tests); also total tests, referrals, and prescriptions by cohort |
| Supplementary Table 2 | `table2_missing.xlsx` | Missing values, n (%), by cohort and overall |
| Maps | `retention_by_district_El_Salvador.pdf` | Retention (%) by district for visits, medication, tests, and referrals |
| Equiplots | `equiplot_retention_panels.pdf` | Retention by education, occupation, and age group |

---

## 3. `DoctorSV_FRD.R`

Uses `df_ge30.rds` (main) or `df_30_60.rds` (alternative; switch at the top of the script).

| Section | Output | Description |
| --- | --- | --- |
| Checking | Console | Cross-tabulation of cohort by BMI side (< 30 vs. ≥ 30) |
| 1.1 Prepare data | `df_rd` | Restricts to NCD vs. one comparison cohort; codes `treatment` (NCD enrollment, 0/1), `treat` (BMI ≥ 30), and outcomes as 0/1 |
| 1.2 First stage | Console | Jump in probability of NCD enrollment at BMI 30 (`rdrobust`) |
| 1.3 FRD by outcome | Console | Reduced form and fuzzy RD estimate for each retention outcome |
| 1.4–1.5 Plots | `DoctorSV_*_ret_bmi30_*.pdf` | Binned retention by BMI with linear fits on each side of BMI 30 |
| Table 2 | `RD_baseline_covariate_continuity.xlsx` | Covariate continuity at BMI 30 (region, age, sex, smoking, occupation, education, BMI, first visit date), one sheet per comparison (NCD vs. Chronic, NCD vs. Non-Chronic) |
| Flow chart | `flow_counts` (console) | Sample sizes at each stage and for each outcome |

**RD specification:** running variable `earliest_BMI`; cutoff 30 kg/m²; local linear regression; triangular kernel; MSE-optimal bandwidth; robust bias-corrected inference (`rdrobust` defaults).

---

## Run order

```r
source("DoctorSV_cleaning.R")      # creates *_clean.rds, df_disc.rds, df_ge30.rds, df_30_60.rds
source("DoctorSV_descriptives.R")  # Table 1, missing-data table, maps, equiplots
source("DoctorSV_FRD.R")           # FRD, continuity table, RD plots, flow chart
```

The scripts are written for interactive, section-by-section use in RStudio. Many lines (`Desc()`, `unique()`, `View()`) only display checks, and some sections reload saved `.rds` files.

---

## Known issues to resolve before final analysis

1. **`test_retained` no longer exists.** `service_flag` now creates `lab_retained`, `imaging_retained`, and `procedure_retained`, but `DoctorSV_FRD.R` and `DoctorSV_descriptives.R` still use `test_retained`. Update those scripts, or add `test_retained` back to `service_flag`.
2. **`retained_all` is not created** in the cleaning script but is referenced in the checking section.
3. **Outcomes are not restricted to events after the index date.** Retention flags count events at any time, including before the earliest BMI. For a common time zero, restrict events to on or after `earliest_BMI_date`.
4. **Visit retention uses a whole-period total** (`completed_visits` ≥ 3 from `patient_clean`), which cannot be restricted by date.
5. **Comparison groups in the FRD.** NCD vs. Chronic and NCD vs. Non-Chronic define the comparison group partly by BMI, which can create artificial discontinuities at the cutoff. The full sample (NCD vs. all other patients) is the preferred RD comparison.
6. **`request_date` is not in `df_disc`.** It exists only in `service_clean` and is filled only for performed tests; use `test_gen_date` as the order date.
7. **Leftover objects:** `DoctorSV_FRD.R` refers to `df_disc`, `df_rd_baseline`, and `first_visit_date_numeric` in the flow-chart section, and `DoctorSV_cleaning.R` refers to `df_new`. These must exist in the session, or the lines will fail.
8. **`education_cat` factor levels:** the cleaning script assigns English labels ("Low", "Medium", "High") but sets Spanish levels ("Baja", "Media", "Alta"), which turns all values to `NA`. Align the labels and levels.

---

## Ethics

Data are de-identified by the DoctorSV NCD team before analysis. The study protocol was approved by the Duke University Institutional Review Board (Pro00120977).
