


# 1. libraries ----

# install.packages("arrow")
library(arrow)
library(DescTools)
library(dplyr)


# =========================================.
# 2. Patients dataset ----
# =========================================.

patient <- read_parquet(
  "C:/Users/hgq/Box/AI Diagnostics/raw_data/01_pacientes.parquet"
)

Desc(patient$cohorte_1_inscrito_ent)
Desc(patient$cohorte_2_diagnostico_cronico)
Desc(patient$cohorte_3_medicion_sin_diagnostico)
Desc(patient$request_date)

names(patient)

# .........................
# Demographic  
# .........................

patient <- patient %>%
  rename(id = paciente_seudonimo)

patient <- patient %>%
  mutate(
    cohort = factor(
      case_when(
        cohorte == "1 · Inscrito en DoctorSV/ENT" ~ "1",
        cohorte == "2 · Cronico atendido por la via general" ~ "2",
        cohorte == "3 · Sin diagnostico cronico, con medicion relevante" ~ "3"
      ),
      levels = c("1", "2", "3"),
      labels = c(
        "DoctorSV NCD",
        "DoctorSV Chronic",
        "DoctorSV Non-Chronic"
      )
    )
  ) %>%
  mutate(
    cohort = relevel(
      cohort,
      ref = "DoctorSV Non-Chronic"
    )
  )

patient <- patient %>%
  mutate(
    DoctorSV_NCD = factor(
      cohorte_1_inscrito_ent,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

patient <- patient %>%
  mutate(
    DoctorSV_chronic = factor(
      cohorte_2_diagnostico_cronico,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

patient <- patient %>%
  mutate(
    DoctorSV_non_Chronic = factor(
      cohorte_3_medicion_sin_diagnostico,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$sexo_paciente)
unique(patient$sexo_paciente)

patient <- patient %>%
  mutate(
    Sex = factor(
      recode(
        sexo_paciente,
        "Femenino" = "Female",
        "Masculino" = "Male"
      ),
      levels = c("Female", "Male")
    )
  )

Desc(patient$edad_al_ingreso_a_la_cohorte)

patient <- patient %>%
  mutate(
    age_at_enrollment = as.numeric(edad_al_ingreso_a_la_cohorte)
  )

Desc(patient$direccion_paciente_pais)
unique(patient$direccion_paciente_pais)

patient <- patient %>% # filter by SalVador
  mutate(
    country = factor(
      na_if(
        recode(
          direccion_paciente_pais,
          "SAINT MAR TIN" = "SAINT MARTIN"
        ),
        "País no especificado."
      )
    )
  )

Desc(patient$direccion_paciente_departamento)
unique(patient$direccion_paciente_departamento)

patient <- patient %>%
  mutate(
    department = factor(
      na_if(
        trimws(direccion_paciente_departamento),
        "Departamento no especificado"
      )
    )
  )

unique(patient$department)
Desc(patient$region)

patient <- patient %>%
  mutate(
    region = case_when(
      department %in% c("SANTA ANA", "AHUACHAPAN", "SONSONATE") ~ "Western Region",
      department %in% c("SAN SALVADOR", "LA LIBERTAD", "CHALATENANGO", "CUSCATLAN", "LA PAZ", "CABAÑAS", "SAN VICENTE") ~ "Central Region",
      department %in% c("SAN MIGUEL", "MORAZAN", "LA UNION", "USULUTAN") ~ "Eastern Region",
      TRUE ~ NA_character_
    ),
    region = factor(region, levels = c("Western Region", "Central Region", "Eastern Region"))
  )

Desc(patient$direccion_paciente_municipio)
unique(patient$direccion_paciente_municipio)

patient <- patient %>% 
  mutate(
    municipality = factor(
      na_if(
        trimws(direccion_paciente_municipio),
        "Departamento no especificado" # district!
      )
    )
  )

Desc(patient$direccion_paciente_distrito)
unique(patient$direccion_paciente_distrito)

patient <- patient %>%
  mutate(
    district = factor(
      na_if(
        trimws(direccion_paciente_distrito),
        "Distrito no especificado"
      )
    )
  )

Desc(patient$estado_expediente_paciente)
unique(patient$estado_expediente_paciente)

patient <- patient %>% # filter by active 
  mutate(
    record_status = factor(
      estado_expediente_paciente,
      levels = c(
        "Expediente activo",
        "Expediente dado de baja"
      ),
      labels = c(
        "Active",
        "Inactive"
      )
    )
  )

Desc(patient$tipo_documento_paciente)
unique(patient$tipo_documento_paciente)

patient <- patient %>%
  mutate(
    document_type = factor(tipo_documento_paciente)
  )

Desc(patient$tipo_paciente)
unique(patient$patient_type)

patient <- patient %>%
  mutate(
    patient_type = factor(
      tipo_paciente,
      levels = c("Titular", "Dependiente"),
      labels = c("Primary", "Dependent")
    )
  )

Desc(patient$tipo_paciente)
unique(patient$patient_type)

patient <- patient %>%
  mutate(
    patient_type = factor(
      tipo_paciente,
      levels = c("Titular", "Dependiente"),
      labels = c("Primary", "Dependent")
    )
  )

Desc(patient$parentesco)
unique(patient$parentesco)

patient <- patient %>%
  mutate(
    relationship = factor(
      parentesco,
      levels = c(
        "Hijo(a)",
        "Madre",
        "Esposo(a)",
        "Padre",
        "Abuelo(a)",
        "Tío(a)",
        "Hermano(a)",
        "Nieto(a)",
        "Primo(a)",
        "Sobrino(a)",
        "Bisabuelo(a)",
        "Bisnieto(a)"
      ),
      labels = c(
        "Child",
        "Mother",
        "Spouse",
        "Father",
        "Grandparent",
        "Aunt/Uncle",
        "Sibling",
        "Grandchild",
        "Cousin",
        "Niece/Nephew",
        "Great-grandparent",
        "Great-grandchild"
      )
    )
  )

#...............................................
# ncd 
#...............................................

Desc(patient$tiene_diabetes_mellitus)
unique(patient$tiene_diabetes_mellitus)

patient <- patient %>%
  mutate(
    diabetes = factor(
      tiene_diabetes_mellitus,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$tipo_de_diabetes_declarado)
unique(patient$tipo_de_diabetes_declarado)

patient <- patient %>%
  mutate(
    diabetes_type = factor(
      tipo_de_diabetes_declarado,
      levels = c(
        "No aplica",
        "Tipo 2",
        "No especificado",
        "Tipo 1",
        "Otro tipo especificado"
      ),
      labels = c(
        "Not applicable",
        "Type 2",
        "Not specified",
        "Type 1",
        "Other specified type"
      )
    )
  )

# T2DM variable
patient$T2DM <- factor(ifelse(patient$diabetes_type == "Type 2", "Yes", "No"),
                       levels = c("No", "Yes"))

Desc(patient$tiene_hipertension)
unique(patient$tiene_hipertension)

patient <- patient %>%
  mutate(
    hypertension = factor(
      tiene_hipertension,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$tiene_enfermedad_renal_cronica)
unique(patient$tiene_enfermedad_renal_cronica)

patient <- patient %>%
  mutate(
    CKD = factor(
      tiene_enfermedad_renal_cronica,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$tiene_dislipidemia)
unique(patient$tiene_dislipidemia)

patient <- patient %>%
  mutate(
    dyslipidemia = factor(
      tiene_dislipidemia,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$tiene_obesidad)
unique(patient$tiene_obesidad)

patient <- patient %>%
  mutate(
    obesity = factor(
      tiene_obesidad,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$tiene_asma)
unique(patient$tiene_asma)

patient <- patient %>%
  mutate(
    asthma = factor(
      tiene_asma,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$tiene_epoc)
unique(patient$tiene_epoc)

patient <- patient %>%
  mutate(
    COPD = factor(
      tiene_epoc,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$tiene_otra_respiratoria_cronica)
unique(patient$tiene_otra_respiratoria_cronica)

patient <- patient %>%
  mutate(
    other_chronic_respiratory_disease = factor(
      tiene_otra_respiratoria_cronica,
      levels = c(FALSE, TRUE),
      labels = c("0", "1")
    )
  )

Desc(patient$condiciones_cronicas_distintas)
unique(patient$condiciones_cronicas_distintas)

patient <- patient %>%
  mutate(
    other_chronic_conditions = as.numeric(condiciones_cronicas_distintas)
  )


#...............................................
# visits and dates
#...............................................

Desc(patient$fecha_primera_cita_en_el_periodo)
unique(patient$fecha_primera_cita_en_el_periodo)

patient <- patient %>%
  mutate(
    first_visit_date = as.Date(fecha_primera_cita_en_el_periodo)
  )

Desc(patient$fecha_ultima_cita_en_el_periodo)
unique(patient$fecha_ultima_cita_en_el_periodo)

patient <- patient %>%
  mutate(
    last_visit_date = as.Date(fecha_ultima_cita_en_el_periodo)
  )

Desc(patient$dias_entre_primera_y_ultima_cita)
unique(patient$dias_entre_primera_y_ultima_cita)

patient <- patient %>%
  mutate(
    days_between_first_and_last_visit =
      as.numeric(dias_entre_primera_y_ultima_cita)
  )

Desc(patient$citas_totales)
unique(patient$citas_totales)

patient <- patient %>%
  mutate(
    total_visits = as.numeric(citas_totales)
  )

Desc(patient$citas_finalizadas)
unique(patient$citas_finalizadas)

patient <- patient %>%
  mutate(
    completed_visits = as.numeric(citas_finalizadas)
  )

Desc(patient$citas_inasistidas)
unique(patient$citas_inasistidas)

patient <- patient %>%
  mutate(
    missed_visits = as.numeric(citas_inasistidas)
  )

Desc(patient$citas_canceladas)
unique(patient$citas_canceladas)

patient <- patient %>%
  mutate(
    cancelled_visits = as.numeric(citas_canceladas)
  )

Desc(patient$citas_abandonadas)
unique(patient$citas_abandonadas)

patient <- patient %>%
  mutate(
    abandoned_visits = as.numeric(citas_abandonadas)
  )

Desc(patient$citas_marcadas_es_cita_cronicos)
unique(patient$citas_marcadas_es_cita_cronicos)

patient <- patient %>%
  mutate(
    chronic_care_visits =
      as.numeric(citas_marcadas_es_cita_cronicos)
  )

Desc(patient$especialidades_distintas)
unique(patient$especialidades_distintas)

patient <- patient %>%
  mutate(
    number_of_specialties =
      as.numeric(especialidades_distintas)
  )


#...............................................
# clinical measurements 
#...............................................

Desc(patient$imc_ultimo)
unique(patient$imc_ultimo)

patient <- patient %>%
  mutate(
    latest_bmi = as.numeric(imc_ultimo)
  )

Desc(patient$imc_ultimo_fecha)
unique(patient$imc_ultimo_fecha)

patient <- patient %>%
  mutate(
    latest_bmi_date = as.Date(imc_ultimo_fecha)
  )

Desc(patient$imc_maximo) # Mayor indice de masa corporal del periodo.
unique(patient$imc_maximo)

patient <- patient %>% 
  mutate(
    maximum_bmi = as.numeric(imc_maximo)
  )

Desc(patient$peso_actual_lb)
unique(patient$peso_actual_lb)

patient <- patient %>%
  mutate(
    current_weight_lb = as.numeric(peso_actual_lb)
  )

patient <- patient %>%
  mutate(
    current_weight_kg = as.numeric(peso_actual_lb) * 0.45359237
  )

Desc(patient$peso_actual_lb_fecha)
unique(patient$peso_actual_lb_fecha)

patient <- patient %>%
  mutate(
    current_weight_lb_date = as.Date(peso_actual_lb_fecha)
  )

Desc(patient$talla) # all missing 
unique(patient$talla)

patient <- patient %>%
  mutate(
    height = as.numeric(talla)
  )

Desc(patient$consumo_de_alcohol) # all missing
unique(patient$consumo_de_alcohol)

patient <- patient %>%
  mutate(
    alcohol_consumption = factor(consumo_de_alcohol)
  )


#...............................................
# social factors 
#...............................................

Desc(patient$formulario_ultima_respuesta_fecha)
unique(patient$formulario_ultima_respuesta_fecha)

patient <- patient %>%
  mutate(
    latest_questionnaire_date =
      as.Date(formulario_ultima_respuesta_fecha)
  )

Desc(patient$formulario_preguntas_respondidas)
unique(patient$formulario_preguntas_respondidas)

patient <- patient %>%
  mutate(
    questionnaire_questions_answered =
      as.numeric(formulario_preguntas_respondidas)
  )

Desc(patient$formulario_fuma)
unique(patient$formulario_fuma)

patient <- patient %>%
  mutate(
    smoking = factor(
      formulario_fuma,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$cigarettes_per_day) # 9,258
unique(patient$formulario_cantidad_cigarrillos_diaria)

patient <- patient %>%
  mutate(
    cigarettes_per_day = as.numeric(
      na_if(formulario_cantidad_cigarrillos_diaria, "null")
    )
  )

Desc(patient$formulario_anios_fumando)
unique(patient$formulario_anios_fumando)

patient <- patient %>%
  mutate(
    years_smoking = case_when(
      formulario_anios_fumando == "null" ~ NA_real_,
      as.numeric(formulario_anios_fumando) > 100 ~ NA_real_,
      TRUE ~ as.numeric(formulario_anios_fumando)
    )
  )

Desc(patient$formulario_adicciones_del_hijo)
unique(patient$formulario_adicciones_del_hijo)

patient <- patient %>%
  mutate(
    child_addictions = factor(
      formulario_adicciones_del_hijo,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$formulario_nivel_educacion)
unique(patient$formulario_nivel_educacion)

patient <- patient %>%
  mutate(
    education_level =
      factor(formulario_nivel_educacion)
  )

Desc(patient$formulario_fue_a_la_escuela)
unique(patient$formulario_fue_a_la_escuela)

patient <- patient %>%
  mutate(
    ever_attended_school = factor(
      formulario_fue_a_la_escuela,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$formulario_ocupacion)
unique(patient$formulario_ocupacion)

patient <- patient %>%
  mutate(
    occupation =
      factor(formulario_ocupacion)
  )

Desc(patient$formulario_jefe_familia_trabajo_estable)
unique(patient$formulario_jefe_familia_trabajo_estable)

patient <- patient %>%
  mutate(
    household_head_stable_employment = factor(
      formulario_jefe_familia_trabajo_estable,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$formulario_habitantes_vivienda)
unique(patient$formulario_habitantes_vivienda)

patient <- patient %>%
  mutate(
    household_size = as.numeric(
      na_if(formulario_habitantes_vivienda, "null")
    )
  )

Desc(patient$formulario_numero_habitaciones)
unique(patient$formulario_numero_habitaciones)

patient <- patient %>%
  mutate(
    number_of_rooms = as.numeric(
      na_if(formulario_numero_habitaciones, "null")
    )
  )

Desc(patient$formulario_agua_potable) # 90% missing
unique(patient$formulario_agua_potable)

patient <- patient %>%
  mutate(
    potable_water = factor(
      formulario_agua_potable,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$formulario_servicio_sanitario)
unique(patient$formulario_servicio_sanitario)

patient <- patient %>%
  mutate(
    sanitation_service = factor(
      formulario_servicio_sanitario,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$formulario_electricidad)
unique(patient$formulario_electricidad)

patient <- patient %>%
  mutate(
    electricity = factor(
      formulario_electricidad,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$formulario_cocina_lenia) # 92.7 missing
unique(patient$formulario_cocina_lenia)

patient <- patient %>%
  mutate(
    firewood_cooking = factor(
      formulario_cocina_lenia,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$formulario_recoleccion_basura)
unique(patient$formulario_recoleccion_basura)

patient <- patient %>%
  mutate(
    garbage_collection = factor(
      formulario_recoleccion_basura,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

#...............................................
# social and access 
#...............................................

Desc(patient$formulario_agricultura_plaguicidas)
unique(patient$formulario_agricultura_plaguicidas)

patient <- patient %>%
  mutate(
    agricultural_pesticide_exposure = factor(
      formulario_agricultura_plaguicidas,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_tiempo_al_centro_de_salud)# 96.9 missing
unique(patient$formulario_tiempo_al_centro_de_salud)

options(max.print = 500)
unique(patient$formulario_tiempo_al_centro_de_salud)

library(dplyr)
library(stringr)

recode_travel_time <- function(x) {
  x <- str_to_lower(str_trim(x))
  
  # --- Special phrases ---
  is_media_hora    <- str_detect(x, "media\\s*hora|media\\s*horas|una media hora")
  is_hora_y_media  <- str_detect(x, "hora[s]?\\s*y\\s*media|hora[s]?\\s*y\\s*1/2|1\\s*1/2\\s*hora|hora media")
  
  # --- H:M patterns: "1:30", "1h30min", "1h 30 min", "1 hora 30 minutos" ---
  hm <- str_match(x, "(\\d+)\\s*(?:h|hr|hrs|hora|horas|:)\\s*(\\d+)\\s*(?:min|mnt|mn|minutos?)?")
  h_num <- suppressWarnings(as.numeric(hm[, 2]))
  m_num <- suppressWarnings(as.numeric(hm[, 3]))
  has_hm <- !is.na(h_num) & !is.na(m_num) & str_detect(x, "[h:]")
  
  # --- Generic first number ---
  num <- suppressWarnings(as.numeric(str_extract(x, "\\d+\\.?\\d*")))
  
  # --- Unit detection ---
  is_hour_unit <- str_detect(x, "\\d\\s*(h|hr|hrs|hora|horas)\\b") |
    str_detect(x, "hora|horas|\\bhrs?\\b|\\bh\\b")
  # (if it also says "min", treat as minutes unless H:M already matched)
  
  # --- Build minutes ---
  minutos <- rep(NA_real_, length(x))
  
  # H:M first (highest priority)
  minutos[has_hm] <- h_num[has_hm] * 60 + m_num[has_hm]
  
  # Pure hours ("2 horas", "1h", "1 hr")
  idx_h <- is.na(minutos) & !is.na(num) & is_hour_unit & !str_detect(x, "min|mnt|mn")
  minutos[idx_h] <- num[idx_h] * 60
  
  # Special phrases
  minutos[is.na(minutos) & is_media_hora]   <- 30
  minutos[is.na(minutos) & is_hora_y_media] <- 90
  
  # Everything else with a number → assume minutes
  idx_m <- is.na(minutos) & !is.na(num)
  minutos[idx_m] <- num[idx_m]
  
  # --- Ranges "15-20 min", "30 a 40 minutos", "5-10" → midpoint ---
  rng <- str_match(x, "(\\d+)\\s*(?:-|a|o)\\s*(\\d+)\\s*(min|mnt|mn|hora|hr|h)?")
  r1 <- suppressWarnings(as.numeric(rng[, 2]))
  r2 <- suppressWarnings(as.numeric(rng[, 3]))
  r_unit <- rng[, 4]
  has_range <- !is.na(r1) & !is.na(r2) & r2 > r1 & r2 <= 240
  mid <- (r1 + r2) / 2
  mid <- ifelse(!is.na(r_unit) & str_detect(r_unit, "h"), mid * 60, mid)
  minutos[has_range] <- mid[has_range]
  
  # --- Recode ---
  out <- case_when(
    is.na(minutos) ~ NA_character_,
    minutos <= 15  ~ "≤ 15 min",
    minutos <= 60  ~ "15-60 min",
    TRUE           ~ "> 60 min"
  )
  
  factor(out, levels = c("≤ 15 min", "15-60 min", "> 60 min"), ordered = TRUE)
}

patient$travel_time_health_center <- recode_travel_time(patient$formulario_tiempo_al_centro_de_salud)
table(patient$travel_time_health_center, useNA = "ifany")
Desc(patient$travel_time_health_center)



Desc(patient$formulario_acceso_al_centro_de_salud)
unique(patient$formulario_acceso_al_centro_de_salud)

patient <- patient %>%
  mutate(
    road_access_to_health_center = factor(
      formulario_acceso_al_centro_de_salud,
      levels = c("TIERRA", "PAVIMENTO"),
      labels = c("Unpaved", "Paved")
    )
  )


#...............................................
# self-reported chronic conditions 
#...............................................

Desc(patient$formulario_presion_alta)
unique(patient$formulario_presion_alta)

patient <- patient %>%
  mutate(
    self_reported_hypertension = factor(
      formulario_presion_alta,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_diabetes)
unique(patient$formulario_diabetes)

patient <- patient %>%
  mutate(
    self_reported_diabetes = factor(
      formulario_diabetes,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

Desc(patient$formulario_enfermedad_renal)
unique(patient$formulario_enfermedad_renal)

patient <- patient %>%
  mutate(
    self_reported_kidney_disease = factor(
      formulario_enfermedad_renal,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_enfermedad_pulmones)
unique(patient$formulario_enfermedad_pulmones)

patient <- patient %>%
  mutate(
    self_reported_lung_disease = factor(
      formulario_enfermedad_pulmones,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_enfermedad_corazon)
unique(patient$formulario_enfermedad_corazon)

patient <- patient %>%
  mutate(
    self_reported_heart_disease = factor(
      formulario_enfermedad_corazon,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_control_de_colesterol)
unique(patient$formulario_control_de_colesterol)

patient <- patient %>%
  mutate(
    cholesterol_control = factor(
      formulario_control_de_colesterol,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_dificultad_para_caminar)
unique(patient$formulario_dificultad_para_caminar)

patient <- patient %>%
  mutate(
    difficulty_walking = factor(
      formulario_dificultad_para_caminar,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_come_verduras)
unique(patient$formulario_come_verduras)

patient <- patient %>%
  mutate(
    eats_vegetables = factor(
      formulario_come_verduras,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_come_frutas)
unique(patient$formulario_come_frutas)

patient <- patient %>%
  mutate(
    eats_fruit = factor(
      formulario_come_frutas,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


#...............................................
# family history 
#...............................................

Desc(patient$formulario_familiar_con_diabetes)
unique(patient$formulario_familiar_con_diabetes)

patient <- patient %>%
  mutate(
    family_history_diabetes = factor(
      formulario_familiar_con_diabetes,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_familiar_con_hipertension)
unique(patient$formulario_familiar_con_hipertension)

patient <- patient %>%
  mutate(
    family_history_hypertension = factor(
      formulario_familiar_con_hipertension,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_familiar_con_enfermedad_renal)
unique(patient$formulario_familiar_con_enfermedad_renal)

patient <- patient %>%
  mutate(
    family_history_kidney_disease = factor(
      formulario_familiar_con_enfermedad_renal,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_hipercolesterolemia_familiar)
unique(patient$formulario_hipercolesterolemia_familiar)

patient <- patient %>%
  mutate(
    family_history_hypercholesterolemia = factor(
      formulario_hipercolesterolemia_familiar,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


Desc(patient$formulario_muerte_cardiovascular_familiar_antes_60)
unique(patient$formulario_muerte_cardiovascular_familiar_antes_60)

patient <- patient %>%
  mutate(
    family_history_premature_cardiovascular_death = factor(
      formulario_muerte_cardiovascular_familiar_antes_60,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )


#...............................................
# education and occupation 
#...............................................

Desc(patient$nivel_educativo_etiqueta)# 19 missing
unique(patient$nivel_educativo_etiqueta)

patient <- patient %>%
  mutate(
    education_level = factor(
      nivel_educativo_etiqueta,
      levels = c(
        "Primaria incompleta",
        "Primaria completa",
        "Secundaria incompleta",
        "Secundaria completa",
        "Técnica incompleta",
        "Técnica completa",
        "Universitaria incompleta",
        "Universitaria completa"
      ),
      labels = c(
        "Primary incomplete",
        "Primary complete",
        "Secondary incomplete",
        "Secondary complete",
        "Technical incomplete",
        "Technical complete",
        "University incomplete",
        "University complete"
      )
    )
  )

patient <- patient %>%
  mutate(
    education_cat = factor(
      case_when(
        is.na(nivel_educativo_etiqueta) ~ NA_character_,
        
        nivel_educativo_etiqueta %in% c(
          "Primaria incompleta",
          "Primaria completa",
          "Secundaria incompleta",
          "Secundaria completa"
        ) ~ "Low",
        
        nivel_educativo_etiqueta %in% c(
          "Técnica incompleta",
          "Técnica completa"
        ) ~ "Medium",
        
        nivel_educativo_etiqueta %in% c(
          "Universitaria incompleta",
          "Universitaria completa"
        ) ~ "High",
        
        TRUE ~ NA_character_
      ),
      levels = c("Baja", "Media", "Alta")
    )
  )

table(patient$nivel_educativo, useNA = "ifany")

#...............................................
# occupation 
#...............................................

Desc(patient$ocupacion_etiqueta)# 15% missing
unique(patient$ocupacion_etiqueta)

patient <- patient %>%
  mutate(
    occupation_cat = case_when(
      is.na(ocupacion_etiqueta)                     ~ NA_character_,
      ocupacion_etiqueta == "Ama de casa"           ~ "Homemaker",
      ocupacion_etiqueta %in% c("Estudiante",
                                "Jubilado / Pensionado") ~ "Student/Retired",
      ocupacion_etiqueta == "Desempleado"           ~ "Unemployed",
      TRUE                                          ~ "Employed"
    ),
    occupation_cat = factor(occupation_cat,
                            levels = c("Homemaker", "Student/Retired",
                                       "Employed", "Unemployed"))
  )

table(patient$occupation_cat, useNA = "ifany")

#...............................................
# tests requested 
#...............................................

Desc(patient$pruebas_solicitadas_total)
unique(patient$pruebas_solicitadas_total)

patient <- patient %>%
  mutate(
    total_tests_requested = as.numeric(pruebas_solicitadas_total)
  )


#...............................................
# tests performed 
#...............................................

Desc(patient$pruebas_realizadas_total)
unique(patient$pruebas_realizadas_total)

patient <- patient %>%
  mutate(
    total_tests_performed = as.integer(pruebas_realizadas_total)
  )


#...............................................
# lipid profile tests requested 
#...............................................

Desc(patient$pruebas_perfil_lipidico_solicitadas)
unique(patient$pruebas_perfil_lipidico_solicitadas)

patient <- patient %>%
  mutate(
    lipid_profile_tests_requested =
      as.integer(pruebas_perfil_lipidico_solicitadas)
  )


#...............................................
# lipid profile tests performed 
#...............................................

Desc(patient$pruebas_perfil_lipidico_realizadas)
unique(patient$pruebas_perfil_lipidico_realizadas)

patient <- patient %>%
  mutate(
    lipid_profile_tests_performed =
      as.integer(pruebas_perfil_lipidico_realizadas)
  )


#...............................................
# glucose tests requested 
#...............................................

Desc(patient$pruebas_glucemia_solicitadas)
unique(patient$pruebas_glucemia_solicitadas)

patient <- patient %>%
  mutate(
    glucose_tests_requested =
      as.integer(pruebas_glucemia_solicitadas)
  )


#...............................................
# glucose tests performed 
#...............................................

Desc(patient$pruebas_glucemia_realizadas)
unique(patient$pruebas_glucemia_realizadas)

patient <- patient %>%
  mutate(
    glucose_tests_performed =
      as.integer(pruebas_glucemia_realizadas)
  )


#...............................................
# kidney function tests requested 
#...............................................

Desc(patient$pruebas_funcion_renal_solicitadas)
unique(patient$pruebas_funcion_renal_solicitadas)

patient <- patient %>%
  mutate(
    kidney_function_tests_requested =
      as.integer(pruebas_funcion_renal_solicitadas)
  )


#...............................................
# kidney function tests performed 
#...............................................

Desc(patient$pruebas_funcion_renal_realizadas)
unique(patient$pruebas_funcion_renal_realizadas)

patient <- patient %>%
  mutate(
    kidney_function_tests_performed =
      as.integer(pruebas_funcion_renal_realizadas)
  )


#...............................................
# pulmonary function tests requested 
#...............................................

Desc(patient$pruebas_funcion_pulmonar_solicitadas)
unique(patient$pruebas_funcion_pulmonar_solicitadas)

patient <- patient %>%
  mutate(
    pulmonary_function_tests_requested =
      as.integer(pruebas_funcion_pulmonar_solicitadas)
  )


#...............................................
# pulmonary function tests performed 
#...............................................

Desc(patient$pruebas_funcion_pulmonar_realizadas)
unique(patient$pruebas_funcion_pulmonar_realizadas)

patient <- patient %>%
  mutate(
    pulmonary_function_tests_performed =
      as.integer(pruebas_funcion_pulmonar_realizadas)
  )


#...............................................
# blood pressure tests requested 
#...............................................

Desc(patient$pruebas_presion_arterial_solicitadas)
unique(patient$pruebas_presion_arterial_solicitadas)

patient <- patient %>%
  mutate(
    blood_pressure_tests_requested =
      as.integer(pruebas_presion_arterial_solicitadas)
  )


#...............................................
# blood pressure tests performed 
#...............................................

Desc(patient$pruebas_presion_arterial_realizadas)
unique(patient$pruebas_presion_arterial_realizadas)

patient <- patient %>%
  mutate(
    blood_pressure_tests_performed =
      as.integer(pruebas_presion_arterial_realizadas)
  )


#...............................................
# prescription lines 
#...............................................

Desc(patient$lineas_recetadas)
unique(patient$lineas_recetadas)

patient <- patient %>%
  mutate(
    prescription_lines = as.integer(lineas_recetadas)
  )


#...............................................
# dispensed medication lines 
#...............................................

Desc(patient$lineas_dispensadas)
unique(patient$lineas_dispensadas)

patient <- patient %>%
  mutate(
    dispensed_medication_lines = as.integer(lineas_dispensadas)
  )


#...............................................
# distinct prescriptions 
#...............................................

Desc(patient$recetas_distintas)
unique(patient$recetas_distintas)

patient <- patient %>%
  mutate(
    distinct_prescriptions = as.integer(recetas_distintas)
  )


#...............................................
# referrals generated 
#...............................................

Desc(patient$referencias_generadas)
unique(patient$referencias_generadas)

patient <- patient %>%
  mutate(
    referrals_generated = as.integer(referencias_generadas)
  )


# filter clean data 

# .................................................................
# At the very end of your script — subset to new variables only
# .................................................................

new_vars <- c(
  # Demographic
  "id", "cohort", "DoctorSV_NCD", "DoctorSV_chronic", "DoctorSV_non_Chronic",
  "Sex", "age_at_enrollment", "country", "department", "region", "municipality",
  "district", "record_status", "document_type", "patient_type", "relationship",
  
  # NCD
  "diabetes", "diabetes_type", "T2DM", "hypertension", "CKD", "dyslipidemia",
  "obesity", "asthma", "COPD", "other_chronic_respiratory_disease",
  "other_chronic_conditions",
  
  # Visits & dates
  "first_visit_date", "last_visit_date", "days_between_first_and_last_visit",
  "total_visits", "completed_visits", "missed_visits", "cancelled_visits",
  "abandoned_visits", "chronic_care_visits", "number_of_specialties",
  
  # Clinical measurements
  "latest_bmi", "latest_bmi_date", "maximum_bmi",
  "current_weight_lb", "current_weight_kg", "current_weight_lb_date",
  "height", "alcohol_consumption",
  
  # Social factors
  "latest_questionnaire_date", "questionnaire_questions_answered",
  "smoking", "cigarettes_per_day", "years_smoking", "child_addictions",
  "education_level", "education_cat", "ever_attended_school",
  "household_head_stable_employment", "household_size", "number_of_rooms",
  "potable_water", "sanitation_service", "electricity",
  "firewood_cooking", "garbage_collection",
  
  # Social & access
  "agricultural_pesticide_exposure", "travel_time_health_center",
  "road_access_to_health_center",
  
  # Self-reported chronic conditions
  "self_reported_hypertension", "self_reported_diabetes",
  "self_reported_kidney_disease", "self_reported_lung_disease",
  "self_reported_heart_disease", "cholesterol_control",
  "difficulty_walking", "eats_vegetables", "eats_fruit",
  
  # Family history
  "family_history_diabetes", "family_history_hypertension",
  "family_history_kidney_disease", "family_history_hypercholesterolemia",
  "family_history_premature_cardiovascular_death",
  
  # Education & occupation
  "occupation", "occupation_cat",
  
  # Tests requested / performed
  "total_tests_requested", "total_tests_performed",
  "lipid_profile_tests_requested", "lipid_profile_tests_performed",
  "glucose_tests_requested", "glucose_tests_performed",
  "kidney_function_tests_requested", "kidney_function_tests_performed",
  "pulmonary_function_tests_requested", "pulmonary_function_tests_performed",
  "blood_pressure_tests_requested", "blood_pressure_tests_performed",
  "prescription_lines", "dispensed_medication_lines",
  "distinct_prescriptions", "referrals_generated"
)

patient_clean <- patient %>% select(all_of(new_vars))

saveRDS(patient_clean, "patient_clean.rds")

patient_clean <- readRDS("patient_clean.rds")


# =========================================.
# 2. cronicas dataset ----
# =========================================.


condiciones_cronicas <- read_parquet(
  "C:/Users/hgq/Box/AI Diagnostics/raw_data/02_condiciones_cronicas.parquet"
)

str(condiciones_cronicas)
Desc(condiciones_cronicas$paciente_seudonimo)
unique(condiciones_cronicas$paciente_seudonimo)

condiciones_cronicas <- condiciones_cronicas %>%
  rename(id = paciente_seudonimo)

Desc(condiciones_cronicas$condicion)
unique(condiciones_cronicas$condicion)

condiciones_cronicas <- condiciones_cronicas %>%
  mutate(
    condition = factor(
      condicion,
      levels = c(
        "HTA",
        "DISLIPIDEMIA",
        "DIABETES MELLITUS",
        "OBESIDAD",
        "ERC",
        "ASMA",
        "OTRA RESPIRATORIA CRONICA NO ESPECIFICADA",
        "EPOC"
      ),
      labels = c(
        "Hypertension",
        "Dyslipidemia",
        "Diabetes mellitus",
        "Obesity",
        "CKD",
        "Asthma",
        "Other chronic respiratory disease",
        "COPD"
      )
    )
  )

Desc(condiciones_cronicas$origen_del_diagnostico)
unique(condiciones_cronicas$origen_del_diagnostico)

condiciones_cronicas <- condiciones_cronicas %>%
  mutate(
    diagnosis_origin = factor(
      origen_del_diagnostico,
      levels = c(
        "Principal",
        "Secundario",
        "Principal y secundario"
      ),
      labels = c(
        "Primary",
        "Secondary",
        "Primary and secondary"
      )
    )
  )


Desc(condiciones_cronicas$fecha_primer_diagnostico_observado)
unique(condiciones_cronicas$fecha_primer_diagnostico_observado)

Desc(condiciones_cronicas$fecha_ultimo_diagnostico_observado)
unique(condiciones_cronicas$fecha_ultimo_diagnostico_observado)

condiciones_cronicas <- condiciones_cronicas %>%
  mutate(
    first_dx_date = as.Date(fecha_primer_diagnostico_observado),
    last_dx_date  = as.Date(fecha_ultimo_diagnostico_observado)
  )

Desc(condiciones_cronicas$citas_con_la_condicion)
unique(condiciones_cronicas$citas_con_la_condicion)

condiciones_cronicas <- condiciones_cronicas %>%
  mutate(
    visits_with_condition = as.numeric(citas_con_la_condicion)
  )

Desc(condiciones_cronicas$diagnostico_censurado_por_izquierda)
unique(condiciones_cronicas$diagnostico_censurado_por_izquierda)

condiciones_cronicas <- condiciones_cronicas %>%
  mutate(
    left_censored = as.logical(diagnostico_censurado_por_izquierda)
  )

Desc(condiciones_cronicas$codigos_observados)
unique(condiciones_cronicas$codigos_observados)

condiciones_cronicas <- condiciones_cronicas %>%
  mutate(
    observed_codes       = codigos_observados
  )

new_vars <- c(
  "id",
  "condition",
  "diagnosis_origin",
  "first_dx_date",
  "last_dx_date",
  "visits_with_condition",
  "left_censored",
  "observed_codes"
)

condiciones_cronicas <- condiciones_cronicas %>%
  select(all_of(new_vars))

saveRDS(condiciones_cronicas, "chronic_clean.rds")

chronic_clean <- readRDS("chronic_clean.rds")



# =========================================.
# 3. citas dataset ----
# =========================================.

# ..........................
# citas — load
# ..........................

citas <- read_parquet(
  "C:/Users/hgq/Box/AI Diagnostics/raw_data/03_citas.parquet"
)

# ..........................
# id
# ..........................

Desc(citas$paciente_seudonimo)
unique(citas$paciente_seudonimo)

citas <- citas %>%
  rename(id = paciente_seudonimo)

# ..........................
# visit_id
# ..........................

Desc(citas$cita_seudonimo)
unique(citas$cita_seudonimo)

citas <- citas %>%
  mutate(
    visit_id = cita_seudonimo
  )

# ..........................
# visit_date
# ..........................

Desc(citas$fecha_cita)
unique(citas$fecha_cita)

citas <- citas %>%
  mutate(
    visit_date = as.Date(fecha_cita)
  )

citas$imc_de_la_cita
citas$fecha_cita





# ..........................
# visit_status
# ..........................

Desc(citas$estado_cita)
unique(citas$estado_cita)

citas <- citas %>%
  mutate(
    visit_status = factor(
      na_if(estado_cita, "X"),
      levels = c(
        "FINALIZADA",
        "INASISTENCIA",
        "ABANDONO",
        "CANCELADA",
        "PROGRAMADA"
      ),
      labels = c(
        "Completed",
        "No-show",
        "Abandoned",
        "Cancelled",
        "Scheduled"
      )
    )
  )

# ..........................
# visit_type
# ..........................

Desc(citas$tipo_cita)
unique(citas$tipo_cita)

citas <- citas %>%
  mutate(
    visit_type = factor(
      tipo_cita,
      levels = c("Videollamada", "Telefónica"),
      labels = c("Video call", "Phone call")
    )
  )

# ..........................
# area
# ..........................

Desc(citas$area)
unique(citas$area)

citas <- citas %>%
  mutate(
    area = factor(area)
  )

# ..........................
# specialty
# ..........................

Desc(citas$especialidad_1)
unique(citas$especialidad_1)

citas <- citas %>%
  mutate(
    specialty = factor(
      especialidad_1,
      levels = c(
        "MEDICINA GENERAL",
        "MEDICINA METABÓLICA",
        "NUTRICIÓN",
        "MEDICINA INTERNA",
        "GINECOLOGIA Y OBSTETRICIA",
        "SALUD MENTAL",
        "PEDIATRIA",
        "GINECOLOGIA"
      ),
      labels = c(
        "General medicine",
        "Metabolic medicine",
        "Nutrition",
        "Internal medicine",
        "Obstetrics & gynecology",
        "Mental health",
        "Pediatrics",
        "Gynecology"
      )
    )
  )

# ..........................
# chronic care visit
# ..........................

Desc(citas$es_cita_cronicos)
unique(citas$es_cita_cronicos)

citas <- citas %>%
  mutate(
    chronic_care_visit = factor(
      es_cita_cronicos,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# ..........................
# administrative visit
# ..........................

Desc(citas$es_cita_administrativa)
unique(citas$es_cita_administrativa)

citas <- citas %>%
  mutate(
    administrative_visit = factor(
      es_cita_administrativa,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# ..........................
# follow-up visit
# ..........................

Desc(citas$es_cita_seguimiento)
unique(citas$es_cita_seguimiento)

citas <- citas %>%
  mutate(
    followup_visit = factor(
      es_cita_seguimiento,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# ..........................
# row number within patient
# ..........................

Desc(citas$rn_cita_paciente)
unique(citas$rn_cita_paciente)

citas <- citas %>%
  mutate(
    visit_number = as.numeric(rn_cita_paciente)
  )

# ..........................
# primary diagnosis code
# ..........................

Desc(citas$codigo_diagnostico)
unique(citas$codigo_diagnostico)

citas <- citas %>%
  mutate(
    primary_diagnosis_code = na_if(codigo_diagnostico, "-1")
  )

# ..........................
# primary diagnosis description
# ..........................

Desc(citas$descripcion_diagnostico)
unique(citas$descripcion_diagnostico)

citas <- citas %>%
  mutate(
    primary_diagnosis_desc = na_if(descripcion_diagnostico, "N/A")
  )

# ..........................
# secondary diagnosis codes
# ..........................

Desc(citas$diagnosticos_secundarios_codigos)
unique(citas$diagnosticos_secundarios_codigos)

citas <- citas %>%
  mutate(
    secondary_diagnosis_codes = diagnosticos_secundarios_codigos
  )

# ..........................
# secondary diagnosis descriptions
# ..........................

Desc(citas$diagnosticos_secundarios_descripciones)
unique(citas$diagnosticos_secundarios_descripciones)

citas <- citas %>%
  mutate(
    secondary_diagnosis_desc = diagnosticos_secundarios_descripciones
  )

# ..........................
# number of secondary diagnoses
# ..........................

Desc(citas$diagnosticos_secundarios_cuantos)
unique(citas$diagnosticos_secundarios_cuantos)

citas <- citas %>%
  mutate(
    n_secondary_diagnoses = as.numeric(diagnosticos_secundarios_cuantos)
  )

# ..........................
# diagnostic classification
# ..........................

Desc(citas$clasificacion_diagnostica)
unique(citas$clasificacion_diagnostica)

citas <- citas %>%
  mutate(
    diagnosis_classification = factor(
      clasificacion_diagnostica,
      levels = c("PRIMERA VEZ", "SUBSECUENTE"),
      labels = c("First time", "Subsequent")
    )
  )

# ..........................
# triage
# ..........................

Desc(citas$triage)
unique(citas$triage)

citas <- citas %>%
  mutate(
    triage = triage)

# ..........................
# BMI at visit
# ..........................

Desc(citas$imc_de_la_cita)
unique(citas$imc_de_la_cita)

citas <- citas %>%
  mutate(
    visit_bmi = as.numeric(imc_de_la_cita)
  )

# ..........................
# no-show reason
# ..........................

Desc(citas$tipo_inasistencia_desc)
unique(citas$tipo_inasistencia_desc)

citas <- citas %>%
  mutate(
    no_show_reason = factor(
      na_if(tipo_inasistencia_desc, "N/A"),
      levels = c(
        "Paciente no conectado",
        "Paciente se conectó, salió y no volvió",
        "Cita marcada por médico como no viable",
        "Paciente conectado sin ser atendido"
      ),
      labels = c(
        "Patient did not connect",
        "Patient connected, left and did not return",
        "Visit marked as not viable by doctor",
        "Patient connected but not attended"
      )
    )
  )

# ..........................
# visit duration (seconds)
# ..........................

Desc(citas$duracion_cita_segundos)
unique(citas$duracion_cita_segundos)

citas <- citas %>%
  mutate(
    visit_duration_seconds = as.numeric(duracion_cita_segundos)
  )

# ..........................
# video platform
# ..........................

Desc(citas$plataforma_videollamada)
unique(citas$plataforma_videollamada)

citas <- citas %>%
  mutate(
    video_platform = factor(
      na_if(plataforma_videollamada, "N/A"),
      levels = c("Meet", "Jitsi"),
      labels = c("Google Meet", "Jitsi")
    )
  )

# ..........................
# generates referral
# ..........................

Desc(citas$cita_genera_referencia)
unique(citas$cita_genera_referencia)

citas <- citas %>%
  mutate(
    generates_referral = factor(
      cita_genera_referencia,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# ..........................
# in clinical view
# ..........................

Desc(citas$esta_en_la_vista_clinica)
unique(citas$esta_en_la_vista_clinica)

citas <- citas %>%
  mutate(
    in_clinical_view = as.logical(esta_en_la_vista_clinica)
  )

# ..........................
# department
# ..........................

Desc(citas$direccion_paciente_departamento)
unique(citas$direccion_paciente_departamento)

citas <- citas %>%
  mutate(
    department = factor(
      na_if(trimws(direccion_paciente_departamento),
            "Departamento no especificado")
    )
  )

# ..........................
# municipality
# ..........................

Desc(citas$direccion_paciente_municipio)
unique(citas$direccion_paciente_municipio)

citas <- citas %>%
  mutate(
    municipality = factor(
      na_if(trimws(direccion_paciente_municipio),
            "Municipio no especificado")
    )
  )

# ..........................
# district
# ..........................

Desc(citas$direccion_paciente_distrito)
unique(citas$district)

citas <- citas %>%
  mutate(
    district = factor(
      na_if(trimws(direccion_paciente_distrito),
            "Distrito no especificado")
    )
  )

# ..........................
# subset: keep only the new variables
# ..........................

new_vars <- c(
  "id",
  "visit_id",
  "visit_date",
  "visit_status",
  "visit_type",
  "area",
  "specialty",
  "chronic_care_visit",
  "administrative_visit",
  "followup_visit",
  "visit_number",
  "primary_diagnosis_code",
  "primary_diagnosis_desc",
  "secondary_diagnosis_codes",
  "secondary_diagnosis_desc",
  "n_secondary_diagnoses",
  "diagnosis_classification",
  "triage",
  "visit_bmi",
  "no_show_reason",
  "visit_duration_seconds",
  "video_platform",
  "generates_referral",
  "in_clinical_view",
  "department",
  "municipality",
  "district"
)

citas <- citas %>%
  select(all_of(new_vars))

# ..........................
# save
# ..........................

saveRDS(citas, "appoint_clean.rds")

appoint_clean <- readRDS("appoint_clean.rds")




# =========================================.
# 4. medicamentos dataset ----
# =========================================.

medicamentos <- read_parquet(
  "C:/Users/hgq/Box/AI Diagnostics/raw_data/04_medicamentos.parquet"
)

Desc(medicamentos)


# ..........................
# id
# ..........................

Desc(medicamentos$paciente_seudonimo)
unique(medicamentos$paciente_seudonimo)

medicamentos <- medicamentos %>%
  rename(id = paciente_seudonimo)

# ..........................
# visit_id
# ..........................

Desc(medicamentos$cita_seudonimo)
unique(medicamentos$cita_seudonimo)

medicamentos <- medicamentos %>%
  mutate(
    visit_id = cita_seudonimo
  )

# ..........................
# visit_date
# ..........................

Desc(medicamentos$fecha_cita)
unique(medicamentos$fecha_cita)

medicamentos <- medicamentos %>%
  mutate(
    visit_date = as.Date(fecha_cita)
  )

# ..........................
# prescription_date
# ..........................

Desc(medicamentos$fecha_generacion_receta)
unique(medicamentos$fecha_generacion_receta)

medicamentos <- medicamentos %>%
  mutate(
    med_date = as.Date(fecha_generacion_receta)
  )


# ..........................
# medication_code
# ..........................

Desc(medicamentos$codigo_medicamento)
unique(medicamentos$codigo_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    medication_code = codigo_medicamento
  )

# ..........................
# medication_description
# ..........................

Desc(medicamentos$descripcion_medicamento)
unique(medicamentos$descripcion_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    medication_description = descripcion_medicamento
  )

# ..........................
# active_ingredient
# ..........................

Desc(medicamentos$principio_activo)
unique(medicamentos$principio_activo)

medicamentos <- medicamentos %>%
  mutate(
    active_ingredient = principio_activo
  )

# ..........................
# atc_code
# ..........................

Desc(medicamentos$codigo_atc_medicamento)
unique(medicamentos$codigo_atc_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    atc_code = codigo_atc_medicamento
  )

# ..........................
# chronic_therapeutic_class
# ..........................

Desc(medicamentos$clase_terapeutica_cronica)
unique(medicamentos$clase_terapeutica_cronica)

medicamentos <- medicamentos %>%
  mutate(
    chronic_therapeutic_class = factor(
      clase_terapeutica_cronica,
      levels = c(
        "Antihipertensivo",
        "Antidiabetico",
        "Hipolipemiante (dislipidemia)",
        "Respiratorio (asma o EPOC)",
        "Enfermedad renal cronica (analogo de vitamina D)"
      ),
      labels = c(
        "Antihypertensive",
        "Antidiabetic",
        "Lipid-lowering",
        "Respiratory (asthma or COPD)",
        "CKD (vitamin D analogue)"
      )
    )
  )

# ..........................
# group
# ..........................

Desc(medicamentos$agrupador)
unique(medicamentos$agrupador)

# medicamentos <- medicamentos %>%
#   mutate(
#     group = na_if(agrupador, "Agrupador inválido")
#   )

# ..........................
# controlled_medication
# ..........................

Desc(medicamentos$medicamento_controlado)
unique(medicamentos$medicamento_controlado)

medicamentos <- medicamentos %>%
  mutate(
    controlled_medication = factor(
      medicamento_controlado,
      levels = c("N", "S"),
      labels = c("No", "Yes")
    )
  )

# ..........................
# quantity_prescribed
# ..........................

Desc(medicamentos$cantidad_recetada)
unique(medicamentos$cantidad_recetada)

medicamentos <- medicamentos %>%
  mutate(
    quantity_prescribed = as.numeric(cantidad_recetada)
  )

# ..........................
# dose
# ..........................

Desc(medicamentos$dosis_medicamento)
unique(medicamentos$dosis_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    dose = dosis_medicamento
  )

# ..........................
# frequency
# ..........................

Desc(medicamentos$frecuencia_administracion_medicamento)
unique(medicamentos$frecuencia_administracion_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    frequency = frecuencia_administracion_medicamento
  )

# ..........................
# duration
# ..........................

Desc(medicamentos$duracion_administracion_medicamento)
unique(medicamentos$duracion_administracion_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    duration = duracion_administracion_medicamento
  )

# ..........................
# single_dose
# ..........................

Desc(medicamentos$dosis_unica_medicamento)
unique(medicamentos$dosis_unica_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    single_dose = factor(
      dosis_unica_medicamento,
      levels = c("Dosis única", "Dosis múltiple"),
      labels = c("Single dose", "Multiple dose")
    )
  )

# ..........................
# units
# ..........................

Desc(medicamentos$unidades_medicamento)
unique(medicamentos$unidades_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    units = unidades_medicamento
    )

# ..........................
# route
# ..........................

Desc(medicamentos$via_administracion_medicamento)
unique(medicamentos$via_administracion_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    route = factor(
      via_administracion_medicamento,
      levels = c(
        "Oral", "Intramuscular", "Topico", "Nasal",
        "Subcutánea", "Oftálmica", "Vaginal", "Inhalada",
        "Inhalación o", "Ótica", "Inhalación oral", "Rectal"
      ),
      labels = c(
        "Oral", "Intramuscular", "Topical", "Nasal",
        "Subcutaneous", "Ophthalmic", "Vaginal", "Inhaled",
        "Inhalation or", "Otic", "Oral inhalation", "Rectal"
      )
    )
  )

# ..........................
# medication_status
# ..........................

Desc(medicamentos$estado_medicamento)
unique(medicamentos$estado_medicamento)

medicamentos <- medicamentos %>%
  mutate(
    medication_status = factor(
      estado_medicamento,
      levels = c("Activo", "Inactivo"),
      labels = c("Active", "Inactive")
    )
  )

# ..........................
# quantity_dispensed
# ..........................

Desc(medicamentos$cantidad_despachada)
unique(medicamentos$cantidad_despachada)

medicamentos <- medicamentos %>%
  mutate(
    quantity_dispensed = as.numeric(cantidad_despachada)
  )

# ..........................
# quantity_pending
# ..........................

Desc(medicamentos$cantidad_pendiente_despacho)
unique(medicamentos$cantidad_pendiente_despacho)

medicamentos <- medicamentos %>%
  mutate(
    quantity_pending = as.numeric(cantidad_pendiente_despacho)
  )

# ..........................
# dispensing_date
# ..........................

Desc(medicamentos$fecha_dispensacion_receta)
unique(medicamentos$fecha_dispensacion_receta)

medicamentos <- medicamentos %>%
  mutate(
    med_dispensing_date = as.Date(fecha_dispensacion_receta)
  )

# ..........................
# days_to_dispensing
# ..........................

Desc(medicamentos$dias_hasta_la_dispensacion)
unique(medicamentos$dias_hasta_la_dispensacion)

medicamentos <- medicamentos %>%
  mutate(
    med_dispensing_time = as.numeric(dias_hasta_la_dispensacion)
  )

# ..........................
# first_dispensing_visit_date
# ..........................

Desc(medicamentos$fecha_primer_consulta_dispensacion)
unique(medicamentos$fecha_primer_consulta_dispensacion)

medicamentos <- medicamentos %>%
  mutate(
    first_dispensing_visit_date = as.Date(fecha_primer_consulta_dispensacion)
  )

# ..........................
# pending_reason
# ..........................

Desc(medicamentos$motivo_pendiente)
unique(medicamentos$motivo_pendiente)

medicamentos <- medicamentos %>%
  mutate(
    pending_reason = factor(
      motivo_pendiente,
      levels = c(
        "LOGISTICA FARMACIA",
        "AGOTADO POR PROVEEDOR",
        "AGOTADO POR PROVEEDOR|LOGISTICA FARMACIA",
        "LOGISTICA FARMACIA|AGOTADO POR PROVEEDOR",
        "MEDICAMENTO AGOTADO"
      ),
      labels = c(
        "Pharmacy logistics",
        "Supplier stock-out",
        "Supplier stock-out | Pharmacy logistics",
        "Pharmacy logistics | Supplier stock-out",
        "Medication stock-out"
      )
    )
  )

# ..........................
# cancellation_reason
# ..........................

Desc(medicamentos$motivo_cancelacion)
unique(medicamentos$motivo_cancelacion)

medicamentos <- medicamentos %>%
  mutate(
    cancellation_reason = factor(
      motivo_cancelacion,
      levels = c(
        "ERROR EN RECETA",
        "PACIENTE NO DESEA EL MEDICAMENTO",
        "ERROR EN RECETA|PACIENTE NO DESEA EL MEDICAMENTO"
      ),
      labels = c(
        "Prescription error",
        "Patient declined medication",
        "Prescription error | Patient declined medication"
      )
    )
  )

# ..........................
# medication_cancelled
# ..........................

Desc(medicamentos$medicamento_cancelado)
unique(medicamentos$medicamento_cancelado)

medicamentos <- medicamentos %>%
  mutate(
    medication_cancelled = factor(
      medicamento_cancelado,
      levels = c(
        "Medicamento no cancelado",
        "Medicamento cancelado"
      ),
      labels = c("No", "Yes")
    )
  )

# ..........................
# dispensing_status
# ..........................

Desc(medicamentos$estado_de_la_dispensacion)
unique(medicamentos$estado_de_la_dispensacion)

medicamentos <- medicamentos %>%
  mutate(
    dispensing_status = factor(
      estado_de_la_dispensacion,
      levels = c(
        "Sin registro de dispensacion",
        "Dispensado",
        "Dispensado parcialmente",
        "Registrado como pendiente"
      ),
      labels = c(
        "No dispensing record",
        "Dispensed",
        "Partially dispensed",
        "Recorded as pending"
      )
    )
  )

medicamentos <- medicamentos %>%
  mutate(
    med_dispensed = factor(
      case_when(
        estado_de_la_dispensacion %in% c(
          "Dispensado",
          "Dispensado parcialmente"
        ) ~ "Yes",
        estado_de_la_dispensacion %in% c(
          "Sin registro de dispensacion",
          "Registrado como pendiente"
        ) ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    )
  )

# ..........................
# has_dispensing_record
# ..........................

Desc(medicamentos$tiene_registro_de_dispensacion)
unique(medicamentos$tiene_registro_de_dispensacion)

medicamentos <- medicamentos %>%
  mutate(
    has_dispensing_record = as.logical(tiene_registro_de_dispensacion)
  )

# ..........................
# branch_department
# ..........................

Desc(medicamentos$departamento_sucursal)
unique(medicamentos$departamento_sucursal)

medicamentos <- medicamentos %>%
  mutate(
    branch_department = departamento_sucursal
    )

# ..........................
# subset: keep only the new variables
# ..........................

new_vars <- c(
  "id",
  "visit_id",
  "visit_date",
  "med_date",
  "medication_code",
  "medication_description",
  "active_ingredient",
  "atc_code",
  "chronic_therapeutic_class",
  "controlled_medication",
  "quantity_prescribed",
  "dose",
  "frequency",
  "duration",
  "single_dose",
  "units",
  "route",
  "medication_status",
  "quantity_dispensed",
  "quantity_pending",
  "med_dispensing_date",
  "med_dispensing_time",
  "first_dispensing_visit_date",
  "pending_reason",
  "cancellation_reason",
  "medication_cancelled",
  "dispensing_status",
  "med_dispensed",
  "has_dispensing_record",
  "branch_department"
)

medicamentos <- medicamentos %>%
  select(all_of(new_vars))

# ..........................
# save
# ..........................

saveRDS(medicamentos, "medicin_clean.rds")

medicin_clean <- readRDS("medicin_clean.rds")



# =========================================.
# 5. servicios clinicos dataset ----
# =========================================.

servicios_clinicos <- read_parquet(
  "C:/Users/hgq/Box/AI Diagnostics/raw_data/05_servicios_clinicos.parquet"
)
names(servicios_clinicos)
Desc(servicios_clinicos$valor_del_resultado)

# ..........................
# id
# ..........................

Desc(servicios_clinicos$paciente_seudonimo)
unique(servicios_clinicos$paciente_seudonimo)

servicios_clinicos <- servicios_clinicos %>%
  rename(id = paciente_seudonimo)

# ..........................
# visit_id
# ..........................

Desc(servicios_clinicos$cita_seudonimo)
unique(servicios_clinicos$cita_seudonimo)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    visit_id = cita_seudonimo
  )

# ..........................
# test_type
# ..........................

Desc(servicios_clinicos$tipo_prueba)
unique(servicios_clinicos$tipo_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_gen_type = factor(
      tipo_prueba,
      levels = c("Laboratorio", "Imagen", "Procedimiento"),
      labels = c("Laboratory", "Imaging", "Procedure")
    )
  )

# ..........................
# test_category
# ..........................

Desc(servicios_clinicos$categoria_prueba)
unique(servicios_clinicos$categoria_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_category = factor(
      na_if(categoria_prueba, "Sin categoría"),
      levels = c(
        "QUÍMICA CLÍNICA",
        "HORMONALES",
        "UROANÁLISIS",
        "COPROLOGÍA",
        "ULTRASONOGRAFÍA",
        "RADIOGRAFÍA",
        "BACTERIOLOGÍA NO BAAR",
        "BACTERIOLOGÍA BAAR",
        "INMUNOLOGÍA",
        "HEMATOLOGÍA",
        "MARCADORES TUMORALES",
        "PRUEBAS DE COAGULACIÓN",
        "RESONANCIA MAGNÉTICA",
        "TOMOGRAFÍA",
        "ENFERMEDADES INFECCIOSAS",
        "MAMOGRAFÍA"
      ),
      labels = c(
        "Clinical chemistry",
        "Hormones",
        "Urinalysis",
        "Stool analysis",
        "Ultrasound",
        "X-ray",
        "Bacteriology (non-BAAR)",
        "Bacteriology (BAAR)",
        "Immunology",
        "Hematology",
        "Tumor markers",
        "Coagulation tests",
        "MRI",
        "CT scan",
        "Infectious diseases",
        "Mammography"
      )
    )
  )

# ..........................
# test_group
# ..........................

Desc(servicios_clinicos$grupo_prueba)
unique(servicios_clinicos$grupo_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_group = factor(
      na_if(grupo_prueba, "Sin categoría"),
      levels = c(
        "1. QUÍMICA",
        "2. HEMATOLOGÍA",
        "3. INMUNOHEMATOLOGÍA",
        "4. ORINA",
        "5. HECES",
        "6. BACTERIOLOGÍA",
        "1. RADIOLOGIA SIN CONTRASTE",
        "3.ULTRASONOGRAFIA",
        "TOMOGRAFÍA",
        "5.TOMOGRAFIA COMPUTARIZADA",
        "7.RESONANCIA MAGNETICA",
        "RESONANCIA MAGNÉTICA"
      ),
      labels = c(
        "1. Chemistry",
        "2. Hematology",
        "3. Immunohematology",
        "4. Urine",
        "5. Stool",
        "6. Bacteriology",
        "1. Radiology without contrast",
        "3. Ultrasound",
        "Tomography",
        "5. Computerized tomography",
        "7. Magnetic resonance",
        "Magnetic resonance"
      )
    )
  )

# ..........................
# test_description
# ..........................

Desc(servicios_clinicos$descripcion_prueba)
unique(servicios_clinicos$descripcion_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_description = descripcion_prueba
  )

# ..........................
# test_family
# ..........................

Desc(servicios_clinicos$familia_de_la_carta)
unique(servicios_clinicos$familia_de_la_carta)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_family = factor(
      familia_de_la_carta,
      levels = c(
        "OTRA PRUEBA",
        "PERFIL LIPIDICO",
        "GLUCEMIA Y CONTROL GLUCEMICO",
        "FUNCION RENAL",
        "PRESION ARTERIAL",
        "IMAGEN DE TORAX",
        "FUNCION PULMONAR"
      ),
      labels = c(
        "Other test",
        "Lipid profile",
        "Glucose and glycemic control",
        "Renal function",
        "Blood pressure",
        "Chest imaging",
        "Pulmonary function"
      )
    )
  )

# ..........................
# test_code
# ..........................

Desc(servicios_clinicos$codigo_prueba)
unique(servicios_clinicos$codigo_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_code = codigo_prueba
  )

# ..........................
# test_atc_code
# ..........................

Desc(servicios_clinicos$codigo_atc_prueba)
unique(servicios_clinicos$codigo_atc_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_atc_code = codigo_atc_prueba
  )

# ..........................
# test_quantity
# ..........................

Desc(servicios_clinicos$cantidad_prueba)
unique(servicios_clinicos$cantidad_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_quantity = as.numeric(cantidad_prueba)
  )

# ..........................
# order_status
# ..........................

Desc(servicios_clinicos$estado_orden)
unique(servicios_clinicos$estado_orden)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    order_status = factor(
      estado_orden,
      levels = c(
        "Liberada",
        "Parcial",
        "Pendiente",
        "Declinado",
        "Cancelada",
        "Caducada",
        "Completada",
        "Rechazado"
      ),
      labels = c(
        "Released",
        "Partial",
        "Pending",
        "Declined",
        "Cancelled",
        "Expired",
        "Completed",
        "Rejected"
      )
    )
  )

# ..........................
# test_status
# ..........................

Desc(servicios_clinicos$estado_prueba)
unique(servicios_clinicos$estado_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_status = factor(
      estado_prueba,
      levels = c(
        "Liberada",
        "Pendiente",
        "En proceso",
        "Programado",
        "Declinado",
        "Cancelado",
        "Rechazado",
        "Caducado"
      ),
      labels = c(
        "Released",
        "Pending",
        "In progress",
        "Scheduled",
        "Declined",
        "Cancelled",
        "Rejected",
        "Expired"
      )
    )
  )

# ..........................
# order_date
# ..........................

Desc(servicios_clinicos$fecha_generacion_prueba)
unique(servicios_clinicos$fecha_generacion_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_gen_date = as.Date(fecha_generacion_prueba)
  )

tail(servicios_clinicos[,c('fecha_generacion_prueba','fecha_solicitud_prueba')])

# ..........................
# request_date
# ..........................

Desc(servicios_clinicos$fecha_solicitud_prueba)
unique(servicios_clinicos$fecha_solicitud_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    request_date = as.Date(fecha_solicitud_prueba)
  )

# ..........................
# performed_date
# ..........................

Desc(servicios_clinicos$fecha_realizacion_prueba)
unique(servicios_clinicos$fecha_realizacion_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_perfomed_date = as.Date(fecha_realizacion_prueba)
  )

# ..........................
# result_delivery_date (3000-12-31 -> NA)
# ..........................

Desc(servicios_clinicos$fecha_entrega_resultado_prueba)
unique(servicios_clinicos$fecha_entrega_resultado_prueba)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    result_delivery_date = na_if(
      as.Date(fecha_entrega_resultado_prueba),
      as.Date("3000-12-31")
    )
  )

# ..........................
# was_performed
# ..........................

Desc(servicios_clinicos$fue_realizada)
unique(servicios_clinicos$fue_realizada)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    test_performed = as.logical(fue_realizada)
  )

# ..........................
# days_to_performance
# ..........................

Desc(servicios_clinicos$dias_hasta_la_realizacion) # 32% missing 
unique(servicios_clinicos$dias_hasta_la_realizacion)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    Test_performed_time = as.integer(dias_hasta_la_realizacion)
  )

# ..........................
# days_to_result_delivery (huge values -> NA)
# ..........................

Desc(servicios_clinicos$dias_hasta_la_entrega_del_resultado)
unique(servicios_clinicos$dias_hasta_la_entrega_del_resultado)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    days_to_result_delivery = case_when(
      as.numeric(dias_hasta_la_entrega_del_resultado) > 365 ~ NA_integer_,
      TRUE ~ as.integer(dias_hasta_la_entrega_del_resultado)
    )
  )

# ..........................
# result_value (100% NA — keep as-is)
# ..........................

Desc(servicios_clinicos$valor_del_resultado)
unique(servicios_clinicos$valor_del_resultado)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    result_value = valor_del_resultado
  )

# ..........................
# branch_department
# ..........................

Desc(servicios_clinicos$departamento_sucursal)
unique(servicios_clinicos$departamento_sucursal)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    branch_department = factor(departamento_sucursal)
  )

# ..........................
# branch_municipality
# ..........................

Desc(servicios_clinicos$municipio_sucursal)
unique(servicios_clinicos$municipio_sucursal)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    branch_municipality = factor(municipio_sucursal)
  )

# ..........................
# branch_type
# ..........................

Desc(servicios_clinicos$tipo_sucursal)
unique(servicios_clinicos$tipo_sucursal)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    branch_type = factor(
      na_if(str_to_upper(str_trim(tipo_sucursal)), "SUCURSAL INVÁLIDA"),
      levels = c(
        "LABORATORIO",
        "IMAGENES",
        "PROCEDIMIENTOS",
        "ATENCIONES PRESENCIA",
        "LABORATORIO CLINICO",
        "LABORATORIOS"
      ),
      labels = c(
        "Laboratory",
        "Imaging",
        "Procedures",
        "In-person care",
        "Clinical laboratory",
        "Laboratories"
      )
    )
  )

# ..........................
# meets_sla
# ..........................

Desc(servicios_clinicos$cumple_sla)
unique(servicios_clinicos$cumple_sla)

servicios_clinicos <- servicios_clinicos %>%
  mutate(
    meets_sla = factor(
      cumple_sla,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# ..........................
# subset: keep only the new variables
# ..........................

new_vars <- c(
  "id",
  "test_gen_type",
  "test_category",
  "test_group",
  "test_description",
  "test_family",
  "test_code",
  "test_atc_code",
  "test_quantity",
  "order_status",
  "test_status",
  "test_gen_date",
  "request_date",
  "test_perfomed_date",
  "result_delivery_date",
  "test_performed",
  "Test_performed_time",
  "days_to_result_delivery",
  "result_value",
  "branch_department",
  "branch_municipality",
  "branch_type",
  "meets_sla"
)

servicios_clinicos <- servicios_clinicos %>%
  select(all_of(new_vars))

# ..........................
# save
# ..........................

saveRDS(servicios_clinicos, "service_clean.rds")

service_clean <- readRDS("service_clean.rds")




# =========================================.
# 6. referencias dataset ----
# =========================================.

referencias <- read_parquet(
  "C:/Users/hgq/Box/AI Diagnostics/raw_data/06_referencias.parquet"
)

# ..........................
# id
# ..........................

Desc(referencias$paciente_seudonimo)
unique(referencias$paciente_seudonimo)

referencias <- referencias %>%
  mutate(
    id = paciente_seudonimo
  )

# ..........................
# visit_id
# ..........................

Desc(referencias$cita_seudonimo)
unique(referencias$cita_seudonimo)

referencias <- referencias %>%
  rename(visit_id = cita_seudonimo)

# ..........................
# visit_date
# ..........................

Desc(referencias$fecha_cita)
unique(referencias$fecha_cita)

referencias <- referencias %>%
  mutate(
    visit_date = as.Date(fecha_cita)
  )

# ..........................
# referral_generation_date
# ..........................

Desc(referencias$fecha_generacion_referencia)
unique(referencias$fecha_generacion_referencia)

referencias <- referencias %>%
  mutate(
    referral_gen_date = as.Date(fecha_generacion_referencia)
  )

# ..........................
# referral_date
# ..........................

Desc(referencias$fecha_referencia)
unique(referencias$fecha_referencia)

referencias <- referencias %>%
  mutate(
    referral_date = as.Date(fecha_referencia)
  )

# ..........................
# referral_valid_until
# ..........................

Desc(referencias$fecha_vigencia)
unique(referencias$fecha_vigencia)

referencias <- referencias %>%
  mutate(
    referral_valid_date = as.Date(fecha_vigencia)
  )

# ..........................
# referral_type
# ..........................

Desc(referencias$tipo_referencia)
unique(referencias$tipo_referencia)

referencias <- referencias %>%
  mutate(
    referral_type = factor(
      tipo_referencia,
      levels = c("Referencia Interna", "Referencia Externa"),
      labels = c("Internal", "External")
    )
  )

# ..........................
# referral_status
# ..........................

Desc(referencias$estado_referencia)
unique(referencias$estado_referencia)

Desc(referencias$referral_status)

referencias <- referencias %>%
  mutate(
    referral_status = recode(
      estado_referencia,
      "Creada"                          = "Created",
      "Cerrada/Concluida"               = "Closed/Completed",
      "En Evaluación"                   = "Under evaluation",
      "Pendiente De Evaluar"            = "Pending evaluation",
      "Cancelada"                       = "Cancelled",
      "Aprobada"                        = "Approved",
      "Declinada (No pertinente)"       = "Declined (not pertinent)",
      "Retorno Pendiente de Evaluar"    = "Return pending evaluation",
      "NA"                              = NA_character_
    ),
    referral_status = factor(
      referral_status,
      levels = c(
        "Created",
        "Closed/Completed",
        "Under evaluation",
        "Pending evaluation",
        "Cancelled",
        "Approved",
        "Declined (not pertinent)",
        "Return pending evaluation"
      )
    )
  )

Desc(referencias$referral_performed)

referencias <- referencias %>%
  mutate(
    referral_performed = factor(
      case_when(
        referral_status %in% c(
          "Closed/Completed",
          "Approved",
          "Return pending evaluation"
        ) ~ "Yes",
        referral_status %in% c(
          "Created",
          "Under evaluation",
          "Cancelled",
          "Pending evaluation"
        ) ~ "No",
        referral_status == "Declined (not pertinent)" ~ "Not applicable",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes", "Not applicable")
    )
  )

# View(referencias[,c('referral_performed', 'fecha_generacion_referencia')])
# 
# head(referencias[,c('fecha_cita', 
#                     'fecha_generacion_referencia', 'fecha_referencia', 'estado_referencia')])

# ..........................
# referred_specialty
# ..........................

Desc(referencias$especialidad_referida)
unique(referencias$especialidad_referida)

referencias <- referencias %>%
  mutate(
    referred_specialty = factor(especialidad_referida)
  )

# ..........................
# referred_area
# ..........................

Desc(referencias$area_referida)
unique(referencias$area_referida)

referencias <- referencias %>%
  mutate(
    referred_area = factor(
      area_referida,
      levels = c("Consulta Externa", "Emergencia"),
      labels = c("Outpatient", "Emergency")
    )
  )

# ..........................
# referred_facility
# ..........................

Desc(referencias$unidad_medica_referida)
unique(referencias$unidad_medica_referida)

referencias <- referencias %>%
  mutate(
    referred_facility = unidad_medica_referida
    )

# ..........................
# referral_reason
# ..........................

Desc(referencias$motivo_referencia)
unique(referencias$motivo_referencia)

referencias <- referencias %>%
  mutate(
    referral_reason = motivo_referencia
  )

# ..........................
# primary_diagnosis_code
# ..........................

Desc(referencias$codigo_diagnostico)
unique(referencias$codigo_diagnostico)

referencias <- referencias %>%
  mutate(
    primary_diagnosis_code = codigo_diagnostico
  )

# ..........................
# primary_diagnosis_desc
# ..........................

Desc(referencias$descripcion_diagnostico)
unique(referencias$descripcion_diagnostico)

referencias <- referencias %>%
  mutate(
    primary_diagnosis_desc = descripcion_diagnostico
  )

# ..........................
# immediate_referral
# ..........................

Desc(referencias$cita_referencia_inmediata)
unique(referencias$cita_referencia_inmediata)

referencias <- referencias %>%
  mutate(
    immediate_referral = factor(
      cita_referencia_inmediata,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# ..........................
# referring_physician_specialty
# ..........................

Desc(referencias$especialidad_medico)
unique(referencias$especialidad_medico)

referencias <- referencias %>%
  mutate(
    referring_physician_specialty = factor(
      especialidad_medico,
      levels = c(
        "MEDICINA GENERAL",
        "MEDICINA METABÓLICA",
        "GINECOLOGIA Y OBSTETRICIA",
        "MEDICINA INTERNA",
        "NUTRICIÓN",
        "SALUD MENTAL",
        "PEDIATRIA"
      ),
      labels = c(
        "General medicine",
        "Metabolic medicine",
        "Obstetrics & gynecology",
        "Internal medicine",
        "Nutrition",
        "Mental health",
        "Pediatrics"
      )
    )
  )

# ..........................
# subset: keep only the new variables
# ..........................

new_vars <- c(
  "id",
  "visit_date",
  "referral_gen_date",
  "referral_date",
  "referral_valid_date",
  "referral_type",
  "referral_status",
  "referral_performed",
  "referred_specialty",
  "referred_area",
  "referred_facility",
  "referral_reason",
  "primary_diagnosis_code",
  "primary_diagnosis_desc",
  "immediate_referral",
  "referring_physician_specialty"
)

referencias <- referencias %>%
  select(all_of(new_vars))

# ..........................
# save
# ..........................

saveRDS(referencias, "referen_clean.rds") # save it in project folder. 

referen_clean <- readRDS("referen_clean.rds")

#...........................................
# checking variables 
#...........................................

# ...................................
# medicine 
# ...................................

med_gen_date # fecha_generacion_receta
med_dispensed # recategorized from estado_de_la_dispensacion
med_dispensing_date # fecha_dispensacion_receta
med_dispensing_time # dias_hasta_la_dispensacion

# checking
Desc(medicamentos$fecha_cita)
Desc(medicamentos$fecha_generacion_receta)
Desc(medicamentos$fecha_dispensacion_receta)
Desc(medicamentos$estado_de_la_dispensacion)

tail(medicamentos[,c('fecha_cita', 'fecha_generacion_receta', 'dias_hasta_la_dispensacion', 'fecha_dispensacion_receta')])

identical(medicamentos$fecha_cita,
          medicamentos$fecha_generacion_receta) # generation is after 

medicamentos %>%
  filter(fecha_cita != fecha_generacion_receta |
           is.na(fecha_cita) != is.na(fecha_generacion_receta)) %>%
  select(paciente_seudonimo, fecha_cita, fecha_generacion_receta, 
         fecha_dispensacion_receta, dias_hasta_la_dispensacion) %>%
  tail(50)

# ...................................
# services data
# ...................................

test_gen_date # fecha_generacion_prueba
test_gen_type # tipo_prueba
test_performed # fue_realizada ; estado_prueba is from all performed only
test_perfomed_date # fecha_realizacion_prueba
Test_performed_time # dias_hasta_la_realizacion

# checking 

Desc(servicios_clinicos$fecha_generacion_prueba) # 2'691'403
Desc(servicios_clinicos$fecha_solicitud_prueba) # 1'809'074
Desc(servicios_clinicos$fecha_realizacion_prueba)# 1'809'074 all performed 

Desc(servicios_clinicos$tipo_prueba) # 2'691'403 total performed & non performed 
Desc(servicios_clinicos$estado_prueba) # 1'809'074 detail of all performed 
Desc(servicios_clinicos$fue_realizada) # 1'809'074 all performed

sum(duplicated(servicios_clinicos$paciente_seudonimo))

head(servicios_clinicos[,c('fecha_generacion_prueba', 'fecha_solicitud_prueba', 
                           'fecha_realizacion_prueba', 'fue_realizada')])

identical(servicios_clinicos$fecha_generacion_prueba,
          servicios_clinicos$fecha_solicitud_prueba) # generation is after 

servicios_clinicos %>%
  filter(fecha_generacion_prueba != fecha_solicitud_prueba |
           is.na(fecha_generacion_prueba) != is.na(fecha_solicitud_prueba)) %>%
  select(paciente_seudonimo, cita_seudonimo,
         fecha_generacion_prueba, fecha_solicitud_prueba) %>%
  head(200)

# ...................................
# reference
# ...................................

referral_gen_date # fecha_generacion_referencia = fecha_referencia
referral_status # estado_referencia
referral_performed # yes/no based on estado_referencia
referral_performed_date # ???
referral_performed_time # ????

Desc(referencias$fecha_generacion_referencia)
Desc(referencias$estado_referencia)



head(referencias[,c('fecha_generacion_referencia', 'fecha_referencia', 'fecha_cita')])

identical(referencias$fecha_generacion_referencia,
          referencias$fecha_referencia) # true

identical(referencias$fecha_generacion_referencia,
          referencias$fecha_cita) # true


referencias %>%
  filter(
    fecha_cita != fecha_generacion_referencia |
      fecha_cita != fecha_referencia |
      fecha_generacion_referencia != fecha_referencia
  ) %>%
  select(paciente_seudonimo, cita_seudonimo,
         fecha_cita, fecha_generacion_referencia, fecha_referencia) %>%
  head(50)

"education_cat"

# education_level
# 
# pruebas_solicitadas_total
# pruebas_realizadas_total
# pruebas_perfil_lipidico_solicitadas
# pruebas_perfil_lipidico_realizadas
# pruebas_glucemia_solicitadas
# pruebas_glucemia_realizadas
# pruebas_funcion_renal_solicitadas
# pruebas_funcion_renal_realizadas
# pruebas_funcion_pulmonar_solicitadas
# pruebas_funcion_pulmonar_realizadas
# pruebas_presion_arterial_solicitadas
# pruebas_presion_arterial_realizadas
# lineas_recetadas
# lineas_dispensadas
# recetas_distintas



DoctorSV_NCD_df <- patient_clean %>%
  filter(cohort == "DoctorSV NCD")

Desc(DoctorSV_NCD_df$age_at_enrollment)


# =========================================.
# retention ----
# =========================================.

# create retention variable

# ---------------------------------------------.
# r1. open data ----
# ---------------------------------------------.

patient_clean <- readRDS("patient_clean.rds")
chronic_clean <- readRDS("chronic_clean.rds")
appoint_clean <- readRDS("appoint_clean.rds")
medicin_clean <- readRDS("medicin_clean.rds")
service_clean <- readRDS("service_clean.rds")
referen_clean <- readRDS("referen_clean.rds")


library(dplyr)
# patient per n of visits
bmi_visit_order %>%
  mutate(first_bmi_visit_cat = case_when(
    is.na(first_bmi_visit) ~ "Never",
    first_bmi_visit >= 5   ~ "5th or later",
    TRUE                   ~ paste0("Visit ", first_bmi_visit)
  )) %>%
  count(first_bmi_visit_cat) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# ---------------------------------------------.
# r2. Running variable ----
# ---------------------------------------------.
names(appoint_clean)
head(appoint_clean)

# BMI 
# getting Earliest non-missing BMI per patient
earliest_bmi <- appoint_clean %>%
  filter(!is.na(visit_bmi), !is.na(visit_date)) %>%
  group_by(id) %>%
  slice_min(visit_date, n = 1, with_ties = FALSE) %>%
  select(id, visit_date, visit_bmi) %>%
  ungroup()

Desc(earliest_bmi$visit_bmi)
Desc(earliest_bmi$visit_date)

earliest_bmi <- earliest_bmi %>%
  rename(
    earliest_BMI_date = visit_date,
    earliest_BMI      = visit_bmi
  )

df_disc <- merge(patient_clean, earliest_bmi, by = "id", all.x = TRUE)


# ---------------------------------------------..
# r3. outcome ----
# ---------------------------------------------.
Desc(df_disc$id)# 186'252
Desc(df_disc$completed_visits)
Desc(patient_flag$visit_retained)

# retention visits
patient_flag <- patient_clean %>%
  group_by(id) %>%
  summarise(
    visit_retained = {
      if (all(is.na(completed_visits))) NA
      else any(completed_visits >= 3, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  mutate(
    visit_retained = factor(
      ifelse(visit_retained, "yes", "no"),
      levels = c("yes", "no")
    )
  )


# retention medicine
Desc(medicin_clean$id)# 337'343
Desc(medicin_clean$med_dispensed)
Desc(medicin_flag$med_retained)# 337'343

medicin_flag <- medicin_clean %>%
  group_by(id) %>%
  summarise(
    med_retained = any(med_dispensed == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    med_retained = factor(
      ifelse(med_retained, "yes", "no"),
      levels = c("yes", "no")
    )
  )

# retention services
Desc(service_clean$id)# 306'396 unique
Desc(service_clean$test_performed)
Desc(service_flag$test_retained)# 306'396 
Desc(service_clean$test_quantity)
Desc(service_clean$test_gen_type)
Desc(service_clean$test_performed)


# 2 or more test, 1 or more lab or procedure, use | (or) so just one criteria is necesary
# if no order requested then retained = NA (not applicable or missing) 

yn <- function(x) factor(ifelse(x, "yes", "no"), levels = c("yes", "no"))

service_flag <- service_clean %>%
  group_by(id) %>%
  summarise(
    # ordered
    n_lab_ordered         = sum(test_gen_type == "Laboratory"),
    n_imaging_ordered     = sum(test_gen_type == "Imaging"),
    n_procedure_ordered   = sum(test_gen_type == "Procedure"),
    # performed
    n_lab_performed       = sum(test_gen_type == "Laboratory" & test_performed %in% TRUE),
    n_imaging_performed   = sum(test_gen_type == "Imaging"    & test_performed %in% TRUE),
    n_procedure_performed = sum(test_gen_type == "Procedure"  & test_performed %in% TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # one retention variable per type (NA if none of that type ordered)
    lab_retained       = if_else(n_lab_ordered       > 0, n_lab_performed       >= 2, NA),
    imaging_retained   = if_else(n_imaging_ordered   > 0, n_imaging_performed   >= 1, NA),
    procedure_retained = if_else(n_procedure_ordered > 0, n_procedure_performed >= 1, NA),
    # overall: retained in any type
    test_retained      = n_lab_performed >= 2 | n_imaging_performed >= 1 | n_procedure_performed >= 1,
    across(c(lab_retained, imaging_retained, procedure_retained), yn)
  ) %>%
  select(id, lab_retained, imaging_retained, procedure_retained)

# retention referrals
Desc(referen_clean$referral_performed)# the only one that has not applicable and nas than will work as no 
Desc(referen_flag$referral_retained)

referen_flag <- referen_clean %>%
  group_by(id) %>%
  summarise(
    referral_retained = case_when(
      any(str_to_lower(as.character(referral_performed)) == "yes", na.rm = TRUE) ~ TRUE,
      all(is.na(referral_performed) | referral_performed == "Not applicable") ~ NA,
      TRUE ~ FALSE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    referral_retained = factor(
      ifelse(referral_retained, "yes", "no"),
      levels = c("yes", "no")
    )
  )

Desc(referen_flag$referral_retained)

retention_joined <- patient_flag %>%
  left_join(medicin_flag,  by = "id") %>%
  left_join(service_flag,  by = "id") %>%
  left_join(referen_flag,  by = "id")


df_disc <- df_disc %>%
  left_join(retention_joined, by = "id")


Desc(df_disc$visit_retained)# yes/no 90/9 0% na
Desc(df_disc$med_retained)# 67/32 2% na 
Desc(df_disc$test_retained)# 93 / 6    6% na
Desc(df_disc$referral_retained)# 31/68 46%


# r.4. save---- 
saveRDS(df_disc, file = "df_disc.rds")
df_disc <- readRDS("df_disc.rds")
Desc(df_disc$request_date)


# check by bmi & cohort
Desc(df_disc$earliest_BMI)
Desc(df_disc$earliest_BMI_date)

df_disc %>%
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
  )


Desc(df_new$earliest_BMI_date)
Desc(df_new$followup_days)

# ---------------------------------------------.
# filters ----
# ---------------------------------------------.

# 1 Include no chronic patients or with obesity
# df_disc$diabetes == 0
# df_disc$diabetes_type == 0
# df_disc$hypertension == 0 
# df_disc$CKD == 0
# df_disc$dyslipidemia == 0
# df_disc$asthma == 0
# df_disc$COPD == 0
# df_disc$other_chronic_respiratory_disease == 0

# 2 remove age_at_enrollment < 18
# 3 remove missing values in earliest_BMI or earliest_BMI_date
# 4 include only in plausibility limits df_disc$earliest_BMI between (12, 70) 
# 5 include only those with follow-up 30 days # 2026-09-14 - earliest_BMI_date == 30 


# ============================================================.
# Sequential exclusion — cumulative removal flow
# Data: df_disc
# ============================================================.

library(dplyr)

# --- Snapshot starting N ---
n_start <- nrow(df_disc)
cat("Starting N (df_disc):", n_start, "\n\n")

# Running dataset
df_step <- df_disc

# --- Helper: report each step, keep cumulative log ---
removal_log <- data.frame(
  Step        = character(),
  Label       = character(),
  N_before    = integer(),
  Removed     = integer(),
  Pct_removed = numeric(),
  N_after     = integer(),
  stringsAsFactors = FALSE
)

report_step <- function(old_df, new_df, label, step_num) {
  n_old   <- nrow(old_df)
  n_new   <- nrow(new_df)
  removed <- n_old - n_new
  pct     <- round(100 * removed / n_old, 2)
  
  cat(sprintf("Step %d | %-55s | N before = %6d | removed = %6d (%5.2f%%) | N after = %6d\n",
              step_num, label, n_old, removed, pct, n_new))
  
  removal_log <<- rbind(
    removal_log,
    data.frame(
      Step        = step_num,
      Label       = label,
      N_before    = n_old,
      Removed     = removed,
      Pct_removed = pct,
      N_after     = n_new
    )
  )
  invisible(new_df)
}

# ------------------------------------------------------------.
# f.1. Exclude patients with any chronic condition OTHER than obesity
#    (keep only patients whose diabetes, hypertension, CKD,
#     dyslipidemia, asthma, COPD, other_resp are ALL == "0")
# ------------------------------------------------------------.
df_next <- df_step %>%
  filter(
    diabetes == "0",
    hypertension == "0",
    CKD == "0",
    dyslipidemia == "0",
    asthma == "0",
    COPD == "0",
    other_chronic_respiratory_disease == "0"
  )

df_step <- report_step(df_step, df_next,
                       "1. Exclude any chronic condition other than obesity",
                       step_num = 1)

# ------------------------------------------------------------.
# f.2. Remove age_at_enrollment < 18 (or NA)
# ------------------------------------------------------------.
df_next <- df_step %>%
  filter(
    !is.na(age_at_enrollment),
    age_at_enrollment >= 18
  )

df_step <- report_step(df_step, df_next,
                       "2. Remove age_at_enrollment < 18 (or NA)",
                       step_num = 2)

# ------------------------------------------------------------.
# f.3. Remove missing earliest_BMI / earliest_BMI_date
# ------------------------------------------------------------.
df_next <- df_step %>%
  filter(
    !is.na(earliest_BMI),
    !is.na(earliest_BMI_date)
  )

df_step <- report_step(df_step, df_next,
                       "3. Remove missing earliest_BMI / earliest_BMI_date",
                       step_num = 3)

# ------------------------------------------------------------.
# f.4. Keep only plausible BMI (12 < BMI < 70)
# ------------------------------------------------------------.
df_next <- df_step %>%
  filter(
    earliest_BMI > 12,
    earliest_BMI < 70
  )

df_step <- report_step(df_step, df_next,
                       "4. Keep only BMI in (12, 70)",
                       step_num = 4)



# ------------------------------------------------------------.
# f.5a. Keep only those with follow-up >= 30 days
#     Follow-up = 2026-09-14 - earliest_BMI_date
# ------------------------------------------------------------.
cutoff_date <- as.Date("2026-09-14")

df_ge30 <- df_step %>%
  mutate(
    followup_days = as.numeric(cutoff_date - as.Date(earliest_BMI_date))
  ) %>%
  filter(followup_days >= 30)

df_step1 <- report_step(df_step, df_ge30,
                        "5a. Keep only follow-up >= 30 days",
                        step_num = 5)

Desc(df_ge30$id)
Desc(df_30_60$id)

# ------------------------------------------------------------.
# f.5b. Keep only those with follow-up between 30 and 60 days
#     Follow-up = 2026-09-14 - earliest_BMI_date
# ------------------------------------------------------------.
df_30_60 <- df_step %>%
  mutate(
    followup_days = as.numeric(cutoff_date - as.Date(earliest_BMI_date))
  ) %>%
  filter(followup_days >= 30,
         followup_days <= 60)

df_step2 <- report_step(df_step, df_30_60,
                        "5b. Keep only follow-up between 30 and 60 days",
                        step_num = 6)


# ------------------------------------------------------------.
# Final dataset
# ------------------------------------------------------------.
df_final <- df_step2

cat("\n=============================\n")
cat("Final N:", nrow(df_final), "\n")
cat("Total removed:", n_start - nrow(df_final),
    sprintf("(%.2f%%)\n", 100 * (n_start - nrow(df_final)) / n_start))
cat("=============================\n")


# ------------------------------------------------------------.
# Cumulative removal table
# ------------------------------------------------------------.

removal_log <- rbind(
  data.frame(
    Step        = 0,
    Label       = "Starting N (df_disc)",
    N_before    = n_start,
    Removed     = 0L,
    Pct_removed = 0,
    N_after     = n_start
  ),
  removal_log
)


print(removal_log, row.names = FALSE)


# ------------------------------------------------------------.
# Checking
# ------------------------------------------------------------.
Desc(df_step$visit_retained)
Desc(df_step$med_retained)
Desc(df_step$test_retained)
Desc(df_step$referral_retained)
Desc(df_step$retained_all)
Desc(df_step$cohort)
Desc(df_step$earliest_BMI_date)

# ------------------------------------------------------------.
# f.Save ----
# ------------------------------------------------------------.
saveRDS(df_ge30,  "df_ge30.rds")
saveRDS(df_30_60, "df_30_60.rds")








