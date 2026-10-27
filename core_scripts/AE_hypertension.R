## AE_hypertension
# Hypertension events for the CORE table all_comorbidity_and_ae (see table_info/all_comorbidity_and_ae.yaml)
# Based on hypertension_final_all_patients.R (Noomi).
#
# The logic lives in build_AE_hypertension(), which gets the data as arguments. run_AE_hypertension() loads
# the data and calls it. Nothing runs when this file is sourced; use
#   hypertension = run_AE_hypertension()
# The table is not saved here: writing to core/ is done by the core pipeline.
#
# Output has the shape of all_comorbidity_and_ae (event_type 'hypertension event', one event per patient):
#   patientid, event_id, start_date, end_date, duration_days, event_value, max_value, event_type
# Only hypertension before the first malignant diagnosis is found, so the table covers the patients in
# view_definition_first_diagnosis_malignant(). start_date is the earliest date of any of the three conditions.
# end_date and duration_days are empty, event_value and max_value are 1.

#### ATC codes ####
# First-choice antihypertensives
hypertension_first_atc_codes = c(
  'C03AA01', 'C03AA03', 'C03AB01', 'C07CB03', 'C07BB02', 'C07BB12', 'C07FB02',
  'C08CA01', 'C08CA02', 'C08CA03', 'C08CA05', 'C08CA06', 'C08CA08',
  'C08CA09', 'C08CA13', 'C08DA01', 'C08DA51', 'C08DB01', 'C09AA01',
  'C09AA02', 'C09AA03', 'C09AA04', 'C09AA05', 'C09AA06', 'C09AA07',
  'C09AA09', 'C09AA10', 'C09BA01', 'C09BA02', 'C09BA03', 'C09BA04',
  'C09BA05', 'C09BB02', 'C09BB04', 'C09CA01', 'C09CA02', 'C09CA03',
  'C09CA04', 'C09CA06', 'C09CA07', 'C09CA08', 'C09DA01', 'C09DA02',
  'C09DA03', 'C09DA04', 'C09DA06', 'C09DA07', 'C09DA08', 'C09DB01',
  'C09DX01', 'C09XA02', 'C09XA52')
# Other antihypertensives
hypertension_other_atc_codes = c(
  'C02AB01', 'C02AC05', 'C02CA01', 'C02CA04',
  'C03BA03', 'C03BA08', 'C03BA11', 'C03DA01', 'C03DA04',
  'C03DB01', 'C07AA03', 'C07AA05', 'C07AA06', 'C07AB02',
  'C07AB03', 'C07AB04', 'C07AB05', 'C07AB07', 'C07AB12',
  'C07AG01', 'C07AG02')

#### Build ####
#' Hypertension per patient, before the first malignant diagnosis. One of these conditions is enough:
#' 1) diagnosis codes DI10-DI13 or DI15 on at least 2 dates, at least 7 days apart
#' 2) at least 2 prescriptions of first-choice antihypertensives, at least one within 365 days before the diagnosis
#' 3) prescriptions of at least 2 different ATC groups (3 characters) of antihypertensives within 365 days
#'    before the diagnosis
#' @param diagnoses patientid, date_diagnosis, diagnosis (view_diagnoses_all_distinct())
#' @param prescriptions patientid, eksd, atc (SDS_epikur)
#' @param first_diagnosis patientid, first_diagnosis_date (view_definition_first_diagnosis_malignant())
build_AE_hypertension = function(diagnoses, prescriptions, first_diagnosis){
  first_diagnosis = first_diagnosis %>% select(patientid, first_diagnosis_date) %>% mutate(first_diagnosis_date = as.Date(first_diagnosis_date))
  prescriptions = prescriptions %>% mutate(eksd = as.Date(eksd))

  # Condition 1: ICD-10 codes at least 7 days apart
  icd10_condition = diagnoses %>%
    inner_join(first_diagnosis, by = 'patientid') %>%
    filter(str_sub(diagnosis, 1, 4) %in% c('DI10', 'DI11', 'DI12', 'DI13', 'DI15'),
           date_diagnosis < first_diagnosis_date) %>%
    group_by(patientid) %>%
    filter(n_distinct(date_diagnosis) >= 2,
           as.numeric(max(date_diagnosis) - min(date_diagnosis)) >= 7) %>%
    distinct(patientid, date_diagnosis) %>%
    rename(date_event = date_diagnosis)

  # Condition 2: at least 2 first-choice prescriptions, at least one within 365 days before the diagnosis
  condition2 = prescriptions %>%
    inner_join(first_diagnosis, by = 'patientid') %>%
    filter(atc %in% hypertension_first_atc_codes,
           eksd < first_diagnosis_date) %>%
    mutate(days_before_diagnosis = as.numeric(first_diagnosis_date - eksd)) %>%
    group_by(patientid) %>%
    filter(n() >= 2,
           any(days_before_diagnosis < 365)) %>%
    distinct(patientid, eksd) %>%
    rename(date_event = eksd)

  # Condition 3: at least 2 different ATC groups within 365 days before the diagnosis
  condition3 = prescriptions %>%
    inner_join(first_diagnosis, by = 'patientid') %>%
    filter(atc %in% c(hypertension_first_atc_codes, hypertension_other_atc_codes),
           eksd < first_diagnosis_date,
           as.numeric(first_diagnosis_date - eksd) < 365) %>%
    mutate(atc_group = substr(atc, 1, 3)) %>%
    group_by(patientid) %>%
    filter(n_distinct(atc_group) >= 2) %>%
    distinct(patientid, eksd) %>%
    rename(date_event = eksd)

  bind_rows(icd10_condition, condition2, condition3) %>%
    ungroup() %>%
    distinct(patientid, date_event) %>%
    group_by(patientid) %>%
    summarise(start_date = as.Date(min(date_event)), .groups = 'drop') %>%
    transmute(patientid,
              event_id = 1,
              start_date,
              end_date = as.Date(NA),
              duration_days = as.numeric(NA),
              event_value = 1,
              max_value = 1,
              event_type = 'hypertension event') %>%
    arrange(patientid)
}

#### Run ####
# Loads the data and builds the hypertension events. The dalycare package must be loaded first. Nothing
# runs when this file is sourced.
run_AE_hypertension = function(){
  load_dataset('SDS_epikur')
  build_AE_hypertension(diagnoses = view_diagnoses_all_distinct(),
                        prescriptions = SDS_epikur %>% select(patientid, eksd, atc),
                        first_diagnosis = view_definition_first_diagnosis_malignant())
}
