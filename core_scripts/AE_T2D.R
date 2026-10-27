## AE_T2D
# Type 2 diabetes (T2D) onset for the CORE table all_comorbidity_and_ae (see table_info/all_comorbidity_and_ae.yaml)
# Based on T2D_2_All_patients.R (all patients, no restriction to a diagnosis).
#
# The logic lives in build_AE_T2D(), which gets the data as arguments. run_AE_T2D() loads the data and calls
# it. Nothing runs when this file is sourced; use
#   T2D = run_AE_T2D()
# The table is not saved here: writing to core/ is done by the core pipeline.
#
# Output has the shape of all_comorbidity_and_ae (event_type 'T2D onset', one event per patient):
#   patientid, event_id, start_date, end_date, duration_days, event_value, max_value, event_type
# start_date is the first T2D diagnosis (ICD-10 DE11) or the first anti-diabetic prescription (ATC A10B),
# whichever comes first. end_date and duration_days are empty, event_value and max_value are 1.

#### Build ####
#' T2D onset per patient.
#' 1) Candidates: a diagnosis starting with DE11 or a prescription of an anti-diabetic drug (ATC A10B).
#' 2) Excluded: women aged 20-39 at a metformin (A10BA02) prescription with no other A10B drug and no DE11
#'    diagnosis (probably PCOS); insulin (A10A) or a DE10 diagnosis before the age of 30 (T1D); a DE10
#'    diagnosis before the T2D onset when no non-insulin anti-diabetic drug was ever prescribed.
#' 3) Kept only if the patient has both a first diagnosis date and a first prescription date (the
#'    condition t2d_at_cll_diagnosis in the old script, which with missing dates removes patients that have
#'    only a diagnosis or only a prescription).
#' @param all_patients patientid, sex ('F'), date_birth
#' @param diagnoses patientid, date_diagnosis, diagnosis (view_diagnoses_all_distinct())
#' @param prescriptions patientid, eksd, atc (SDS_epikur and SDS_ekokur)
build_AE_T2D = function(all_patients, diagnoses, prescriptions){
  patients = all_patients
  prescriptions = prescriptions %>% mutate(eksd = as.Date(eksd))

  # T2D identification core
  t2d_candidates = patients %>%
    left_join(
      diagnoses %>%
        filter(str_starts(diagnosis, 'DE11')) %>%
        group_by(patientid) %>%
        summarise(first_diagnosis_date = min(date_diagnosis, na.rm = TRUE)),
      by = 'patientid') %>%
    left_join(
      prescriptions %>%
        filter(str_starts(atc, 'A10B')) %>%
        group_by(patientid) %>%
        summarise(first_med_date = min(eksd, na.rm = TRUE)),
      by = 'patientid') %>%
    filter(!is.na(first_diagnosis_date) | !is.na(first_med_date))

  # Metformin exclusion
  metformin_excl = prescriptions %>%
    inner_join(patients %>% filter(sex == 'F') %>% select(patientid, date_birth), by = 'patientid') %>%
    filter(atc == 'A10BA02') %>%
    mutate(age_at_prescription = interval(date_birth, eksd) / years(1)) %>%
    filter(between(age_at_prescription, 20, 39)) %>%
    anti_join(prescriptions %>% filter(str_starts(atc, 'A10B') & atc != 'A10BA02') %>% select(patientid),
              by = 'patientid') %>%
    anti_join(diagnoses %>% filter(str_starts(diagnosis, 'DE11')) %>% distinct(patientid),
              by = 'patientid') %>%
    distinct(patientid)

  t2d_cases = t2d_candidates %>%
    anti_join(metformin_excl, by = 'patientid')

  # T1D exclusion
  t1d_exclusions = bind_rows(
    prescriptions %>%
      filter(str_starts(atc, 'A10A')) %>% # insulin
      inner_join(patients, by = 'patientid') %>%
      mutate(age = interval(date_birth, eksd) / years(1)) %>%
      filter(age < 30) %>%
      distinct(patientid),
    diagnoses %>%
      filter(str_starts(diagnosis, 'DE10')) %>%
      inner_join(patients, by = 'patientid') %>%
      mutate(age = interval(date_birth, date_diagnosis) / years(1)) %>%
      filter(age < 30) %>%
      distinct(patientid)) %>%
    distinct(patientid)

  # T1D before T2D and no non-insulin anti-diabetic drug
  t1d_before_t2d_no_meds = diagnoses %>%
    filter(str_starts(diagnosis, 'DE10')) %>%
    inner_join(t2d_cases %>% select(patientid, t2d_diag_date = first_diagnosis_date, t2d_med_date = first_med_date),
               by = 'patientid') %>%
    mutate(t2d_onset_date = pmin(t2d_diag_date, t2d_med_date, na.rm = TRUE)) %>%
    filter(date_diagnosis < t2d_onset_date) %>%
    anti_join(prescriptions %>% filter(str_starts(atc, 'A10B')) %>% distinct(patientid), by = 'patientid') %>%
    distinct(patientid)

  final_exclusions = bind_rows(t1d_exclusions, t1d_before_t2d_no_meds) %>% distinct(patientid)

  t2d_cases %>%
    anti_join(final_exclusions, by = 'patientid') %>%
    filter(coalesce(first_diagnosis_date <= first_med_date | first_med_date <= first_diagnosis_date, FALSE)) %>%
    transmute(patientid,
              event_id = 1,
              start_date = as.Date(pmin(first_diagnosis_date, first_med_date)),
              end_date = as.Date(NA),
              duration_days = as.numeric(NA),
              event_value = 1,
              max_value = 1,
              event_type = 'T2D onset') %>%
    arrange(patientid)
}

#### Run ####
# Loads the data for all patients in all_patients and builds the T2D events. The dalycare package must be
# loaded first. Nothing runs when this file is sourced.
run_AE_T2D = function(){
  load_dataset('all_patients')
  load_dataset('SDS_epikur')
  load_dataset('SDS_ekokur')
  diagnoses = view_diagnoses_all_distinct()
  # the old script used SDS_epikur twice (union(SDS_epikur, SDS_epikur)); SDS_ekokur is added here
  prescriptions = union(SDS_epikur %>% select(patientid, eksd, atc),
                        SDS_ekokur %>% select(patientid, eksd, atc))
  build_AE_T2D(all_patients %>% select(patientid, sex, date_birth), diagnoses, prescriptions)
}
