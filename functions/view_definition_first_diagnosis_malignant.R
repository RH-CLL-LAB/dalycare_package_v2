view_definition_first_diagnosis_malignant = function(){
  #' @title
  #' view_definition_first_diagnosis_malignant
  #' @author
  #' christian brieghel (SQL view); R-version 2026
  #' @description
  #' Recreates the SQL view "view_definition_first_diagnosis_malignant" as
  #' an R function. Depends on view_definition_first_diagnosis() -- source
  #' that file first (load_functions.R already does).
  #'
  #' For patients whose overall first diagnosis (per
  #' view_definition_first_diagnosis()) is one of the two precursor/
  #' preliminary MDS codes DD479B or DD472, this looks for that patient's
  #' next diagnosis of ANY kind, dated after the precursor diagnosis, and
  #' replaces (adjusts) their recorded first diagnosis with that one
  #' instead -- so a preliminary MDS code is not counted as the definitive
  #' first diagnosis if a later, more specific diagnosis exists. Patients
  #' whose first diagnosis is anything else are returned unchanged.
  #'
  #' @examples
  #' FIRST_DX_MALIGNANT = view_definition_first_diagnosis_malignant()

  load_dataset('all_dalycare_diagnoses') # NB 2026-09-14: renamed from t_dalycare_diagnoses
  FIRST_DX = view_definition_first_diagnosis()

  precursor_codes = c('DD479B', 'DD472')

  first_diagnoses = FIRST_DX %>%
    filter(first_diagnosis %in% precursor_codes) %>%
    select(patientid, first_diagnosis, first_diagnosis_date)

  subsequent_diagnoses = first_diagnoses %>%
    left_join(all_dalycare_diagnoses %>%
                transmute(patientid, date_diagnosis = as_date(date_diagnosis), diagnosis),
              by = 'patientid') %>%
    filter(date_diagnosis > first_diagnosis_date) %>%
    group_by(patientid, first_diagnosis, first_diagnosis_date) %>%
    summarise(next_diagnosis_date = min(date_diagnosis, na.rm = TRUE),
              next_diagnosis = min(diagnosis[date_diagnosis == min(date_diagnosis, na.rm = TRUE)]),
              .groups = 'drop')

  adjusted_diagnosis = first_diagnoses %>%
    left_join(subsequent_diagnoses, by = c('patientid', 'first_diagnosis', 'first_diagnosis_date')) %>%
    transmute(patientid,
              first_diagnosis = coalesce(next_diagnosis, first_diagnosis),
              first_diagnosis_date = coalesce(next_diagnosis_date, first_diagnosis_date))

  FIRST_DX %>%
    filter(!patientid %in% adjusted_diagnosis$patientid) %>%
    select(patientid, first_diagnosis, first_diagnosis_date) %>%
    bind_rows(adjusted_diagnosis)
}
