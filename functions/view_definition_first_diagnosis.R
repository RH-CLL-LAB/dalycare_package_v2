view_definition_first_diagnosis = function(){
  #' @title
  #' view_definition_first_diagnosis
  #' @author
  #' christian brieghel (SQL view); R-version 2026
  #' @description
  #' Recreates the SQL view "view_definition_first_diagnosis" as an R function.
  #' Views do not exist in the parquet release -- load_dataset() only reads
  #' physical .parquet/.csv files. Call this function instead of
  #' `load_dataset("view_definition_first_diagnosis")`.
  #'
  #' For each patient, picks ONE diagnosis row from all_dalycare_diagnoses,
  #' ranked first by source priority (PATO=1, SUNDK=2, SDS=3, SP=4,
  #' PERSIMUNE=5, else=6), then by earliest date_diagnosis among rows tied
  #' on that priority. This is NOT the same selection rule as
  #' filter_first_diagnosis(), which currently ranks by date alone.
  #'
  #' Also flags patients whose selected first diagnosis is contradicted by
  #' an earlier PATO (pathology bank) diagnosis that wasn't picked (because
  #' PATO's own record for that patient happens to be dated later than
  #' another source's earliest record, despite PATO's rank being highest
  #' priority) -- conflict_note.
  #'
  #' NOTE on source ranking: datasource in all_dalycare_diagnoses only
  #' contains 'SDS', 'SUNDK', 'SP'. PATO appears via
  #' tablename == 'dimpatologiskdiagnose' (within SDS-sourced rows), not as
  #' its own datasource value, so the ranking below uses tablename for PATO
  #' and datasource for the others.
  #'
  #' @examples
  #' FIRST_DX = view_definition_first_diagnosis()

  load_dataset('all_dalycare_diagnoses')

  diagnosis_ranking = all_dalycare_diagnoses %>%
    mutate(diagnosis_date = as_date(date_diagnosis),
           priority_rank = case_when(
             tablename  == 'dimpatologiskdiagnose' ~ 1,
             datasource == 'SUNDK'  ~ 2,   # SUNDK registries
             datasource == 'SDS'    ~ 3,
             datasource == 'SP'     ~ 4,
             TRUE ~ 5
           )) %>%
    group_by(patientid) %>%
    arrange(priority_rank, diagnosis_date, .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  first_diagnoses = diagnosis_ranking %>%
    filter(rank == 1) %>%
    transmute(patientid,
              first_diagnosis_date = diagnosis_date,
              first_diagnosis = diagnosis,
              first_source = datasource)

  pato_conflicts = all_dalycare_diagnoses %>%
    filter(tablename == 'dimpatologiskdiagnose') %>%
    transmute(patientid, pato_date = as_date(date_diagnosis)) %>%
    inner_join(first_diagnoses %>% select(patientid, first_diagnosis_date), by = 'patientid') %>%
    filter(pato_date < first_diagnosis_date) %>%
    distinct(patientid) %>%
    mutate(conflict_note = 'Conflict with patobank')

  first_diagnoses %>%
    left_join(pato_conflicts, by = 'patientid')
}
