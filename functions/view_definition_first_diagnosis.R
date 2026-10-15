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
  #' NB 2026-09-14: t_dalycare_diagnoses was renamed to all_dalycare_diagnoses
  #' (rename only -- same 14 sources, same translation, same approved-code
  #' filter). References to t_dalycare_diagnoses below describe history as
  #' validated at the time and are left as originally written.
  #'
  #' For each patient, picks ONE diagnosis row from t_dalycare_diagnoses
  #' (now all_dalycare_diagnoses),
  #' ranked first by source priority (PATO=1, RKKP=2, SDS=3, SP=4,
  #' PERSIMUNE=5, else=6), then by earliest date_diagnosis among rows tied
  #' on that priority. This is NOT the same selection rule as
  #' filter_first_diagnosis(), which currently ranks by date alone -- see
  #' AENDRINGER_dalycare_package.md, item B3c, for the open question this
  #' raises about filter_first_diagnosis()'s own tie-breaking logic.
  #'
  #' Also flags patients whose selected first diagnosis is contradicted by
  #' an earlier PATO (pathology bank) diagnosis that wasn't picked (because
  #' PATO's own record for that patient happens to be dated later than
  #' another source's earliest record, despite PATO's rank being highest
  #' priority) -- conflict_note.
  #'
  #' NOTE on source ranking: the original SQL view ranked by literal
  #' datasource values 'PATO'/'RKKP'/'SDS'/'SP'/'PERSIMUNE', queried against
  #' pending.t_dalycare_diagnoses (a staging schema). Tested directly against
  #' the actually-released t_dalycare_diagnoses (item B1): datasource only
  #' contains 'SDS', 'SUNDK', 'SP' -- PATO appears via
  #' tablename == 'dimpatologiskdiagnose' (within SDS-sourced rows), not as
  #' its own datasource value, and RKKP is now named SUNDK. Ranking below
  #' uses the same mapping already validated for item B1, not a literal port
  #' of the SQL's (now stale) string matches.
  #' NB 2026-09: tablename value for the PATO source changed from 't_pato'
  #' to 'dimpatologiskdiagnose' (pipeline change, see AENDRINGER punkt D10).
  #'
  #' @examples
  #' FIRST_DX = view_definition_first_diagnosis()

  load_dataset('all_dalycare_diagnoses')

  diagnosis_ranking = all_dalycare_diagnoses %>%
    mutate(diagnosis_date = as_date(date_diagnosis),
           priority_rank = case_when(
             tablename  == 'dimpatologiskdiagnose' ~ 1,
             datasource == 'SUNDK'  ~ 2,   # SUNDK = current name for the old RKKP registries
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
