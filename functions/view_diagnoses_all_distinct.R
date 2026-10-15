view_diagnoses_all_distinct = function(){
  #' @title
  #' view_diagnoses_all_distinct
  #' @author
  #' christian brieghel (SQL view); R-version 2026
  #' @description
  #' Recreates the SQL view "view_diagnoses_all_distinct" as an R function.
  #' Views do not exist in the parquet release -- load_dataset() only reads
  #' physical .parquet/.csv files, so `load_dataset("view_diagnoses_all_distinct")`
  #' (which worked against the old PostgreSQL system) no longer works.
  #' Call this function instead.
  #'
  #' Original SQL:
  #'   SELECT DISTINCT diagnoses_all.patientid, diagnoses_all.date_diagnosis,
  #'          diagnoses_all.diagnosis
  #'   FROM diagnoses_all;
  #'
  #' NB 2026-09-14: the underlying table 'diagnoses_all' was renamed to
  #' 'all_diagnoses_icd10' and its content narrowed (see
  #' AENDRINGER_dalycare_package.md, F3e). It now contains ONLY sources with
  #' native ICD-10 codes (SDS, SP, RKKP_CLL) -- SDS_PATO, RKKP_LYFO, and
  #' RKKP_DaMyDa are excluded entirely, not just NA. `diagnosis` is never
  #' null within this table; `diagnosis_raw` no longer exists. This function
  #' otherwise stays semantically identical to the old view, but the
  #' resulting distinct-diagnoses set is now smaller than before (missing
  #' the three excluded sources) -- see F3e for whether that matters for
  #' your analysis.
  #'
  #' @examples
  #' DX_DISTINCT = view_diagnoses_all_distinct()

  load_dataset('all_diagnoses_icd10')
  all_diagnoses_icd10 %>%
    select(patientid, date_diagnosis, diagnosis) %>%
    distinct()
}
