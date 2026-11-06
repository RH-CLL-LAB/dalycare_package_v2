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
  #' The underlying table is 'all_diagnoses_icd10'. It contains ONLY sources
  #' with native ICD-10 codes (SDS, SP, SUNDK_CLL) -- SDS_PATO, SUNDK_LYFO and
  #' SUNDK_DaMyDa are not in it. `diagnosis` is never null within this table.
  #' The distinct-diagnoses set therefore does not include those three sources.
  #'
  #' @examples
  #' DX_DISTINCT = view_diagnoses_all_distinct()

  load_dataset('all_diagnoses_icd10')
  all_diagnoses_icd10 %>%
    select(patientid, date_diagnosis, diagnosis) %>%
    distinct()
}
