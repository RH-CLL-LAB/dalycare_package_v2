view_codes2persimune = function(){
  #' @title
  #' view_codes2persimune
  #' @author
  #' christian brieghel (SQL view); R-version 2026
  #' @description
  #' Recreates the SQL view "view_codes2persimune" as an R function. Views
  #' do not exist in the parquet release -- load_dataset() only reads
  #' physical .parquet/.csv files. Call this function instead of
  #' `load_dataset("view_codes2persimune")`.
  #'
  #' For NPU-prefixed analysis codes in the CODES_NPU2PERSIMUNE lookup
  #' table, picks an analysisgroup: prefers C_New_AnalysisGroup when it has
  #' content, otherwise falls back to C_AnalysisGroup.
  #'
  #' NOTE: this is a more complete version of the analysisgroup lookup than
  #' the one used to build all_laboratory_measurements, which only uses
  #' C_AnalysisGroup, not C_New_AnalysisGroup.
  #'
  #' NOTE on a SQL quirk preserved from the original view: if C_New_AnalysisGroup is
  #' genuinely missing (NA), the result is NA even when C_AnalysisGroup has
  #' a value -- the original SQL only falls back to C_AnalysisGroup when
  #' C_New_AnalysisGroup is an EMPTY STRING, not when it's NULL. Reproduced
  #' here exactly as written, not "fixed", since we don't know if that was
  #' intentional.
  #'
  #' lookup_tables columns are not lowercased by load_dataset() -- column names below are
  #' written exactly as the SQL view's quoted identifiers. Verify against
  #' the actual delivered CODES_NPU2PERSIMUNE.csv header if this errors.
  #'
  #' @examples
  #' NPU_GROUPS = view_codes2persimune()

  load_dataset('CODES_NPU2PERSIMUNE')

  CODES_NPU2PERSIMUNE %>%
    filter(substr(AnalysisCode, 1, 3) == 'NPU',
           (!is.na(C_AnalysisGroup) & nchar(C_AnalysisGroup) > 1) |
           (!is.na(C_New_AnalysisGroup) & nchar(C_New_AnalysisGroup) > 1)) %>%
    transmute(AnalysisCode,
              analysisgroup = if_else(nchar(C_New_AnalysisGroup) == 0,
                                      C_AnalysisGroup, C_New_AnalysisGroup))
}
