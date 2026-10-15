load_blood_culture_SP = function(assign = FALSE){
  #' @title
  #' load_blood_culture_SP
  #' @author
  #' christian brieghel (PostgreSQL version); parquet version 2026
  #' @description
  #' Loads SP_AlleProvesvar subset from the DALY-CARE data resource based on labs = NPU codes for blood culture
  #' Returns a dataset (default) or assigns dataset to GE if assign = TRUE
  #' @examples
  #' BC = load_blood_culture_SP()

  #' @note
  #' The SP proc_name/PROC_NAME column has changed case at least once between
  #' deliveries (lowercase -> uppercase). The table name itself has also
  #' changed once (SP_AlleProvesvar -> SP_alleprøvesvar). This function looks
  #' the table up by a case-insensitive, partial match on "r.vesvar" instead
  #' of a hardcoded exact name, so a future case or accent change alone does
  #' not break it. A change to the actual column name would still break it.

  labs = c("BLODDYRKNING(BACTERIUM+FUNGUS),B", "BLODDYRKNING(BAKTERIUM+FUNGUS),AB",
          "BLODDYRKNING(BAKTERIUM+FUNGUS),KAT.(SHEET)", "BLODDYRKNING(BAKTERIUM+FUNGUS),NSB",
          "BLODDYRKNING (BØRNEKOLBE),B", "BLODDYRKNING(FUNGUS),AB",
          "BLODDYRKNING(FUNGUS),B(CVK)", "BLODDYRKNING (GÆR)CANDIDA,B")

  catalog = dalycare_catalog()
  hit = catalog[grepl('r.vesvar', catalog$name, ignore.case = TRUE), ]

  if(nrow(hit) == 0){
    stop('Table not found in release: nothing matching "r.vesvar" (expected SP_alleprøvesvar / SP_AlleProvesvar)',
         '\nLooked in: ', paste(dalycare_release_roots(), collapse = ', '),
         call. = FALSE)
  }
  if(nrow(hit) > 1){
    stop('More than one table matches "r.vesvar": ', paste(hit$name, collapse = ', '),
         '\nPick the right one explicitly and adjust this function.',
         call. = FALSE)
  }

  data = arrow::open_dataset(hit$path)

  proc_name_col = grep('^proc_name$', names(data), ignore.case = TRUE, value = TRUE)
  if(length(proc_name_col) == 0){
    stop('Column proc_name / PROC_NAME not found in ', hit$name,
         '\nColumns present: ', paste(names(data), collapse = ', '), call. = FALSE)
  }

  data = data %>%
    filter(!!rlang::sym(proc_name_col) %in% labs) %>%
    collect()

  if(assign){
    assign('SP_AlleProvesvar_subset', data, envir = parent.frame())
    cat('Done!\n')
  }else{
    return(data)
  }
}
