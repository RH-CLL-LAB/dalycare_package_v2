load_biochemistry = function(labs = NULL, assign = FALSE){
  #' @title
  #' load_biochemistry
  #' @author
  #' christian brieghel (PostgreSQL version); parquet version 2026
  #' @description
  #' Loads dataset containing biochemistry from all_laboratory_measurements
  #' (renamed 2026-09-15, formerly laboratorymeasurements -- rename only, no
  #' content/logic change).
  #' 'labs' must contain NPU codes, e.g. from lists from load_npu_common()
  #' @examples
  #' LAB_df = load_biochemistry(c(NPU.B2M, NPU.LDH))
  #' BSI_df = load_biochemistry(NPU.BSI) #Blood cultures # assigns data as all_laboratory_measurements_subset into Global Environment
  #' load_biochemistry(labs = NPU.GROUP.MSPIKE, assign = TRUE)

  catalog = dalycare_catalog()
  row = catalog[catalog$name == 'all_laboratory_measurements', ]

  if(nrow(row) == 0){
    stop('Table not found in release: all_laboratory_measurements',
         '\nLooked in: ', paste(dalycare_release_roots(), collapse = ', '),
         call. = FALSE)
  }

  data = arrow::open_dataset(row$path) %>%
    filter(analysiscode %in% labs) %>%
    collect()

  if(assign){
    assign('all_laboratory_measurements_subset', data, envir = parent.frame())
    cat('Done!\n')
  }else{
    return(data)
  }
}
