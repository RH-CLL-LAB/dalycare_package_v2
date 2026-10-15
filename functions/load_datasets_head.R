load_datasets_head = function(head = 2){
  #' @title
  #' load_datasets_head
  #' @author
  #' christian brieghel (PostgreSQL version); parquet version 2026
  #' @description Loads a head of all import datasets from the NGC DALY-CARE
  #' data resource, which creates suffix "_head" datasets to quickly look
  #' into the data structure

  #' @note
  #' the length of head may be specified as a numeric value in the "head" argument

  #' @examples
  #' load_datasets_head(head = 10)

  catalog = dalycare_catalog()
  files   = sort(catalog$name[catalog$zone == 'import'])

  cat('\nLoading head from all datasets in import... \n')

  for(tbl in files){
    print(tbl)
    row  = catalog[catalog$name == tbl, ]
    data = tryCatch({
      if(row$filetype == 'xlsx'){
        readxl::read_excel(row$path, n_max = head)
      } else if(row$filetype == 'csv'){
        if(isTRUE(row$curated)){
          readr::read_csv(row$path, n_max = head, show_col_types = FALSE)
        } else {
          readr::read_csv(row$path, n_max = head,
                          col_types = readr::cols(.default = readr::col_character()),
                          show_col_types = FALSE)
        }
      } else {
        arrow::open_dataset(row$path) %>% head(head) %>% collect()
      }
    }, error = function(e){
      message('  Skipping ', tbl, ': ', conditionMessage(e))
      NULL
    })
    if(!is.null(data))
      assign(paste0(tbl, '_head'), data, envir = parent.frame())
  }
}
