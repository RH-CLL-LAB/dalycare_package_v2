## table_docs.R
##
## table_info(): documentation of the core curated tables, shown in R.
##
## The content lives in one YAML file per table in <PKG_HOME>/table_info/
## (maintained by the Pipeline team at release). Nothing is hardcoded here --
## add, rename or edit a table by editing its YAML file only.
##
## YAML fields: name, status (RELEASED/PLANNED), former_name, purpose,
## sources (list), one_row, columns (name -> description), and optionally
## cleaning, caveats, open_questions (lists of text).
##
## The folder is found from PKG_HOME at call time (same principle as
## load_dalycare_package.R), so it follows use_version(). system.file() is not
## used -- the package is sourced, not installed. No links are looked up.

.table_info_sections = c('cleaning', 'caveats', 'open_questions', 'columns')

.table_info_dir = function(){
  if(!exists('PKG_HOME') || is.na(PKG_HOME)){
    stop('PKG_HOME is not set. Load the package via load_dalycare_package.R first.', call. = FALSE)
  }
  file.path(PKG_HOME, 'table_info')
}

.table_info_read_all = function(){
  # Reads every YAML file in the table_info folder; returns a named list
  # (one element per table, named by the `name` field).
  if(!requireNamespace('yaml', quietly = TRUE)){
    stop('table_info() needs the R package \'yaml\', which is not installed.', call. = FALSE)
  }
  dir = .table_info_dir()
  if(!dir.exists(dir)){
    stop('The table documentation folder does not exist: ', dir, call. = FALSE)
  }
  files = list.files(dir, pattern = '\\.ya?ml$', full.names = TRUE, ignore.case = TRUE)
  if(length(files) == 0){
    stop('No YAML files found in the table documentation folder: ', dir, call. = FALSE)
  }

  docs = lapply(files, function(f){
    doc = tryCatch(yaml::read_yaml(f, fileEncoding = 'UTF-8'),
                   error = function(e) stop('Could not read ', basename(f), ': ',
                                            conditionMessage(e), call. = FALSE))
    if(!is.list(doc) || is.null(doc$name)){
      stop(basename(f), ' has no \'name\' field.', call. = FALSE)
    }
    doc$name = as.character(doc$name)
    doc
  })
  names(docs) = vapply(docs, function(d) d$name, character(1))
  docs[order(names(docs))]
}

.table_info_text = function(x){
  # Single-line text from a YAML field (folded/multi-line text collapsed).
  if(is.null(x) || length(x) == 0) return(NA_character_)
  trimws(gsub('\\s+', ' ', paste(unlist(x), collapse = ' ')))
}

.table_info_short = function(x, width = 70){
  # First sentence of a purpose text, cut to `width` characters.
  x = .table_info_text(x)
  if(is.na(x)) return('')
  x = sub('(?<=[.!?])\\s.*$', '', x, perl = TRUE)
  if(nchar(x) > width) x = paste0(substr(x, 1, width - 3), '...')
  x
}

.table_info_bullets = function(x){
  for(item in unlist(x)) cat('  - ', .table_info_text(item), '\n', sep = '')
}

.table_info_columns = function(doc){
  cols = doc$columns
  if(is.null(cols) || length(cols) == 0){
    return(tibble::tibble(column = character(), description = character()))
  }
  tibble::tibble(
    column = names(cols),
    description = vapply(cols, .table_info_text, character(1), USE.NAMES = FALSE)
  )
}

table_info = function(name = NULL, section = NULL){
  #' @title
  #' table_info
  #' @description
  #' Shows the documentation of the core curated tables in R.
  #' table_info() lists all tables (name, status, short purpose).
  #' table_info('name') prints purpose, sources, what one row is, and the
  #' columns. The second argument gives a section: 'cleaning', 'caveats',
  #' 'open_questions' or 'columns' ('columns' returns a tibble).
  #' The content is read from the YAML files in <PKG_HOME>/table_info/.
  #' @examples
  #' table_info()
  #' table_info('all_patients')
  #' table_info('all_patients', 'caveats')
  #' table_info('all_patients', 'columns')

  docs = .table_info_read_all()

  ## No name: list all tables
  if(is.null(name)){
    overview = tibble::tibble(
      name = names(docs),
      status = vapply(docs, function(d) .table_info_text(d$status), character(1), USE.NAMES = FALSE),
      purpose = vapply(docs, function(d) .table_info_text(d$purpose), character(1), USE.NAMES = FALSE)
    )
    cat('\nDocumented tables (table_info(\'name\') for details):\n\n')
    w1 = max(nchar(overview$name))
    w2 = max(nchar(overview$status), na.rm = TRUE)
    for(i in seq_len(nrow(overview))){
      cat(formatC(overview$name[i], width = -w1), '  ',
          formatC(ifelse(is.na(overview$status[i]), '', overview$status[i]), width = -w2), '  ',
          .table_info_short(overview$purpose[i]), '\n', sep = '')
    }
    return(invisible(overview))
  }

  ## Unknown name: show what is available
  if(!is.character(name) || length(name) != 1 || !name %in% names(docs)){
    stop('No documentation for \'', paste(name, collapse = ', '), '\'. Available tables: ',
         paste(names(docs), collapse = ', '), call. = FALSE)
  }
  doc = docs[[name]]

  ## Section
  if(!is.null(section)){
    if(!is.character(section) || length(section) != 1 || !section %in% .table_info_sections){
      stop('Unknown section \'', paste(section, collapse = ', '), '\'. Available sections: ',
           paste(.table_info_sections, collapse = ', '), call. = FALSE)
    }
    if(section == 'columns'){
      cols = .table_info_columns(doc)
      if(nrow(cols) == 0){
        cat('No columns for this table\n')
        return(invisible(cols))
      }
      return(cols)
    }
    label = gsub('_', ' ', section)
    if(is.null(doc[[section]]) || length(doc[[section]]) == 0){
      cat('No ', label, ' for this table\n', sep = '')
      return(invisible(NULL))
    }
    print_color(paste0('\n', doc$name, ' -- ', label, '\n'), 'blue')
    .table_info_bullets(doc[[section]])
    return(invisible(doc[[section]]))
  }

  ## Basics: purpose, sources, one row, columns
  print_color(paste0('\n', doc$name, '  [', .table_info_text(doc$status), ']\n'), 'blue')
  has_text = function(x) !is.na(.table_info_text(x)) && nzchar(.table_info_text(x))
  if(has_text(doc$former_name)) cat('Former name: ', .table_info_text(doc$former_name), '\n', sep = '')
  cat('\nPurpose:\n  ', .table_info_text(doc$purpose), '\n', sep = '')
  if(length(doc$sources) > 0){
    cat('\nSources:\n')
    .table_info_bullets(doc$sources)
  }
  if(has_text(doc$one_row)) cat('\nOne row:\n  ', .table_info_text(doc$one_row), '\n', sep = '')
  cols = .table_info_columns(doc)
  if(nrow(cols) > 0){
    cat('\nColumns:\n')
    w = max(nchar(cols$column))
    for(i in seq_len(nrow(cols))){
      cat('  ', formatC(cols$column[i], width = -w), '  ',
          ifelse(is.na(cols$description[i]), '', cols$description[i]), '\n', sep = '')
    }
  }
  more = intersect(c('cleaning', 'caveats', 'open_questions'),
                   names(Filter(function(x) length(x) > 0, doc)))
  if(length(more) > 0){
    cat('\nMore details: ',
        paste0('table_info(\'', doc$name, '\', \'', more, '\')', collapse = '  '), '\n', sep = '')
  }
  invisible(doc)
}
