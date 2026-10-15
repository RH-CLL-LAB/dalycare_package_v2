## load_dataset() -- parquet version
## Replaces the PostgreSQL version. Same signature, same contract: the table is
## assigned into the caller's environment under its own name, with the suffix
## "_subset" when a filter was applied.
##
## Requires: arrow, dplyr, readr, rlang (all loaded by load_dalycare_package.R
## via library(tidyverse)/library(arrow)).
## When testing this file on its own: library(arrow); library(dplyr); library(readr)
##
## Changed 2026-08-18. See AENDRINGER_dalycare_package.md, items A1, A5, B3, C3, C4.

#### Catalogue #############################################################

.dalycare_cache <- new.env(parent = emptyenv())

dalycare_release_roots = function(){
  #' @description The release folders. Hardcoded -- this package only
  #' runs on NGC.

  roots = c(
    import        = '/ngc/projects2/dalyca_r/data/import',
    core          = '/ngc/projects2/dalyca_r/data/core',
    lookup_tables = '/ngc/projects2/dalyca_r/data/lookup_tables'
  )
  # development subfolders under core/ are optional -- included if they exist
  dev_roots = list.files('/ngc/projects2/dalyca_r/data/core/development',
                         full.names = TRUE, include.dirs = TRUE)
  dev_roots = dev_roots[file.info(dev_roots)$isdir]
  if(length(dev_roots) > 0){
    extra = setNames(dev_roots, basename(dev_roots))
    roots = c(roots, extra)
  }
  missing = roots[!dir.exists(roots) & !names(roots) %in% basename(dev_roots)]
  if(length(missing) > 0){
    stop('Release folder not found: ', paste(missing, collapse = ', '), call. = FALSE)
  }
  roots[dir.exists(roots)]
}

dalycare_data_pin = function(){
  #' @description The data pin currently in effect (a 'YYYYMMDD' string), or
  #' NULL if unpinned. Set by use_version('<date>') and cleared by
  #' use_version('current').
  get0('pin_date', envir = .dalycare_cache, ifnotfound = NULL)
}

pin_data_version = function(date){
  #' @description Locks dalycare_catalog()/load_dataset() to the data as it
  #' existed on or before `date` ('YYYYMMDD') -- core/ falls back to the
  #' matching core/old/<name>_<date>.parquet archive (or core/current/ if
  #' that is itself old enough), and import/ falls back to the matching
  #' dated import/<SOURCE>/<date>/ folder. Tables with no data that old are
  #' dropped with a warning, not silently substituted with newer data.
  #' lookup_tables/, core/development/, and researcher-curated datasets
  #' (EHR_curated, lab_data) are NOT pinned -- these are reference data /
  #' out-of-band uploads, not part of the dated release timeline.
  #' Internal: called by use_version('<date>'), which is the only supported
  #' way for users to pin.

  if(is.na(suppressWarnings(as.integer(date)))) stop("date must look like 'YYYYMMDD'", call. = FALSE)
  assign('pin_date', as.character(date), envir = .dalycare_cache)
  cat('Data pinned as of:', date, '-- tables without data that old will be dropped (with a warning) rather than silently substituted.\n')
  # No refresh needed -- the pin only changes which cached raw rows are
  # selected, not what's on disk.
  invisible(dalycare_catalog())
}

unpin_data_version = function(){
  #' @description Clears a data pin set by pin_data_version()/use_version('<date>').
  #' load_dataset() goes back to always reading core/current/ and the latest import/ folder.
  if(!is.null(dalycare_data_pin())){
    assign('pin_date', NULL, envir = .dalycare_cache)
    cat('Data pin cleared -- back to core/current/ and the latest import/ data.\n')
  }
  invisible(dalycare_catalog())
}

dalycare_catalog = function(refresh = FALSE, as_of = dalycare_data_pin()){
  #' @description Scans the release folders for parquet and csv files (csv is
  #' the norm in lookup_tables/, but also picked up wherever else it turns up
  #' -- e.g. a pipeline table delivered as csv instead of parquet in core/),
  #' and builds a catalogue mapping table name to file path. The raw scan is
  #' cached, so disk access happens only once per session (refresh = TRUE
  #' forces a rescan, e.g. after a new release).
  #' @param as_of Optional 'YYYYMMDD' string -- when set, core/ and import/
  #' (excluding curated datasets) are resolved to the latest data available
  #' on or before that date instead of always the newest. For core/, this
  #' means core/current/ is ignored entirely and only the dated
  #' core/old/<name>_<date>.parquet archives are considered -- every release
  #' writes both current/ and a matching dated old/ copy (nothing is ever
  #' current/-only), so there is no need to guess current/'s date from
  #' mtime. Defaults to whatever pin_data_version()/use_version('<date>')
  #' last set; pass as_of = NULL explicitly to bypass any active pin for one
  #' call.
  #' @examples
  #' dalycare_catalog()
  #' dalycare_catalog(refresh = TRUE)
  #' dalycare_catalog(as_of = '20260901')

  if(!refresh && !is.null(.dalycare_cache$raw_catalog)){
    raw = .dalycare_cache$raw_catalog
  } else {
    raw = .dalycare_build_raw_catalog()
    .dalycare_cache$raw_catalog = raw
  }

  if(!is.null(as_of) && is.na(suppressWarnings(as.integer(as_of)))){
    warning("as_of / data pin '", as_of, "' does not look like 'YYYYMMDD' -- ignoring.", call. = FALSE)
    as_of = NULL
  }

  if(is.null(as_of)){
    catalog = raw %>%
      arrange(name, archived, desc(date), desc(mtime)) %>%   # current always wins over old
      distinct(name, .keep_all = TRUE) %>%
      arrange(name)
    return(catalog)
  }

  as_of_int = as.integer(as_of)
  pinned_zone = raw$zone %in% c('core', 'import') & !raw$curated
  # core/current/ has no date of its own to compare against as_of -- every
  # release writes a matching dated core/old/<name>_<date>.parquet copy
  # alongside it, so pinned core/ resolution ignores current/ entirely and
  # only considers dated old/ archives (which always exist). import/ has no
  # such "current" concept -- every import file already sits in a dated
  # folder, so its own `date` is used directly.
  ignore_for_pin = raw$zone == 'core' & !raw$archived
  eligible = !pinned_zone | (!ignore_for_pin & !is.na(raw$date) & raw$date <= as_of_int)

  catalog = raw[eligible, ] %>%
    arrange(name, desc(date), desc(mtime)) %>%
    distinct(name, .keep_all = TRUE) %>%
    arrange(name)

  dropped = setdiff(unique(raw$name[pinned_zone]), catalog$name[catalog$zone %in% c('core', 'import') & !catalog$curated])
  if(length(dropped) > 0){
    warning('No data as of ', as_of, ' for: ', paste(sort(dropped), collapse = ', '),
            ' -- dropped from the catalogue rather than substituted with newer data.', call. = FALSE)
  }
  catalog
}

.dalycare_build_raw_catalog = function(){
  root = dalycare_release_roots()

  # EHR_curated and lab_data: researcher-curated datasets under import/.
  # Scanned separately below (their own README/datadictionary/Codebook
  # exclusion) -- computed here first so the general csv scan can exclude
  # them and avoid duplicate catalog rows for the same file.
  import_root = root['import']
  curated_dirs = c('EHR_curated', 'lab_data')
  curated_paths = file.path(import_root, curated_dirs)
  curated_paths = curated_paths[dir.exists(curated_paths)]

  files_parquet = list.files(root, pattern = '\\.parquet$', recursive = TRUE, full.names = TRUE)
  # csv files can legitimately end up anywhere, not just lookup_tables/ --
  # e.g. all_antineoplastic_treatment.csv sits directly in core/current/
  # alongside otherwise-all-parquet tables. Previously only lookup_tables/
  # was scanned, which made such tables silently invisible to
  # load_dataset() -- not even an informative "table not found" pointing at
  # the real reason. Scan .csv everywhere now (still excluding the curated
  # dirs, handled separately below). Changed 2026-09-18.
  files_csv = list.files(root, pattern = '\\.csv$', recursive = TRUE, full.names = TRUE)
  if(length(curated_paths) > 0){
    files_csv = files_csv[!sapply(files_csv, function(f) any(startsWith(f, curated_paths)))]
  }

  files_curated_csv  = if(length(curated_paths) > 0)
    list.files(curated_paths, pattern = '\\.csv$',  recursive = TRUE, full.names = TRUE) else character(0)
  files_curated_xlsx = if(length(curated_paths) > 0)
    list.files(curated_paths, pattern = '\\.xlsx$', recursive = TRUE, full.names = TRUE) else character(0)
  excl = 'README|datadictionary|Codebook'
  files_curated_csv  = files_curated_csv[!grepl(excl,  files_curated_csv,  ignore.case = TRUE)]
  files_curated_xlsx = files_curated_xlsx[!grepl(excl, files_curated_xlsx, ignore.case = TRUE)]

  files = c(files_parquet, files_csv, files_curated_csv, files_curated_xlsx)

  if(length(files) == 0){
    stop('No parquet or csv files found under ', paste(root, collapse = ', '), call. = FALSE)
  }

  # which root each file came from (its zone), and the path below that root
  zone = names(root)[sapply(files, function(f) which(startsWith(f, root))[1])]
  rel  = substring(files, nchar(root[zone]) + 2)

  # core/old/<n>_<date>.parquet is an archived release; core/current/ is
  # always preferred over it, regardless of date, unless told otherwise
  archived = grepl('^old/', rel) | grepl('/old/', rel, fixed = TRUE)

  base_name = sub('\\.parquet$|\\.csv$|\\.xlsx$', '', basename(files))
  # Dated core files have the date baked INTO the filename itself
  # (<name>_<date>.parquet, whether under old/ or, it turns out, sometimes
  # directly in current/ too) -- strip a trailing _<8 digits> for ANY core
  # file so it resolves to the same catalog name as the dateless
  # core/current/<name>.parquet (same logical table, different dated
  # vintage). Without this, e.g. all_patients_20260911.parquet was
  # catalogued as its own separate table 'all_patients_20260911', invisible
  # to both the normal listing and to pin_data_version(). A no-op for files
  # that don't have a date suffix.
  name = ifelse(zone == 'core', sub('_\\d{8}$', '', base_name), base_name)

  catalog = tibble(
    name     = name,
    path     = files,
    filetype = ifelse(grepl('\\.csv$', files), 'csv', ifelse(grepl('\\.xlsx$', files), 'xlsx', 'parquet')),
    curated  = grepl('EHR_curated|lab_data', files),
    zone     = zone,
    # For import tables: the first path segment below import/, e.g. the
    # <SOURCE> folder in import/<SOURCE>/<date>/ or EHR_curated. Curated
    # sub-folders (EHR_curated/CLL_TREAT, ...) are deliberately NOT split out,
    # so all curated tables print in one block per top-level folder.
    # NA for other zones.
    source   = ifelse(zone == 'import', sub('/.*$', '', rel), NA_character_),
    lookup   = zone == 'lookup_tables',
    archived = archived,
    # Dates in the path come in two formats depending on source: plain
    # YYYYMMDD (e.g. SUNDK: import/SUNDK/20260911/, core/old/<n>_20260911.parquet)
    # and dashed YYYY-MM-DD (e.g. SDS/SP: import/SDS/2026-04-07/). Try the
    # dashed form first (more specific pattern) and strip the dashes so
    # both normalize to the same integer YYYYMMDD for comparison.
    date     = suppressWarnings(as.integer(ifelse(
                 grepl('\\d{4}-\\d{2}-\\d{2}', rel),
                 gsub('-', '', sub('.*?(\\d{4}-\\d{2}-\\d{2}).*', '\\1', rel)),
                 ifelse(grepl('\\d{8}', rel), sub('.*?(\\d{8}).*', '\\1', rel), NA)))),
    mtime    = file.mtime(files)
  )

  catalog   # raw, one row per file -- dalycare_catalog() dedupes per name
}

#### load_dataset ##########################################################

load_dataset = function(dataset = NULL, value = NULL, column = 'patientid',
                        selected_columns = NULL, limit = NULL, quiet = FALSE){
  #' @title
  #' load_dataset
  #' @author christian brieghel (PostgreSQL version); parquet version 2026
  #' @description Loads datasets from the NGC DALY-CARE data resource.

  #' @note
  #' Returns a list of all available datasets when dataset is NULL (default).
  #' Returns a subset of the dataset when specifying value as a string of patientids and column is "patientid" (default).
  #' Returns a subset of the dataset when specifying value as a string of values and column is specified for values.
  #' Stops with an error if a requested table does not exist (changed 2026 -- previously silent).
  #' All column names are lowercased on load (changed 2026 -- the delivered
  #' case varies between tables and even between deliveries of the same table).

  #' @examples
  #' Please see help_info()
  #' load_dataset(c("all_patients", "all_dalycare_diagnoses", "SUNDK_CLL"))
  #' load_dataset("SUNDK_CLL", value = sample(all_patients$patientid, 1000))
  #' load_dataset("SDS_t_sksube", value = sample(SDS_t_adm$k_recnum, 1000), column = "v_recnum")
  #' load_dataset("SDS_t_sksube", value = "UXMA00", column = "c_opr")
  #' load_dataset("SDS_procedurer_andre", value = SKS.CLL_targeted, column = "procedurekode")
  #' load_dataset("SDS_laboratorieproevesvar", value = NPU.LYM, column = "analysiscode")

  catalog = dalycare_catalog()

  #### No dataset given: print the catalogue ####
  #### Mirrors the old per-schema headers: import is split by source
  #### (import/SDS, import/SP, ...), printed first. core is printed after,
  #### archived (core/old) tables excluded from the listing. lookup_tables
  #### printed last.
  if(is.null(dataset)){
    pin = dalycare_data_pin()
    cat('\n=== DALY-CARE data catalogue ===')
    if(!is.null(pin)) cat(' (pinned as of ', pin, ')', sep = '')
    cat('\n')
    for(src in sort(unique(catalog$source[catalog$zone == 'import' & !catalog$curated]))){
      # Date shown next to each source heading -- the delivery date of the
      # resolved (possibly pinned) data actually being loaded for that
      # source, so it's clear at a glance which version is in effect.
      # Sources normally deliver all their tables in one dated folder, so
      # max() across the source's resolved rows is just a safety net for the
      # rare case a source has mixed dates.
      src_dates = catalog$date[catalog$zone == 'import' & catalog$source == src & !catalog$curated]
      src_date = suppressWarnings(max(src_dates, na.rm = TRUE))
      date_str = if(is.infinite(src_date)) '' else
        paste0(' (', format(as.Date(as.character(src_date), '%Y%m%d'), '%Y-%m-%d'), ')')
      cat(paste0('\nList of tables from import/', src, date_str, ':\n'))
      print(sort(catalog$name[catalog$zone == 'import' & catalog$source == src & !catalog$curated]))
    }
    # Curated research datasets -- grouped by their top-level folder under
    # import/ (EHR_curated, lab_data, ...), sub-folders not shown. catalog$source
    # already holds that folder name for curated rows, so print one block per folder.
    for(src in sort(unique(catalog$source[catalog$zone == 'import' & catalog$curated]))){
      cat(paste0('\nList of tables from import/', src, ':\n'))
      print(sort(catalog$name[catalog$zone == 'import' & catalog$curated & catalog$source == src]))
    }
    cat('\nList of tables from core:\n')
    # !archived hides core tables that only exist in old/ (retired, no
    # current version) from the unpinned listing. When pinned, a resolved
    # row CAN legitimately come from old/ (that's the point of the pin) --
    # don't hide those.
    print(sort(catalog$name[catalog$zone == 'core' & (!catalog$archived | !is.null(pin))]))
    # Development tables
    dev_tbls = catalog$name[catalog$zone == 'development']
    if(length(dev_tbls) > 0){
      cat('\nList of tables from core/development:\n')
      print(sort(dev_tbls))
    }
    cat('\nList of tables from lookup_tables:\n')
    print(sort(catalog$name[catalog$zone == 'lookup_tables']))
    return(invisible(NULL))
  }

  #### Stop if a table does not exist ####
  not_found = setdiff(dataset, catalog$name)
  if(length(not_found) > 0){
    is_view = grepl('^view_', not_found, ignore.case = TRUE)
    if(any(is_view)){
      stop('Table not found in release: ', paste(not_found, collapse = ', '),
           '\n\nNote: "view_*" tables were SQL views in the old PostgreSQL setup.',
           '\nIn the new package they are R functions -- call them directly:\n',
           '  view_diagnoses_all_distinct()         # all diagnoses, one row per patient+diagnosis\n',
           '  view_definition_first_diagnosis()     # first diagnosis per patient\n',
           '  view_definition_first_diagnosis_malignant()  # first malignant diagnosis\n',
           '  view_codes2persimune()                # diagnosis codes formatted for PERSIMUNE\n',
           '\nList available tables with load_dataset() and no arguments.',
           call. = FALSE)
    }
    stop('Table not found in release: ', paste(not_found, collapse = ', '),
         '\nLooked in: ', paste(dalycare_release_roots(), collapse = ', '),
         '\nList available tables with load_dataset() and no arguments.',
         call. = FALSE)
  }

  #### Load ####
  for(tbl in dataset){
    row      = catalog[catalog$name == tbl, ]
    filtered = !is.null(value) && !row$lookup && !isTRUE(row$curated)

    if(!quiet) cat(paste0('\nLoading ', tbl, if(filtered) ' for values... ' else '... '))

    #### CSV lookup tables are read fully, as character columns, via readr --
    #### not via arrow::open_dataset(). Arrow infers CSV column types from a
    #### sample of early rows; several lookup tables mix numeric-looking and
    #### free-text values in the same column further down (e.g. Codes_NPU's
    #### formula column), which makes arrow's schema inference fail outright.
    #### Lookup tables are small, so reading them fully as text and letting
    #### downstream code cast as needed is simpler and safer than trying to
    #### get arrow's schema guess right. Parquet tables keep using arrow's
    #### lazy reader as before -- this only affects the lookup_tables zone.
    #### Changed 2026-08-18, see AENDRINGER_dalycare_package.md, item C4.
    ####
    #### csv files can also now come from core/ or import/ (a pipeline table
    #### delivered as csv instead of parquet, e.g.
    #### core/current/all_antineoplastic_treatment.csv) -- see the catalog
    #### scan above, changed 2026-09-18. Those get normal readr type
    #### inference (same as curated datasets), NOT the all-character
    #### lookup_tables workaround above, since the mixed-type-column problem
    #### it works around is specific to the small hand-maintained lookup
    #### tables, not large pipeline output.
    if(row$filetype == 'csv'){
      if(row$zone == 'lookup_tables'){
        data = readr::read_csv(row$path, col_types = readr::cols(.default = readr::col_character()),
                               show_col_types = FALSE)
      } else {
        data = readr::read_csv(row$path, show_col_types = FALSE)
      }
    } else if(row$filetype == 'xlsx'){
      data = readxl::read_excel(row$path)
    } else {
      data = arrow::open_dataset(row$path, format = row$filetype)
    }

    #### Lowercase column names -- ONLY for import/core (bulk source tables).
    #### The old, pre-2026 Postgres workflow explicitly lowercased these
    #### (documented for SP; confirmed by testing for SDS_t_adm, SDS_t_sksopr,
    #### and others -- the current pipeline delivers them with varying case,
    #### which broke package code written against the old lowercase
    #### convention). Restoring lowercase here is a faithful restoration of
    #### the OLD contract, not a new one.
    ####
    #### lookup_tables are DELIBERATELY EXCLUDED. Evidence: the package's own
    #### original, untouched code references lookup-table columns with exact
    #### mixed case written without quoting (e.g. DIAG_CODE, `NPU code`,
    #### Kode, Tekst, SKSkode, Kodetekst) -- since R is case-sensitive
    #### (unlike Postgres, which folds unquoted identifiers to lowercase),
    #### this only works in production if the real underlying column
    #### genuinely has that exact case. Small reference tables are typically
    #### uploaded via dbWriteTable(), which preserves the R data.frame's
    #### original column case in Postgres -- unlike the bulk tables' import
    #### path. Forcing these to lowercase does not restore an old contract;
    #### it invents a new one, and risks breaking any researcher's own
    #### script written against the original names. Left exactly as
    #### delivered instead.
    ####
    #### data may still be a lazy Arrow Dataset here (parquet), not a
    #### data.frame -- names()<- does not work on it, use rename_with()
    #### instead, which works on both a tibble and an arrow_dplyr_query.
    #### Checked for collisions across the whole catalogue before enabling
    #### this -- no table had two columns differing only by case.
    #### Changed 2026-08-18, reconsidered and narrowed 2026-08-26.
    #### See AENDRINGER_dalycare_package.md, item C3.
    # Lowercase column names for all bulk tables except the RKKP registries
    # (renamed SUNDK_* 2026-09-17, formerly RKKP_*; kept matching both since
    # older pinned/archived data may still resolve to RKKP_* names).
    # These preserve their original mixed case (as delivered by the source)
    # because clean_SUNDK_*() functions are written against that case.
    # lookup_tables are also excluded (see below).
    is_rkkp = grepl('^(SUNDK|RKKP)_', tbl, ignore.case = FALSE)
    if(row$zone != 'lookup_tables' && !is_rkkp && !isTRUE(row$curated)){
      data = data %>% rename_with(tolower)
      column = tolower(column)
      if(!is.null(selected_columns)) selected_columns = tolower(selected_columns)
    } else if(is_rkkp){
      # RKKP preserves mixed case but patientid must be lowercase for joins
      data = data %>% rename_with(~ ifelse(tolower(.x) == 'patientid', 'patientid', .x))
    }

    if(filtered){
      # Cast filter value to match column type to avoid Arrow type errors
      # (e.g. patientid is int64 in some tables, string in others)
      col_type = tryCatch(class(dplyr::pull(data, !!rlang::sym(column))[0]), error = function(e) NULL)
      typed_value = tryCatch({
        if(!is.null(col_type) && col_type[1] %in% c('integer64','integer','int32')) {
          if(col_type[1] == 'integer') as.integer(value)
          else bit64::as.integer64(value)
        }
        else if(!is.null(col_type) && col_type[1] %in% c('numeric','double')) as.numeric(value)
        else as.character(value)
      }, error = function(e) value)
      data = data %>% filter(!!rlang::sym(column) %in% typed_value)
    }
    if(!is.null(selected_columns)){
      data = data %>% select(all_of(selected_columns))
    }
    if(!is.null(limit)){
      data = data %>% head(limit)
    }

    data = data %>% collect()

    #### Diagnostics when a filter returns nothing ####
    if(filtered && nrow(data) == 0){
      if(row$filetype == 'csv'){
        raw_names = names(readr::read_csv(row$path, n_max = 0, show_col_types = FALSE))
        col_type  = 'character (csv, all columns read as text)'
      } else {
        raw_names = names(arrow::open_dataset(row$path, format = row$filetype))
        orig_col  = raw_names[tolower(raw_names) == tolower(column)]
        col_type  = arrow::open_dataset(row$path, format = row$filetype)$schema[[orig_col]]$type$ToString()
      }
      cat('\n  0 rows matched. Types on each side:\n')
      cat('    ', column, ' in ', tbl, ': ', col_type, '\n', sep = '')
      cat('    value: ', class(value)[1], ' -- e.g. ',
          paste(utils::head(value, 3), collapse = ', '), '\n', sep = '')
    }

    assign(paste0(tbl, if(filtered) '_subset' else ''), data, envir = parent.frame())
  }
  invisible(NULL)
}
