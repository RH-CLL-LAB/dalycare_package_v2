## load_dalycare_package.R
##
## Entry point for researchers. Sources current/ by default.
## Works in both RStudio and headless Rscript sessions.
##
## Usage:
##   source('.../load_dalycare_package.R')               # current (default)
##   source('.../load_dalycare_package.R'); use_version('20260901')
##   use_version()                                       # list available versions
##
## use_version('<date>') locks BOTH code and data to that date:
##   - data is pinned to '<date>' -- load_dataset() reads core/old/... and
##     the matching dated import/ folder instead of always the newest.
##   - the CODE loaded is the newest package release on or before '<date>'
##     -- it does NOT need to be an exact match. There is no requirement
##     that a package release happened on that exact date; you always get
##     the code that was actually in use at that point in time.
## use_version('current') clears the pin again. use_version() is the only
## supported way to pin; pin_data_version()/unpin_data_version() are internal.

PKG_ROOT = '/ngc/projects2/dalyca_r/data/dalycare_r_package'

# Load insight for print_color -- suppress startup messages
suppressPackageStartupMessages(
  tryCatch(library(insight), error = function(e) NULL)
)

use_version = function(version = NULL){
  # Dated snapshots available under old/
  old_root = file.path(PKG_ROOT, 'old')
  dates = if(dir.exists(old_root))
    sort(list.dirs(old_root, full.names = FALSE, recursive = FALSE), decreasing = TRUE)
  else character(0)

  # No argument: just print the list of versions you can load
  if(is.null(version) || !nzchar(version)){
    cat("Available versions:\n")
    cat("  use_version('current')      # latest release (default)\n")
    if(length(dates))
      cat(paste0("  use_version('", dates, "')\n"), sep = '')
    else
      cat("  (no dated snapshots archived yet)\n")
    return(invisible(c('current', dates)))
  }

  # 'current' / 'development': load that folder directly, clear any pin.
  if(version %in% c('current', 'development')){
    path = file.path(PKG_ROOT, version)
    lf = file.path(path, 'load_functions.R')
    if(!file.exists(lf)) stop("Unknown version '", version, "'.", call. = FALSE)
    cat('Loading dalycare package version:', version, '\n')
    assign('PKG_HOME', path, envir = .GlobalEnv)
    source(lf)
    if(exists('unpin_data_version', envir = .GlobalEnv, mode = 'function')) unpin_data_version()
    return(invisible(NULL))
  }

  if(is.na(suppressWarnings(as.integer(version)))){
    message("dalycare: version '", version, "' not found.")
    cat("Available versions: current",
        if(length(dates)) paste0(', ', paste(dates, collapse = ', ')) else '',
        " -- or any 'YYYYMMDD' date to pin data and auto-load the matching code.\n", sep = '')
    stop("Unknown version '", version, "'.", call. = FALSE)
  }

  # A dated request: pin data to `version`, and load whichever code release
  # was newest on or before that date -- no exact-match release required.
  # This is "the code that actually worked with data from that point".
  current_date = tryCatch({
    pv = readLines(file.path(PKG_ROOT, 'current', 'package_releases.R'), warn = FALSE)
    pv = pv[grepl('DALYCARE_PACKAGE_VERSION', pv)][1]
    sub(".*DALYCARE_PACKAGE_VERSION = '([0-9]+)'.*", '\\1', pv)
  }, error = function(e) NA_character_)

  code_dates = c(current = current_date, setNames(dates, dates))
  code_dates = code_dates[!is.na(code_dates) & !is.na(suppressWarnings(as.integer(code_dates)))]
  eligible = code_dates[as.integer(code_dates) <= as.integer(version)]

  if(length(eligible) == 0){
    earliest = names(code_dates)[which.min(as.integer(code_dates))]
    stop("No package code exists on or before '", version, "'. Earliest available: ",
         earliest, " (released ", code_dates[[earliest]], ").", call. = FALSE)
  }

  best = names(eligible)[which.max(as.integer(eligible))]
  path = if(best == 'current') file.path(PKG_ROOT, 'current') else file.path(PKG_ROOT, 'old', best)
  lf = file.path(path, 'load_functions.R')

  cat("Loading dalycare package code as of '", version, "' -- closest match: ", eligible[[best]], "\n", sep = '')
  assign('PKG_HOME', path, envir = .GlobalEnv)
  source(lf)

  if(exists('pin_data_version', envir = .GlobalEnv, mode = 'function')) pin_data_version(version)
}

## Default: always load current
use_version('current')
