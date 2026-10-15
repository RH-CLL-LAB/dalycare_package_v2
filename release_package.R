## release_package.R
##
## Promotes development/ to current/ and archives the previous current/ to
## old/<YYYYMMDD>/. Run when development/ has been tested and approved.
##
## Usage:
##   source('.../release_package.R')
##   release_package()                    # uses today's date
##   release_package(date = '20260901')   # explicit date
##   release_package(dry_run = TRUE)      # preview only

release_package = function(date = format(Sys.Date(), '%Y%m%d'), dry_run = FALSE){

  PKG_ROOT = '/ngc/projects2/dalyca_r/data/dalycare_r_package'
  dev_path  = file.path(PKG_ROOT, 'development')
  curr_path = file.path(PKG_ROOT, 'current')
  old_path  = file.path(PKG_ROOT, 'old', date)

  cat('==========================================================\n')
  cat('DALYCARE R PACKAGE RELEASE\n')
  cat('==========================================================\n')
  cat('Date:         ', date, '\n')
  cat('development/: ', dev_path, '\n')
  cat('current/ ->:  ', old_path, '\n')
  cat('development/ -> current/\n')
  if(dry_run) cat('\n*** DRY RUN -- no files will be changed ***\n')
  cat('\n')

  if(!dir.exists(dev_path))
    stop('development/ not found: ', dev_path, call. = FALSE)
  if(!file.exists(file.path(dev_path, 'load_functions.R')))
    stop('development/load_functions.R not found -- is development/ complete?', call. = FALSE)

  if(dry_run){
    cat('Would archive', length(list.files(curr_path, recursive=TRUE)), 'files from current/.\n')
    cat('Would promote', length(list.files(dev_path, recursive=TRUE)), 'files from development/.\n')
    cat('\nRun release_package() without dry_run=TRUE to execute.\n')
    return(invisible(NULL))
  }

  # 1. Archive current/ -> old/<date>/ (skip if already archived)
  if(dir.exists(old_path)){
    cat('old/', date, '/ already exists -- skipping archive.\n', sep='')
  } else if(dir.exists(curr_path)){
    cat('Archiving current/ -> old/', date, '/...\n', sep='')
    dir.create(old_path, recursive = TRUE)
    curr_files = list.files(curr_path, recursive = TRUE, full.names = FALSE)
    for(f in curr_files){
      src = file.path(curr_path, f)
      dst = file.path(old_path, f)
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      file.copy(src, dst)
    }
    cat('  Archived', length(curr_files), 'files.\n')
  } else {
    cat('No existing current/ -- creating fresh.\n')
    dir.create(curr_path, recursive = TRUE)
  }

  # 2. Copy development/ -> current/
  cat('Promoting development/ -> current/...\n')
  dev_files = list.files(dev_path, recursive = TRUE, full.names = FALSE)
  for(f in dev_files){
    src = file.path(dev_path, f)
    dst = file.path(curr_path, f)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    file.copy(src, dst, overwrite = TRUE)
  }
  cat('  Promoted', length(dev_files), 'files.\n')

  if(!'load_clinical_trial_data.R' %in% dev_files)
    warning('load_clinical_trial_data.R missing from development/ -- RCT users will fail to load.', call. = FALSE)

  # 3. Update package_releases.R in current/
  releases_path = file.path(curr_path, 'package_releases.R')
  writeLines(c(
    '## package_releases.R -- updated automatically by release_package()',
    paste0("DALYCARE_PACKAGE_VERSION = '", date, "'")
  ), releases_path)

  cat('\nDone. Researchers will now get version', date, 'when sourcing load_dalycare_package.R\n')
  cat('To pin to this version later:\n')
  cat("  use_version('", date, "')\n", sep='')
}

## Run immediately if sourced directly
release_package()
