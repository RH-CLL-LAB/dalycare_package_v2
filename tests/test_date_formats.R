## test_date_formats.R
## Audits date/datetime columns in the package tables.
##
## Prints and writes ONLY schema and aggregates (classes, NA shares, counts,
## year ranges, format shapes with digits masked) -- never real values.
##
## Usage (on NGC):
##   source('/ngc/projects2/dalyca_r/data/dalycare_r_package/load_dalycare_package.R')
##   source('tests/test_date_formats.R')
##   res = run_date_audit()                       # default tables
##   res = run_date_audit(c('all_patients'))      # subset
## Result: list(columns = <per-column audit>, checks = <pass/fail checks>).
## Optional: write_audit(res, 'date_audit_out') writes two csv files (aggregates only).

suppressPackageStartupMessages({library(dplyr); library(lubridate); library(stringr)})

DATE_AUDIT_TABLES = c(
  'all_patients', 'all_death_dates', 'patient_data_overview',
  'all_dalycare_diagnoses', 'all_diagnoses_icd10', 'all_pathology',
  'all_antineoplastic_treatment', 'all_medicine',
  'all_laboratory_measurements', 'all_comorbidity_and_ae', 'all_AKI', 'all_IPI',
  'SUNDK_CLL_clean', 'SUNDK_LYFO_clean', 'SUNDK_DaMyDa_clean'
)

MIN_YEAR = 1900   # earlier than this is treated as implausible
NAME_RE  = '(^date)|(date$)|(_date_)|(^dato)|(_dt$)|(^datetime)|(time$)|(^aki_start)'

mask_shape = function(x){
  #' Replaces digits by 9 and letters by a, so only the FORMAT is reported.
  x = str_replace_all(as.character(x), '[0-9]', '9')
  str_replace_all(x, '[A-Za-z]', 'a')
}

looks_like_date_chr = function(x){
  x = x[!is.na(x) & x != '']
  if(length(x) == 0) return(FALSE)
  x = head(x, 5000)
  mean(str_detect(x, '^\\d{4}-\\d{2}-\\d{2}|^\\d{1,2}[-./]\\d{1,2}[-./]\\d{2,4}|^\\d{8}$')) > 0.5
}

audit_column = function(tab, col, x){
  n = length(x)
  cls = paste(class(x), collapse = '/')
  tz = if(inherits(x, 'POSIXct')) (if(is.null(attr(x, 'tzone'))) '' else attr(x, 'tzone')) else NA_character_
  out = tibble(table = tab, column = col, class = cls, tz = as.character(tz),
               n = n, na_share = round(mean(is.na(x)), 4),
               name_says_date = str_detect(col, NAME_RE),
               n_formats = NA_integer_, formats = NA_character_,
               n_unparsable = NA_integer_, n_before_min_year = NA_integer_,
               n_in_future = NA_integer_, year_min = NA_integer_, year_max = NA_integer_,
               share_midnight = NA_real_, issue = '')
  issues = c()

  if(is.character(x) || is.factor(x)){
    x = as.character(x)
    nn = x[!is.na(x) & x != '']
    shapes = sort(table(mask_shape(nn)), decreasing = TRUE)
    out$n_formats = length(shapes)
    out$formats = paste0(names(head(shapes, 5)), ' (n=', head(as.integer(shapes), 5), ')', collapse = '; ')
    if(looks_like_date_chr(x)) issues = c(issues, 'date stored as text')
    if(length(shapes) > 1 && looks_like_date_chr(x)) issues = c(issues, 'mixed formats in one column')
    if(looks_like_date_chr(x)){
      iso = suppressWarnings(as_date(nn)); dm = suppressWarnings(dmy(nn))
      out$n_unparsable = sum(is.na(iso) & is.na(dm))
      if(out$n_unparsable > 0) issues = c(issues, 'unparsable values')
      # values valid as both ISO-ish and dmy would be ambiguous
      if(any(!is.na(iso)) && any(is.na(iso) & !is.na(dm))) issues = c(issues, 'ISO and dmy mixed')
    }
    if(out$name_says_date && !looks_like_date_chr(x) && length(nn) > 0)
      issues = c(issues, 'name says date but content is not date-like')
  } else if(inherits(x, c('Date', 'POSIXct'))){
    d = as_date(x); d = d[!is.na(d)]
    if(length(d) > 0){
      out$year_min = year(min(d)); out$year_max = year(max(d))
      out$n_before_min_year = sum(year(d) < MIN_YEAR)
      out$n_in_future = sum(d > Sys.Date())
      if(out$n_before_min_year > 0) issues = c(issues, paste0('dates before ', MIN_YEAR))
      if(out$n_in_future > 0) issues = c(issues, 'dates in the future')
      if(out$year_min < 1900 || out$year_max > year(Sys.Date()) + 1) issues = c(issues, 'implausible year range')
    }
    if(inherits(x, 'POSIXct')){
      xx = x[!is.na(x)]
      out$share_midnight = if(length(xx)) round(mean(format(xx, '%H:%M:%S') == '00:00:00'), 4) else NA
      if(identical(as.character(tz), '') || is.null(tz)) issues = c(issues, 'POSIXct without timezone attribute')
      if(!is.na(out$share_midnight) && out$share_midnight == 1) issues = c(issues, 'POSIXct but time is always 00:00:00 (could be Date)')
    }
  } else if(inherits(x, c('hms', 'difftime'))){
    # time of day; fine, only reported
  } else if(is.numeric(x) && out$name_says_date && !str_detect(col, 'time$|duration')){
    issues = c(issues, 'name says date but column is numeric')
  } else if(out$name_says_date && !is.logical(x) && !inherits(x, c('hms', 'difftime'))){
    issues = c(issues, paste0('name says date but class is ', cls))
  }
  if(!out$name_says_date && inherits(x, c('Date', 'POSIXct')))
    issues = c(issues, 'date class but name does not say date')
  out$issue = paste(unique(issues), collapse = ' | ')
  out
}

audit_table = function(tab){
  data = tryCatch({
    load_dataset(tab, quiet = TRUE)   # assigns the table into this function's environment
    get0(tab, inherits = FALSE)
  }, error = function(e){ message('  cannot load ', tab, ': ', conditionMessage(e)); NULL })
  if(is.null(data)) return(NULL)
  data = as_tibble(data)
  cols = names(data)
  cand = cols[str_detect(cols, NAME_RE) | vapply(data, function(x) inherits(x, c('Date', 'POSIXct')), logical(1)) |
                vapply(data, function(x) is.character(x) && looks_like_date_chr(x), logical(1))]
  list(data = data, audit = bind_rows(lapply(cand, function(cn) audit_column(tab, cn, data[[cn]]))))
}

pair_checks = function(tab, d){
  #' Logical-order checks (counts only). Skipped if a column is missing.
  chk = function(a, b, label){
    if(all(c(a, b) %in% names(d)) && inherits(d[[a]], c('Date', 'POSIXct')) && inherits(d[[b]], c('Date', 'POSIXct'))){
      bad = sum(as_date(d[[a]]) > as_date(d[[b]]), na.rm = TRUE)
      tibble(table = tab, check = label, n_violations = bad, pass = bad == 0)
    }
  }
  bind_rows(
    chk('date_birth', 'date_death_fu', 'date_birth <= date_death_fu'),
    chk('date_birth', 'date_diagnosis', 'date_birth <= date_diagnosis'),
    chk('date_start', 'date_end', 'date_start <= date_end'),
    chk('start_date', 'end_date', 'start_date <= end_date'),
    chk('date_diagnosis', 'date_death', 'date_diagnosis <= date_death'),
    chk('date_diagnosis', 'date_treatment_1st_line', 'date_diagnosis <= date_treatment_1st_line')
  )
}

run_date_audit = function(tables = DATE_AUDIT_TABLES){
  cols = list(); checks = list()
  for(tab in tables){
    cat('\n== ', tab, '\n', sep = '')
    r = audit_table(tab)
    if(is.null(r)) next
    cols[[tab]] = r$audit
    checks[[tab]] = pair_checks(tab, r$data)
    print(as.data.frame(r$audit %>% select(column, class, tz, na_share, year_min, year_max, issue)), row.names = FALSE)
  }
  cols = bind_rows(cols); checks = bind_rows(checks)

  # Cross-table consistency: same column name, different class
  incons = cols %>% group_by(column) %>%
    filter(n_distinct(class) > 1) %>%
    summarise(classes = paste(unique(paste0(table, ':', class)), collapse = '; '), .groups = 'drop')
  cat('\n== Same column name with different class across tables\n')
  if(nrow(incons)) print(as.data.frame(incons), row.names = FALSE) else cat('  none\n')

  # Timezone consistency across POSIXct columns
  tzs = cols %>% filter(grepl('POSIXct', class)) %>% count(tz)
  cat('\n== Timezone attributes on POSIXct columns\n'); print(as.data.frame(tzs), row.names = FALSE)

  cat('\n== Order checks\n'); if(nrow(checks)) print(as.data.frame(checks), row.names = FALSE) else cat('  none applicable\n')
  flagged = cols %>% filter(issue != '')
  cat('\nSummary: ', nrow(cols), ' date-like columns, ', nrow(flagged), ' flagged, ',
      sum(!checks$pass), ' failed order checks\n', sep = '')
  invisible(list(columns = cols, checks = checks, inconsistent = incons))
}

write_audit = function(res, dir = 'date_audit_out'){
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(res$columns, file.path(dir, 'date_columns.csv'))
  readr::write_csv(res$checks, file.path(dir, 'date_order_checks.csv'))
  readr::write_csv(res$inconsistent, file.path(dir, 'date_class_inconsistencies.csv'))
  cat('Wrote aggregate-only csv files to', dir, '\n')
}
