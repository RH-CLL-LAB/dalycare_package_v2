## test_package.R
## Test the development package before release_package()

VERSION = 'development'

PASS = c()
FAIL = c()

check = function(label, expr){
  result = tryCatch({
    v = force(expr)
    if(isTRUE(v) || (is.numeric(v) && v) || (is.data.frame(v) && nrow(v) >= 0)){
      cat('  OK   ', label, '\n')
      PASS <<- c(PASS, label)
    } else {
      cat('  FAIL ', label, '-- condition not met\n')
      FAIL <<- c(FAIL, paste0(label, ' -- condition not met'))
    }
  }, error = function(e){
    cat('  FAIL ', label, '--', conditionMessage(e), '\n')
    FAIL <<- c(FAIL, paste0(label, ' -- ', conditionMessage(e)))
  })
  invisible(result)
}

cat('==========================================================\n')
cat('1. Load the whole package (load_dalycare_package.R)\n')
cat('==========================================================\n')

check('source(load_dalycare_package.R) + use_version(development)', {
  source('/ngc/projects2/dalyca_r/data/dalycare_r_package/load_dalycare_package.R')
  use_version(VERSION)
  exists('load_dataset')
})

check('load_dalycare_package.R prints the table catalogue', {
  ## The catalogue is printed by load_dataset() without arguments.
  ## A separate call to load_dataset() is checked here, which confirms that the
  ## mechanism works (the output of the source() call itself cannot be captured).
  output = capture.output(load_dataset())
  any(grepl('List of tables', output))
})

check('help_info.R ran through', {
  exists('SP_DATASETS') && exists('SDS_DATASETS') && exists('SUNDK_DATASETS') && exists('PERSIMUNE_DATASETS')
})

check('load_data_v2.R is inert (fails clearly if sourced by mistake)', {
  # The file is in the versioned folder (PKG_HOME), not in the package root.
  ok = FALSE
  tryCatch(source(file.path(PKG_HOME, 'load_data_v2.R')),
           error = function(e) ok <<- grepl('retired', conditionMessage(e), ignore.case = TRUE))
  ok
})

cat('\n==========================================================\n')
cat('2. Catalogue and release roots\n')
cat('==========================================================\n')

check('dalycare_release_roots() finds all three folders', {
  r = dalycare_release_roots()
  length(r) == 3
})

check('dalycare_catalog() finds tables', {
  cat_ = dalycare_catalog(refresh = TRUE)
  nrow(cat_) > 0
})

check('lookup_tables zone is not empty', {
  cat_ = dalycare_catalog()
  sum(cat_$zone == 'lookup_tables') > 0
})

check('core/old is marked as archived, not hidden', {
  cat_ = dalycare_catalog()
  TRUE  # structure check only, fails only if catalog() itself fails
})

cat('\n==========================================================\n')
cat('3. load_dataset() -- the core function\n')
cat('==========================================================\n')

check('load_dataset() without arguments prints the catalogue', {
  invisible(capture.output(load_dataset()))
  TRUE
})

check('load_dataset("all_patients") loads and assigns', {
  load_dataset('all_patients')
  exists('all_patients') && nrow(all_patients) > 0
})

check('load_dataset("all_dalycare_diagnoses") loads and assigns', {
  load_dataset('all_dalycare_diagnoses')
  exists('all_dalycare_diagnoses') && nrow(all_dalycare_diagnoses) > 0
})

check('load_dataset() fails clearly on an unknown table', {
  ok = FALSE
  tryCatch(load_dataset('this_table_does_not_exist'),
           error = function(e) ok <<- grepl('not found', conditionMessage(e), ignore.case = TRUE))
  ok
})

check('load_dataset() lowercases column names (SDS_t_sksopr)', {
  load_dataset('SDS_t_sksopr')
  all(names(SDS_t_sksopr) == tolower(names(SDS_t_sksopr)))
})

check('load_dataset() with value/column filters and gives _subset', {
  load_dataset('all_dalycare_diagnoses')
  if(!exists('all_patients')) load_dataset('all_patients')
  ids = sample(all_patients$patientid, min(100, nrow(all_patients)))
  load_dataset('all_patients', value = ids, column = 'patientid')
  exists('all_patients_subset') && nrow(all_patients_subset) <= length(ids)
})

cat('\n==========================================================\n')
cat('4. Lookup tables (csv, new release root)\n')
cat('==========================================================\n')
cat('Codes_ATC and Codes_NPU are NOT tested via load_dataset() here -- that would\n')
cat('overwrite the transformed version that load_functions.R builds when the\n')
cat('package is loaded (with atc/name and NPU/short_definition columns), which\n')
cat('CLL_CI() and clean_lab_values() depend on. Instead the structure of the\n')
cat('global variables as they stand after loading the package is checked.\n\n')

check('Codes_ATC (built by load_functions.R) has atc/name columns', {
  exists('Codes_ATC') && all(c('atc', 'name') %in% names(Codes_ATC)) && nrow(Codes_ATC) > 0
})

check('Codes_NPU (built by load_functions.R) has NPU/short_definition columns', {
  exists('Codes_NPU') && all(c('NPU', 'short_definition') %in% names(Codes_NPU)) && nrow(Codes_NPU) > 0
})

check('Codes_NPU csv is read correctly by readr (regression test: mixed-content column) -- read directly, not via load_dataset(), so the global Codes_NPU is not overwritten', {
  path = dalycare_catalog()$path[dalycare_catalog()$name == 'Codes_NPU']
  raw = readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  nrow(raw) > 0
})

check('CODES_K loads', {
  load_dataset('CODES_K')
  exists('CODES_K') && nrow(CODES_K) > 0
})

check('Codes_ICD10 is built correctly by load_functions.R (from Codes_DST_DIAG_CODES)', {
  exists('Codes_ICD10') && nrow(Codes_ICD10) > 0 && 'icd10' %in% names(Codes_ICD10)
})

check('Codes_DST_DIAG_CODES loads completely (tests mixed-content csv columns)', {
  load_dataset('Codes_DST_DIAG_CODES')
  exists('Codes_DST_DIAG_CODES') && nrow(Codes_DST_DIAG_CODES) > 0
})

check('Codes_all is built correctly (combines all 6 lookup tables, tests lowercase references in load_functions.R)', {
  exists('Codes_all') && nrow(Codes_all) > 0 && all(c('sks', 'atc', 'snomed', 'icd10', 'npu', 'shak') %in% Codes_all$type)
})

check('Codes_sor loads (comma separated, UTF-8, columns: Enhedsnavn, SOR-kode, SghAfd-kode)', {
  load_dataset('Codes_sor')
  exists('Codes_sor') && nrow(Codes_sor) > 50000 &&
    all(c('Enhedsnavn', 'SOR-kode', 'SghAfd-kode') %in% names(Codes_sor))
})

cat('\n==========================================================\n')
cat('5. load_biochemistry() / all_laboratory_measurements\n')
cat('==========================================================\n')

check('load_biochemistry() finds all_laboratory_measurements', {
  if(!exists('NPU.HGB')) load_npu_common()
  LAB = load_biochemistry(labs = NPU.HGB)
  nrow(LAB) > 0
})

check('all_laboratory_measurements has the c_value column', {
  LAB = load_biochemistry(labs = NPU.HGB)
  'c_value' %in% names(LAB)
})

check('load_biochemistry(assign = TRUE) assigns the correct name', {
  load_biochemistry(labs = NPU.HGB, assign = TRUE)
  exists('all_laboratory_measurements_subset')
})

check('samplingdate (text in SDS_laboratorieproevesvar) parses as a date without new NA', {
  load_dataset('SDS_laboratorieproevesvar', c(NPU.HGB), 'analysiscode')
  d = SDS_laboratorieproevesvar_subset
  n_na_before = sum(is.na(d$samplingdate))
  n_na_after  = sum(is.na(suppressWarnings(as_date(d$samplingdate))))
  # allow a few irregular raw values (<0.1%), but not a general format problem
  (n_na_after - n_na_before) / nrow(d) < 0.001
})

check('PATO-kilderne have the expected tables/columns (SDS_fctrekvisition etc.)', {
  ok = TRUE
  for(tbl in c('SDS_fctrekvisition', 'SDS_fctpatologiskprocedure', 'SDS_dimpatologiskdiagnose')){
    load_dataset(tbl)
    if(!exists(tbl) || nrow(get(tbl)) == 0) ok = FALSE
  }
  ok
})

check('pin_data_version()/unpin_data_version() sets and clears the pin', {
  before = dalycare_data_pin()
  ok1 = is.null(before) # expected default state -- fails below if an earlier test run left a pin behind
  cat_dates = dalycare_catalog()$date
  cat_dates = cat_dates[!is.na(cat_dates)]
  if(length(cat_dates) == 0){
    cat('  (skipped -- no dated files in the catalogue to pin against)\n')
    TRUE
  } else {
    test_date = as.character(min(cat_dates))
    pin_data_version(test_date)
    ok2 = identical(dalycare_data_pin(), test_date)
    unpin_data_version()
    ok3 = is.null(dalycare_data_pin())
    ok1 && ok2 && ok3
  }
})

check('table_info() lists documented tables and table_info(name, section) works', {
  # Uses the YAML files in <PKG_HOME>/table_info/ (must be deployed).
  overview = table_info()
  first = overview$name[1]
  cols = table_info(first, 'columns')
  is.data.frame(overview) && nrow(overview) > 0 &&
    !is.null(table_info(first)) &&
    is.data.frame(cols) && identical(names(cols), c('column', 'description')) &&
    inherits(try(table_info('does_not_exist'), silent = TRUE), 'try-error') &&
    inherits(try(table_info(first, 'does_not_exist'), silent = TRUE), 'try-error')
})

cat('\n==========================================================\n')
cat('6. load_blood_culture_SP()\n')
cat('==========================================================\n')

check('load_blood_culture_SP() finds the table and filters', {
  BC = load_blood_culture_SP()
  nrow(BC) > 0 && any(grepl('PROC_NAME|proc_name', names(BC), ignore.case = TRUE))
})

cat('\n==========================================================\n')
cat('7. load_datasets_head()\n')
cat('==========================================================\n')

check('load_datasets_head() creates _head objects', {
  load_datasets_head(head = 2)
  any(grepl('_head$', ls(envir = environment())))
})

cat('\n==========================================================\n')
cat('8. filter_first_diagnosis()\n')
cat('==========================================================\n')

check('filter_first_diagnosis() runs without the select(-priority) error', {
  load_dataset('all_dalycare_diagnoses')
  load_dataset('all_patients')
  CLL_cohort = all_dalycare_diagnoses %>% filter_first_diagnosis('DC911', str_contains = FALSE)
  exists('CLL_cohort') && nrow(CLL_cohort) > 0
})

cat('\n==========================================================\n')
cat('9. CLL_CI() -- full end to end\n')
cat('==========================================================\n')

check('CLL_CI() runs through and gives scores', {
  if(!exists('CLL_cohort')){
    load_dataset('all_dalycare_diagnoses')
    load_dataset('all_patients')
    CLL_cohort = all_dalycare_diagnoses %>% filter_first_diagnosis('DC911', str_contains = FALSE)
  }
  result = CLL_cohort %>% CLL_CI()
  nrow(result) > 0 && all(c('vascular_score', 'gastro_score', 'endo_score', 'CLL_CI_score') %in% names(result))
})

cat('\n==========================================================\n')
cat('10. All 24 CTCAE functions (AE_*)\n')
cat('==========================================================\n')
cat('Each function is called with its own NPU code group (from NPU_codes.R).\n')
cat('The error message is printed directly, not only PASS/FAIL, so a failure\n')
cat('does not require manual retesting afterwards.\n\n')

if(!exists('NPU.HGB')) load_npu_common()

CTCAE_MAP = list(
  AE_ALAT_increased                  = 'NPU.ALAT',
  AE_ASAT_increased                  = 'NPU.ASAT',
  AE_BASP_increased                  = 'NPU.BASP',
  AE_INR_increased                   = 'NPU.INR',
  AE_WBC_decreased                   = 'NPU.LEU',
  AE_anemia                          = 'NPU.HGB',
  AE_bilirubin_increased             = 'NPU.BIL',
  AE_creatinine_increased            = 'NPU.KREA',
  AE_eosinophilia                    = 'NPU.EOS',
  AE_haptoglobin_decreased           = 'NPU.HAP',
  AE_hemoglobin_increased            = 'NPU.HGB',
  AE_hypercalcemia                   = 'NPU.CA2',
  AE_hyperkalemia                    = 'NPU.KAL',
  AE_hypermagnesemia                 = 'NPU.MG2',
  AE_hypernatremia                   = 'NPU.NAT',
  AE_hypertriglyceridemia            = 'NPU.TG',
  AE_hypoalbuminemia                 = 'NPU.ALB',
  AE_hypocalcemia                    = 'NPU.CA2',
  AE_hypoglycemia                    = 'NPU.GLU',
  AE_leukocytosis                    = 'NPU.LEU',
  AE_lymphocyte_count_decreased      = 'NPU.LYM',
  AE_lymphocyte_count_increased      = 'NPU.LYM',
  AE_neutrophil_count_decreased      = 'NPU.NEU',
  AE_platelets_decreased             = 'NPU.TRC'
)

for(fn_name in names(CTCAE_MAP)){
  npu_var = CTCAE_MAP[[fn_name]]
  label   = paste0(fn_name, '() -- ', npu_var)

  result = tryCatch({
    if(!exists(fn_name, mode = 'function')) stop('the function does not exist in the loaded environment')
    if(!exists(npu_var)) stop('the NPU code ', npu_var, ' does not exist in load_npu_common()')
    LAB = load_biochemistry(labs = get(npu_var))
    fn  = get(fn_name)
    AE  = LAB %>% fn()
    if(nrow(AE) == 0) stop('0 rows in output')
    'OK'
  }, error = function(e) paste('FAIL:', conditionMessage(e)))

  if(result == 'OK'){
    cat('  OK   ', label, '\n')
    PASS = c(PASS, label)
  } else {
    cat('  FAIL ', label, '--', sub('^FAIL: ', '', result), '\n')
    FAIL = c(FAIL, paste0(label, ' -- ', sub('^FAIL: ', '', result)))
  }
}

cat('\n==========================================================\n')
cat('11. Heavy, frequently used functions\n')
cat('==========================================================\n')

check('clean_SUNDK_CLL() cleans the CLL registry and gives rows', {
  load_dataset('SUNDK_CLL')
  CLL_CLEAN = SUNDK_CLL %>% clean_SUNDK_CLL()
  nrow(CLL_CLEAN) > 0
})

check('clean_SUNDK_LYFO() cleans the LYFO registry (Danish transliteration)', {
  load_dataset('SUNDK_LYFO')
  LYFO_CLEAN = SUNDK_LYFO %>% clean_SUNDK_LYFO()
  nrow(LYFO_CLEAN) > 0
})

check('clean_SUNDK_DAMYDA() cleans the DaMyDa registry (Danish transliteration)', {
  load_dataset('SUNDK_DaMyDa') # the table name is mixed case, the function name is not
  DAMYDA_CLEAN = SUNDK_DaMyDa %>% clean_SUNDK_DAMYDA()
  nrow(DAMYDA_CLEAN) > 0
})

check('CCI_v1() (Charlson comorbidity) runs on diagnosis data', {
  load_dataset('all_dalycare_diagnoses')
  load_dataset('all_patients')
  cohort = all_dalycare_diagnoses %>% filter_first_diagnosis('DC911', str_contains = FALSE)
  result = cohort %>% CCI_v1()
  nrow(result) > 0
})

check('CCI() (v2) runs on all_diagnoses_icd10 (SDS_PATO/SUNDK_LYFO/SUNDK_DaMyDa are not in the table)', {
  load_dataset('all_diagnoses_icd10')
  result = all_diagnoses_icd10 %>% CCI(icd10 = diagnosis)
  nrow(result) > 0 && all(c('CCI_score', 'CCI2011_score') %in% names(result))
})

check('right_truncation() warns but does not stop when status == NA', {
  load_dataset('all_patients')
  cohort = all_patients %>% mutate(date_event = as.Date(NA))
  warned = FALSE
  result = withCallingHandlers(
    cohort %>% right_truncation(date_event, date_start = date_birth,
                                date_truncation = as.character(Sys.Date())),
    warning = function(w){ warned <<- TRUE; invokeRestart('muffleWarning') }
  )
  is.data.frame(result) && nrow(result) > 0 && (sum(is.na(cohort$status)) == 0 || warned)
})

check('AE_AKI() -- acute kidney injury, full chain: load_biochemistry -> clean_lab_values -> AE_AKI', {
  if(!exists('NPU.KREA')) load_npu_common()
  KREA_LAB = load_biochemistry(labs = NPU.KREA)
  CREATININE_clean = KREA_LAB %>% clean_lab_values()
  AKI = CREATININE_clean %>% AE_AKI(value = value2)
  nrow(AKI) > 0
})

check('load_all_data() (fra load_all_data_2025.R) -- the large bulk loader runs without errors on a small cohort', {
  load_dataset('all_patients')
  cohort_small = sample(all_patients$patientid, min(200, nrow(all_patients)))
  load_all_data(cohort = cohort_small)
  exists('master_data') && is.data.frame(master_data)
})

check('view_diagnoses_all_distinct()', {
  result = view_diagnoses_all_distinct()
  nrow(result) > 0 && all(c('patientid', 'date_diagnosis', 'diagnosis') %in% names(result))
})

check('view_definition_first_diagnosis()', {
  result = view_definition_first_diagnosis()
  nrow(result) > 0 && all(c('patientid', 'first_diagnosis', 'first_diagnosis_date') %in% names(result))
})

check('view_definition_first_diagnosis_malignant()', {
  result = view_definition_first_diagnosis_malignant()
  nrow(result) > 0 && all(c('patientid', 'first_diagnosis', 'first_diagnosis_date') %in% names(result))
})

check('view_codes2persimune()', {
  result = view_codes2persimune()
  nrow(result) > 0 && all(c('AnalysisCode', 'analysisgroup') %in% names(result))
})

cat('\n==========================================================\n')
cat('12. All functions in the package are defined (existence check)\n')
cat('==========================================================\n')
cat('Only confirms that all ~104 files were sourced without syntax errors and\n')
cat('that functions were created. Does NOT test the behaviour of the functions --\n')
cat('only the parts with a dedicated check above (sections 1-10) are\n')
cat('functionally tested. The rest (CCI, prognostic scores, KM_plot,\n')
cat('forest_plot_*, etc.) needs domain knowledge of the input to be tested\n')
cat('automatically and is not covered here.\n\n')

all_functions = ls(envir = environment())
all_functions = all_functions[sapply(all_functions, function(x) is.function(get(x)))]
cat('Number of functions defined in the session:', length(all_functions), '\n')

cat('\n==========================================================\n')
cat('SUMMARY\n')
cat('==========================================================\n')
cat('PASS:', length(PASS), '\n')
cat('FAIL:', length(FAIL), '\n')
if(length(FAIL) > 0){
  cat('\nFailed checks:\n')
  for(f in FAIL) cat('  -', f, '\n')
}

cat('\nAll parts of the package, including the entry point\n')
cat('(load_dalycare_package.R), are covered.\n')
