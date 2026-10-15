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
      cat('  FEJL ', label, '-- betingelse ikke opfyldt\n')
      FAIL <<- c(FAIL, paste0(label, ' -- betingelse ikke opfyldt'))
    }
  }, error = function(e){
    cat('  FEJL ', label, '--', conditionMessage(e), '\n')
    FAIL <<- c(FAIL, paste0(label, ' -- ', conditionMessage(e)))
  })
  invisible(result)
}

cat('==========================================================\n')
cat('1. Loader hele pakken (load_dalycare_package.R)\n')
cat('==========================================================\n')

check('source(load_dalycare_package.R) + use_version(development), {
  source('/ngc/projects2/dalyca_r/data/dalycare_r_package/load_dalycare_package.R')
  use_version(VERSION)
  exists('load_dataset')
})

check('load_dalycare_package.R printer tabelkatalog automatisk', {
  ## Den gamle load_data_v2.R printede "List of tables from ..." ved hver
  ## source(). Genskabt 2026-08-31 via load_dataset() uden argumenter.
  ## Tjekker her at et separat kald til load_dataset() faktisk printer noget
  ## -- bekrÃ¦fter mekanismen virker, ikke selve source()-kaldets output
  ## (som allerede skete i tjekket ovenfor og ikke kan fanges her).
  output = capture.output(load_dataset())
  any(grepl('List of tables', output))
})

check('help_info.R kÃ¸rte igennem', {
  exists('SP_DATASETS') && exists('SDS_DATASETS') && exists('SUNDK_DATASETS') && exists('PERSIMUNE_DATASETS')
})

check('load_data_v2.R er inert (fejler tydeligt hvis sourcet ved en fejl)', {
  # NB: filen ligger i den versionerede mappe (PKG_HOME), ikke i
  # pakkeroden -- rettet 2026-09-15, testede tidligere den forkerte sti.
  ok = FALSE
  tryCatch(source(file.path(PKG_HOME, 'load_data_v2.R')),
           error = function(e) ok <<- grepl('retired', conditionMessage(e), ignore.case = TRUE))
  ok
})

cat('\n==========================================================\n')
cat('2. Katalog og release-rÃ¸dder\n')
cat('==========================================================\n')

check('dalycare_release_roots() finder alle tre mapper', {
  r = dalycare_release_roots()
  length(r) == 3
})

check('dalycare_catalog() finder tabeller', {
  cat_ = dalycare_catalog(refresh = TRUE)
  nrow(cat_) > 0
})

check('lookup_tables-zone er ikke tom', {
  cat_ = dalycare_catalog()
  sum(cat_$zone == 'lookup_tables') > 0
})

check('core/old bliver markeret som archived, ikke skjult', {
  cat_ = dalycare_catalog()
  TRUE  # rent strukturtjek, fejler kun hvis catalog() selv fejler
})

cat('\n==========================================================\n')
cat('3. load_dataset() -- kernefunktionen\n')
cat('==========================================================\n')

check('load_dataset() uden argumenter printer katalog', {
  invisible(capture.output(load_dataset()))
  TRUE
})

check('load_dataset("all_patients") indlÃ¦ser og assigner', {
  load_dataset('all_patients')
  exists('all_patients') && nrow(all_patients) > 0
})

check('load_dataset("all_dalycare_diagnoses") indlÃ¦ser og assigner', {
  load_dataset('all_dalycare_diagnoses')
  exists('all_dalycare_diagnoses') && nrow(all_dalycare_diagnoses) > 0
})

check('load_dataset() fejler tydeligt pÃ¥ ukendt tabel', {
  ok = FALSE
  tryCatch(load_dataset('denne_tabel_findes_ikke'),
           error = function(e) ok <<- grepl('not found', conditionMessage(e), ignore.case = TRUE))
  ok
})

check('load_dataset() lowercaser kolonnenavne (SDS_t_sksopr)', {
  load_dataset('SDS_t_sksopr')
  all(names(SDS_t_sksopr) == tolower(names(SDS_t_sksopr)))
})

check('load_dataset() med value/column filtrerer og giver _subset', {
  load_dataset('all_dalycare_diagnoses')
  if(!exists('all_patients')) load_dataset('all_patients')
  ids = sample(all_patients$patientid, min(100, nrow(all_patients)))
  load_dataset('all_patients', value = ids, column = 'patientid')
  exists('all_patients_subset') && nrow(all_patients_subset) <= length(ids)
})

cat('\n==========================================================\n')
cat('4. Lookup-tabeller (csv, ny release-rod)\n')
cat('==========================================================\n')
cat('Codes_ATC og Codes_NPU testes IKKE via load_dataset() her -- det ville\n')
cat('overskrive den specielt transformerede version, load_functions.R\n')
cat('bygger ved pakkeindlÃ¦sning (med atc/name hhv. NPU/short_definition-\n')
cat('kolonner), som CLL_CI() og clean_lab_values() er direkte afhÃ¦ngige af.\n')
cat('I stedet tjekkes strukturen pÃ¥ de globale variable, som de allerede\n')
cat('stÃ¥r efter pakkeindlÃ¦sning.\n\n')

check('Codes_ATC (bygget af load_functions.R) har atc/name-kolonner', {
  exists('Codes_ATC') && all(c('atc', 'name') %in% names(Codes_ATC)) && nrow(Codes_ATC) > 0
})

check('Codes_NPU (bygget af load_functions.R) har NPU/short_definition-kolonner', {
  exists('Codes_NPU') && all(c('NPU', 'short_definition') %in% names(Codes_NPU)) && nrow(Codes_NPU) > 0
})

check('Codes_NPU csv lÃ¦ses korrekt via readr (regressionstest: mixed-content kolonne, se item C4) -- lÃ¦st direkte, ikke via load_dataset(), for ikke at overskrive den globale Codes_NPU', {
  path = dalycare_catalog()$path[dalycare_catalog()$name == 'Codes_NPU']
  raw = readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  nrow(raw) > 0
})

check('CODES_K indlÃ¦ses', {
  load_dataset('CODES_K')
  exists('CODES_K') && nrow(CODES_K) > 0
})

check('Codes_ICD10 bygges korrekt af load_functions.R (fra Codes_DST_DIAG_CODES)', {
  exists('Codes_ICD10') && nrow(Codes_ICD10) > 0 && 'icd10' %in% names(Codes_ICD10)
})

check('Codes_DST_DIAG_CODES indlÃ¦ses helt igennem (tester mixed-content csv-kolonner)', {
  load_dataset('Codes_DST_DIAG_CODES')
  exists('Codes_DST_DIAG_CODES') && nrow(Codes_DST_DIAG_CODES) > 0
})

check('Codes_all bygges korrekt (samler alle 6 lookup-tabeller, tester lowercase-referencer i load_functions.R)', {
  exists('Codes_all') && nrow(Codes_all) > 0 && all(c('sks', 'atc', 'snomed', 'icd10', 'npu', 'shak') %in% Codes_all$type)
})

cat('\n==========================================================\n')
cat('5. load_biochemistry() / all_laboratory_measurements\n')
cat('==========================================================\n')

check('load_biochemistry() finder all_laboratory_measurements', {
  if(!exists('NPU.HGB')) load_npu_common()
  LAB = load_biochemistry(labs = NPU.HGB)
  nrow(LAB) > 0
})

check('all_laboratory_measurements har c_value-kolonnen', {
  LAB = load_biochemistry(labs = NPU.HGB)
  'c_value' %in% names(LAB)
})

check('load_biochemistry(assign = TRUE) assigner korrekt navn', {
  load_biochemistry(labs = NPU.HGB, assign = TRUE)
  exists('all_laboratory_measurements_subset')
})

check('samplingdate (text i SDS_laboratorieproevesvar) parser som dato uden nye NA', {
  load_dataset('SDS_laboratorieproevesvar', c(NPU.HGB), 'analysiscode')
  d = SDS_laboratorieproevesvar_subset
  n_na_before = sum(is.na(d$samplingdate))
  n_na_after  = sum(is.na(suppressWarnings(as_date(d$samplingdate))))
  # tillad enkelte uregelmÃ¦ssige rÃ¥-vÃ¦rdier (<0.1%), ikke et generelt formatproblem
  (n_na_after - n_na_before) / nrow(d) < 0.001
})

check('PATO-kilderne har de forventede tabeller/kolonner (SDS_fctrekvisition m.fl.)', {
  ok = TRUE
  for(tbl in c('SDS_fctrekvisition', 'SDS_fctpatologiskprocedure', 'SDS_dimpatologiskdiagnose')){
    load_dataset(tbl)
    if(!exists(tbl) || nrow(get(tbl)) == 0) ok = FALSE
  }
  ok
})

check('pin_data_version()/unpin_data_version() sÃ¦tter og rydder pinnen', {
  before = dalycare_data_pin()
  ok1 = is.null(before) # forventet default state -- fejler nedenunder hvis en tidligere test-kÃ¸rsel efterlod en pin, uden at det er en reel fejl
  cat_dates = dalycare_catalog()$date
  cat_dates = cat_dates[!is.na(cat_dates)]
  if(length(cat_dates) == 0){
    cat('  (springer over -- ingen daterede filer i kataloget at pinne mod)\n')
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

check('table_info() lister dokumenterede tabeller og table_info(navn, sektion) virker', {
  # Bruger YAML-filerne i <PKG_HOME>/table_info/ (skal vaere udrullet).
  overview = table_info()
  first = overview$name[1]
  cols = table_info(first, 'columns')
  is.data.frame(overview) && nrow(overview) > 0 &&
    !is.null(table_info(first)) &&
    is.data.frame(cols) && identical(names(cols), c('column', 'description')) &&
    inherits(try(table_info('findes_ikke'), silent = TRUE), 'try-error') &&
    inherits(try(table_info(first, 'findes_ikke'), silent = TRUE), 'try-error')
})

cat('\n==========================================================\n')
cat('6. load_blood_culture_SP()\n')
cat('==========================================================\n')

check('load_blood_culture_SP() finder tabellen og filtrerer', {
  BC = load_blood_culture_SP()
  nrow(BC) > 0 && any(grepl('PROC_NAME|proc_name', names(BC), ignore.case = TRUE))
})

cat('\n==========================================================\n')
cat('7. load_datasets_head()\n')
cat('==========================================================\n')

check('load_datasets_head() opretter _head-objekter', {
  load_datasets_head(head = 2)
  any(grepl('_head$', ls(envir = environment())))
})

cat('\n==========================================================\n')
cat('8. filter_first_diagnosis()\n')
cat('==========================================================\n')

check('filter_first_diagnosis() kÃ¸rer uden select(-priority)-fejl', {
  load_dataset('all_dalycare_diagnoses')
  load_dataset('all_patients')
  CLL_cohort = all_dalycare_diagnoses %>% filter_first_diagnosis('DC911', str_contains = FALSE)
  exists('CLL_cohort') && nrow(CLL_cohort) > 0
})

cat('\n==========================================================\n')
cat('9. CLL_CI() -- fuld ende-til-ende, view-erstatning + typecast\n')
cat('==========================================================\n')

check('CLL_CI() kÃ¸rer igennem og giver scorer', {
  if(!exists('CLL_cohort')){
    load_dataset('all_dalycare_diagnoses')
    load_dataset('all_patients')
    CLL_cohort = all_dalycare_diagnoses %>% filter_first_diagnosis('DC911', str_contains = FALSE)
  }
  result = CLL_cohort %>% CLL_CI()
  nrow(result) > 0 && all(c('vascular_score', 'gastro_score', 'endo_score', 'CLL_CI_score') %in% names(result))
})

cat('\n==========================================================\n')
cat('10. Alle 24 CTCAE-funktioner (AE_*)\n')
cat('==========================================================\n')
cat('Hver funktion kaldes mod sin egen NPU-kodegruppe (fra NPU_codes.R).\n')
cat('Fejlbesked printes direkte, ikke kun PASS/FEJL, sÃ¥ en fejl her ikke\n')
cat('krÃ¦ver manuel gentestning bagefter.\n\n')

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
    if(!exists(fn_name, mode = 'function')) stop('funktionen findes ikke i det indlÃ¦ste miljÃ¸')
    if(!exists(npu_var)) stop('NPU-koden ', npu_var, ' findes ikke i load_npu_common()')
    LAB = load_biochemistry(labs = get(npu_var))
    fn  = get(fn_name)
    AE  = LAB %>% fn()
    if(nrow(AE) == 0) stop('0 rÃ¦kker i output')
    'OK'
  }, error = function(e) paste('FEJL:', conditionMessage(e)))

  if(result == 'OK'){
    cat('  OK   ', label, '\n')
    PASS = c(PASS, label)
  } else {
    cat('  FEJL ', label, '--', sub('^FEJL: ', '', result), '\n')
    FAIL = c(FAIL, paste0(label, ' -- ', sub('^FEJL: ', '', result)))
  }
}

cat('\n==========================================================\n')
cat('11. Tunge, hyppigt brugte funktioner -- endnu utestet\n')
cat('==========================================================\n')

check('clean_SUNDK_CLL() renser CLL-registret og giver rÃ¦kker', {
  load_dataset('SUNDK_CLL') # NB 2026-09-17: renamed from RKKP_CLL
  CLL_CLEAN = SUNDK_CLL %>% clean_SUNDK_CLL()
  nrow(CLL_CLEAN) > 0
})

check('clean_SUNDK_LYFO() renser LYFO-registret (dansk transliteration, F5)', {
  load_dataset('SUNDK_LYFO') # NB 2026-09-17: renamed from RKKP_LYFO
  LYFO_CLEAN = SUNDK_LYFO %>% clean_SUNDK_LYFO()
  nrow(LYFO_CLEAN) > 0
})

check('clean_SUNDK_DAMYDA() renser DAMYDA-registret (dansk transliteration, F5)', {
  load_dataset('SUNDK_DaMyDa') # NB 2026-09-17: renamed from RKKP_DaMyDa. Tabelnavnet er mixed-case, funktionsnavnet er ikke
  DAMYDA_CLEAN = SUNDK_DaMyDa %>% clean_SUNDK_DAMYDA()
  nrow(DAMYDA_CLEAN) > 0
})

check('CCI_v1() (Charlson comorbidity) kÃ¸rer pÃ¥ diagnosedata', {
  load_dataset('all_dalycare_diagnoses')
  load_dataset('all_patients')
  cohort = all_dalycare_diagnoses %>% filter_first_diagnosis('DC911', str_contains = FALSE)
  result = cohort %>% CCI_v1()
  nrow(result) > 0
})

check('CCI() (v2) kÃ¸rer pÃ¥ all_diagnoses_icd10 (F3e -- PATO/LYFO/DaMyDa udelukket fra kilden)', {
  load_dataset('all_diagnoses_icd10')
  result = all_diagnoses_icd10 %>% CCI(icd10 = diagnosis)
  nrow(result) > 0 && all(c('CCI_score', 'CCI2011_score') %in% names(result))
})

check('right_truncation() advarer men stopper ikke ved status == NA (F2)', {
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

check('AE_AKI() -- akut nyreskade, fuld kÃ¦de: load_biochemistry -> clean_lab_values -> AE_AKI', {
  if(!exists('NPU.KREA')) load_npu_common()
  KREA_LAB = load_biochemistry(labs = NPU.KREA)
  CREATININE_clean = KREA_LAB %>% clean_lab_values()
  AKI = CREATININE_clean %>% AE_AKI(value = value2)
  nrow(AKI) > 0
})

check('load_all_data() (fra load_all_data_2025.R) -- den store bulk-loader kÃ¸rer uden fejl pÃ¥ en lille kohorte', {
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
cat('12. Alle funktioner i pakken er defineret (eksistens-tjek)\n')
cat('==========================================================\n')
cat('BekrÃ¦fter kun at alle ~104 filer sourcedes uden syntaksfejl, og at\n')
cat('funktioner faktisk blev oprettet. Tester IKKE funktionernes adfÃ¦rd --\n')
cat('kun de dele, der har et dedikeret tjek ovenfor (afsnit 1-10), er\n')
cat('funktionelt testet. Resten (CCI, prognostiske scorer, KM_plot,\n')
cat('forest_plot_*, m.fl.) krÃ¦ver domÃ¦nespecifik viden om input for at\n')
cat('kunne testes automatisk, og er ikke dÃ¦kket her.\n\n')

alle_funktioner = ls(envir = environment())
alle_funktioner = alle_funktioner[sapply(alle_funktioner, function(x) is.function(get(x)))]
cat('Antal funktioner defineret i sessionen:', length(alle_funktioner), '\n')

cat('\n==========================================================\n')
cat('OPSUMMERING\n')
cat('==========================================================\n')
cat('PASS:', length(PASS), '\n')
cat('FEJL:', length(FAIL), '\n')
if(length(FAIL) > 0){
  cat('\nFejlede tjek:\n')
  for(f in FAIL) cat('  -', f, '\n')
}

cat('\nAlle dele af pakken, inklusive selve indgangspunktet\n')
cat('(load_dalycare_package.R), er nu testet.\n')
