## This script prints help info for the dalycare package 
## Sys.Date() # "2024-09-30"
## Sys.info()[["user"]] # "chribr_r"

print_color('\nThe DALY-CARE protocol is approved for epidemiological, molecular, and machine-learning studies in patients diagnosed with:\n', 'black')
print_color('C81.x-C90.x, C91.1-C91.9, C95.1, C95.7, C95.9, D47.2, D47.9B, and E85.8A\n\n', 'blue')

#### Amyloidosis extension approved 2026-09-23 - this lies OUTSIDE the diagnoses listed
#### above from the originally published method paper. Mirrored in the
#### Python pipeline's SPECIFIC_CODES (diagnoses.py). 'DE858A' (AL-amyloidose)
#### is NOT listed below because it was already part of the original protocol
#### list above (see 'E85.8A' on the line above, and ICD10.AMYLOIDOSIS /
#### ICD10.DALYCARE in load_dalycare_entities.R) - only the 11 genuinely new
#### codes are listed here.
print_color('EXTENSION beyond the original method paper (subsequently approved to be part of cohort in 2026):\n', 'red')
print_color('11 additional amyloidosis ICD-10 codes approved for the DALYCARE cohort -- NOT part of the originally published protocol above (DE858A/AL-amyloidose was already covered by E85.8A above):\n', 'black')
print_color(' DE859   Amyloidose UNS\n', 'blue')
print_color(' DI680   Cerebral amyloidangiopati\n', 'blue')
print_color(' DE854   Lokaliseret amyloidose\n', 'blue')
print_color(' DE858   Anden amyloidose\n', 'blue')
print_color(' DE851A  Amyloidose med polyneuropati\n', 'blue')
print_color(' DE853   Sekundaer systemisk amyloidose\n', 'blue')
print_color(' DL990   Amyloidosis cutis UNS\n', 'blue')
print_color(' DL990A  Lichen-amyloidose\n', 'blue')
print_color(' DL990B  Makulaer amyloidose\n', 'blue')
print_color(' DL990C  Nodulaer amyloidose\n', 'blue')
print_color(' DM144   Artropati ved amyloidose\n\n', 'blue')

print_color('Please use ', 'black')
print_color('load_dataset()', 'blue') 
print_color(' to load one or more datasets from the data resource:\n', 'black')
print_color(' load_dataset(c("all_patients", "all_dalycare_diagnoses", "SUNDK_CLL"))\n', 'blue')
print_color('You may also load a subset of patients by specifying a cohort:\n', 'black')
print_color(' load_dataset("SUNDK_CLL", value = sample(all_patients$patientid, 1000))\n', 'blue')
print_color('Please note that LPR data do not contain patientid. Instead, use the backbone LPR/LPR3 datasets (see Table 1) to specify, e.g.:\n', 'black')
print_color(' load_dataset("SDS_t_sksube", value = sample(SDS_t_adm$k_recnum, 1000), column = "v_recnum")\n', 'blue')
print_color('Or load a subset of data with a specific value by specifying from which key column (please see Table S4) or try:\n', 'black')
print_color(' load_datasets_head()', 'blue')
print_color(' #to look for key column and then load data_subset:\n', 'black')
print_color(' load_dataset("SDS_t_sksube", value = "UXMA00", column = "c_opr")\n', 'blue')
print_color(' load_SKS_antineoplastics()', 'blue')
print_color(' #which loads antineoplastic SKS B-codes\n', 'black')
print_color('load_dataset("SDS_procedurer_andre", value = SKS.CLL_targeted, column = "procedurekode")\n', 'blue')
print_color(' load_npu_common()', 'blue')
print_color(' #which loads common NPU (biochemistry) codes\n', 'black')
print_color('load_dataset("SDS_laboratorieproevesvar", value = NPU.LYM, column = "analysiscode")\n', 'blue')
print_color('\nSee exhaustive lists of datasets by running', 'black')
print_color(' load_dataset()', 'blue')
print_color(' without any argument.\n', 'black')
print_color('NPU, ATC, ICD10, SNOMED, and SKS codes are found in tables with prefix "Codes_" \n', 'black')
print_color('We recommend that you define your cohort and calculate follow-up time like this:\n', 'black')
print_color(' load_dataset(c("all_patients", "all_dalycare_diagnoses"))\n', 'blue')
print_color(' load_dalycare_icd10()\n', 'blue')
print_color(' CLL = all_dalycare_diagnoses %>%\n   filter_first_diagnosis(ICD10.CLL, str_contains = FALSE)\n', 'blue')

IMPORT_TABLES = dalycare_catalog()$name[dalycare_catalog()$zone == 'import']
SP_DATASETS = IMPORT_TABLES[grepl('SP_', IMPORT_TABLES)]
PERSIMUNE_DATASETS = IMPORT_TABLES[grepl('PERSIMUNE_', IMPORT_TABLES)]
SDS_DATASETS = IMPORT_TABLES[grepl('SDS_', IMPORT_TABLES)]
SUNDK_DATASETS = IMPORT_TABLES[grepl('^(SUNDK|RKKP)_', IMPORT_TABLES)]

print_color('\nFor documentation of a core curated table (purpose, sources, one row, columns):\n', 'black')
print_color(" table_info('all_patients')\n", 'blue')
print_color(' table_info()', 'blue')
print_color(' #to list every documented table\n', 'black')
print_color(" table_info('all_patients', 'cleaning')", 'blue')
print_color(" #sections: 'cleaning', 'caveats', 'open_questions', 'columns'\n", 'black')

.pin_version = get0('.pkg_version', ifnotfound = 'current')
if(.pin_version == 'development') .pin_version = 'current'
print_color(paste0("\n\nPin this version to keep your analysis stable across data releases: use_version('", .pin_version, "').\n"), 'black')
print_color("use_version('current') for most current release (default)\n", 'black')
print_color('use_version() with no argument, or an unknown version, prints the list of available versions.\n', 'black')
rm(.pin_version)

## END OF SCRIPT
