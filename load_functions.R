## description: This script loads all functions in DALY-CARE
## created: Sys.Date() #"2024-06-09"
## author: christian brieghel

# Determine package home from this file's location
# PKG_HOME must be set before sourcing this file.
# Use load_dalycare_package.R as the entry point, or set PKG_HOME manually:
#   PKG_HOME = '/ngc/projects2/dalyca_r/data/dalycare_r_package/current'
#   source(file.path(PKG_HOME, 'load_functions.R'))
if(!exists('PKG_HOME') || is.na(PKG_HOME)){
  stop('PKG_HOME is not set. Source via load_dalycare_package.R or set PKG_HOME first.', call.=FALSE)
}

# Attach core dependencies. The package code throughout uses the magrittr
# pipe (%>%) and dplyr/tibble verbs unqualified, so these must be on the
# search path before any package script is sourced.
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(lubridate)
  library(data.table)
})

# arrow: keep pull() on Arrow data returning an R vector (current behaviour).
# Without this, arrow warns that the default will change to a ChunkedArray.
options(arrow.pull_as_vector = TRUE)

# Attach plotting/survival/misc dependencies used unqualified (no `pkg::`
# prefix) inside several package functions -- e.g. KM_plot()/KM_plot_3()/
# tile_pairwise_survdiff()/pairwise_k_sample() call survminer::ggsurvplot()
# and survival::Surv()/survfit() bare, forest_plot_univariate_*() use ggpubr,
# right_truncation()'s documented workflow needs Publish, and some curated
# imports need haven::read_dta(). 
for(.pkg in c('survival', 'survminer', 'ggpubr', 'Publish', 'haven', 'docstring')){
  tryCatch(
    suppressPackageStartupMessages(library(.pkg, character.only = TRUE)),
    error = function(e) warning('dalycare: failed to load \'', .pkg, '\'.', call. = FALSE)
  )
}
rm(.pkg)

# Get current user for RCT access check
un = Sys.info()[['user']]
rct_un = tryCatch(
  data.table::fread('/ngc/projects2/dalyca_r/clean_r/tables/rct_users.txt') %>% pull(user),
  error = function(e) character(0)
)

# print_color from insight -- define a fallback for headless/Rscript sessions
if(!exists('print_color')){
  if(requireNamespace('insight', quietly = TRUE)){
    print_color = insight::print_color
  } else {
    print_color = function(text, ...) cat(text)
  }
}

print_color('\nLoading dalycare package...', 'black')
#### START with load_dataset ####

source(file.path(PKG_HOME, 'functions/load_dataset.R'))
source(file.path(PKG_HOME, 'functions/load_datasets_head.R'))
source(file.path(PKG_HOME, 'functions/helpers.R'))
source(file.path(PKG_HOME, 'functions/table_docs.R')) # table_info() -- documentation for core curated tables

print_color('\nLoading look-up tables...', 'black')
load_dataset(c('Codes_ATC', 'Codes_NPU', 'Codes_DST_DIAG_CODES', 'CODES_SNOMED', 'Codes_SKS', 'Codes_SHAK_long', 'Codes_sor'), quiet = TRUE)

#### lookup_tables
Codes_NPU = Codes_NPU %>% 
  dplyr::rename(NPU = `NPU code`,
                short_definition = `Short definition`)

Codes_ATC = Codes_ATC %>% 
  dplyr::rename(atc = class_code) %>% 
  mutate(name = tolower(class_name),
         atc = toupper(atc))

Codes_ICD10 = Codes_DST_DIAG_CODES %>% 
  transmute(Gyldig_fra = valid_from, #as_Date_from_seconds(`Gyldig fra`),
            Gyldig_til= valid_to, #as_Date_from_seconds(`Gyldig til`), #casfre 28/2-25
            icd10 = Kode,
            Tekst)

Codes_SNOMED = CODES_SNOMED %>% 
  select(c_snomedkode = SKSkode, Text = Kodetekst)

Codes_all = Codes_SKS %>%
  transmute(code = sks_kode, text = sks_langtekst, type = 'sks') %>%
  bind_rows(Codes_ATC %>% transmute(code = atc, text = class_name, type = 'atc')) %>%
  bind_rows(Codes_SNOMED %>% transmute(code = c_snomedkode, text = Text, type = 'snomed')) %>%
  bind_rows(Codes_ICD10 %>% transmute(code = icd10, text = Tekst, type = 'icd10')) %>%
  bind_rows(Codes_NPU %>% transmute(code = NPU, text = short_definition, type = 'npu')) %>%
  bind_rows(Codes_SHAK_long %>% transmute(code = kode, text = paste(hospital, department), type = 'shak'))


#### FUNCTIONS ####

# clean_SUNDK_LYFO_snomed was moved to independent script: See clean_SUNDK_LYFO.R
# clean_SUNDK_DAMYDA_snomed was moved to independent script: See clean_SUNDK_DAMYDA.R # 2025-02-24 - CB

##### Definers #####
source(file.path(PKG_HOME, 'functions/filter_first_diagnosis.R'))
source(file.path(PKG_HOME, 'functions/censor_med_keep_first.R'))
source(file.path(PKG_HOME, 'functions/clean_lab_values.R'))
source(file.path(PKG_HOME, 'functions/slice_closest_value.R'))
source(file.path(PKG_HOME, 'functions/clean_SDS_t_mikro.R'))
source(file.path(PKG_HOME, 'functions/CTCAE_lab.R'))
source(file.path(PKG_HOME, 'functions/scr_low_48h.R'))
source(file.path(PKG_HOME, 'functions/scr_low_7d.R'))
source(file.path(PKG_HOME, 'functions/AE_AKI.R'))
source(file.path(PKG_HOME, 'functions/AE_infection.R'))
source(file.path(PKG_HOME, 'functions/AE_hospitalization.R'))
source(file.path(PKG_HOME, 'functions/BMI.R'))
source(file.path(PKG_HOME, 'functions/AE_obesity.R'))
source(file.path(PKG_HOME, 'functions/TX_group.R')) #needs an update. e.g. MARTA
source(file.path(PKG_HOME, 'functions/COD.R')) 
source(file.path(PKG_HOME, 'functions/COD2.R')) 
source(file.path(PKG_HOME, 'functions/COD3.R')) 
source(file.path(PKG_HOME, 'functions/CCI_v2.R'))
source(file.path(PKG_HOME, 'functions/CCI.R'))
source(file.path(PKG_HOME, 'functions/CLL_CI.R')) 
## Recreates the SQL view as an R functions
source(file.path(PKG_HOME, 'functions/view_diagnoses_all_distinct.R')) 
source(file.path(PKG_HOME, 'functions/view_definition_first_diagnosis.R')) 
source(file.path(PKG_HOME, 'functions/view_definition_first_diagnosis_malignant.R')) 
source(file.path(PKG_HOME, 'functions/view_codes2persimune.R')) 
source(file.path(PKG_HOME, 'functions/NMI.R')) 
# source(file.path(PKG_HOME, 'functions/ATC_polypharmacy.R'))
source(file.path(PKG_HOME, 'functions/ATC_polypharmacy_v2.R'))
source(file.path(PKG_HOME, 'functions/ATC_AB.R'))
source(file.path(PKG_HOME, 'functions/ATC_opioids.R'))
source(file.path(PKG_HOME, 'functions/ATC_hypertensives.R'))
source(file.path(PKG_HOME, 'functions/qSOFA.R'))
source(file.path(PKG_HOME, 'functions/filter_virus.R'))
source(file.path(PKG_HOME, 'functions/clean_abbreviations.R'))
source(file.path(PKG_HOME, 'functions/right_truncation.R'))
source(file.path(PKG_HOME, 'functions/load_CTCAE.R')) # All labs
source(file.path(PKG_HOME, 'functions/clean_antineoplastics.R')) 
source(file.path(PKG_HOME, 'functions/create_antineoplastic_study_v3.R'))


##### RKKP #####
source(file.path(PKG_HOME, 'functions/CLL_WONT.R'))
source(file.path(PKG_HOME, 'functions/CLL_IPI_2023.R'))
source(file.path(PKG_HOME, 'functions/CLL_IPI.R'))
source(file.path(PKG_HOME, 'functions/MIPI.R'))
source(file.path(PKG_HOME, 'functions/IPSSWM.R'))
source(file.path(PKG_HOME, 'functions/rIPSSWM.R'))
source(file.path(PKG_HOME, 'functions/IPS.R'))
source(file.path(PKG_HOME, 'functions/MALT_IPI.R'))
source(file.path(PKG_HOME, 'functions/PRIMA_PI.R'))
source(file.path(PKG_HOME, 'functions/NCCN_IPI.R'))
source(file.path(PKG_HOME, 'functions/CNS_IPI.R'))
source(file.path(PKG_HOME, 'functions/aaIPI.R')) #needs split?

source(file.path(PKG_HOME, 'functions/MAYO_20_20_20.R')) # Update when RKKP_DAMYDA has been updated
source(file.path(PKG_HOME, 'functions/R_ISS.R')) 
source(file.path(PKG_HOME, 'functions/R2_ISS.R'))
source(file.path(PKG_HOME, 'functions/RW_ISS.R'))

##### clean SUNDK #####
source(file.path(PKG_HOME, 'clean_SUNDK/clean_SUNDK_CLL.R'))
source(file.path(PKG_HOME, 'clean_SUNDK/clean_SUNDK_DAMYDA.R'))
source(file.path(PKG_HOME, 'clean_SUNDK/clean_SUNDK_LYFO.R'))

##### clean SDS #####
source(file.path(PKG_HOME, 'clean_SDS/clean_SDS_pato.R'))

#### plotters ####
source(file.path(PKG_HOME, 'functions/KM_plot.R')) 
source(file.path(PKG_HOME, 'functions/KM_plot_3.R'))
source(file.path(PKG_HOME, 'functions/tile_pairwise_survdiff.R')) 
source(file.path(PKG_HOME, 'functions/pairwise_k_sample.R')) 
source(file.path(PKG_HOME, 'functions/forest_plot_univariate_os.R')) 
source(file.path(PKG_HOME, 'functions/forest_plot_univariate_hospitalization.R')) 
source(file.path(PKG_HOME, 'functions/forest_plot_univariate_infection.R')) 

#### Loaders/Writers ####
source(file.path(PKG_HOME, 'functions/transform_2_ERIC.R'))
source(file.path(PKG_HOME, 'functions/write_utable.R'))
source(file.path(PKG_HOME, 'functions/load_blood_culture_SP.R'))
source(file.path(PKG_HOME, 'functions/load_biochemistry.R'))
source(file.path(PKG_HOME, 'functions/load_all_data_2025.R')) # former aggregate_all_data.R - updated 8/1-26 - CB

load_SKS_antineoplastics <- function(){
source(file.path(PKG_HOME, 'load_SKS_antineoplastic.R'))
}

load_ATC_antineoplastics <- function(){
  source(file.path(PKG_HOME, 'functions/load_atc_antineoplastic_codes.R'))
}

load_SKS_radiotherapy_codes <- function(){
  source(file.path(PKG_HOME, 'load_radiotherapy_codes.R'))
}

load_SKS_antimicrobials <- function(){
  source(file.path(PKG_HOME, 'load_SKS_antimicrobials.R'))
}

load_all_variables = function(){
  read_csv2(file.path(PKG_HOME, 'shared_projects/DALYCARE_methods/dummy_tables/All_variables.csv'))
}

load_dalycare_icd10 = function() {
  source(file.path(PKG_HOME, 'load_dalycare_entities.R')) 
}

load_npu_common = function() {
  source(file.path(PKG_HOME, 'NPU_codes.R')) 
}

help_info = function(){
  source(file.path(PKG_HOME, 'help_info.R'))
}

# load_treatment_table = function(){
#   treatment_table <- readRDS('/ngc/projects2/dalyca_r/andkat_r/medicine data/data/output tables/table_treatment_clean.rds')
# }

load_snomed_myeloid_neoplasia = function(){
  source(file.path(PKG_HOME, 'load_snomed_myeloid_neoplasia.R'))
}

load_snomed_molecular = function(){
  source(file.path(PKG_HOME, 'load_snomed_molecular.R'))
}


if(un %in% rct_un){
  tryCatch(
    source(file.path(PKG_HOME, 'load_clinical_trial_data.R')), # unmuted 1/11-24 # should not be uploaded to github
    error = function(e) warning('dalycare: clinical trial data not loaded: ', conditionMessage(e), call. = FALSE)
  )
}

#### COLORS ####
Osborne.Ext = c('#DADEDF', '#A7AFB2', '#6F7C80', '#000000',
                '#3C6478', '#0D3D56', '#0C374D', '#093145',
                '#43ABC9', '#1496BB', '#1287A8', 'blue',
                '#B5C689', '#A3B86C', '#93A661', '#829356',
                '#EFD469', '#EBC944', '#D3B53D', '#BCA136',
                '#F58B4C', '#F26D21', '#DA621E', '#C2571A',
                '#CD594A','#C02F1D', '#AD2A1A', '#9A2617',
                '#67001f', '#005582')
JCO  = c('#0073C2FF', '#EFC000FF', '#868686FF', '#CD534CFF', '#7AA6DCFF', '#003C67FF', '#8F7700FF')
BLOOD2023 = c('#a40128', '#2e4a7c', '#69a841', '#512562', '#da8c15', '#4d5565') #BLOOD
BLOOD = c('#C41130', '#135284', '#36B44B', '#5C256B', '#F7901E', '#29ABE2')
JAMA = c('#374e55', '#df8f44', '#00a1d5', '#b24745', '#79af97') # JAMA 2025
LANCET = c('#00468bff', '#ed0000ff', '#42b540ff', '#0099b4ff' )
palette(BLOOD)
options(scipen = 9999)

# Default the on-screen X11 graphics device to the 'cairo' font backend
# instead of R's legacy Xlib backend. Xlib expects old X core fonts that
# are often missing on modern Linux servers (NGC included), causing
# "X11 protocol error: BadFont (invalid Font parameter)" and a plot that
# silently never appears -- no error, just nothing on screen -- when
# connecting via X11-forwarding (e.g. through MobaXTerm, one of the two
# documented ways to reach the NGC HPC). cairo uses the system's normal
# fontconfig instead and avoids this. Confirmed fix 2026-09-18.
# Guarded: only set where X11/cairo are actually available, so this is a
# no-op (not an error) on a platform without them, e.g. RStudio Server
# (renders its Plots pane over the browser, not via X11 at all) or a local
# Windows/Mac R session.
if(capabilities('X11') && capabilities('cairo')){
  options(X11type = 'cairo')
}

## Read package version if available
.pkg_version = tryCatch({
  releases_file = file.path(PKG_HOME, 'package_releases.R')
  if(!file.exists(releases_file)) stop('no releases file')
  pv = readLines(releases_file, warn=FALSE)
  pv = pv[grepl("DALYCARE_PACKAGE_VERSION", pv)][1]
  sub(".*DALYCARE_PACKAGE_VERSION = '([0-9]+)'.*", '\\1', pv)
}, error = function(e) 'development')

print_color(paste0('\nPackage loaded. Version: ', .pkg_version, '\n'), 'black')

load_dataset() # print catalog

source(file.path(PKG_HOME, 'help_info.R'))

print_color('\n\nRun help_info() for full documentation.', 'black')
