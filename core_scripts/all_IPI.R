## all_IPI
# Builds the all_IPI table for CORE, see docs/all_IPI.md
# Originally IPI_eop_2026.R, updated "2026-10-08"
#
# The logic lives in build_all_IPI(). The code at the bottom loads the data and writes
# the csv when this script is run directly. A pipeline can instead set
# options(dalycare.define_only = TRUE), source this file and call build_all_IPI() itself.

#' @param SUNDK_CLL,SUNDK_DaMyDa,SUNDK_LYFO raw SUNDK registries (from SUNDK_DATASETS)
#' @param all_patients the all_patients table
#' @param IGHV_IMGT IGHV IMGT dataset (patientid, IGHV)
#' @param SDS_fctrekvisition_subset SDS_fctrekvisition loaded for the CLL patients
#' @param all_laboratory_measurements_subset all_laboratory_measurements filtered on analysiscode
#'   to the NPU codes ALB, B2M, LDH, IGM, HGB, LEU and LYM
#' @return one row per patient and disease: patientid, IPI, index, disease
#' Needs the dalycare package (and clean_SDS_pato.R) to be loaded.
build_all_IPI = function(SUNDK_CLL, SUNDK_DaMyDa, SUNDK_LYFO, all_patients, IGHV_IMGT,
                         SDS_fctrekvisition_subset, all_laboratory_measurements_subset){

  #### SUNDK IPI ####
  ALL_SUNDK = c(SUNDK_CLL$patientid, SUNDK_DaMyDa$patientid, SUNDK_LYFO$patientid) %>% unique()
  # ALL_SUNDK %>% n_distinct()

  load_snomed_molecular()
  pato_clean = SDS_fctrekvisition_subset %>% 
    clean_SDS_pato() %>% 
    filter(snomed %in% SNOMED.CLL_IGHV)

  load_npu_common()
  BIOCHEMISTRY = all_laboratory_measurements_subset %>% 
      filter(patientid %in% ALL_SUNDK) %>% 
    clean_lab_values()

  lab_helper = function(data, lab_name){
    name = lab_name
    npu_code = get(paste0('NPU.', {{lab_name}}))
    data %>% 
    filter(NPU %in% npu_code) %>% 
      transmute(patientid, 
                'date_{name}' := as_date(samplingdate), 
                '{name}_value' := value2)
  }

  B2M = BIOCHEMISTRY %>% 
    lab_helper(lab_name = 'B2M') %>% 
    mutate(B2M_lab = factor(ifelse(B2M_value < 4.0, '<4.0 mg/L', '>4.0 mg/L')))

  ALB = BIOCHEMISTRY %>% 
    lab_helper(lab_name = 'ALB')

  IGM = BIOCHEMISTRY %>% 
    lab_helper(lab_name = 'IGM')

  HGB = BIOCHEMISTRY %>% 
    lab_helper(lab_name = 'HGB')

  WBC =  BIOCHEMISTRY %>% 
    lab_helper(lab_name = 'LEU')

  ALC = BIOCHEMISTRY %>% 
    lab_helper(lab_name = 'LYM')

  LDH =  BIOCHEMISTRY %>% 
    lab_helper(lab_name = 'LDH')

  #### CLL ####
  # SUNDK_CLL$CPR_Doedsdato %>% summary # Use this! 
  # SUNDK_CLL$FU_Doedsdato %>% summary 
  # all_patients %>% nrow_npatients()
  # LAB_IGHVIMGT$ighv
  SUNDK_CLL_clean = SUNDK_CLL %>% 
    clean_SUNDK_CLL() %>% 
    left_join(all_patients %>% select(-sex, -date_birth, -date_death_fu), 'patientid') %>%
    left_join(IGHV_IMGT %>% 
                transmute(patientid, IGHVIMGT = factor(IGHV)) %>% 
                group_by(patientid) %>% 
                arrange(patientid, IGHVIMGT) %>% 
                slice(1) %>% 
                ungroup(), 'patientid') %>%
    left_join(pato_clean %>% 
                transmute(patientid, 
                          snomed = recode_factor(text,
                                                   `IgHV gen ikke hypermuteret` = 'Unmutated',
                                                   `IgHV gen hypermuteret` = 'Mutated')) %>% 
                arrange(patientid, snomed) %>% 
                group_by(patientid) %>% 
                slice(1) %>% 
                ungroup(), 'patientid') %>%
    mutate(IGHV = if_else(is.na(IGHV), IGHVIMGT, IGHV)) %>%
    mutate(IGHV = if_else(is.na(IGHV), snomed, IGHV)) %>%
    left_join(B2M, 'patientid', relationship = "many-to-many") %>% 
    slice_closest_value(date_diagnosis, date_B2M, value = B2M_value, interval_days = c(-30, 30), name = 'B2M') %>% 
    mutate(B2M = if_else(is.na(B2M), B2M_lab, B2M)) %>% 
    CLL_IPI() %>%
    mutate(CLL_IPI = if_else(IPI_score_minus_B2M >= 7, 'Very high', CLL_IPI),
           CLL_IPI = if_else(IPI_score_minus_IGHV >= 7, 'Very high', CLL_IPI),
           CLL_IPI = if_else(IPI_score_minus_B2M == 4, 'High', CLL_IPI),
           CLL_IPI = if_else(IPI_score_minus_IGHV == 4, 'High', CLL_IPI),
           CLL_IPI = factor(CLL_IPI, levels = c('Low', 'Intermediate', 'High', 'Very high'))) %>% 
    mutate(year = year(date_diagnosis))

  # SUNDK_CLL_clean %>% select(patientid, IGHV_new, IGHV, IGHVIMGT, snomed) %>% 
  #   arrange(IGHV_new, IGHV, IGHVIMGT, snomed) %>% View
  # SUNDK_CLL_clean$CLL_IPI %>% table(exclude = NULL)

  #### DAMYDA ####
  SUNDK_DaMyDa_clean = SUNDK_DaMyDa %>% 
    clean_SUNDK_DAMYDA() %>% 
    left_join(B2M, 'patientid', relationship = "many-to-many") %>% 
    slice_closest_value(date_diagnosis, date_B2M, value = B2M_value,  interval_days = c(-30, 30)) %>% 
    mutate(B2M = if_else(is.na(B2M_diagnosis), B2M_value, B2M_diagnosis))  %>% 
  
    left_join(ALB, 'patientid', relationship = "many-to-many") %>% 
    slice_closest_value(date_diagnosis, date_ALB, value = ALB_value, interval_days = c(-30, 30), name = 'ALB_val') %>% 
    mutate(ALB = if_else(is.na(ALB_diagnosis), ALB_value, ALB_diagnosis)) %>% 
  
    left_join(LDH, 'patientid', relationship = "many-to-many") %>% 
    slice_closest_value(date_diagnosis, date_LDH, value = LDH_value, interval_days = c(-30, 30), name = 'LDH_val') %>% 
    mutate(LDH = if_else(is.na(LDH_diagnosis), LDH_value, LDH_diagnosis)) %>% 
  
    mutate(ISS_ext = ifelse(ALB >= 35 & B2M <3.5, 1, 2),
           ISS_ext = ifelse(B2M >= 5.5, 3, ISS_ext),
           ISS = ifelse(is.na(ISS_diagnosis), ISS_ext, ISS_diagnosis)) %>% 
    mutate(LDH_high = ifelse(age_diagnosis < 70 & LDH > 205, 'Yes', NA),
           LDH_high = ifelse(age_diagnosis < 70 & LDH <= 205, 'No', LDH_high),
           LDH_high = ifelse(age_diagnosis >= 70 & LDH > 255, 'Yes', LDH_high),
           LDH_high = ifelse(age_diagnosis >= 70 & LDH <= 255, 'No', LDH_high),
           FISH.score = ifelse(FISH_t4_14 =='Yes' | FISH_t14_16 =='Yes' | FISH_del17p =='Yes', 'Yes', 'No')) %>%
    mutate(RISS_Addon = ifelse(LDH_high =='Yes' | FISH.score =='Yes', 'Yes', 'No')) %>%
    mutate(RISS = ifelse(ISS == 3 & RISS_Addon == 'Yes', 3, NA),
           RISS = ifelse(ISS == 1 & RISS_Addon == "No", 1, RISS),
           RISS = ifelse(ISS == 3 & RISS_Addon == "No", 2, RISS),
           RISS = ifelse(ISS == 1 & RISS_Addon == "Yes", 2, RISS),
           RISS = ifelse(ISS == 2, 2, RISS)) 

  # SUNDK_DaMyDa_clean$ISS %>% table(exclude = NULL)
  # SUNDK_DaMyDa_clean$RISS %>% table(exclude = NULL)

  #### LYFO ####
  SUNDK_LYFO_clean = SUNDK_LYFO %>% 
    clean_SUNDK_LYFO() %>% 
    left_join(HGB, by = 'patientid', relationship = "many-to-many") %>%
    slice_closest_value(date_diagnosis, date_HGB, value = HGB_value, interval_days = c(-30,30), name = 'HGB') %>% 
    left_join(WBC, by = 'patientid', relationship = "many-to-many") %>% 
    slice_closest_value(date_diagnosis, date_LEU, value = LEU_value, interval_days = c(-30,30), name = 'WBC') %>% 
    left_join(ALC, by = 'patientid', relationship = "many-to-many") %>% 
    slice_closest_value(date_diagnosis, date_LYM, value = LYM_value, interval_days = c(-30,30), name = 'ALC') %>% 
    left_join(LDH,  by = 'patientid', relationship = "many-to-many") %>% 
    slice_closest_value(date_diagnosis, date_LDH, value = LDH_value, interval_days = c(-30,30), name = 'LDH') %>% 
    left_join(IGM,  by = 'patientid', relationship = "many-to-many") %>% 
    slice_closest_value(date_diagnosis, date_IGM, value = IGM_value, interval_days = c(-30,30), name = 'IGM')  %>%
    mutate(HB_diagnosis = ifelse(is.na(HB_diagnosis), valueHGB, HB_diagnosis),
           WBC_diagnosis = ifelse(is.na(WBC_diagnosis), valueWBC, WBC_diagnosis),
           ALC_diagnosis = ifelse(is.na(ALC_diagnosis), valueALC, ALC_diagnosis),
           LDH_diagnosis = ifelse(is.na(LDH_diagnosis), valueLDH, LDH_diagnosis),
           IgM_gL_diagnosis = ifelse(is.na(IgM_gL_diagnosis), valueIGM, IgM_gL_diagnosis))

  # SUNDK_LYFO_clean$RIPI_diagnosis

  #### all_IPI ####
  all_IPI = bind_rows(SUNDK_CLL_clean %>% 
                        transmute(patientid, 
                                  IPI = CLL_IPI,
                                  index = 'CLL-IPI',
                                  # IPI = recode_factor(CLL.IPI,  `Very high` = 'High'),
                                  disease = 'CLL'),
                      SUNDK_DaMyDa_clean %>% 
                        filter(subtype == 'DC900') %>% 
                        transmute(patientid, 
                                  IPI = recode_factor(RISS,
                                                      `1` = 'Low',
                                                      `2` = 'Intermediate',
                                                      `3` = 'High'),
                                  index = 'R-ISS',
                                  disease = 'MM'),
                      SUNDK_LYFO_clean %>% 
                        filter(subtype  == 'DLBCL') %>% 
                        transmute(patientid, 
                                  IPI = RIPI_diagnosis,
                                  index = 'R-IPI',
                                  disease = 'DLBCL'),
                      SUNDK_LYFO_clean %>% 
                        filter(subtype  == 'FL') %>% 
                        transmute(patientid, 
                                  IPI = FLIPI2_diagnosis,
                                  index = 'FLIPI2',
                                  disease = 'FL'),
                      SUNDK_LYFO_clean %>% 
                        filter(subtype  == 'MCL') %>% 
                        MIPI(subtype = subtype) %>% 
                        transmute(patientid, 
                                  IPI = MIPI,
                                  index = 'MIPI',
                                  disease = 'MCL'),
                      SUNDK_LYFO_clean %>% 
                        filter(subtype  == 'WM') %>% 
                        IPSSWM() %>%
                        # rIPSSWM(SUBTYPE = SUBTYPE) %>%
                        transmute(patientid, 
                                  IPI = IPSSWM,
                                  # IPI = r.IPSSWM,
                                  index = 'IPSSWM',
                                  disease = 'LPL'),
                      SUNDK_LYFO_clean %>% 
                        filter(subtype == 'MZL') %>% 
                        MALT_IPI() %>% 
                        transmute(patientid, 
                                  IPI = MALT_IPI,
                                  index = 'MALT-IPI',
                                  disease = 'MZL'),
                      SUNDK_LYFO_clean %>% 
                        filter(subtype =='cHL') %>% 
                        IPS() %>%
                        mutate(IPS = cut(IPS.score, c(-Inf, 2, Inf), labels = c('Low', 'High'))) %>% 
                        transmute(patientid, 
                                  # IPI = IPS,
                                  IPI = cut(IPS.score, c(-Inf, 2, Inf), labels = c('Low', 'High')),
                                  index = 'IPS',
                                  disease = 'cHL'))  %>% 
    mutate(IPI = factor(IPI, levels =  c('Low', 'Intermediate', 'High', 'Very high'))) %>% 
    filter(!is.na(IPI))

  # all_IPI %>% nrow_npatients()
  # all_IPI$IPI %>% table(exclude = NULL)

  all_IPI
}

#### Run ####
if(!isTRUE(getOption('dalycare.define_only'))){
  # Set paths before running
  PACKAGE_DIR = '/ngc/projects2/dalyca_r/data/dalycare_r_package'  # folder containing load_dalycare_package.R
  CORE_DIR = '/ngc/projects2/dalyca_r/data/core'  # all_IPI.csv is written to current/ and a dated copy to old/
  source(file.path(PACKAGE_DIR, 'load_dalycare_package.R'))
  source(file.path(PACKAGE_DIR, 'clean_SDS', 'clean_SDS_pato.R'))

  load_dataset(c(SUNDK_DATASETS, 'all_patients', 'IGHV_IMGT_20261006'))
  load_dataset('SDS_fctrekvisition', SUNDK_CLL$patientid)
  load_npu_common()
  load_dataset('all_laboratory_measurements', 
               value = c(NPU.ALB, NPU.B2M, NPU.LDH, NPU.IGM, NPU.HGB, NPU.LEU, NPU.LYM, NPU.LDH),
               'analysiscode') 

  all_IPI = build_all_IPI(SUNDK_CLL, SUNDK_DaMyDa, SUNDK_LYFO, all_patients, IGHV_IMGT_20261006,
                          SDS_fctrekvisition_subset, all_laboratory_measurements_subset)

  all_IPI %>% nrow_npatients()
  all_IPI$IPI %>% table(exclude = NULL)

  # Same structure as the other core tables: core/current/<name> and a dated copy core/old/<name>_<YYYYMMDD>
  write_csv2(all_IPI, file.path(CORE_DIR, 'current', 'all_IPI.csv'))
  write_csv2(all_IPI, file.path(CORE_DIR, 'old', paste0('all_IPI_', format(Sys.Date(), '%Y%m%d'), '.csv')))
}
