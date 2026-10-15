CLL_CI = function(data, patientid, date_diagnosis = date_diagnosis) {
  #' @title
  #' CCI
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates CLL comorbidity index (CLL-CI) scores from vascular, gastro, and endocrinology defined SKS codes (LPR), ATC codes (EPIKUR), and ICD10 codes (all_diagnoses_icd10, formerly diagnoses_all).
  #' @note
  #' Input only requires variable with 'patientid'
  #' @examples
  #' load_dataset('all_dalycare_diagnoses')
  #' CLL_cohort = all_dalycare_diagnoses %>%
  #'   filter_first_diagnosis('DC911', str_contains = FALSE) #create CLL cohort
  #' CLL_CI_cohort = CLL_cohort %>% 
  #'    CLL_CI() # 
  
  #' @references 
  #' Rotbain et al. Blood Adv. 2022;6(8):2701-6
  if(!names(CLL_cohort) %in% 'patientid' %>% sort(decreasing = T) %>%  head(1)){stop('Your input must contain data$patientid')}
  cohort = data %>% pull(patientid) %>% unique() %>% sort
  print_color('Rotbain et al CLEP 2024 Suppl. Table S1', 'black')
  ICD10_vascular =  c('DI872', 'DI890', 'DI652', 'DI800','DI801', 'DI802', 
                      'DI803E', 'DT817C', 'DI26', 'DT817D', 'DI74', 'DI70', 
                      'DI739A', 'DG458', 'DG459', 'DI63', 'DI64', 
                      'DI21', 'DI22', 'DI20', 'DI24', 'DI25')
  ATC_vascular = c('B01AA', 'B01AB', 'B01AC', 'B01AE', 'B01AF') #x2/y
  SKS_vascular = c('KFNG', 'KAAL', 'KFNA',  'KFNC', 'KFNE', 'KFNF', 'KFNG', 'KPAE', 'KPAF', 'KPAH', 'KPAP', 
                   'KPAQ', 'KPBE', 'KPBF', 'KPBH', 'KPBQ', 'KPCE', 'KPCH', 'KPCQ', 'KPCP', 'KPDE', 'KPDF', 'KPDH', 
                   'KPDQ', 'KPDP', 'KPGH', 'KPEE', 'KPEF', 'KPEH', 'KPEN', 'KPEP', 'KPEQ', 'KPFE', 'KPFH', 'KPEQ')
  
  ICD10_gastro = c('DK25', 'DK26', 'DK27', 'DK28', 'DK227', 'DK85', 'DK860', 'DK861', 'DC15', 'DC16', 'DK921', 'DK920')
  ATC_gastro = c('A02BA', 'A02BC') #x2/y
  
  ICD10_endo = c('DE660C','DE660D','DE660E','DE660F','DE660G','DE660H', 'DE662', 'DE661', 
                 'DE05', 'DC50', 'DC73', 'DC74', 'DE271', 'DE272', 'DE273', 'DE274')
  ATC_endo = c('A10B', 'A10A', 'H03C')
  
  ICD10_codes = Codes_ICD10 %>% filter_str_detect(icd10, c(ICD10_vascular, ICD10_gastro, ICD10_endo)) %>% pull(icd10)
  ATC_codes = Codes_ATC %>% filter_str_detect(atc, c(ATC_vascular, ATC_gastro, ATC_endo)) %>% pull(atc)
  load_dataset('CODES_K')
  SKS_codes = CODES_K %>%  filter_str_detect(DIAG_CODE, SKS_vascular) %>% pull(DIAG_CODE)
  
  # load data
  load_dataset('all_diagnoses_icd10', ICD10_codes, column = 'diagnosis') # NB 2026-09-14: renamed from diagnoses_all; now excludes SDS_PATO/RKKP_LYFO/RKKP_DaMyDa entirely
  load_dataset('SDS_epikur', ATC_codes, column = 'atc') # 2 mio instead of 13
  
  # use lpr surgery procedures
  # NB 2026: view_sds_t_adm_t_sksopr was a Postgres view (join of SDS_t_adm
  # and SDS_t_sksopr on k_recnum). Replaced with direct join here.
  load_dataset('SDS_t_adm', cohort)
  load_dataset('SDS_t_sksopr', value = SKS_codes, column = 'c_opr')
  view_sds_t_adm_t_sksopr_subset = SDS_t_adm_subset %>%
    mutate(k_recnum = as.character(k_recnum)) %>%
    inner_join(SDS_t_sksopr_subset %>% mutate(v_recnum = as.character(v_recnum)),
               by = c('k_recnum' = 'v_recnum'), relationship = 'many-to-many')
  load_dataset('SDS_procedurer_kirurgi', SKS_codes, column = 'procedurekode')
  load_dataset('SDS_kontakter', cohort)
  lpr_admin = bind_rows(view_sds_t_adm_t_sksopr_subset %>% transmute(patientid, date_SKS = d_odto, SKS = c_opr, lpr_id = as.numeric(k_recnum), version = 'lpr'),
                        SDS_procedurer_kirurgi_subset %>%  transmute(date_SKS = dato_start, SKS = procedurekode, lpr_id = as.numeric(dw_ek_kontakt), version = 'lpr3') %>%
                          left_join(SDS_kontakter_subset %>%  transmute(patientid, lpr_id = as.numeric(dw_ek_kontakt)), by = 'lpr_id', relationship = 'many-to-many')) %>% 
    distinct() %>% 
    filter(!is.na(patientid)) %>% 
    right_join(data, 'patientid') %>%
    mutate(date_SKS = if_else(date_SKS > {{date_diagnosis}}, NA, date_SKS)) %>% # pre-diagnostic
    group_by(patientid) %>% 
    arrange(patientid, date_SKS) %>% 
    slice(1) %>% # corrects relationship = "many-to-many" for by = 'lpr_id' 
    ungroup()
  
  # Use catalog mtime (file modification time) as availability date proxy.
  # import/ tables have a parsed date from the folder name; core/current/ tables
  # do not, so mtime is the most reliable cross-zone fallback.
  # NB 2026-09-14: date_max_icd10 tracks 'all_diagnoses_icd10' (formerly
  # 'diagnoses_all'), which is what this function actually loads above --
  # not 'all_dalycare_diagnoses' (formerly 't_dalycare_diagnoses'), which is
  # never loaded here. The name below previously said 't_dalycare_diagnoses',
  # a pre-existing mismatch corrected here rather than carried forward.
  cat_dates = dalycare_catalog() %>%
    filter(name %in% c('SDS_epikur', 'SDS_kontakter', 'all_diagnoses_icd10')) %>%
    mutate(avail_date = as_date(ifelse(!is.na(date),
                                       as.character(date),
                                       format(as_date(mtime), '%Y%m%d')))) %>%
    group_by(name) %>% summarise(avail_date = max(avail_date, na.rm = TRUE))
  date_max_epikur = cat_dates %>% filter(name == 'SDS_epikur') %>% pull(avail_date)
  date_max_lpr    = cat_dates %>% filter(name == 'SDS_kontakter') %>% pull(avail_date)
  date_max_icd10  = cat_dates %>% filter(name == 'all_diagnoses_icd10') %>% pull(avail_date)
  date_max_min = min(c(date_max_epikur, date_max_lpr, date_max_icd10), na.rm = TRUE)
  
  cat(paste('\nCohort filtered to available lpr, epikur and icd10 availabilty before:\n', date_max_min, '\n'))
  
  data %>% 
    select(patientid, date_diagnosis) %>% 
    filter(date_diagnosis < as_date(date_max_min)) %>%
    left_join(bind_rows(SDS_epikur_subset %>%
                          transmute(patientid, date = eksd,  type = 'atc', code = atc) %>%
                          distinct(),
                        # NB 2026-09-14: all_diagnoses_icd10 (formerly diagnoses_all)
                        # already excludes PATO/RKKP_LYFO/RKKP_DaMyDa entirely at the
                        # source, so these two filters are now redundant for those
                        # three -- kept as-is since they still correctly exclude
                        # RKKP_CLL, which the table does include.
                        all_diagnoses_icd10_subset %>%
                          filter(tablename != 'dimpatologiskdiagnose') %>%
                          filter_str_detect(tablename, c('RKKP_', 'DaMyDa'), negate = T) %>%
                          filter_str_detect(diagnosis, '^D') %>%
                          transmute(patientid, date = date_diagnosis,  type = 'icd10' , code = diagnosis) %>% 
                          distinct(),
                        lpr_admin %>% 
                          filter(!is.na(date_SKS)) %>% 
                          transmute(patientid, date = date_SKS,  type = 'sks' , code = SKS) ), by = 'patientid') %>% 
    filter(date < date_diagnosis | is.na(date)) %>% 
    distinct() %>%
    mutate(icd10_group = ifelse(type == 'icd10' & str_detect(code, str_flatten(c(ICD10_vascular), '|')), 'vascular', NA),
           icd10_group = ifelse(type == 'icd10' & str_detect(code, str_flatten(c(ICD10_gastro), '|')), 'gastro', icd10_group),
           icd10_group = ifelse(type == 'icd10' & str_detect(code, str_flatten(c(ICD10_endo), '|')), 'endo', icd10_group),
           
           atc_group = ifelse(type == 'atc' & str_detect(code, str_flatten(c(ATC_vascular), '|')), 'vascular', NA),
           atc_group = ifelse(type == 'atc' & str_detect(code, str_flatten(c(ATC_gastro), '|')), 'gastro', atc_group),
           atc_group = ifelse(type == 'atc' & str_detect(code, str_flatten(c(ATC_endo), '|')), 'endo', atc_group)) %>% 
    group_by(patientid, atc_group) %>% 
    arrange(patientid, atc_group, date) %>% 
    mutate(DIFF = diff_years(date, lead(date))) %>%
    filter(DIFF <= 1 & type =='atc' | type !='atc') %>%
    ungroup() %>% 
    group_by(patientid, type, atc_group, icd10_group) %>% 
    slice(1) %>% 
    ungroup() %>% 
    right_join(data %>% 
                 filter(date_diagnosis < as_date(date_max_min)) %>% 
                 select(patientid), by = 'patientid') %>%
    select(-contains('date')) %>%
    mutate(code = ifelse(is.na(code), 'dummy_for_NA', code),
           vascular_score = ifelse(str_detect(code, str_flatten(c(ICD10_vascular, ATC_vascular, SKS_vascular), '|')), 1, 0),
           gastro_score = ifelse(str_detect(code, str_flatten(c(ICD10_gastro, ATC_gastro), '|')), 1, 0),
           endo_score = ifelse(str_detect(code, str_flatten(c(ICD10_endo, ATC_endo), '|')), 1, 0)) %>% 
    group_by(patientid) %>%
    mutate(vascular_score = sum(vascular_score),
           gastro_score = sum(gastro_score),
           endo_score = sum(endo_score)) %>% 
    mutate(code_sum = toupper(paste0(code, collapse = ';'))) %>%
    slice(1) %>% 
    ungroup() %>%
    mutate(vascular_score = ifelse(vascular_score ==0, 0, 1),
           gastro_score = ifelse(gastro_score ==0, 0, 1),
           endo_score = ifelse(endo_score ==0, 0, 1)) %>% 
    mutate(CLL_CI_score = rowSums(across(vascular_score:endo_score))) %>% 
    select(patientid, code_sum, contains('_score'))
}
