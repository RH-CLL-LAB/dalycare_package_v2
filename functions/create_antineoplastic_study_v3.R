
create_antineoplastic_study = function(data, atc = NULL, atc2 = NULL, atc3 = NULL, 
                                       interval_within_polypharmacy = 365, atc_level = 3){
  #' @title
  #' create_antineoplastic_study
  #' @author
  #' christian brieghel
  #' @description
  #' Uses first DALY-CARE diagnosis from 'all_dalycare_diagnoses' (formerly 't_dalycare_diagnoses') to identify antineoplastics and create atc study  #'
  #' @examples
  #' load_dataset(c('all_dalycare_diagnoses', 'all_patients')) #loads all DALY-CARE diagnoses
  #' load_dalycare_icd10()
  #' load_ATC_antineoplastics()
  #' CLL = all_dalycare_diagnoses %>%
  #'    filter_first_diagnosis(ICD10.CLL, str_contains = FALSE) %>%
  #'    create_antineoplastic_study(atc = ATC.BTKi)
  #'
  #' MM = all_dalycare_diagnoses %>%
  #'    filter_first_diagnosis(ICD10.MM, str_contains = FALSE) %>% 
  #'    create_antineoplastic_study(atc = ATC.lenalidomide, atc2 = ATC.bortezomib, atc3 = ATC.daratumumab)
  #' @references 
  #' Brieghel et al. CLEP 2025
  
  # NB 2026-09-18: 't_antineoplastic_treatment_draft_20250616' was a stale
  # draft table name that no longer exists -- the current core table is
  # 'all_antineoplastic_treatment'. See TODO.md: this table is currently
  # delivered as CSV in core/current/ instead of parquet like its siblings,
  # so it may still fail to be found until the pipeline/core team fixes the
  # delivery format (load_dataset()'s catalog deliberately only scans .csv
  # under lookup_tables/, not core/).
  load_dataset(c('all_antineoplastic_treatment'), data$patientid)
  load_dataset('SP_PatientInfo')
  # NB 2026-09-18: SP_PatientInfo no longer has a date_received column (it's
  # gone from every table in the package, checked). Follow-up was previously
  # censored at the latest per-row date_received; the equivalent now is the
  # table's own delivery date -- the dated folder it was imported from (e.g.
  # import/SP/<date>/), which dalycare_catalog()$date already resolves per
  # table, respecting any active use_version()/pin_data_version() pin.
  SP_max_date_fu = dalycare_catalog() %>%
    filter(name == 'SP_PatientInfo') %>%
    pull(date) %>%
    as.character() %>%
    ymd()
  
  if(!is.null({{atc2}})){
    print('atc2')
    atc2_data = data %>% 
      left_join(all_antineoplastic_treatment_subset, 'patientid') %>% 
      clean_antineoplastics() %>% 
      filter(atc %in% {{atc2}}) %>% 
      group_by(patientid) %>% 
      arrange(date_start) %>% 
      mutate(date_max_atc2 = max(date_start)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(patientid, atc2 = atc, tx_text_atc2 = tx_text, date_start_atc2 = date_start, date_max_atc2)
  }
  if(!is.null({{atc3}})){
    print('atc3')
    atc3_data = data %>% 
      left_join(all_antineoplastic_treatment_subset, 'patientid') %>% 
      clean_antineoplastics() %>% 
      filter(atc %in% {{atc3}}) %>% 
      group_by(patientid) %>% 
      arrange(date_start) %>% 
      mutate(date_max_atc3 = max(date_start)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(patientid, atc3 = atc, tx_text_atc3 = tx_text, date_start_atc3 = date_start, date_max_atc3)
  }
    
  if(!is.null({{atc}})){ # default
  data = data %>% 
    left_join(all_antineoplastic_treatment_subset, 'patientid') %>% 
    clean_antineoplastics() %>% 
    filter(atc %in% {{atc}}) %>% 
    group_by(patientid) %>% 
    arrange(date_start) %>% 
    mutate(date_max = max(date_start)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(date_start_latest_atc = date_start,
           age_at_latest_atc = diff_years(date_birth, date_start_latest_atc))
  }
  
  if(!is.null({{atc2}})){
    data = data %>% 
      inner_join(atc2_data, 'patientid') %>% 
      mutate(date_start_latest_atc = pmax(date_start, date_start_atc2), #overwrite date_start_latest_atc as pmax
             time_atc_atc2 = diff_years(date_start, date_start_atc2))
  }
  if(!is.null({{atc3}})){
    if(is.null({{atc2}})){errorCondition('atc2 not defined')}
    data = data %>% 
      inner_join(atc3_data, 'patientid') %>% 
      mutate(date_start_latest_atc = pmax(date_start, date_start_atc2, date_start_atc3), #overwrite date_start_latest_atc as pmax
             time_atc2_atc3 = diff_years(date_start_atc2, date_start_atc3),
             time_atc_atc3 = diff_years(date_start, date_start_atc3))
  }
  
  load_dataset(c('SP_AdministreretMedicin', 'SP_ADT_haendelser', 'all_diagnoses_icd10', 'SDS_epikur'), data$patientid) # NB 2026-09-14: renamed from diagnoses_all
  
  if(is.null({{atc}})){
    warning('\natc is NULL. Time to event will be calculated from date of diagnosis
            \n_latest_atc is thus at date of diagnosis')
    data = data %>% 
      group_by(patientid) %>% 
      arrange(date_diagnosis) %>% 
      slice(1) %>% 
      ungroup() %>% 
      mutate(date_start_latest_atc = date_diagnosis)
  }
  
  PolyRx_data = data %>% 
    select(patientid, date_start_latest_atc) %>% 
    left_join(SDS_epikur_subset, 'patientid') %>% 
    mutate(diff_rx = diff_days(date_start_latest_atc, eksd)) %>% 
    filter(diff_rx < 0,
           diff_rx > - {{interval_within_polypharmacy}}) %>% #.default within a year from diagnosis
    ATC_polypharmacy(level = {{atc_level}}) 
  # NB 2026-09-14: all_diagnoses_icd10 (formerly diagnoses_all) no longer
  # contains SDS_PATO/RKKP_LYFO/RKKP_DaMyDa rows at all (used to be
  # translated, now excluded outright) -- CCI(include_LC_score = TRUE) below
  # will silently see less second-malignancy/LC history than before for
  # patients whose relevant diagnoses came from those three sources. No
  # error, just fewer matches. Flagged, not fixed -- see
  # AENDRINGER_dalycare_package.md F3e.
  CCI_data = data %>%
    select(patientid, date_start_latest_atc) %>%
    left_join(all_diagnoses_icd10_subset, 'patientid') %>%
    mutate(diff_dx = diff_days(date_start_latest_atc, date_diagnosis)) %>%
    filter(diff_dx < 0) %>%
    CCI(include_LC_score = TRUE)
  SP_infection = SP_AdministreretMedicin_subset %>% ATC_AB() %>% AE_infection() %>%
    dplyr::rename(atc_AB = atc) %>%
    # NB 2026-09-18: any_of() instead of a bare vector -- datetime_created no
    # longer exists in this table (removed from the source), and select(-c(...))
    # errors on a column that isn't there. any_of() silently skips columns
    # that don't exist instead of failing, for the others too.
    select(-any_of(c('mar_action_c', 'name', 'medication_id', 'plan_id', 'datetime_created')))
  SP_ADT = SP_ADT_haendelser_subset %>% 
    AE_hospitalization()
  
  data %>% 
    left_join(SP_infection, 'patientid') %>% #infections
    mutate(date_first_infection = if_else(date_infection < date_start_latest_atc, NA, date_infection)) %>% # Only infections after tx/date_start_latest_atc
    group_by(patientid) %>% 
    arrange(date_first_infection) %>% 
    slice(1) %>% 
    ungroup() %>% 
    left_join(SP_ADT, 'patientid') %>% #hospitalizations
    mutate(date_first_hospitalization = if_else(date_hospitalization < date_start_latest_atc, NA, date_hospitalization)) %>% # Only hospitalizations after tx/date_start_latest_atc
    group_by(patientid) %>% 
    arrange(date_first_hospitalization) %>% 
    slice(1) %>% 
    ungroup() %>% 
    left_join(CCI_data, 'patientid') %>% # add CCI data
    mutate(DIAG_SUM = ifelse(is.na(DIAG_SUM), '', DIAG_SUM),
           Primary_cancer = ifelse(is.na(Primary_cancer), 'No', Primary_cancer),
           across(contains('.score'), ~ ifelse(is.na(.), 0, .)),
           across(contains('_score'), ~ ifelse(is.na(.), 0, .))) %>% 
    left_join(PolyRx_data, 'patientid') %>%  # add Polypharmacy data
    mutate(ATC_SUM = ifelse(is.na(ATC_SUM), '', ATC_SUM),
           across(start:n.ATC, ~ifelse(is.na(.), 0, .)),
           ATC.group = factor(if_else(is.na(ATC.group), '0', ATC.group), c('0', '1', '2', '3', '4', '5', '6', '7',  '8', '9' , '>9')), 
           ATC.group2 = factor(if_else(is.na(ATC.group2), '0-2', ATC.group2), c('0-2', '3-4', '5-6', '7-8', '9-10', '>10')),
           ATC.group3 = factor(if_else(is.na(ATC.group3), '0-4', ATC.group3), c('0-4', '5-8', '>9')),
           Polypharmacy = ifelse(is.na(Polypharmacy), 'No', Polypharmacy)) %>%
    mutate(time_tx_death_fu = diff_years(date_start_latest_atc, date_death_fu), #time from atc 1, 2 or 3, whichever came last; until death or end of followup, whichever came first
           # SP_max_date_fu is already a Date (see its computation above, fixed 2026-09-18) -- no need to re-parse with ymd() here
           date_received = if_else(patientid %in% SP_PatientInfo$patientid, SP_max_date_fu, NA),
           date_received_fu = if_else(date_received < date_death_fu, date_received, date_death_fu), # only until SP fu
           date_first_infection_fu = if_else(is.na(date_first_infection), date_received_fu, date_first_infection),
           time_infection_death_fu = diff_years(date_start_latest_atc, date_first_infection_fu), #time from atc 1, 2 or 3, whichever came last; until infection, death or end of followup, whichever came first
           infection = ifelse(is.na(date_first_infection), 0, 1),
           infection = ifelse(patientid %in% SP_PatientInfo$patientid, infection, NA),
           
           date_first_hospitalization_fu = if_else(is.na(date_first_hospitalization), date_received_fu, date_first_hospitalization),
           time_hospitalization_death_fu = diff_years(date_start_latest_atc, date_first_hospitalization_fu),  #time from atc 1, 2 or 3, whichever came last; until hospitalization, death or end of followup, whichever came first
           hospitalization = ifelse(is.na(date_first_hospitalization), 0, 1),
           hospitalization = ifelse(patientid %in% SP_PatientInfo$patientid, hospitalization, NA),
           strata = 1 # needed for KM_plot_3
           ) %>% 
    mutate(time_tx_death_fu = ifelse(time_tx_death_fu>= 0, time_tx_death_fu, NA),
           time_hospitalization_death_fu = ifelse(time_hospitalization_death_fu>= 0, time_hospitalization_death_fu, NA),
           time_infection_death_fu = ifelse(time_infection_death_fu>= 0, time_infection_death_fu, NA)) 
}
