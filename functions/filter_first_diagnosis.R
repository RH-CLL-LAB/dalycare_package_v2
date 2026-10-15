filter_first_diagnosis = function(data, diagnosis = NULL, str_contains = TRUE, multiple = 'first'){
  #' @title
  #' filter_first_diagnosis
  #' @author
  #' christian brieghel
  #' @description
  #' Defines first DALY-CARE diagnosis from 'all_dalycare_diagnoses' (formerly 't_dalycare_diagnoses') as the earliest occurrence and calculates KM years from table 'all_patients' (formerly 'patient').
  #' Argument multiple = 'both' allows for patients with both diagnoses
  #' @note
  #' Note that filter_first_diagnosis comes with a caveat, because it uses all diagnoses regardless of origin to define the first occurring diagnosis.
  #' Thus, patients may in fact have been diagnosed years prior to 2002, and if they are then admitted with a LC diagnosis after 2002,
  #' this date admission diagnosis will define their first diagnosis
  #' Seriously consider filtering all_dalycare_diagnoses based on tablename first:
  #' NB 2026-09: tablename for PATO changed 't_pato' -> 'dimpatologiskdiagnose',
  #' 't_tumor' -> 'tumor_aarlig'. SUNDK (formerly RKKP) tablenames resolve to
  #' the file stem: 'CLL', 'LYFO', 'DaMyDa' (NOT 'SUNDK_CLL'/'RKKP_CLL').
  #' Confirmed 2026-09-18 via unique(all_dalycare_diagnoses$tablename) --
  #' full observed set: 'dimpatologiskdiagnose', 'diagnoser', 't_adm', 't_diag',
  #' 'BehandlingskontakterOgDiagnoser', 'doedsaarsag_3', 'CLL', 't_dodsaarsag_2',
  #' 'DaMyDa', 't_psyk_diag', 'LYFO', 'ADT_haendelser', 'tumor_aarlig', 'kontakter'.
  #' Re-run unique(all_dalycare_diagnoses$tablename) yourself before filtering --
  #' the source composition can change between releases.
  #' NB 2026-09-14: table renamed from t_dalycare_diagnoses to
  #' all_dalycare_diagnoses -- rename only, no change to logic or content.
  #' @examples
  #' load_dataset('all_dalycare_diagnoses', 'all_patients') #loads all DALY-CARE diagnoses
  #' PCD = all_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('dimpatologiskdiagnose', 'DaMyDa', 'tumor_aarlig')) %>%
  #'   filter_first_diagnosis('DC90') #includes any DC90.x
  #'
  #' CLL = all_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('dimpatologiskdiagnose', 'CLL', 'tumor_aarlig')) %>% # only diagnoses from
  #'   filter_first_diagnosis('DC911', str_contains = FALSE)
  #'
  #' MZL = all_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('dimpatologiskdiagnose', 'LYFO', 'tumor_aarlig')) %>% #
  #'   filter_first_diagnosis(c('DC830C', 'DC830D', 'DC884', 'DC884A', 'DC884B', 'DC884C'))
  #'
  #' load_dalycare_icd10() # loads a list of vectors with ICD10 LC diagnoses
  #' MZL = all_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('dimpatologiskdiagnose', 'LYFO', 'tumor_aarlig')) %>% #
  #'   filter_first_diagnosis(ICD10.MZL, str_contains = FALSE) #all MZL diagnoses
  #'
  #' SLL = all_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('dimpatologiskdiagnose', 'LYFO', 'tumor_aarlig')) %>% #
  #'   filter_first_diagnosis('DC830', str_contains = FALSE) #matches 'DC830'
  #'
  #' RICHTER = all_dalycare_diagnoses %>%
  #'   filter(tablename %in% c('dimpatologiskdiagnose', 'LYFO', 'CLL', 'tumor_aarlig')) %>% #
  #'   filter_first_diagnosis(c('DC833', 'DC911'), multiple = 'both') #matches both
  
  #' @references 
  #' Brieghel et al. CLEP 2025  
  
  
  #' filters first occourrence of any ICD10 diagnosis and adds KM years
  #' str_contains T diagnosis contained, str_contains F diagnosis matches 
  #' multiple may contain "first" (defualt) or "both" or "all" for which all multiple diagnoses most be present 
  if(!exists('all_patients')){
    load_dataset('all_patients') # NB 2026-09-15: renamed from patient
  }
  if(multiple == 'first'){
    if(str_contains){print('contains')
      data = data %>% 
        filter_str_detect(diagnosis, {{diagnosis}})}
    else{print('match')
      data = data %>% 
        filter(diagnosis %in% {{diagnosis}})}
    print('slice(1)')
    data =  data %>% 
      group_by(patientid) %>% 
      arrange(date_diagnosis) %>% 
      slice(1) %>% 
      ungroup() %>% 
      left_join(all_patients, 'patientid') %>%
      mutate(age_at_diagnosis = floor(diff_years(date_birth, date_diagnosis)),
             time_dx_death = diff_years(date_diagnosis, date_death_fu)) %>% 
      select(-any_of(c('priority'))) 
    return(data)
  }
  if(multiple %in% c('both', 'all') %>% sort(decreasing = T) %>% head(1)){print('multiple match')
    data = data %>%
      filter(diagnosis %in% {{diagnosis}}) %>% 
      group_by(patientid, diagnosis) %>% 
      arrange(patientid, diagnosis, date_diagnosis) %>% 
      slice(1) %>% 
      ungroup() %>%  
      select(-any_of(c('tablename', 'datasource', 'priority'))) %>% 
      pivot_wider(names_from = diagnosis, values_from = date_diagnosis, names_prefix = 'icd10_') %>% 
      filter(if_all(contains('icd10'), ~!is.na(.))) %>% 
      left_join(all_patients, 'patientid')
  }
}
