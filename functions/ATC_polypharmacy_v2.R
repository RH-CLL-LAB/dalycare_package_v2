ATC_polypharmacy = function(data, 
                            patientid = patientid,
                            atc = atc,
                            level = 3){
  #' @title
  #' ATC_polypharmacy
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates number of 1st to 5th level ATC codes per patient and defines polypharmacy as â‰¥5 drug classes.
  #' @note 
  #' 'level' argument changes the ATC level. Eg. Penicillins, level = 4: "J01CE" 
  #' @examples
  #' load_dataset(c('SDS_epikur', 'SUNDK_CLL'))
  #' CLL_clean = SUNDK_CLL %>%
  #'   clean_SUNDK_CLL()
  #' CLL_clean_polyrx = CLL_clean %>% 
  #'   select(patientid, date_diagnosis) %>% 
  #'   left_join(SDS_epikur) %>% 
  #'   filter(eksd < date_diagnosis,
  #'          eksd > date_diagnosis-365) %>% #e.g. within a year from diagnosis
  #'   ATC_polypharmacy(level = 3)
  #'   
  #' SDS_epikur %>% ATC_polypharmacy(level = 3) %>% pull(Polypharmacy)
  #' @references 
  #' Brieghel et al. HemaSphere 2025
  
  if(!exists('Codes_ATC')){load_dataset('Codes_ATC')}
  SUM_data = data %>%  
    select({{patientid}}, {{atc}}) %>%
    group_by({{patientid}}) %>% 
    summarise(ATC_SUM = toupper(paste0({{atc}}, collapse = ";"))) %>% 
    ungroup()
  data = data %>% 
    mutate(atc = substr({{atc}}, 1,{{level}})) %>% 
    left_join(Codes_ATC %>% select(atc), by = 'atc', relationship = "many-to-many") %>%  #= class_code removed 29/12-25 - CB
    transmute({{patientid}}, atc, value = 1) %>% 
    group_by({{patientid}}, atc) %>% 
    slice(1) %>% 
    ungroup() %>% 
    spread(atc, value) %>% 
    replace(is.na(.), 0)
  
  names(data)[2] = 'start'
  names(data)[length(names(data))] = 'end'
  
  data = data %>% 
    mutate(n.ATC = rowSums(across(start:end))) %>% 
    mutate(ATC.group = cut(n.ATC, c(-Inf, 0, 1, 2, 3, 4,5, 6, 7, 8, 9, Inf), 
                           labels = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9','>9')),
           ATC.group2 = cut(n.ATC, c(-Inf, 2,  4, 6, 8, 10, Inf), labels = c('0-2', '3-4', '5-6', '7-8', '9-10', '>10')),
           ATC.group3 = cut(n.ATC, c(-Inf, 4, 8,  Inf), labels = c('0-4', '5-8', '>9')),
           Polypharmacy = ifelse(n.ATC >= 5, 'Yes', 'No')) %>% 
    left_join(SUM_data, 'patientid')
}

