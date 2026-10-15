clean_SDS_t_mikro = function(data){
  #' @title
  #' clean_SDS_t_mikro
  #' @author
  #' christian brieghel
  #' @description  
  #' Cleans the dataset "SDS_t_mikro" containing pathology notes 
  #' @examples 
  #' t_mikro_clean = SDS_t_mikro %>% clean_SDS_t_mikro()
  #' 
  #' cohort = sample(all_patients$patientid, 100)
  #' load_dataset('SDS_pato', cohort)
  #' load_dataset('SDS_t_mikro_ny', SDS_pato_subset$k_rekvnr, 'k_rekvnr') #NB! Early k_rekvnr are not unique without k_inst
  #' Patology_text = SDS_t_mikro_ny_subset %>%
  #'   left_join(SDS_pato_subset %>% 
  #'               group_by(k_rekvnr) %>% 
  #'               left_join(Codes_SNOMED) %>% 
  #'   summarise(patientid, 
  #'             snomed = paste0(c_snomedkode, collapse = ':'),
  #'             snomed_text = paste0(Text, collapse = ':')), 'k_rekvnr') %>% 
  #'   distinct()
  
  data %>% 
    distinct() %>% 
    group_by(k_inst, k_rekvnr) %>% 
    summarise(text = paste0(v_fritekst, collapse = ' ')) %>% 
    ungroup() 
}
