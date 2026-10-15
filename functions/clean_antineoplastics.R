clean_antineoplastics = function(data, expand = FALSE, doxorubicin = TRUE){
  #' @title
  #' clean_antineoplastics
  #' @author
  #' christian brieghel, andreas katsimigas
  #' @description
  #' Cleans t_antineoplastic_treatment_draft_20250616 and adds atc
  #' @note  
  #' 
  #' @examples
  #' load_dataset('t_antineoplastic_treatment_draft_20250616', 'all_patients') #loads all DALY-CARE diagnoses
  #' antineoplastics_clean = t_antineoplastic_treatment_draft_20250616 %>%
  #'   clean_antineoplastics()

  if(expand){
    data %>% 
      mutate(count = ifelse(tx_text %in% c('abvd', 'chop', 'cope', 'chic', 'beam', 'ccvp', 'cvbp', 'dhap', 'lvpp'), 4, 1)) %>% 
      mutate(count = ifelse(tx_text %in% c('bac', 'cop'), 3, count)) %>% 
      uncount(count)  # Next fill 3s and 4s
      
  }else{
  load_SKS_antineoplastics()
  data = data %>% 
    left_join(Codes_ATC %>% 
                select(tx_text = class_name, atc) %>% 
                group_by(tx_text) %>% 
                slice(1) %>% 
                ungroup(), 'tx_text') %>% 
    mutate(atc = ifelse(is.na(atc) & tx_code_type == 'atc', tx_code, atc),
           atc = ifelse(tx_code %in% SKS.antiCD20, 'L01XC02', atc),
           atc = ifelse(tx_code %in% 'humax', 'L01XC02', atc),
           atc = ifelse(tx_code %in% "5-fluorouracil", 'L01BC02', atc),
           atc = ifelse(tx_code %in% "glofitamab", 'L01FX28', atc),
           atc = ifelse(tx_code %in% "high-dose cyclophosphamide", 'L01AA01', atc),
           atc = ifelse(tx_code %in% "high-dose cytarabine", 'L01BC01', atc),
           atc = ifelse(tx_code %in% "high-dose etoposide", 'L01CB01', atc),
           atc = ifelse(tx_code %in% "high-dose melphalan", 'L01AA03', atc),
           atc = ifelse(tx_code %in% "high-dose methotrexate", 'L01BA01', atc),
           atc = ifelse(tx_code %in% "doxorubicin (liposomal)", 'L01DB01', atc)
    ) 
  if(doxorubicin == TRUE){
     data %>% 
      mutate(atc = if_else(tx_text %in% c('chop', 'choep', 'abvd') 
                          | tx_code %in% c('BWHA170', 'BWHA102'), 'L01DB01', atc))
  }
  }
}

