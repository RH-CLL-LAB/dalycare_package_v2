MAYO_20_20_20 = function(data, 
                         plasmacell_percentage_BM_diagnosis = plasmacell_percentage_BM_diagnosis,
                         mspike_diagnosis = mspike_diagnosis,
                         kappa_lambda_ratio_diagnosis = kappa_lambda_ratio_diagnosis,
                         subtype_am_klausen = subtype_am_klausen){
  #' @title
  #' MAYO_20_20_20
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates Mayo Institute 20-20-20 risk for progression of smoldering myeloma as class factor.
  #' Only works with DaMyDa v18 or lower (<2023)
  #' @example
  #' MM_clean = SUNDK_DAMYDA_CLEAN %>% clean_SUNDK_DAMYDA()
  #' MM_clean %>%  MAYO_20_20_20() %>%  pull(MAYO_20_20_20) %>% table()
  #' @note
  #' Needs update  
  #' @references 
  #' Mateos et al. Blood cancer journal. Oct 16 2020;10(10):102
  
  data %>% 
    mutate(MAYO_20_20_20.BM = ifelse({{plasmacell_percentage_BM_diagnosis}} > 20, 1, 0),
           MAYO_20_20_20.MSPIKE = ifelse({{mspike_diagnosis}} > 20, 1, 0),
           MAYO_20_20_20.KLR = ifelse({{kappa_lambda_ratio_diagnosis}} > 20 | {{kappa_lambda_ratio_diagnosis}} < 0.05, 1, 0)) %>%
    mutate(MAYO_20_20_20.score = rowSums(across(MAYO_20_20_20.BM:MAYO_20_20_20.KLR))) %>%
    mutate(MAYO_20_20_20.score = ifelse({{subtype_am_klausen}} == 'SMM', MAYO_20_20_20.score, NA),
           MAYO_20_20_20 = cut(MAYO_20_20_20.score, c(-Inf, 0, 1, Inf), c('Low', 'Intermediate', 'High')))
}
