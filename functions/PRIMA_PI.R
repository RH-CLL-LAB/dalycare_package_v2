PRIMA_PI = function(data){
  #' @title
  #' PRIMA_PI
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates NCCN-IPI risk for DLBCL as class factor. 
  #' Works with LYFO v20 and higher
  #' @examples
  #' LYFO_clean = SUNDK_LYFO %>% clean_SUNDK_LYFO()
  #' LYFO_clean %>% PRIMA_PI() %>% pull(PRIMA_PI) %>% table
  #' @references 
  #' Bachy et al. Blood. 2018;132(1):49-58.
  
  data %>% 
    mutate(PRIMA_PI = ifelse(B2M_diagnosis > 3.0, 'High', NA),
           PRIMA_PI = ifelse(B2M_diagnosis <= 3.0 & bone_marrow_diagnosis =='Yes', 'Intermediate', PRIMA_PI),
           PRIMA_PI = ifelse(B2M_diagnosis <= 3.0 & bone_marrow_diagnosis =='No', 'Low', PRIMA_PI),
           PRIMA_PI = factor(PRIMA_PI, levels = c('Low', 'Intermediate', 'High')))
}