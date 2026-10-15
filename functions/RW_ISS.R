RW_ISS = function(data){
  #' @title
  #' RW_ISS
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates revised-world ISS (RW-ISS) risk for multiple myeloma as class factor.
  #' Updated for DaMyDa v18 or higher (>2024)
  #' @examples
  #' MM_clean = SUNDK_DaMyDa %>% clean_SUNDK_DAMYDA()
  #' MM_clean %>% RW_ISS() %>% pull(RW_ISS) %>% table()
  #' @references 
  #' Brieghel et al. BCJ. 2025;15(1):59.
  
  data %>% 
    mutate(PS.cut = cut(as.numeric(as.character(PS_diagnosis)), c(-Inf, 0, 1,Inf)),
           LDH.F = ifelse(LDH_diagnosis <= 205, 'Normal', 'Elevated'), #defines as above or below 205 regardless of age
           LDH.F = factor(LDH.F, levels = c('Normal', 'Elevated'))) %>% 
    # "RW-ISS"
    mutate(Aged.score.RW = ifelse(aged70_diagnosis  =='<70 years', 0, 2),
           PS.score.RW = as.numeric(as.character(factor(PS.cut, labels = c(0, 1, 2)))),
           t14.16.score.RW = ifelse(FISH_t14_16 == 'Yes', 1, 0),
           ISS.score.RW = ifelse(as.numeric(as.character(ISS_diagnosis)) == 1, 0 , 0.5),
           ISS.score.RW = ifelse(as.numeric(as.character(ISS_diagnosis)) == 3, 1 , ISS.score.RW),
           del17p.score.RW = ifelse(FISH_del17p == 'Yes', 0.5, 0),
           LDH.score.RW = ifelse(LDH.F == 'Elevated', 0.5, 0)) %>% 
    mutate(RWISS.score = rowSums(across(Aged.score.RW:LDH.score.RW)),
           RWISS.score = ifelse(subtype == 'DC900', RWISS.score, NA)) %>%
    mutate(RWISS.score.int = cut(RWISS.score, c(-Inf, 2, 3, 4.5, Inf)),
           RW_ISS = cut(RWISS.score, c(-Inf, 2, 3, 4.5, Inf), labels = c('I', 'II', 'III', 'IV')))
}
