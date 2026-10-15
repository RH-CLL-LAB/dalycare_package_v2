
CNS_IPI = function(data){
  #' @title
  #' CNS_IPI
  #' @author
  #' christian brieghel
  #' @description
  #' Calculates NCCN-IPI risk for DLBCL as class factor. 
  #' Updated to LYFO v20 and higher
  #' @examples
  #' LYFO_clean = SUNDK_LYFO %>% clean_SUNDK_LYFO()
  #' LYFO_clean %>% CNS_IPI() %>% pull(CNS_IPI) %>% table()
  #' @references 
  #' Schmitz et al. JCO 2016. 34(26):3150-6. 

  data %>% 
    mutate(CNS.IPI_bone_marrow = ifelse(bone_marrow_diagnosis == 'Yes', 1, 0),
           CNS.IPI_liver = ifelse(liver_diagnosis == 'Yes', 1, 0),
           CNS.IPI_salivary_glands = ifelse(salivary_glands_diagnosis == 'Yes', 1, 0),
           CNS.IPI_cavum_oris = ifelse(cavum_oris_diagnosis == 'Yes', 1, 0),
           CNS.IPI_ventricle = ifelse(ventricle_diagnosis == 'Yes', 1, 0),
           CNS.IPI_small_intestine = ifelse(small_intestine_diagnosis == 'Yes', 1, 0),
           CNS.IPI_colon = ifelse(colon_diagnosis == 'Yes', 1, 0),
           CNS.IPI_pancreas = ifelse(pancreas_diagnosis == 'Yes', 1, 0),
           CNS.IPI_lung = ifelse(lung_diagnosis == 'Yes', 1, 0),
           CNS.IPI_pleura = ifelse(pleura_diagnosis == 'Yes', 1, 0))  %>%  #marrow, CNS, liver/GI tract, or lungs
    mutate(n_EN_regions_CNSIPI = rowSums(across(CNS.IPI_bone_marrow:CNS.IPI_pleura))) %>% 
    mutate(Age.score = ifelse(age_diagnosis <= 60, 0, 1),
           LDH.score = ifelse(LDH_elevated_diagnosis =='No', 0, 1),
           PS.score = ifelse(PS_diagnosis %in% c(0,1), 0, 1),
           Stage.score = ifelse(AA_stage_diagnosis %in% c(1, 2), 0, 1),
           Kidney.score = ifelse(kidneys_diagnosis =='No', 0, 1),
           EN.score = ifelse(n_EN_regions_CNSIPI <=1, 0, 1)) %>% 
    mutate(CNS_IPI.score = rowSums(across(Age.score:EN.score)),
           CNS_IPI.score = ifelse(subtype == 'DLBCL', CNS_IPI.score, NA)) %>% 
    mutate(CNS_IPI = cut(CNS_IPI.score, c(-Inf, 1, 3, Inf), labels =  c('Low', 'Intermediate', 'High')))
} 