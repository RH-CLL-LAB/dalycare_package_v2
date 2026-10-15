
forest_plot_univariate_os = function(data, strata = 'ATC', p_adjustment = FALSE){
  #' @title
  #' forest_plot_univariate_os
  #' @author
  #' christian brieghel
  #' @description
  #' Forest plot for univariate analyses of overall survival stratified on ATC or CCI categories
  #' @note  
  #' Must be used after create_antineoplastic_study()
  #' @examples
  #' load_dataset(c('all_patients', 'all_dalycare_diagnoses'))
  #' load_dalycare_icd10()
  #' load_ATC_antineoplastics()
  #' CLL = all_dalycare_diagnoses %>%
  #'   filter_first_diagnosis(ICD10.CLL) %>% 
  #'   create_antineoplastic_study(ATC.BTKi) 
  #'  CLL %>% 
  #'   forest_plot_univariate_os()
  #'  CLL %>% 
  #'   forest_plot_univariate_os('CCI')
  
  
  if(!{{strata}} %in% c('ATC', 'CCI') %>% unique() %>% sort() %>% head(1)){stop('strata must be "ATC" or "CCI"')}
  if({{strata}} == 'ATC'){
    data = data %>% select(start:end, time_dx_death, status) %>% select(-c(start, end))
  }
  if({{strata}} == 'CCI'){
    data = data %>% select(contains('CCI2011'), time_dx_death, status) %>% select(-c(CCI2011.Cancer.and.Hem.score, CCI2011.AIDS.score, CCI2011_score))
  }
  VARS = data %>% select(-c( time_dx_death, status)) %>% names()
  
  LIST = purrr::map(VARS, ~coxph(as.formula(paste("Surv(time_dx_death, status) ~ ", .x)), 
                                 data))
  UNI_OS = data.table()
  for(i in 1:length(LIST)){
    
    pval = as.numeric(coef(summary(LIST[[i]]))[,5][3])
    UNI_OS = publish(LIST[[i]])$regressionTable %>% 
      mutate(
        n = as.numeric(table(data[,i])[2]),
        pval = pval) %>% 
      bind_rows(UNI_OS)
  }
  
  UNI_OS2 = UNI_OS %>% 
    mutate(Variable = ifelse(Variable =='', NA, Variable)) %>% 
    fill(Variable) %>% 
    filter(CI.95 != '') %>% 
    mutate(HR = as.numeric(HazardRatio),
           lower = as.numeric(gsub('\\[', '', str_split_fixed(CI.95, ';', 2)[,1])),
           upper = as.numeric(gsub('\\]', '', str_split_fixed(CI.95, ';', 2)[,2])),
           n = ifelse(n<5, '<5', n)) %>% 
    arrange(desc(HR)) %>% 
    dplyr::rename(ATC= Variable) %>% 
    left_join(Codes_ATC %>% dplyr::select(ATC = atc, name), 'ATC') %>% 
    mutate(name = ifelse(ATC == 'A07', 'antidiarrheals, intestinal antiinflammatory', name),
           name = ifelse(ATC == 'G03', 'sex hormones and genital modulators', name)) %>% 
    filter(HR != 0.00)
  
  if({{p_adjustment}}){
    UNI_OS2$FDR = p.adjust(UNI_OS2$pval, 'fdr')
    UNI_OS2$bonferroni = p.adjust(UNI_OS2$pval, 'bonferroni')
    UNI_OS2 = UNI_OS2 %>%
      filter(FDR <0.1) 
    if(nrow(UNI_OS2)==0){
      errorCondition('0 rows')
    }
    print(paste(nrow(UNI_OS2), 'rows'))
    UNI_OS2 = UNI_OS2 %>%
      mutate(  FDR_round = sprintf('%.3f', round(FDR, 3)),
               FDR_round = ifelse(FDR_round =='0.000', '<0.001', paste(FDR_round)),
               bonferroni_round = sprintf('%.3f', round(FDR, 3)),
               bonferroni_round = ifelse(bonferroni_round =='0.000', '<0.001', paste(bonferroni_round)))
    return(
      plotConfidence(x=UNI_OS2$HR,
                     lower = UNI_OS2$lower,
                     upper = UNI_OS2$upper,
                     labels = UNI_OS2[,c(1,11,6,12)],
                     title.labels= c('ATC', 'Description', 'n', 'fdr'),
                     title.values=expression(bold(HR (CI[95]))),
                     cex=1.5,
                     col="black",
                     xlab.cex=1.3,
                     xlim = c(0.1, 20),
                     points.col=c(1:9),
                     xlab="Hazard ratio for overall survival",
                     plot.log="x")
    )
  }else{
    return(
      plotConfidence(x=UNI_OS2$HR,
                     lower = UNI_OS2$lower,
                     upper = UNI_OS2$upper,
                     labels = UNI_OS2[,c(1,11,6)],
                     title.labels= c(paste({{strata}}), 'Description', 'n'),
                     title.values=expression(bold(HR (CI[95]))),
                     cex=1.5,
                     col="black",
                     xlab.cex=1.3,
                     xlim = c(0.1, 20),
                     points.col=c(1:9),
                     xlab="Hazard ratio for overall survival",
                     plot.log="x")
    )
  }
}
