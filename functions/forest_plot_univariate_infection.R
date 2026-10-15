
forest_plot_univariate_infection = function(data, strata = 'ATC', p_adjustment = FALSE){
  #' @title
  #' forest_plot_univariate_infection
  #' @author
  #' christian brieghel
  #' @description
  #' Forest plot for univariate analyses of infection stratified on ATC or CCI categories
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
  #'   forest_plot_univariate_infection()
  #'  CLL %>% 
  #'   forest_plot_univariate_infection('CCI')
  
  
  if(!{{strata}} %in% c('ATC', 'CCI') %>% unique() %>% sort() %>% head(1)){stop('strata must be "ATC" or "CCI"')}
  if({{strata}} == 'ATC'){
    data = data %>% select(start:end, time_infection_death_fu, infection) %>% select(-c(start, end))
  }
  if({{strata}} == 'CCI'){
    data = data %>% select(contains('CCI2011'), time_infection_death_fu, infection) %>% select(-c(CCI2011.Cancer.and.Hem.score, CCI2011.AIDS.score, CCI2011_score))
  }
  VARS = data %>% select(-c( time_infection_death_fu, infection)) %>% names()
  
  LIST = purrr::map(VARS, ~coxph(as.formula(paste("Surv(time_infection_death_fu, infection) ~ ", .x)), 
                                 data))
  UNI_infection = data.table()
  for(i in 1:length(LIST)){
    
    pval = as.numeric(coef(summary(LIST[[i]]))[,5][3])
    UNI_infection = publish(LIST[[i]])$regressionTable %>% 
      mutate(
        n = as.numeric(table(data[,i])[2]),
        pval = pval) %>% 
      bind_rows(UNI_infection)
  }
  
  UNI_infection2 = UNI_infection %>% 
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
    UNI_infection2$FDR = p.adjust(UNI_infection2$pval, 'fdr')
    UNI_infection2$bonferroni = p.adjust(UNI_infection2$pval, 'bonferroni')
    UNI_infection2 = UNI_infection2 %>%
      filter(FDR <0.1) 
    if(nrow(UNI_infection2)==0){
      errorCondition('0 rows')
    }
    print(paste(nrow(UNI_infection2), 'rows'))
    UNI_infection2 = UNI_infection2 %>%
      mutate(  FDR_round = sprintf('%.3f', round(FDR, 3)),
               FDR_round = ifelse(FDR_round =='0.000', '<0.001', paste(FDR_round)),
               bonferroni_round = sprintf('%.3f', round(FDR, 3)),
               bonferroni_round = ifelse(bonferroni_round =='0.000', '<0.001', paste(bonferroni_round)))
    return(
      plotConfidence(x=UNI_infection2$HR,
                     lower = UNI_infection2$lower,
                     upper = UNI_infection2$upper,
                     labels = UNI_infection2[,c(1,11,6,12)],
                     title.labels= c(paste({{strata}}), 'Description', 'n', 'fdr'),
                     title.values=expression(bold(HR (CI[95]))),
                     cex=1.5,
                     col="black",
                     xlab.cex=1.3,
                     xlim = c(0.1, 20),
                     points.col=c(1:9),
                     xlab="Hazard ratio for infection",
                     plot.log="x")
    )
  }else{
    return(
      plotConfidence(x=UNI_infection2$HR,
                     lower = UNI_infection2$lower,
                     upper = UNI_infection2$upper,
                     labels = UNI_infection2[,c(1,11,6)],
                     title.labels= c(paste({{strata}}), 'Description', 'n'),
                     title.values=expression(bold(HR (CI[95]))),
                     cex=1.5,
                     col="black",
                     xlab.cex=1.3,
                     xlim = c(0.1, 20),
                     points.col=c(1:9),
                     xlab="Hazard ratio for infection",
                     plot.log="x")
    )
  }
}
