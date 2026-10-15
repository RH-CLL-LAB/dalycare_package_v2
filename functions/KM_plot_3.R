KM_plot_3 = function(data, fun = NULL, title = NULL, labs = NULL, xlab = 'Time (years)', ylab = '% alive', xlim = c(0,5),ylim = c(0,1), 
                     breaks = 1, palette = c(1, 2, 3, 4, 5, 6), pval = FALSE, pval.coord = c(0, 0.1), surv.median.line = 'none', linewidth = 1){
  #' @title
  #' KM_plot_3
  #' @author
  #' christian brieghel
  #' @description
  #' Plots survminer::ggsurvplot for survival, hospitalization and infection, specifically for create_antineoplastic_study()
  #' @note 
  #' Depends on using the exact (!) pipeline below. ICD10 and ATC may be deplaced.
  #' @examples 
  #' CLL = all_dalycare_diagnoses %>%
  #'   filter_first_diagnosis('DC911') %>%
  #'     create_antineoplastic_study('L01XX52') # ATC.venetoclax = 'L01XX52'
  #' CLL %>% KM_plot_3()
  #' 
  #' #or add strata
  #' CLL = CLL %>% 
  #'   left_join(SUNDK_CLL %>% clean_SUNDK_CLL() %>% select(patientid, IGHV), 'patientid') %>% 
  #'    mutate(strata = IGHV)
  #' CLL %>% KM_plot_3()

   
  arrange_ggsurvplots(list(
    data %>%
      survfit(Surv(time_tx_death_fu, status) ~ strata, data = .) %>%
      ggsurvplot(data = data,
                 fun = fun,
                 censor.shape= '|',
                 censor.size = 1.8,
                 break.time.by = breaks,
                 legend.title = 'Survival',
                 legend.labs = labs,
                 font.main = c(10, 'black'), 
                 xlab = xlab, 
                 ylab = ylab,
                 xlim = xlim, 
                 ylim = ylim, 
                 font.x = c(10, 'plain', 'black'), 
                 font.y = c(10, 'plain', 'black'), 
                 font.tickslab = c(10, 'plain', 'black'),
                 risk.table = TRUE,
                 risk.table.y.text = FALSE,
                 tables.theme = theme_cleantable(),
                 palette = palette,
                 pval = pval,
                 pval.coord = pval.coord,
                 conf.int = FALSE,
                 linewidth = linewidth,
                 surv.median.line = surv.median.line,
                 ggtheme = theme_classic() + theme(plot.title = element_text(size = 8),
                                                   panel.grid.major = element_blank(), 
                                                   panel.grid.minor = element_blank())),
    data %>%
      survfit(Surv(time_hospitalization_death_fu, hospitalization) ~ strata, data = .) %>%
      ggsurvplot(data = data,
                 fun = 'event',
                 censor.shape= '|',
                 censor.size = 1.8,
                 break.time.by = breaks,
                 legend.title = 'Hospitalizations',
                 legend.labs = labs,
                 font.main = c(10, 'black'), 
                 xlab = xlab, 
                 ylab = '% hospitalized',
                 xlim = xlim, 
                 ylim = ylim, 
                 font.x = c(10, 'plain', 'black'), 
                 font.y = c(10, 'plain', 'black'), 
                 font.tickslab = c(10, 'plain', 'black'),
                 risk.table = TRUE,
                 risk.table.y.text = FALSE,
                 tables.theme = theme_cleantable(),
                 palette = palette,
                 pval = pval,
                 pval.coord = pval.coord,
                 conf.int = FALSE,
                 linewidth = linewidth,
                 surv.median.line = surv.median.line,
                 ggtheme = theme_classic() + theme(plot.title = element_text(size = 8),
                                                   panel.grid.major = element_blank(), 
                                                   panel.grid.minor = element_blank())),
    data %>%
      survfit(Surv(time_infection_death_fu, infection) ~ strata, data = .) %>%
      ggsurvplot(data = data,
                 fun = 'event',
                 censor.shape= '|',
                 censor.size = 1.8,
                 break.time.by = breaks,
                 legend.title = 'Infections',
                 legend.labs = labs,
                 font.main = c(10, 'black'), 
                 xlab = xlab, 
                 ylab = '% infected',
                 xlim = xlim, 
                 ylim = ylim, 
                 font.x = c(10, 'plain', 'black'), 
                 font.y = c(10, 'plain', 'black'), 
                 font.tickslab = c(10, 'plain', 'black'),
                 risk.table = TRUE,
                 risk.table.y.text = FALSE,
                 tables.theme = theme_cleantable(),
                 palette = palette,
                 pval = pval,
                 pval.coord = pval.coord,
                 conf.int = FALSE,
                 linewidth = linewidth,
                 surv.median.line = surv.median.line,
                 ggtheme = theme_classic() + theme(plot.title = element_text(size = 8),
                                                   panel.grid.major = element_blank(), 
                                                   panel.grid.minor = element_blank()))),
    nrow = 1,
    ncol = 3)
}