load_all_data = function(cohort = NULL,  baseline_icd10 = NULL, coersion = FALSE){
  #' @title
  #' load_all_data
  #' @author
  #' christian brieghel
  #' @description
  #' Loads all datasets from the import public DALY-CARE data resource on subset of patients by assessing key variables from key datasets
  #' @examples 
  #' load_data('all_patients')
  #' cohort = all_patients$patientid %>% head(20)
  #' load_all_data(cohort)
  
  if(length({{cohort}}) > 10000){warning('Cohort > 10,000')}
  master_data = tibble()
  LPR_backbone = c('SDS_t_adm', 'SDS_forloeb', 'SDS_kontakter')
  LPR_datasets = c('SDS_t_sksopr', 'SDS_t_sksube', 'SDS_t_diag', 'SDS_t_udtilsgh')
  LPR3_datasets_kontakt = c('SDS_procedurer_kirurgi', 'SDS_procedurer_andre', 'SDS_diagnoser', 'SDS_resultater') #SDS_laegemiddel? SDS_forloebsmarkoerer?
  LPR3_datasets_forloeb = c('SDS_koder',  'SDS_organisationer') 
  RKKP_datasets = c('SUNDK_CLL', 'SUNDK_LYFO', 'SUNDK_DaMyDa') # NB 2026-09-17: renamed from RKKP_*
  SDS_other = c('SDS_epikur', 'SDS_indberetningmedpris', 'SDS_fctrekvisition', 'SDS_tumor_aarlig', 'SDS_laboratorieproevesvar', 'SDS_t_dodsaarsag_2') #
  SP_dataset = c("SP_AdministreretMedicin", "SP_ADT_haendelser",  "SP_AlleProvesvar",
                 "SP_Behandlingsniveau",
                 'SP_BilleddiagnostiskeUndersoegelser_Del1', 'SP_BilleddiagnostiskeUndersoegelser_Del2',
                 "SP_Behandlingsplaner_Del1",  
                 "SP_Bloddyrkning_del1","SP_Bloddyrkning_del2", "SP_Bloddyrkning_del3", "SP_Bloddyrkning_del4", 
                 "SP_ITAOphold", "SP_Journalnotater_Del1", 
                  "SP_OrdineretMedicin", "SP_Social_Hx", "SP_VitaleVaerdier")
  load_dataset(c('all_patients'), {{cohort}}) # NB 2026-09-15: renamed from patient
  
  #' loads SP_datasets
  for(.sp_tbl in SP_dataset){
    tryCatch(
      load_dataset(.sp_tbl, {{cohort}}),
      error = function(e) message('load_all_data(): tabel ikke fundet, springer over: ', .sp_tbl)
    )
  }
  rm(.sp_tbl)
  
  if(exists('SP_AdministreretMedicin_subset') && nrow(SP_AdministreretMedicin_subset) > 0){
    master_data = SP_AdministreretMedicin_subset %>% 
      transmute(patientid, date = as_date(taken_time), atc) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SP_AdministreretMedicin') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_OrdineretMedicin_subset') && nrow(SP_OrdineretMedicin_subset) > 0){
    master_data = SP_OrdineretMedicin_subset %>% 
      transmute(patientid, date = as_date(order_start_time), atc) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>%
      mutate(source = 'SP_OrdineretMedicin') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_Social_Hx_subset') && nrow(SP_Social_Hx_subset) > 0){
    master_data = SP_Social_Hx_subset %>% 
      transmute(patientid, date = as_date(registringsdato),  ryger, drikker) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>%
      mutate(source = 'SP_Social_Hx') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_VitaleVaerdier_subset') && nrow(SP_VitaleVaerdier_subset) > 0){
    master_data = SP_VitaleVaerdier_subset %>% 
      transmute(patientid, date = as_date(recorded_time), key = displayname, value = as.character(numericvalue)) %>% 
      mutate(source = 'SP_VitaleVaerdier') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_ADT_haendelser_subset') && nrow(SP_ADT_haendelser_subset) > 0){
    master_data = SP_ADT_haendelser_subset %>% 
      transmute(patientid, date = as_date(effective_time), patient_class, event_type_name) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_ADT_haendelser') %>% 
      bind_rows(master_data)
  }

  if(exists('SP_AlleProvesvar_subset') && nrow(SP_AlleProvesvar_subset) > 0){
    master_data = SP_AlleProvesvar_subset %>% 
      mutate(component = ifelse(component == 'NULL', external_name, component)) %>% 
      transmute(patientid, date = as_date(specimn_taken_time), key = component, value = as.character(ord_value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_AlleProvesvar') %>% 
      bind_rows(master_data)
  }
  
  if(exists('SP_Behandlingsplaner_Del1_subset') && nrow(SP_Behandlingsplaner_Del1_subset) > 0){
    master_data = SP_Behandlingsplaner_Del1_subset %>%
      mutate(protokol_navn = if('protokol_navn' %in% names(.)) protokol_navn else NA_character_) %>%
      TX_group(protocol = protokol_navn) %>% 
      transmute(patientid, date = as_date(behandlingsstartdato), TX_group) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_Behandlingsplaner_Del1') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_Bloddyrkning_del1_subset') && nrow(SP_Bloddyrkning_del1_subset) > 0){
    master_data = SP_Bloddyrkning_del1_subset %>% 
      transmute(patientid, date = as_date(proevetagningstidspunkt), komponentnavn) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_Bloddyrkning_del1') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_Bloddyrkning_del2_subset') && nrow(SP_Bloddyrkning_del2_subset) > 0){
    master_data = SP_Bloddyrkning_del2_subset %>% 
      transmute(patientid, date = as_date(proevetagningstidspunkt), resultat_tekst) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_Bloddyrkning_del2') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_Bloddyrkning_del3_subset') && nrow(SP_Bloddyrkning_del3_subset) > 0){
    master_data = SP_Bloddyrkning_del3_subset %>% 
      transmute(patientid, best_ord_id, organisme) %>% 
      left_join(SP_Bloddyrkning_del1_subset %>% 
                  transmute(best_ord_id, date = as_date(proevetagningstidspunkt)), 'best_ord_id', relationship = "many-to-many") %>% 
      select(-best_ord_id) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_Bloddyrkning_del3') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_Bloddyrkning_del4_subset') && nrow(SP_Bloddyrkning_del4_subset) > 0){
    master_data = SP_Bloddyrkning_del4_subset %>% 
      transmute(patientid, best_ord_id = best_ord_id, kliniske_oplysninger) %>% 
      left_join(SP_Bloddyrkning_del1_subset %>% 
                  transmute(best_ord_id, date = as_date(proevetagningstidspunkt)), 'best_ord_id', relationship = "many-to-many") %>% 
      select(-best_ord_id) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_Bloddyrkning_del4') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_Behandlingsniveau_subset') && nrow(SP_Behandlingsniveau_subset) > 0){
    master_data = SP_Behandlingsniveau_subset %>% 
      transmute(patientid, date = as_date(aktiv_dato), behandlingsniveau) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_Behandlingsniveau') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_ITAOphold_subset') && nrow(SP_ITAOphold_subset) > 0){
    master_data = SP_ITAOphold_subset %>% 
      transmute(patientid, date = as_date(icu_stay_start), mechanical_ventilation = resp_yn) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_ITAOphold') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_Journalnotater_Del1_subset') && nrow(SP_Journalnotater_Del1_subset) > 0){
    master_data = SP_Journalnotater_Del1_subset %>% 
      transmute(patientid, date = as_date(ydelsesdato), 
                text_notes = paste0(notat_type, ': ', notat_text)) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_Journalnotater_Del1') %>% 
      bind_rows(master_data)
  }
  if(exists('SP_BilleddiagnostiskeUndersoegelser_Del1_subset') && nrow(SP_BilleddiagnostiskeUndersoegelser_Del1_subset) > 0){
    master_data = SP_BilleddiagnostiskeUndersoegelser_Del1_subset %>% 
      transmute(patientid, date = as_date(bestillingstidspunkt), bestillingsnavn) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_BilleddiagnostiskeUndersoegelser_Del1') %>% 
      bind_rows(master_data)
  }
  
  if(exists('SP_BilleddiagnostiskeUndersoegelser_Del2_subset') && nrow(SP_BilleddiagnostiskeUndersoegelser_Del2_subset) > 0){
    master_data = SP_BilleddiagnostiskeUndersoegelser_Del2_subset %>% 
      transmute(best_ord_id, date = as_date(bestillingstidspunkt), beskrivelse) %>% 
      left_join(SP_BilleddiagnostiskeUndersoegelser_Del1_subset %>% 
                  select(patientid, best_ord_id), 'best_ord_id', relationship = "many-to-many") %>% 
      select(-best_ord_id) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      distinct() %>% 
      mutate(source = 'SP_BilleddiagnostiskeUndersoegelser_Del2') %>% 
      bind_rows(master_data)
  }
  
  
  #' loads RKKP_datasets
  load_dataset(RKKP_datasets, value = {{cohort}})
  if(exists('SUNDK_CLL_subset') && nrow(SUNDK_CLL_subset) > 0){
    master_data = SUNDK_CLL_subset %>%
      left_join(all_patients %>% select(patientid, date_birth), by = 'patientid') %>%
      transmute(patientid, 
                date = parse_sundk_date(Reg_Diagnose_dt),
                Age = floor(diff_years(date_birth, date)),
                Aged65 = ifelse(Age >= 65, '>65 years', '<65 years'),
                PS = Reg_Performancestatus,
                Binet = Reg_BinetStadium,
                IGHV = recode_factor(Reg_Umuteret, 
                                     No = 'Mutated',
                                     Yes = 'Unmutated'),
                TP53.ab = ifelse(Reg_Del17p =='Yes' | Reg_TP53 == 'Yes', 
                                 'Yes', 'No'),
                DEL17P = Reg_Del17p,
                TP53.mut = Reg_TP53) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>%
    mutate(source = 'SUNDK_CLL') %>%
    bind_rows(master_data)
  }
  if(exists('SUNDK_LYFO_subset') && nrow(SUNDK_LYFO_subset) > 0){
    master_data = SUNDK_LYFO_subset %>%
      left_join(all_patients_subset %>% select(patientid, date_birth), by = 'patientid') %>%
      transmute(patientid,
                date = parse_sundk_date(Reg_DiagnostiskBiopsi_dt),
                Age  = floor(diff_years(date_birth, date)),
                subtype,
                AA_STAGE = Reg_Stadium,
                PS = Reg_PerformanceStatusWHO) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SUNDK_LYFO') %>%
      bind_rows(master_data)
  }
  if(exists('SUNDK_DaMyDa_subset') && nrow(SUNDK_DaMyDa_subset) > 0){
    master_data = SUNDK_DaMyDa_subset %>%
      left_join(all_patients_subset %>% select(patientid, date_birth), by = 'patientid') %>%
      transmute(patientid,
                date = parse_sundk_date(Reg_Diagnose_dt),
                Age = floor(diff_years(date_birth, date)),
                PS = Reg_PerformanceStatus,
                ISS = Stadie,
                FISH_t4_14 = Cyto_FishResultat_FGFR3,
                FISH_t11_14 = Cyto_FishResultat_CCND1,
                FISH_t14_16 = Cyto_FishResultat_MBF,
                FISH_DEL17P = Cyto_FishResultat_TP53,
                FISH_AMP1Q = Cyto_FishResultat_1q21, 
                subtype = Reg_Diagnose) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SUNDK_DaMyDa') %>%
      bind_rows(master_data)
  }
  #' loads LPR_backbone
  load_dataset(LPR_backbone, {{cohort}})
  if(nrow(SDS_t_adm_subset) > 0){
    master_data = SDS_t_adm_subset %>%
      transmute(patientid,
                date = as_date(d_inddto),
                c_adiag) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SDS_t_adm') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_forloeb_subset) > 0){
    master_data = SDS_forloeb_subset %>%
      transmute(patientid,
                date = as_date(dato_henvisning),
                henvisningsaarsag) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SDS_forloeb') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_kontakter_subset) > 0){
    master_data = SDS_kontakter_subset %>%
      transmute(patientid,
                date = as_date(dato_start),
                aktionsdiagnose) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SDS_kontakter') %>% 
      bind_rows(master_data)
  }
  #' loads LPR_datasets
  load_dataset(LPR_datasets, SDS_t_adm_subset$k_recnum, column = 'v_recnum')
  if(nrow(SDS_t_udtilsgh_subset) > 0){
    master_data = SDS_t_udtilsgh_subset %>%
      mutate(v_recnum = as.character(v_recnum)) %>%
      left_join(SDS_t_adm_subset %>% transmute(patientid, date = as_date(d_inddto),
                                               v_recnum = as.character(k_recnum)), by = 'v_recnum') %>%
      transmute(patientid, date, c_udtilsgh) %>%
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>%
      filter(value != '') %>%
      mutate(source = 'SDS_t_udtilsgh') %>%
      bind_rows(master_data)
  }
  
  if(exists('SDS_t_diag_subset')){
    
    master_data = SDS_t_diag_subset %>% 
      mutate(v_recnum = as.character(v_recnum)) %>%
      left_join(SDS_t_adm_subset %>% transmute(patientid, date = as_date(d_inddto), 
                                               v_recnum = as.character(k_recnum)), by = 'v_recnum', 
                relationship = "many-to-many") %>% 
      transmute(patientid, date, c_diag, c_tildiag) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_t_diag') %>% 
      bind_rows(master_data)
  }
  
  if(nrow(SDS_t_sksube_subset) > 0){
    master_data = SDS_t_sksube_subset %>% 
      mutate(v_recnum = as.character(v_recnum)) %>%
      left_join(SDS_t_adm_subset %>% transmute(patientid, 
                                            v_recnum = as.character(k_recnum)), by = 'v_recnum',
                relationship = "many-to-many") %>% 
      transmute(patientid, date = as_date(d_odto), c_opr, c_tilopr) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_t_sksube') %>% 
      bind_rows(master_data)
  }

  if(exists('SDS_t_sksopr_subset')){
    master_data = SDS_t_sksopr_subset %>% 
      mutate(v_recnum = as.character(v_recnum)) %>%
      left_join(SDS_t_adm_subset %>% transmute(patientid, v_recnum = as.character(k_recnum)), by = 'v_recnum',
                relationship = "many-to-many") %>% 
      transmute(patientid, date = as_date(d_odto), c_opr, c_tilopr) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_t_sksopr') %>% 
      bind_rows(master_data)
  }

  #' loads LPR3_datasets_kontakt
  load_dataset(LPR3_datasets_kontakt, value = as.character(unique(SDS_kontakter_subset$dw_ek_kontakt)), column = 'dw_ek_kontakt')
  if(nrow(SDS_procedurer_kirurgi_subset) > 0){
    master_data = SDS_procedurer_kirurgi_subset %>% 
      mutate(dw_ek_kontakt = as.character(dw_ek_kontakt)) %>%
      left_join(SDS_kontakter_subset %>% transmute(patientid, dw_ek_kontakt = as.character(dw_ek_kontakt)), by = 'dw_ek_kontakt',
                relationship = "many-to-many") %>% 
      transmute(patientid, date = as_date(dato_start), procedurekode, procedurekode_parent) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_procedurer_kirurgi') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_procedurer_andre_subset) > 0){
    master_data = SDS_procedurer_andre_subset %>% 
      mutate(dw_ek_kontakt = as.character(dw_ek_kontakt)) %>%
      left_join(SDS_kontakter_subset %>% transmute(patientid,  
                                                   dw_ek_kontakt = as.character(dw_ek_kontakt)), by = 'dw_ek_kontakt',
                relationship = "many-to-many") %>% 
      transmute(patientid, date = as_date(dato_start), procedurekode, procedurekode_parent) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_procedurer_andre') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_diagnoser_subset) > 0){
    master_data = SDS_diagnoser_subset %>% 
      mutate(dw_ek_kontakt = as.character(dw_ek_kontakt)) %>%
      left_join(SDS_kontakter_subset %>% transmute(patientid, date = as_date(dato_start), 
                                                   dw_ek_kontakt = as.character(dw_ek_kontakt)), by = 'dw_ek_kontakt',
                relationship = "many-to-many") %>% 
      transmute(patientid, date, diagnosekode, diagnosekode_parent) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_diagnoser') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_resultater_subset) > 0){
    master_data = SDS_resultater_subset %>% 
      mutate(dw_ek_kontakt = as.character(dw_ek_kontakt)) %>%
      left_join(SDS_kontakter_subset %>% transmute(patientid,  dw_ek_kontakt = as.character(dw_ek_kontakt)), by = 'dw_ek_kontakt',
                relationship = "many-to-many") %>% 
      transmute(patientid, date = as_date(dato_indberetning), triggerkode) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_resultater') %>% 
      bind_rows(master_data)
  }
  
  #' loads SDS_other
  load_dataset(SDS_other, {{cohort}})
  # NB 2026: SDS_t_mikro_ny retired -- fritekst-patologinoter leveres ikke
  # længere fra kilden. Se AENDRINGER punkt D8.
  if(nrow(SDS_epikur_subset) > 0){
    master_data = SDS_epikur_subset %>% 
      transmute(patientid, date = as_date(eksd), atc) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SDS_epikur') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_indberetningmedpris_subset) > 0){
    master_data = SDS_indberetningmedpris_subset %>% 
      transmute(patientid, date = as_date(d_adm), c_atc) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SDS_indberetningmedpris') %>% 
      bind_rows(master_data)
  }
  # NB 2026: SDS_pato erstattet af SDS_fctrekvisition + SDS_dimpatologiskdiagnose.
  # SNOMED-kode hentes via join på dw_ek_rekvisition. Se AENDRINGER punkt D8.
  if(exists('SDS_fctrekvisition_subset') && nrow(SDS_fctrekvisition_subset) > 0 &&
     exists('SDS_dimpatologiskdiagnose_subset') && nrow(SDS_dimpatologiskdiagnose_subset) > 0){
    master_data = SDS_fctrekvisition_subset %>%
      mutate(dw_ek_rekvisition = as.character(dw_ek_rekvisition)) %>%
      left_join(SDS_dimpatologiskdiagnose_subset %>%
                  mutate(dw_ek_rekvisition = as.character(dw_ek_rekvisition)) %>%
                  transmute(dw_ek_rekvisition, c_snomedkode = diagnose_snomed_kode),
                by = 'dw_ek_rekvisition', relationship = 'many-to-many') %>%
      transmute(patientid, date = as_date(dato_rekvirering), c_snomedkode) %>%
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>%
      mutate(source = 'SDS_t_pato') %>%
      bind_rows(master_data)
  }
  if(nrow(SDS_tumor_aarlig_subset) > 0){
    master_data = SDS_tumor_aarlig_subset %>% 
      transmute(patientid, date = as_date(diagnosedato), c_icd10 = diagnose_kode_icd10) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      mutate(source = 'SDS_tumor_aarlig') %>% 
      bind_rows(master_data)
  }
  if(nrow(SDS_t_dodsaarsag_2_subset) > 0){
    master_data = SDS_t_dodsaarsag_2_subset %>%
      transmute(patientid, date = as_date(d_statdato), c_dod_1a, c_dod_1b, c_dod_1c, c_dod_1d) %>% 
      gather(key, value, -patientid, -date) %>% mutate(value = as.character(value)) %>% 
      filter(value != '') %>% 
      mutate(source = 'SDS_t_dodsaarsag_2') %>% 
      bind_rows(master_data)
  }
  
  if(nrow(SDS_laboratorieproevesvar_subset) > 0){
    master_data = SDS_laboratorieproevesvar_subset %>% 
      transmute(patientid, date = as_date(samplingdate), key = analysiscode, value = as.character(samplevalue)) %>% 
      distinct() %>% 
      mutate(source = 'SDS_laboratorieproevesvar') %>% 
      bind_rows(master_data)
  }
  
  master_data = master_data %>% 
    arrange(patientid, date)
  
  load_dataset('all_dalycare_diagnoses', {{cohort}}) # NB 2026-09-14: renamed from t_dalycare_diagnoses (logic unchanged)
  if(is.null({{baseline_icd10}})){
    master_data = master_data %>% 
      group_by(patientid) %>% 
      mutate(date_first_diagnosis = min(date), 
             time = diff_years(date_first_diagnosis, date)) %>% 
      ungroup()
  }else{
    master_data = master_data %>% 
      left_join(all_dalycare_diagnoses_subset %>%
                  filter_first_diagnosis({{baseline_icd10}}) %>% 
                  select(patientid, date_first_diagnosis = date_diagnosis), by = 'patientid') %>% 
      mutate(time = diff_years(date_first_diagnosis, date))
  }
  assign('master_data', master_data, envir = parent.frame())    
  print_color('master_data prepared and assigned to Global Environment\n', color = 'blue')
  
}
# load_all_data(cohort) - commented out by to make dalycare package run - MW 
# cohort = sample(all_patients$patientid, 1000)
# load_all_data(cohort = cohort, 'DC911')

#' END OF SCRIPT