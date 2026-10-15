
clean_SUNDK_DAMYDA = function(data){
  #' @title
  #' clean_SUNDK_DAMYDA
  #' @author
  #' mikkel werling, christian brieghel, emma ellegard pedersen
  #' @description
  #' Cleans RKKP DaMyDa v18 or higher (since 2024)
  #' @examples
  #' MM_clean = SUNDK_DaMyDa %>% clean_SUNDK_DAMYDA() # NB 2026-09-17: raw table renamed from RKKP_DaMyDa to SUNDK_DaMyDa
  #' @references 
  #' Gimsing et al. CLEP. 2016:8
  #' https://db-dokumentation-sundk.dk/Public/
  #' https://www.db-dokumentation-sundk.dk/Public/Databases.aspx?db2=1000001086 # 
  #' Published 2026-01-06
  
  ### -- ENSURE NECESSARY TABLES ARE IMPORTED --
  
  if(!exists('all_patients')){load_dataset('all_patients')} # NB 2026-09-15: renamed from patient
  if(!exists('Codes_kommunekoder')){load_dataset('Codes_kommunekoder')}
  Codes_kommunekoder = Codes_kommunekoder %>% 
    dplyr::rename(Kommunenr = KODE) %>%
    dplyr::mutate(Kommunenr = as.character(Kommunenr))
  source(file.path(PKG_HOME, 'clean_SUNDK/clean_SUNDK_DAMYDA_snomed.R'))
  
  ### -- CALCULATE, CREATE, AND RENAME COLUMNS --
  
  data %>% 
    select(-any_of('Reg_PlasmaMKomp_IgE_kappa')) %>%  # only NA, need checked every update
    mutate(Kommunenr = as.character(Kommunenr)) %>%
    left_join(Codes_kommunekoder, by = 'Kommunenr') %>% 
    
    ### -- REFORMAT ACROSS DIFFERENT COLUMNS --
    
    mutate(across(contains(c('Reg_FISH', 'Reg_SammenfaldCorpora', 'Reg_AndreKnogleundersoegelse',
                             'Reg_MedullaertTvaersnitSyndrom', 'Reg_ExtramedullaerMyelom', 'Reg_Amyloidose',
                             'Reg_Neuropati', 'Reg_Hyperviskositet', 'Reg_Dialysekraevende', 'Reg_BAKTINF', 
                             'Reg_Mkomponent')
                           ),
                  ~ recode_factor(ifelse(.=='UNK', NA, .),
                                  N = 'No',
                                  Y = 'Yes'))) %>%
    mutate(across(c('Reg_ProcentKlonalePlasmaceller', 'Reg_Hoejde', 'Reg_Vaegt', 'Reg_UrinMKomponent_gL',
                    'Reg_Haemoglobin', 'Reg_Beta2Microglobulin_gl', 'Reg_Beta2Microglobulin_nMoll',
                    'Reg_Creatinin_mikmoll', 'Reg_Creatinin_mmoll', 'Reg_CReaktivtProtein_gl', 'Reg_LDH',
                    'Reg_IgA_gl', 'Reg_IgA_mikmoll', 'Reg_IgG_gl', 'Reg_IgG_mikmoll', 'Reg_IgM_gl', 'Reg_IgM_mikmoll',
                    'Reg_IgD', 'Reg_IgE', 'Reg_FRIKAEDEGL', 'Reg_FrieKappaKaeder', 'Reg_FrieLambdaKaeder',
                    'PB_Haemoglobin', 'PB_Beta2Microglobulin_gl', 'PB_Beta2Microglobulin_nMoll',
                    'PB_Creatinin_MikMoll', 'PB_Creatinin_mMoll', 'Reg_CalciumIoniseret', 'Reg_CalciumAlbuminkorrigeret',
                    'Reg_CReaktivtProtein_nMoll',
                    'Reg_PlasmaMKomponent'), 
                  ~ ifelse(.==-1, NA, .))) %>% 
    mutate(across(c('Reg_TidligereMGUS', 'Reg_PCINFMARV', 'Reg_Karyotypning_gennemfoert'), 
                  ~ ifelse(.=='UNK', NA, .))) %>% 
    mutate(across(contains('Cyto_FishResultat'), ~ ifelse(. =='Y', 'Yes', NA))) %>% 
    mutate(across(c('Cyto_FishUdfoert'), 
                  ~ ifelse(. %in% c('U', '4'), NA, .))) %>% 
    mutate(across(c('Reg_TidligereMGUS', 'Reg_PCINFMARV', 'Reg_Karyotypning_gennemfoert', 'Reg_Knogleforandringer',
                    'Cyto_FishUdfoert', contains('Reg_PlasmaMKomp_Ig'), contains('PB_VedligeholdBeh_')), 
                  ~ recode_factor(., 
                                  N = 'No',
                                  Y = 'Yes'))) %>% 

    left_join(all_patients, 'patientid') %>%
    transmute(patientid,
              date_diagnosis = parse_sundk_date(Reg_Diagnose_dt),
              subtype = recode(Reg_Diagnose, 
                               `9730` = 'DC900',
                               `9731` = 'DC903',
                               `9732` = 'DC900',
                               `9733` = 'DC901',
                               `9734` = 'DC902'),
              subtype_text = recode(Reg_Diagnose, 
                                    `9730` = 'Smoldering myelomatose',
                                    `9731` = 'SolitÃƒÂ¦rt ossÃƒÂ¸st plasmacytom', #
                                    `9732` = 'Myelomatose',
                                    `9733` = 'PlasmacelleleukÃƒÂ¦mi',
                                    `9734` = 'SolitÃƒÂ¦rt myelom (ekstraossÃƒÂ¸st)',
                                    DD472= 'MGUS'),
              subtype_short = recode(Reg_Diagnose, 
                                     `9730` = 'SMM',
                                     `9731` = 'SBP', #
                                     `9732` = 'MM',
                                     `9733` = 'PCL',
                                     `9734` = 'EMP',
                                     DD472= 'MGUS'),
              
              ### -- DEMOGRAPHICS --
              
              # change to be consistent with naming scheme - MW
              age_diagnosis = diff_years(date_birth, date_diagnosis),
              aged70_diagnosis = ifelse(age_diagnosis > 70, ">70 years", "<70 years"),
              sex = recode_factor(sex , 
                                  M = 'Male',
                                  `F` = 'Female'),
              shak = Reg_OrganisationKode_Shak,
              region = recode(Region,
                              `1081` = 'Region Nord',
                              `1082` = 'Region Midt',
                              `1083` = 'Region Syd',
                              `1084` = 'Region Hovedstad',
                              `1085` = 'Region SjÃƒÂ¦lland',
                              `1099`  = 'Ukendt'),
              hospital_id = recode_factor(Reg_OrganisationKode_Shak,
                                          `1301101` = 'RH',
                                          `1516230` = 'HER',
                                          `2000228` = 'HIL',
                                          `3800A20` = 'ROS',
                                          `3800N80` = 'NAE',
                                          `4202560` = 'OUH',
                                          `5000505` = 'SHS',
                                          `500061A` = 'SHS',
                                          `5501053` = 'ESBJERG',
                                          `6008260` = 'VEJLE',
                                          `6620141` = 'AUH',
                                          `664038K` = 'GODSTRUP',
                                          `665033L` = 'HOLSTEBRO',
                                          `7601041` = 'VIBORG',
                                          `8001101` = 'AALBORG'),
              kommune = KOMMUNE, #should be placed here - CB
              
              ### -- DIAGNOSIS --
              PS_diagnosis = factor(Reg_PerformanceStatus, c(0,1,2,3,4)),
              ISS_diagnosis = recode_factor(Stadie, 
                                  `1` = '1',
                                  `2` = '2',
                                  `3` = '3'),
              ISS_roman_diagnosis = recode_factor(Stadie, 
                                            `1` = 'I',
                                            `2` = 'II',
                                            `3` = 'III'),
              progression_from_prior_MGUS = Reg_TidligereMGUS,
              date_prior_MGUS = parse_sundk_date(Reg_TidligereMGUS_dt),
              weight_diagnosis = Reg_Vaegt,
              height_diagnosis = Reg_Hoejde,
              plasmacell_percentage_BM_diagnosis = Reg_ProcentKlonalePlasmaceller,
              plasmacell_infiltration_BM_diagnosis = Reg_PCINFMARV,
              # FISH_performed_diagnosis = Reg_FISH, # Not found in RKKP-docu https://www.db-dokumentation-sundk.dk/Public/Variable.aspx?db2=1000001086
              FISH_performed_diagnosis = as.character(Cyto_FishUdfoert),  
              karyotype_performed_diagnosis = Reg_Karyotypning_gennemfoert, # alias MDX50_CYTOGEN
              bone_lesion_diagnosis = Reg_Knogleforandringer,
              vertebral_compression_fracture_diagnosis = Reg_SammenfaldCorpora,
              bone_examination_other_diagnosis = Reg_AndreKnogleundersoegelse,
              MTS_diagnosis = Reg_MedullaertTvaersnitSyndrom,
              extramedullary_myeloma_diagnosis = Reg_ExtramedullaerMyelom,
              amyloidosis_diagnosis = Reg_Amyloidose,
              neuropathy_diagnosis = Reg_Neuropati,
              hyperviscosity_diagnosis = Reg_Hyperviskositet,
              dialysis_required_diagnosis = Reg_Dialysekraevende,
              bacterial_infection_diagnosis = Reg_BAKTINF,
              mspike_p_diagnosis = Reg_Mkomponent,
              mspike_u_diagnosis = Reg_UrinMKomponent,
              mspike_u_gl_diagnosis = Reg_UrinMKomponent_gL,
              
              bone_lesion_type_diagnosis = recode_factor(Reg_Knogleforandringer_type,
                                                            `0` = 'None',
                                                            `1` = 'Diffuse halisterese only',
                                                            `2` = 'Single region',
                                                            `3` = 'Multiple regions',
                                                            `9` = NA_character_), #9 = NA
              ### LAB VALUES FROM DIAGNOSIS
              HB_diagnosis = Reg_Haemoglobin,
              B2M_diagnosis = ifelse(is.na(Reg_Beta2Microglobulin_gl), Reg_Beta2Microglobulin_nMoll*0.0118, Reg_Beta2Microglobulin_gl),
              ALB_diagnosis = ifelse(Reg_Albumin_gl ==-1, NA, Reg_Albumin_gl),
              ALB_gL_diagnosis = round(ifelse(Reg_Albumin_mMoll ==-1, NA, Reg_Albumin_mMoll*0.0665),1), #uM => g/L
              KREA_diagnosis = round(ifelse(is.na(Reg_Creatinin_mikmoll), Reg_Creatinin_mmoll*1000, Reg_Creatinin_mikmoll),1), #uM => g/L
              Calcium_type = Reg_HvilkenCalcium, # should be "_diagnosis" - it comes from the "Reg" formula - should also be either "calcium" or "CA" according to naming scheme - MW
              CA2_diagnosis = Reg_CalciumIoniseret,
              CA_albumin_corrected_diagnosis  = Reg_CalciumAlbuminkorrigeret,
              CRP_diagnosis_mgL = ifelse(!is.na(Reg_CReaktivtProtein_gl), Reg_CReaktivtProtein_gl, Reg_CReaktivtProtein_nMoll*0.105), # Reg_CReaktivtProtein_gl is in mg/L - CB
              IgA_diagnosis = ifelse(is.na(Reg_IgA_gl), Reg_IgA_mikmoll*0.16, Reg_IgA_gl),
              IgG_diagnosis = ifelse(is.na(Reg_IgG_gl), Reg_IgG_mikmoll*0.1499, Reg_IgG_gl),
              IgM_diagnosis = ifelse(is.na(Reg_IgM_gl), Reg_IgM_mikmoll*0.971, Reg_IgM_gl),
              IgD_diagnosis = Reg_IgD,
              IgE_diagnosis = Reg_IgE,
              
              FLC_mgL_diagnosis = Reg_FRIKAEDEGL,
              FLC_uM_diagnosis = Reg_FRIKAEDEMIKMO,
              FLC_kappa_diagnosis = Reg_FrieKappaKaeder,
              FLC_lambda_diagnosis = Reg_FrieLambdaKaeder,
              FLC_highest_diagnosis = ifelse(Reg_FrieKappaKaeder > Reg_FrieLambdaKaeder, Reg_FrieKappaKaeder, Reg_FrieLambdaKaeder),
              FLC_above_100_diagnosis = ifelse(FLC_kappa_diagnosis > 100 | FLC_lambda_diagnosis > 100, 'Yes', 'No'),
              FLC_above_250_diagnosis = ifelse(FLC_kappa_diagnosis > 250 | FLC_lambda_diagnosis > 250, 'Yes', 'No'),
              
              kappa_lambda_ratio_diagnosis = Reg_FrieKappaKaeder/Reg_FrieLambdaKaeder, 
              FLC_involved_ratio_above_100_diagnosis = ifelse(kappa_lambda_ratio_diagnosis >100 & FLC_kappa_diagnosis > 100, 'Yes', 'No'),
              FLC_involved_ratio_above_100_diagnosis = ifelse(kappa_lambda_ratio_diagnosis <0.01 & FLC_lambda_diagnosis > 100, 'Yes', FLC_involved_ratio_above_100_diagnosis),
              
              LDH_diagnosis = Reg_LDH,
              
              # M-komp/M-spike
              mspike_number_diagnosis = Reg_PlasmaMKomp_Antal,
              mspike_diagnosis = Reg_PlasmaMKomponent, #g/L
              mspike_IgA_kappa_diagnosis = Reg_PlasmaMKomp_IgA_Kappa,
              mspike_IgA_lambda_diagnosis = Reg_PlasmaMKomp_IgA_Lambda,
              mspike_IgG_kappa_diagnosis = Reg_PlasmaMKomp_IgG_Kappa,
              mspike_IgG_lambda_diagnosis = Reg_PlasmaMKomp_IgG_Lambda,
              mspike_IgM_kappa_diagnosis = Reg_PlasmaMKomp_IgM_Kappa,
              mspike_IgM_lambda_diagnosis = Reg_PlasmaMKomp_IgM_Lambda,
              mspike_IgD_kappa_diagnosis = Reg_PlasmaMKomp_IgD_Kappa,
              mspike_IgD_lambda_diagnosis = Reg_PlasmaMKomp_IgD_Lambda,
              mspike_IgE_lambda_diagnosis = Reg_PlasmaMKomp_IgE_lambda,
              # Reg_PlasmaMKomp_Kappa, # For some reason not formatted - MW; Only available for 3% of cohort, thus calculated below instead as mspike_kappa_diagnosis - CB 
              # Reg_PlasmaMKomp_Lambda, # -- // --; Only available for 3% of cohort, thus calculated below instead as mspike_lambda_diagnosis - CB
              
              mspike_kappa_diagnosis = ifelse(mspike_IgA_kappa_diagnosis == 'Yes'
                                     | mspike_IgG_kappa_diagnosis == 'Yes'
                                     | mspike_IgM_kappa_diagnosis == 'Yes'
                                     | mspike_IgD_kappa_diagnosis == 'Yes' 
                                     | FLC_kappa_diagnosis > 100 , 'Yes', 'No'),
              mspike_lambda_diagnosis = ifelse(mspike_IgA_lambda_diagnosis == 'Yes'
                                     | mspike_IgG_lambda_diagnosis == 'Yes'
                                     | mspike_IgM_lambda_diagnosis == 'Yes'
                                     | mspike_IgD_lambda_diagnosis == 'Yes'
                                     | mspike_IgE_lambda_diagnosis == 'Yes'
                                     | FLC_lambda_diagnosis > 100, 'Yes', 'No'),
              mspike_isotype_diagnosis = gsub('NA', '', paste0(ifelse(mspike_IgA_kappa_diagnosis == 'Yes' 
                                                        | mspike_IgA_lambda_diagnosis == 'Yes' , 'IgA', ''),
                                                 ifelse(mspike_IgG_kappa_diagnosis == 'Yes' 
                                                        | mspike_IgG_lambda_diagnosis == 'Yes' , 'IgG', ''),
                                                 ifelse(mspike_IgM_kappa_diagnosis == 'Yes' 
                                                        | mspike_IgM_lambda_diagnosis == 'Yes' , 'IgM', ''),
                                                 ifelse(mspike_IgD_kappa_diagnosis == 'Yes' 
                                                        | mspike_IgD_lambda_diagnosis == 'Yes' , 'IgD', ''),
                                                 ifelse(mspike_IgE_lambda_diagnosis == 'Yes' , 'IgE', ''))),
              
              # Note: Definiation of LC-MM depending on absent of mspike with light-chain above 100 mg/L or FLC > heavy-chain M-spike protein
              mspike_isotype_diagnosis = ifelse(mspike_isotype_diagnosis %in% c('') & FLC_above_100_diagnosis == 'Yes', 'LC', mspike_isotype_diagnosis),
              mspike_isotype_diagnosis = ifelse(mspike_isotype_diagnosis  %in% c('') & (FLC_highest_diagnosis/1000) > mspike_diagnosis, 'LC', mspike_isotype_diagnosis),
              
              # NOTE: I've added some extra columns for the planned treatment at diagnosis with the suffix
              # _diagnosis, as it is planned treatment no later than 3 month after diagnosis date. 
              # What they are actually getting is seen in treatmentcolumns with _1st_line - EEP
              
              # treatments planned 
              cyclophosphamide_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Cyklophosphamid == "Y", "Yes", NA),
              melphalan_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Melfalan == "Y", "Yes", NA),
              bendamustine_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Bendamustin == "Y", "Yes", NA),
              doxorubicin_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Adriamycin == "Y", "Yes", NA),
              radiation_therapy_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Straalebeh == "Y", "Yes", NA),
              zoledronic_acid_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Zolendronat == "Y", "Yes", NA),
              pamidronic_acid_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Pamidronat == "Y", "Yes", NA),
              denosumab_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Denosumab == "Y", "Yes", NA),
              melphalan_stem_cell_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_MelfalanStamcell == "Y", "Yes", NA),
              allogeneic_stem_cell_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Allogen == "Y", "Yes", NA),
              elotuzumab_treatment_planned_diagnosis = ifelse(Reg_Planlagtbeh_Elotuzumab == "Y", "Yes", NA),
              daratumumab_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Daratumumab == "Y", "Yes", NA),
              isatuximab_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Isatuximab == "Y", "Yes", NA),
              panobinostat_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Panobinostat == "Y", "Yes", NA),
              thalidomide_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Thalidomid == "Y", "Yes", NA),
              lenalidomide_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Lenalidomid == "Y", "Yes", NA),
              pomalidomide_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Pomalidomid == "Y", "Yes", NA),
              bortezomib_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Bortezomib == "Y", "Yes", NA),
              carfilzomib_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Carfilzomib == "Y", "Yes", NA),
              ixazomib_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Ixazomib == "Y", "Yes", NA),
              steroid_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Steroid == "Y", "Yes", NA),
              cytosstatika_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_Cytosstatika == "Y", "Yes", NA),
              bisphosphonate_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_bisfosfonater == "Y", "Yes", NA),
              auto_stem_cell_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_AutoStam == "Y", "Yes", NA),
              other_treatment_planned_diagnosis = ifelse(Reg_PlanlagtBeh_other == "Y", "Yes", NA),
              
              ### -- FISH --
               
              # Negatives and not examined (NA) needs to be defined here: - CB
              date_FISH = parse_sundk_date(Cyto_Diagnose_dt),
              FISH_no_aberrations = Cyto_FishResultat_none,
              FISH_t4_14 = ifelse(is.na(Cyto_FishResultat_FGFR3) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_FGFR3),
              FISH_t11_14 = ifelse(is.na(Cyto_FishResultat_CCND1) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_CCND1),
              FISH_t14_16 = ifelse(is.na(Cyto_FishResultat_MBF) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_MBF),
              FISH_t14_20 = ifelse(is.na(Cyto_FishResultat_MAFB) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_MAFB),
              FISH_split_signal = ifelse(is.na(Cyto_FishResultat_SplitSignal) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_SplitSignal),
              FISH_del13q = ifelse(is.na(Cyto_FishResultat_13q) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_13q),
              FISH_del17p = ifelse(is.na(Cyto_FishResultat_TP53) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_TP53), # FISH tests del17p
              FISH_amp1q = ifelse(is.na(Cyto_FishResultat_1q21) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_1q21), 
              FISH_amp11q = ifelse(is.na(Cyto_FishResultat_11q22) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_11q22),
              FISH_other = ifelse(is.na(Cyto_FishResultat_other) & FISH_performed_diagnosis =='Yes', 'No', Cyto_FishResultat_other),

              ## RISS
              LDH_high = ifelse(age_diagnosis < 70 & LDH_diagnosis > 205, 'Yes', NA),
              LDH_high = ifelse(age_diagnosis < 70 & LDH_diagnosis <= 205, 'No', LDH_high),
              LDH_high = ifelse(age_diagnosis >= 70 & LDH_diagnosis > 255, 'Yes', LDH_high),
              LDH_high = factor(ifelse(age_diagnosis >= 70 & LDH_diagnosis <= 255, 'No', LDH_high), levels = c('Yes', 'No')),
              FISH_score = factor(ifelse(FISH_t4_14 =='Yes' | FISH_t14_16 =='Yes' | FISH_del17p =='Yes', 'Yes', 'No'), levels = c('Yes', 'No')),
              # FISH_score = ifelse(FISH_performed_diagnosis =='No', NA, FISH_score),
              
              ## this RISS coding seems very mysterious to me - MW; simplified, but FISH_score "No" - CB
              RISS_addon = ifelse(LDH_high =='Yes' | FISH_score =='Yes', 'Yes', 'No'),
              RISS_diagnosis = ifelse(ISS_diagnosis == 3 & RISS_addon == 'Yes', 3, 2),
              RISS_diagnosis = ifelse(ISS_diagnosis == 1 & RISS_addon == 'No', 1, RISS_diagnosis),
              RISS_diagnosis = ifelse(subtype == 'DC900', RISS_diagnosis, NA),
              RISS_diagnosis = factor(RISS_diagnosis, c(1, 2, 3)),
              renal_insufficiency_diagnosis = factor(ifelse(KREA_diagnosis >177, 'Yes', 'No'), levels = c('Yes', 'No')),
              BONE_LESION_wo_halisteresis_diagnosis = factor(ifelse(bone_lesion_type_diagnosis %in% c('Diffuse halisterese only', 'None'),
                                                           'No', 'Yes'), levels = c('Yes', 'No')), # CB - changed 2026-02-04 - was opposite 
              CRAB_calcium_high = factor(ifelse(CA2_diagnosis > 1.4, 'Yes', 'No'), levels = c('Yes', 'No') ),
              CRAB_renal_insufficiency = renal_insufficiency_diagnosis,
              CRAB_anemia = factor(ifelse(HB_diagnosis < 6.2, 'Yes', 'No'), levels = c('Yes', 'No')),
              CRAB_bone_disease = BONE_LESION_wo_halisteresis_diagnosis,
              
              CRAB_diagnosis = factor(ifelse(
                CA2_diagnosis > 1.4 
                | renal_insufficiency_diagnosis == 'Yes'
                | HB_diagnosis < 6.2
                | BONE_LESION_wo_halisteresis_diagnosis =='Yes', 'Yes', 'No'), levels = c('Yes', 'No')),
                
              ### -- 1ST LINE --
              PS_1st_line = factor(PB_PerformanceStatus, levels = c(0,1,2,3,4)),
              
              # complications at 1st line 
              #Note: This is during/after 1st line? Criteria to start relaps/2nd line treatment according to 'Registervejledning', which can also be found for SB and FU? EEP
              complication_mspike_increasinf_p_1st_line = ifelse(PB_Manifestekomp_MKomp =='Y','Yes',NA), # 	Stigende M-komponent, manifest kompl. (from RKKP-docu) - CB 
              complication_mspike_hypercalcimia_1st_line = ifelse(PB_Manifestekomp_Hypercal =='Y','Yes',NA), #HypercalcÃƒÂ¦mi (klin betydende),manifest kompl. 
              complication_osteofoci_1st_line = ifelse(PB_Manifestekomp_OsteoFoci =='Y','Yes',NA),
              complication_MTS_1st_line = ifelse(PB_Manifestekomp_MedulTvaer =='Y','Yes',NA),
              complication_extramedullary_myeloma_1st_line = ifelse(PB_Manifestekomp_EkstraMedul =='Y','Yes',NA),
              complication_plasmacell_leukemia_1st_line = ifelse(PB_Manifestekomp_PlasmaLeu=='Y','Yes',NA),
              complication_clinical_uraemia_1st_line = ifelse(PB_Manifestekomp_KliniskUraemi=='Y','Yes',NA),
              complication_chronic_uraemia_1st_line = ifelse(PB_Manifestekomp_KroniskUraemi=='Y','Yes',NA),
              complication_amyloidosis_1st_line = ifelse(PB_Manifestekomp_Amyloidose=='Y','Yes',NA),
              complication_neuropathy_1st_line = ifelse(PB_Manifestekomp_Neuropati=='Y','Yes',NA),
              complication_hyperviscosity_1st_line = ifelse(PB_Manifestekomp_HyperViskosi=='Y','Yes',NA),
              # complications_n_1st_line = PB_Manifestekomp_antal, # not found 2025 extract - CB

              ## treatments at 1st line given
              cyclophosphamide_treatment_1st_line = ifelse(PB_Behandling_Cyklophosphamid == "Y", "Yes", NA),
              melphalan_treatment_1st_line = ifelse(PB_Behandling_Melfalan == "Y", "Yes", NA),
              bendamustine_treatment_1st_line = ifelse(PB_Behandling_Bendamustin == "Y", "Yes", NA),
              doxorubicin_treatment_1st_line = ifelse(PB_Behandling_Adriamycin == "Y", "Yes", NA),
              radiation_therapy_treatment_1st_line = ifelse(PB_Behandling_Straalebeh == "Y", "Yes", NA),
              zoledronic_acid_treatment_1st_line = ifelse(PB_Behandling_Zolendronat == "Y", "Yes", NA),
              pamidronic_acid_treatment_1st_line = ifelse(PB_Behandling_Pamidronat == "Y", "Yes", NA),
              denosumab_treatment_1st_line = ifelse(PB_Behandling_Denosumab == "Y", "Yes", NA),
              melphalan_stem_cell_treatment_1st_line = ifelse(PB_Behandling_MelfalanStamcelle == "Y", "Yes", NA),
              allogeneic_stem_cell_treatment_1st_line = ifelse(PB_Behandling_Allogen == "Y", "Yes", NA),
              elotuzumab_treatment_1st_line = ifelse(PB_Behandling_Elotuzumab == "Y", "Yes", NA),
              daratumumab_treatment_1st_line = ifelse(PB_Behandling_Daratumumab == "Y", "Yes", NA),
              isatuximab_treatment_1st_line = ifelse(PB_Behandling_Isatuximab == "Y", "Yes", NA),
              panobinostat_treatment_1st_line = ifelse(PB_Behandling_Panobinostat == "Y", "Yes", NA),
              thalidomide_treatment_1st_line = ifelse(PB_Behandling_Thalidomid == "Y", "Yes", NA),
              lenalidomide_treatment_1st_line = ifelse(PB_Behandling_Lenalidomid == "Y", "Yes", NA),
              pomalidomide_treatment_1st_line = ifelse(PB_Behandling_Pomalidomid == "Y", "Yes", NA),
              bortezomib_treatment_1st_line = ifelse(PB_Behandling_Bortezomib == "Y", "Yes", NA),
              carfilzomib_treatment_1st_line = ifelse(PB_Behandling_Carfilzomib == "Y", "Yes", NA),
              ixazomib_treatment_1st_line = ifelse(PB_Behandling_Ixazomib == "Y", "Yes", NA),
              steroid_treatment_1st_line = ifelse(PB_Behandling_Steroid == "Y", "Yes", NA),
              cytosstatika_treatment_1st_line = ifelse(PB_Behandling_Cytosstatika == "Y", "Yes", NA),
              biphosphonate_treatment_1st_line = ifelse(PB_Behandling_bisfosfonater == "Y", "Yes", NA),
              auto_stem_cell_treatment_1st_line = ifelse(PB_Behandling_AutoStam == "Y", "Yes", NA),
              other_treatment_1st_line = ifelse(PB_Behandling_other == "Y", "Yes", NA),
              
              # note - very weird column with free text
              auxiliary_treatment_1st_line = PB_OevrigBehandling,
              
              chemotherapy_1st_line = ifelse(cytosstatika_treatment_1st_line == 'Yes' 
                                             | cyclophosphamide_treatment_1st_line == 'Yes'
                                             | melphalan_treatment_1st_line == 'Yes'
                                             | bendamustine_treatment_1st_line == 'Yes'
                                             | doxorubicin_treatment_1st_line == 'Yes', 'Yes', 'No'),
              immunotherapy_1st_line = ifelse(elotuzumab_treatment_1st_line == 'Yes' 
                                             | daratumumab_treatment_1st_line == 'Yes'
                                             | isatuximab_treatment_1st_line == 'Yes', 'Yes', 'No'),
              IMID_1st_line = ifelse(thalidomide_treatment_1st_line == 'Yes' 
                                              | lenalidomide_treatment_1st_line == 'Yes'
                                              | pomalidomide_treatment_1st_line == 'Yes', 'Yes', 'No'),
              
              proteasome_inihibitor_1st_line = ifelse(bortezomib_treatment_1st_line == 'Yes' 
                                     | carfilzomib_treatment_1st_line == 'Yes'
                                     | ixazomib_treatment_1st_line == 'Yes', 'Yes', 'No'),
              treatment_type_1st_line_all = gsub('NA', '', paste0(ifelse(elotuzumab_treatment_1st_line == 'Yes', 'Elo', ''),
                                        ifelse(daratumumab_treatment_1st_line == 'Yes', 'Dara', ''),
                                        ifelse(isatuximab_treatment_1st_line == 'Yes', 'Isa', ''),
                                        
                                        ifelse(bortezomib_treatment_1st_line == 'Yes', 'Bor', ''),
                                        ifelse(carfilzomib_treatment_1st_line == 'Yes', 'Car', ''),
                                        ifelse(ixazomib_treatment_1st_line == 'Yes', 'Ixa', ''),
                                        
                                        ifelse(thalidomide_treatment_1st_line == 'Yes', 'Thal', ''),
                                        ifelse(lenalidomide_treatment_1st_line == 'Yes', 'Len', ''),
                                        ifelse(pomalidomide_treatment_1st_line == 'Yes', 'Pom', ''),
                                        
                                        ifelse(cyclophosphamide_treatment_1st_line == 'Yes', 'Cy', ''),
                                        ifelse(melphalan_treatment_1st_line == 'Yes', 'Mel', ''),
                                        ifelse(bendamustine_treatment_1st_line == 'Yes', 'Ben', ''),
                                        ifelse(doxorubicin_treatment_1st_line == 'Yes', 'Doxo', ''))),
              response_1st_line = recode_factor(PB_MaksimaleRespons,
                                                CR = 'CR',  
                                                sCR = 'sCR',
                                                pCR = 'pCR',
                                                vgPR = 'VGPR',
                                                PR = 'PR',
                                                MR = 'MR',
                                                SD = 'SD',
                                                PD   = 'PD',
                                                Mors = 'Died', 
                                                UNK = NA_character_),
              response_simplified_1st_line = recode_factor(PB_MaksimaleRespons,
                                                           CR = 'CR',  
                                                           sCR = 'CR',
                                                           pCR = 'CR',
                                                           vgPR = 'VGPR',
                                                           PR = 'PR',
                                                           MR = 'SD',# minimal response
                                                           SD = 'SD',
                                                           PD   = 'PD',
                                                           Mors = NA_character_, 
                                                           UNK = NA_character_),
              treatment_type_1st_line = recode(treatment_type_1st_line_all,
                                               Bor = 'VelDex',
                                               BorLen = 'VRD',
                                               BorCy = 'VCD',
                                               Thal = 'ThalDex',
                                               BorMel = 'MPV',
                                               BorLenCy = 'VRD/VCD',
                                               Len = 'LenDex',
                                               DaraBorMel = 'D-VMP',
                                               BorPom = 'PomVelDex',
                                               BorThal = 'ThalVelDex',
                                               BorLenMel = 'VMP/VRD/RD', # AS replaced 'MPV+R'
                                               Mel = 'MP',
                                               Pom = 'PomDex',
                                               Cy = 'CyDex',
                                               DaraBorLen = 'D-VRD',
                                               BorThalCy = 'CyVelDex/ThalVelDex',
                                               DaraBor = 'DVD',
                                               DaraBorLenMel = 'D-VMP/D-RD' ,# AS replaced 'D-VMP+R'
                                               BorCyMel = 'VMP/VCD',
                                               BorThalLen = 'VTD/VRD', # AS replaced 'ThalVelDex+R',
                                               DaraLen = 'DRD',
                                               DaraBorLenCy = 'CVD/D-RD', # AS replaced 'D-VCD/VRD', #
                                               BorCarLenCy = 'VCD/CarLenDex',
                                               BorThalMel = 'MPV/MPT',
                                               IsaBorLen = 'Isa-VRD',
                                               ThalMel = 'MPT',
                                               ThalPom = 'ThalDex/PomDex', # AS replaced 'ThalPomDex',
                                               CarLen = 'KRD',
                                               IxaThal = 'IxaThalDex',
                                               LenMel = 'LenDex/MP', # AS replaced 'MelLenDex', 
                                               BorCarLen = 'VRD/KRD',
                                               LenCy = 'LenDex/CyDex' ), # AS replaced 'RCD'
              maintenance_lenalidomide_1st_line = PB_VedligeholdBeh_Lenalidomid, 
              maintenance_pomalidomide_1st_line= PB_VedligeholdBeh_Pomalidomid, 
              maintenance_thalidomide_1st_line= PB_VedligeholdBeh_Thalidomid, 
              maintenance_bortezomib_1st_line= PB_VedligeholdBeh_Bortezomib, 
              maintenance_carfilzomib_1st_line= PB_VedligeholdBeh_Carfilzomib,
              maintenance_steroid_1st_line= PB_VedligeholdBeh_Steroid,
              maintenance_other_1st_line = PB_VedligeholdBeh_other,
              maintenance_therapy_1st_line = recode_factor(+(if_any(contains('maintenance_'), ~ .x == 'Yes')),
                                                           `1` = 'Yes',
                                                           `0` = 'No'),
              
              
              # n_treatments_planned_diagnosis = Reg_planlagtBeh_antal, #not found 2026
              # n_treatments_1st_line = PB_Behandling_antal, #not found 2026
              
              #included_in_protocol_1st_line = ifelse(PB_PatientProtokol == 'Y', 1, 0), 
              
              # measurements at 1st line treatment
              HB_1st_line = PB_Haemoglobin,
              B2M_1st_line = ifelse(is.na(PB_Beta2Microglobulin_gl), PB_Beta2Microglobulin_nMoll*0.0118, PB_Beta2Microglobulin_gl),
              
              ALB_1st_line = ifelse(PB_Albumin_gl ==-1, NA, PB_Albumin_gl),
              ALB_gL_1st_line = round(ifelse(PB_Albumin_mMoll ==-1, NA, PB_Albumin_mMoll*0.0665),1), #uM => g/L
              CREA_1st_line = round(ifelse(is.na(PB_Creatinin_MikMoll), PB_Creatinin_mMoll*1000, PB_Creatinin_MikMoll),1),
              CREA_latest_1st_line = PB_CreatininSeneste,
              
              
              
              ### -- 2ND LINE --
              cyclophosphamide_treatment_2nd_line = ifelse(SB_Behandling_Cyklophosphamid == "Y", "Yes", NA),
              melphalan_treatment_2nd_line = ifelse(SB_Behandling_Melfalan == "Y", "Yes", NA),
              bendamustine_treatment_2nd_line = ifelse(SB_Behandling_Bendamustin == "Y", "Yes", NA),
              doxorubicin_treatment_2nd_line = ifelse(SB_Behandling_Adriamycin == "Y", "Yes", NA),
              radiation_therapy_treatment_2nd_line = ifelse(SB_Behandling_Straalebeh == "Y", "Yes", NA),
              zoledronic_acid_treatment_2nd_line = ifelse(SB_Behandling_Zolendronat == "Y", "Yes", NA),
              pamidronic_acid_treatment_2nd_line = ifelse(SB_Behandling_Pamidronat == "Y", "Yes", NA),
              denosumab_treatment_2nd_line = ifelse(SB_Behandling_Denosumab == "Y", "Yes", NA),
              melphalan_stem_cell_treatment_2nd_line = ifelse(SB_Behandling_MelfalanStamcelle == "Y", "Yes", NA),
              allogeneic_stem_cell_treatment_2nd_line = ifelse(SB_Behandling_Allogen == "Y", "Yes", NA),
              elotuzumab_treatment_2nd_line = ifelse(SB_Behandling_Elotuzumab == "Y", "Yes", NA),
              daratumumab_treatment_2nd_line = ifelse(SB_Behandling_Daratumumab == "Y", "Yes", NA),
              isatuximab_treatment_2nd_line = ifelse(SB_Behandling_Isatuximab == "Y", "Yes", NA),
              panobinostat_treatment_2nd_line = ifelse(SB_Behandling_Panobinostat == "Y", "Yes", NA),
              thalidomide_treatment_2nd_line = ifelse(SB_Behandling_Thalidomid == "Y", "Yes", NA),
              lenalidomide_treatment_2nd_line = ifelse(SB_Behandling_Lenalidomid == "Y", "Yes", NA),
              pomalidomide_treatment_2nd_line = ifelse(SB_Behandling_Pomalidomid == "Y", "Yes", NA),
              bortezomib_treatment_2nd_line = ifelse(SB_Behandling_Bortezomib == "Y", "Yes", NA),
              carfilzomib_treatment_2nd_line = ifelse(SB_Behandling_Carfilzomib == "Y", "Yes", NA),
              ixazomib_treatment_2nd_line = ifelse(SB_Behandling_Ixazomib == "Y", "Yes", NA),
              steroid_treatment_2nd_line = ifelse(SB_Behandling_Steroid == "Y", "Yes", NA),
              cytosstatika_treatment_2nd_line = ifelse(SB_Behandling_Cytosstatika == "Y", "Yes", NA),
              biphosphonate_treatment_2nd_line = ifelse(SB_Behandling_bisfosfonater == "Y", "Yes", NA),
              auto_stem_cell_treatment_2nd_line = ifelse(SB_Behandling_AutoStam == "Y", "Yes", NA),
              other_treatment_2nd_line = ifelse(SB_Behandling_other == "Y", "Yes", NA),
              
              response_2nd_line = recode_factor(SB_Respons,
                                                CR = 'CR',  
                                                sCR = 'sCR',
                                                pCR = 'pCR',
                                                vgPR = 'VGPR',
                                                PR = 'PR',
                                                MR = 'MR',
                                                SD = 'SD',
                                                PD   = 'PD',
                                                Mors = 'Died', 
                                                UNK = NA_character_),
              response_simplified_2nd_line = recode_factor(SB_Respons,
                                                           CR = 'CR',  
                                                           sCR = 'CR',
                                                           pCR = 'CR',
                                                           vgPR = 'VGPR',
                                                           PR = 'PR',
                                                           MR = 'SD', # minimal response
                                                           SD = 'SD',
                                                           PD   = 'PD',
                                                           Mors = NA_character_, 
                                                           UNK = NA_character_),
              
              # dates
              
              date_treatment_1st_line_start = parse_sundk_date(PB_PrimaerbehandlingStart_dt),
              date_treatment_1st_line_end = parse_sundk_date(PB_PrimaerbehandlingSlut_dt),
              date_treatment_2nd_line = parse_sundk_date(SB_SekundaerBehandlingstart_dt),
              date_HDT_1st_line = parse_sundk_date(PB_AutologeStamcellestoette_dt),
              date_HDT2_1st_line = parse_sundk_date(PB_AutologeStamcellestoette2_dt),
              HDT_tandem_1st_line = ifelse(diff_days(date_HDT_1st_line, date_HDT2_1st_line) < 180, 'Yes', 'No'),
              
              date_treatment_2nd_line_start = parse_sundk_date(SB_SekundaerBehandlingstart_dt),
              date_death = parse_sundk_date(CPR_Doedsdato),# FU_Doed_dt # too many NAs compared with CPR_Doedsdato
              date_last_fu = parse_sundk_date(CPR_Opdat_dt),
              date_death_fu = if_else(is.na(date_death), date_last_fu, date_death),
              treatment = ifelse(is.na(date_treatment_1st_line_start), 0, 1),
              treatment_2nd_line = ifelse(is.na(date_treatment_2nd_line_start), 0, 1),
              dead = ifelse(is.na(date_death), 0, 1)) %>%  
    ## make treatment grouping variable - concatenated all treatments - doesn't quite group like that - CB
  
    mutate(date_treatment_1st_fu = if_else(is.na(date_treatment_1st_line_start), date_death_fu, date_treatment_1st_line_start),
           date_treatment_2nd_fu = if_else(is.na(date_treatment_2nd_line_start), date_death_fu, date_treatment_2nd_line_start),
           time_OS = diff_days(date_diagnosis, date_death_fu),
           time_to_death = time_OS,
           time_to_treatment = diff_days(date_diagnosis, date_treatment_1st_fu),
           time_1st_to_2nd_treatment = diff_days(date_treatment_1st_line_start, date_treatment_2nd_fu)) %>% 
    # mutate(across(contains('FISH_'), ~if_else(FISHperformed == 'Yes' & is.na(.), 'No', .))) %>% 
    mutate(treated_within_90_days = ifelse(time_to_treatment <= 90 & treatment == 1, 'Yes', 'No'),
           died_within_90_days = ifelse(time_to_death <= 90 & dead == 1, 'Yes', 'No'),
           subtype_am_klausen = factor(ifelse(treated_within_90_days !='Yes'
                                              & CRAB_diagnosis != 'Yes'
                                              & died_within_90_days !='Yes'
                                              & time_to_treatment > 90, 'SMM', 'MM'),
                                       levels = c('SMM', 'MM')),
           subtype_am_klausen = if_else(subtype == 'DC900', subtype_am_klausen, NA))
}

#### test ####
# MM_clean = SUNDK_DaMyDa %>%
#   clean_SUNDK_DAMYDA()