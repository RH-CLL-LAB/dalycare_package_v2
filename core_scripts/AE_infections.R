## AE_infections
# Infection events for the CORE table all_comorbidity_and_ae (see table_info/all_comorbidity_and_ae.yaml)
# Based on 20250401_infections_def_cmf.Rmd (5 definitions). The earlier separate IVAB_Infection event
# (AB_AE_infections.R) was identical to definition 2 (IV_AB) and is not a separate event type anymore.
#
# The logic lives in build_AE_infection_5D(), which gets the data as arguments.
# run_AE_infections() loads the data and calls it. Nothing runs when this file is
# sourced; use
#   infections = run_AE_infections()
# The table is not saved here: writing to core/ is done by the core pipeline.
#
# Output has the shape of all_comorbidity_and_ae:
#   patientid, event_id, start_date, end_date, duration_days, event_value, max_value, event_type
# event_id counts the events of one patient within one event_type, in date order.

#### 5 definitions ####
#' Infection events by 5 definitions. Infections of one definition are separated by more than 7 days.
#' 1) BC+IV_AB: IV antibiotics (at least 24 hours) within 48 hours of a blood culture drawn
#' 2) IV_AB: IV antibiotics for at least 24 hours, independent of blood culture
#' 3) BC_drawn: blood culture drawn, independent of antibiotics
#' 4) any_AB_pre_or_adm: any antibiotics prescribed or administered for at least 24 hours (not the
#'    prophylactic antimicrobials in prophylactic_AB)
#' 5) BC_OR_any_AB: either 3) or 4)
#' @param SP_AdministreretMedicin,SP_OrdineretMedicin medication tables (loaded for the cohort)
#' @param SP_BC blood cultures from load_blood_culture_SP() (patientid, ord_value, specimn_taken_time;
#'   column names are lowercased here), already restricted to the cohort
#' @return one row per infection event and definition, with event_value 1
build_AE_infection_5D = function(SP_AdministreretMedicin, SP_OrdineretMedicin, SP_BC,
                                 days_min_AB_duration = 1,
                                 days_between_separating_infections = 7,
                                 prophylactic_AB = c('Aciclovir', 'Sulfotrim', 'Valaciclovir'),
                                 exclude_lab = c('Afbestilt', 'Aflyst', 'Annulleret', 'Dublet', 'cancelled', 'Ikke taget',
                                                 'Ej modtaget', 'Ej tilstede', 'Ikke Udført', 'Slettet', 'Mislykket')){

  #### Blood cultures drawn ####
  SP_BC = SP_BC %>%
    rename_with(tolower) %>%
    filter(!ord_value %in% exclude_lab) %>%
    mutate(specimn_taken_time = as.Date(specimn_taken_time)) %>%
    select(patientid, date_BC_drawn = specimn_taken_time, value = ord_value) %>%
    group_by(patientid, date_BC_drawn, value)

  #### Administered antibiotics ####
  SP_AB = SP_AdministreretMedicin %>% ATC_AB()
  SP_IV_infections = SP_AB %>%
    AE_infection(days_min_AB_duration = days_min_AB_duration,
                 days_between_separating_infections = days_between_separating_infections)

  ## Definition 1: BC and >24 hours i.v. AB within 2 days of each other
  def_1_AB_BC = SP_BC %>%
    inner_join(SP_IV_infections, by = 'patientid', relationship = 'many-to-many') %>%
    mutate(infection_def_1 = case_when(
      abs(difftime(date_BC_drawn, date_infection, units = "days")) <= 2 ~ 1, TRUE ~ 0),
      date_inf_def_1 = pmin(date_BC_drawn, date_infection)) %>%
    filter(infection_def_1 == 1) %>%
    group_by(patientid, date_inf_def_1) %>%
    arrange(patientid, date_inf_def_1) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(patientid) %>%
    arrange(date_inf_def_1) %>%
    mutate(keep = c(TRUE, diff(date_inf_def_1) > days_between_separating_infections)) %>%
    filter(keep)  %>%
    ungroup() %>%
    select(patientid, infection_def_1, date_inf_def_1)

  ## Definition 2: Intravenous AB administered >24 hours
  def_2_iv_AB = SP_IV_infections %>%
    mutate(date_inf_def_2 = as.Date(as.POSIXct(date_infection)),
           infection_def_2 = ifelse(!is.na(date_inf_def_2), 1, 0)) %>%
    arrange(patientid, date_inf_def_2) %>%
    select(patientid, infection_def_2, date_inf_def_2) %>%
    group_by(patientid, date_inf_def_2) %>%
    slice(1) %>%
    ungroup()

  ## Definition 3: Blood Culture drawn (one per day, then more than 7 days apart)
  def_3_BC_filtered = SP_BC %>%
    mutate(date_inf_def_3 = date_BC_drawn,
           infection_def_3 = ifelse(!is.na(date_inf_def_3), 1, 0)) %>%
    arrange(patientid, date_inf_def_3) %>%
    group_by(patientid, date_inf_def_3) %>%
    slice(1) %>%
    ungroup() %>%
    select(patientid, infection_def_3, date_inf_def_3) %>%
    group_by(patientid) %>%
    arrange(date_inf_def_3) %>%
    mutate(keep = c(TRUE, diff(date_inf_def_3) > days_between_separating_infections)) %>%
    filter(keep) %>%
    ungroup() %>%
    select(patientid, infection_def_3, date_inf_def_3)

  ## Definition 4: Any non-prophylactic AB administered/prescribed
  # SP administered:
  SP_adm_AB_inf = SP_AB %>%
    mutate(prophylactic = ifelse(AB.DETAILED %in% prophylactic_AB & zc_admin_route_name == "Oral anvendelse", "yes", "no"))  %>%
    filter(prophylactic == "no") %>%
    group_by(patientid) %>%
    arrange(patientid, taken_time) %>%
    mutate(AB_type = "administered",
           DIFF = diff_days(lag(taken_time), taken_time)) %>%
    ungroup() %>%
    mutate(N = ifelse(DIFF > days_between_separating_infections, row_number(), NA),
           N = ifelse(row_number() == 1, 1, N)) %>%
    mutate(N = as.numeric(factor(N))) %>%
    fill(N) %>%
    ungroup() %>%
    mutate(DIFF = ifelse(is.na(DIFF), 0, DIFF),
           DIFF = ifelse(DIFF > days_between_separating_infections, 0, DIFF)) %>%
    group_by(patientid, N) %>%
    mutate(n_days_AB = round(sum(DIFF), 2)) %>%
    slice(1) %>%
    ungroup() %>%
    filter(n_days_AB >= days_min_AB_duration) %>%
    group_by(patientid) %>%
    mutate(N = row_number()) %>%
    ungroup() %>%
    mutate(date_infection = as_date(taken_time)) %>%
    select(patientid, date_infection, DIFF, n_inf = N, n_days_AB, AB_type, everything())

  # SP prescribed (we do not know that prescribed medicine was taken, or for how long):
  SP_ord_AB_inf = SP_OrdineretMedicin %>%
    ATC_AB() %>%
    mutate(prophylactic = ifelse(AB.DETAILED %in% prophylactic_AB, "yes", "no")) %>%
    filter(prophylactic == "no") %>%
    group_by(patientid) %>%
    arrange(patientid, order_start_time) %>%
    mutate(AB_type = "prescribed",
           DIFF = diff_days(lag(order_start_time), order_start_time)) %>%
    ungroup() %>%
    mutate(N = ifelse(DIFF > days_between_separating_infections, row_number(), NA),
           N = ifelse(row_number() == 1, 1, N)) %>%
    mutate(N = as.numeric(factor(N))) %>%
    fill(N) %>%
    ungroup() %>%
    mutate(DIFF = ifelse(is.na(DIFF), 0, DIFF),
           DIFF = ifelse(DIFF > days_between_separating_infections, 0, DIFF)) %>%
    group_by(patientid, N) %>%
    mutate(n_days_AB = round(diff_days(order_start_time, order_end_time), 2)) %>%
    slice(1) %>%
    ungroup() %>%
    filter(n_days_AB >= days_min_AB_duration) %>%
    group_by(patientid) %>%
    mutate(N = row_number())  %>%
    ungroup() %>%
    mutate(date_infection = as_date(order_start_time)) %>%
    select(patientid, date_infection, DIFF, n_inf = N, n_days_AB, AB_type, everything())

  def_4_any_AB_filtered = bind_rows(SP_adm_AB_inf, SP_ord_AB_inf) %>%
    mutate(date_inf_def_4 = as.Date(as.POSIXct(date_infection)),
           infection_def_4 = ifelse(!is.na(date_inf_def_4), 1, 0)) %>%
    arrange(patientid, date_inf_def_4) %>%
    group_by(patientid, date_inf_def_4) %>%
    slice(1) %>%
    ungroup() %>%
    select(patientid, infection_def_4, date_inf_def_4, AB_type, n_days_AB, everything()) %>%
    group_by(patientid) %>%
    arrange(patientid, date_inf_def_4) %>%
    mutate(keep = c(TRUE, diff(date_inf_def_4) > days_between_separating_infections)) %>%
    filter(keep) %>%
    ungroup() %>%
    select(patientid, infection_def_4, date_inf_def_4)

  ## Definition 5: BC or AB: either one of them (AB for at least 24 hours)
  def_5_AB_OR_BC_filtered = bind_rows(
    def_3_BC_filtered %>% rename(date_inf_def_5 = date_inf_def_3, infection_def_5 = infection_def_3),
    def_4_any_AB_filtered %>% rename(date_inf_def_5 = date_inf_def_4, infection_def_5 = infection_def_4)) %>%
    group_by(patientid, date_inf_def_5) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(patientid) %>%
    arrange(date_inf_def_5) %>%
    mutate(keep = c(TRUE, diff(date_inf_def_5) > days_between_separating_infections)) %>%
    filter(keep) %>%
    ungroup() %>%
    select(patientid, infection_def_5, date_inf_def_5)

  #### Events in the shape of all_comorbidity_and_ae ####
  as_AE = function(data, date, type_label){
    data %>%
      ungroup() %>%
      transmute(patientid,
                start_date = as.Date({{date}}),
                end_date = as.Date(NA),
                duration_days = as.numeric(NA),
                event_value = 1,
                max_value = as.numeric(NA),
                event_type = type_label)
  }

  bind_rows(
    def_1_AB_BC %>% as_AE(date_inf_def_1, 'BC+IV_AB'),
    def_2_iv_AB %>% as_AE(date_inf_def_2, 'IV_AB'),
    def_3_BC_filtered %>% as_AE(date_inf_def_3, 'BC_drawn'),
    def_4_any_AB_filtered %>% as_AE(date_inf_def_4, 'any_AB_pre_or_adm'),
    def_5_AB_OR_BC_filtered %>% as_AE(date_inf_def_5, 'BC_OR_any_AB')) %>%
    arrange(patientid, start_date, event_type) %>%
    group_by(patientid, event_type) %>%
    mutate(event_id = row_number()) %>%
    ungroup() %>%
    select(patientid, event_id, start_date, end_date, duration_days, event_value, max_value, event_type)
}

#### Run ####
# Loads the data for all patients in all_patients and builds the infection events (the 5
# definitions). The dalycare package must be loaded first. Nothing runs when this file is sourced.
run_AE_infections = function(){
  load_dataset('all_patients')
  load_dataset('SP_AdministreretMedicin', all_patients$patientid)
  load_dataset('SP_OrdineretMedicin', all_patients$patientid)
  SP_BC = load_blood_culture_SP() %>%
    rename_with(tolower) %>%
    filter(patientid %in% all_patients$patientid)

  build_AE_infection_5D(SP_AdministreretMedicin_subset, SP_OrdineretMedicin_subset, SP_BC) %>%
    arrange(patientid, start_date, event_type)
}
