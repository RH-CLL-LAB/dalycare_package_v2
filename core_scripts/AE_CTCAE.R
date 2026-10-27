## AE_CTCAE
# CTCAE-graded laboratory adverse events for the CORE table all_comorbidity_and_ae (see table_info/all_comorbidity_and_ae.yaml)
# Based on CTCAE_v2.R, function_summarize_events_v2.R and AE_combined.R (the C++ program process_ae_files is replaced
# by summarize_AE_events() below).
#
# The grading is done by the AE_*() functions in functions/CTCAE/. build_AE_CTCAE() grades the laboratory
# measurements and summarises the grades to events, run_AE_CTCAE() loads the data and calls it. Nothing runs when
# this file is sourced; use
#   CTCAE = run_AE_CTCAE()
# The table is not saved here: writing to core/ is done by the core pipeline.
#
# Output has the shape of all_comorbidity_and_ae:
#   patientid, event_id, start_date, end_date, duration_days, event_value, max_value, event_type
# event_value is the CTCAE grade (1-4) the event started with, max_value the highest grade during the event.

#### Population ####
# In CTCAE_v2.R, ALAT, ASAT and BASP run on patients with a first malignant diagnosis
# (view_definition_first_diagnosis_malignant), while the other types run on all laboratory data. Here the
# malignant group is used for all types. The whole cohort has a malignant diagnosis, so the result is the same.

#### Which adverse events ####
# fn = grading function in functions/CTCAE/, col = grade column it returns, event_type = label in the table.
# The same 22 types as in the old table. AE_leukocytosis() and AE_hemoglobin_increased() were not part of it.
CTCAE_types = data.frame(
  fn = c('AE_ALAT_increased', 'AE_ASAT_increased', 'AE_BASP_increased', 'AE_INR_increased',
         'AE_hyperkalemia', 'AE_WBC_decreased', 'AE_anemia', 'AE_bilirubin_increased',
         'AE_creatinine_increased', 'AE_eosinophilia', 'AE_haptoglobin_decreased',
         'AE_hypercalcemia', 'AE_hypermagnesemia', 'AE_hypernatremia', 'AE_hypertriglyceridemia',
         'AE_hypoalbuminemia', 'AE_hypocalcemia', 'AE_hypoglycemia',
         'AE_lymphocyte_count_decreased', 'AE_lymphocyte_count_increased',
         'AE_neutrophil_count_decreased', 'AE_platelets_decreased'),
  stringsAsFactors = FALSE)
CTCAE_types$col = sub('^AE_', 'ae_', CTCAE_types$fn)
CTCAE_types$event_type = sub('^AE_', 'ae_', CTCAE_types$fn)
CTCAE_types$event_type[CTCAE_types$fn == 'AE_WBC_decreased'] = 'ae_wbc_decreased'

#### Summarise grades to events ####
#' Summarise graded measurements to events. An event is a run of the same positive grade; it ends at the
#' first measurement with grade 0 or at a measurement with another positive grade (which starts a new event).
#' An event that is never followed by a grade 0 or another grade ends at the last measurement of the patient.
#' The grade '3-4' (AE_anemia(), AE_hypoalbuminemia()) is counted as grade 3. The old workflow turned it into
#' a missing value, which the C++ program read as grade 0, so the most severe measurements ended the event.
#' @param data patientid, date_event, value_event (grade 0-4) and type_event, one type at a time
#' @return patientid, event_id (per patient), start_date, end_date, duration_days (end - start + 1),
#'   event_value, max_value, event_type
summarize_AE_events = function(data){
  data = data %>%
    mutate(date_event = as.Date(date_event),
           # AE_anemia() and AE_hypoalbuminemia() return '3-4' for the most severe grade: counted as grade 3
           value_event = as.integer(if_else(as.character(value_event) == '3-4', '3', as.character(value_event)))) %>%
    filter(!is.na(date_event), !is.na(value_event))

  marked_data = data %>%
    arrange(patientid, date_event) %>%
    group_by(patientid) %>%
    mutate(new_event = (value_event > 0) &
             ((lag(value_event, default = 0) == 0) |
                (lag(value_event, default = 0) > 0 & value_event != lag(value_event))),
           event_id = cumsum(new_event),
           prev_event_id = lag(event_id, default = 0),
           max_date = max(date_event)) %>%
    ungroup()

  events_info = marked_data %>%
    filter(value_event > 0) %>%
    group_by(patientid, event_id) %>%
    summarise(start_date = min(date_event),
              event_value = first(value_event),
              max_value = max(value_event),
              event_type = first(type_event),
              max_date = first(max_date),
              .groups = 'drop')

  # a change to another positive grade ends the previous event
  positive_transitions = marked_data %>%
    group_by(patientid) %>%
    filter(new_event, lag(value_event, default = 0) > 0) %>%
    ungroup() %>%
    select(patientid, event_id = prev_event_id, pos_transition_date = date_event)

  # the first grade 0 after an event ends it
  zero_transitions = marked_data %>%
    group_by(patientid) %>%
    mutate(last_event = if_else(value_event == 0, lag(event_id), NA_integer_)) %>%
    ungroup() %>%
    filter(value_event == 0, !is.na(last_event)) %>%
    group_by(patientid, event_id = last_event) %>%
    summarise(zero_date = min(date_event), .groups = 'drop')

  events_info %>%
    left_join(positive_transitions, by = c('patientid', 'event_id')) %>%
    left_join(zero_transitions, by = c('patientid', 'event_id')) %>%
    mutate(end_date = case_when(!is.na(pos_transition_date) ~ pos_transition_date,
                                !is.na(zero_date) ~ zero_date,
                                TRUE ~ max_date),
           duration_days = as.numeric(difftime(end_date, start_date, units = 'days')) + 1) %>%
    select(patientid, event_id, start_date, end_date, duration_days, event_value, max_value, event_type)
}

#### Build ####
#' Grade the laboratory measurements and summarise the grades to events, one adverse event type at a time.
#' @param lab all_laboratory_measurements with patientid, samplingdate, analysiscode, c_value,
#'   referenceinterval_lowerlimit and referenceinterval_upperlimit (only the patients that should be included)
#' @param types which adverse events: rows of CTCAE_types
#' @return one row per event, see the top of this file
build_AE_CTCAE = function(lab, types = CTCAE_types){
  if(!exists('NPU.ALAT')){load_npu_common()}
  events = list()
  for(i in seq_len(nrow(types))){
    fn = types$fn[i]; col = types$col[i]; label = types$event_type[i]
    graded = match.fun(fn)(lab) %>%
      transmute(patientid, date_event = as.Date(samplingdate), value_event = .data[[col]], type_event = label) %>%
      distinct()
    events[[label]] = summarize_AE_events(graded)
    cat(fn, ':', nrow(events[[label]]), 'events\n')
  }
  bind_rows(events) %>%
    mutate(end_date = as.Date(end_date), start_date = as.Date(start_date),
           duration_days = as.numeric(duration_days),
           event_value = as.numeric(event_value), max_value = as.numeric(max_value)) %>%
    arrange(patientid, start_date, event_type)
}

#### Run ####
# Loads the laboratory measurements of the patients with a first malignant diagnosis (as the old script did)
# and builds the CTCAE events. The dalycare package must be loaded first. Nothing runs when this file is sourced.
run_AE_CTCAE = function(){
  load_npu_common()
  ids = view_definition_first_diagnosis_malignant()$patientid
  load_dataset('all_laboratory_measurements', ids)
  lab = all_laboratory_measurements_subset %>%
    select(patientid, samplingdate, analysiscode, c_value, referenceinterval_lowerlimit, referenceinterval_upperlimit)
  rm(all_laboratory_measurements_subset)
  build_AE_CTCAE(lab)
}
