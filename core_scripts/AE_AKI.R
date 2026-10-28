## AE_AKI
# Acute kidney injury (AKI) events for the CORE table all_comorbidity_and_ae (see table_info/all_comorbidity_and_ae.yaml)
# Based on AKI_function_toNGC_v2.R (Tereza Fait Kadlec). Note that this is not the same definition as functions/AE_AKI.R.
#
# AKI definition (KDIGO), compared with the creatinine measurements before each measurement:
#   an absolute increase of 26.5 umol/L within 48 hours,
#   or a 1.5x increase compared to the minimum value over the past 7 days,
#   or a 1.5x increase compared to the mean value over the past year (the 7 most recent days excluded),
#   or a 1.5x increase from the most recent value.
# NB: the last criterion is not part of the official definition. It was added because the data might not have
# enough measurements to identify AKI with the 48 hour or 7 day windows. Be transparent about this when used in
# publications.
#
# The logic lives in build_AE_AKI_measurements() (one row per creatinine measurement, with baselines, AKI
# criteria, stage and episode) and build_AE_AKI() (one event per AKI episode). run_AE_AKI_all() loads the data
# and calls them once: the events go to all_comorbidity_and_ae and the measurements to the table all_AKI.
# Nothing runs when this file is sourced; use
#   AKI = run_AE_AKI_all()   # AKI$events and AKI$all_AKI
# The table is not saved here: writing to core/ is done by the core pipeline.
#
# Output has the shape of all_comorbidity_and_ae (event_type 'ae_AKI'):
#   patientid, event_id, start_date, end_date, duration_days, event_value, max_value, event_type
# start_date is the first measurement of the episode that fulfils the definition, event_value is the stage at
# that measurement and max_value the highest stage within 7 days of it. end_date and duration_days are empty.

#### Measurements ####
#' New baselines are made at every new creatinine measurement, so that the creatinine level is always compared
#' with a baseline within 48 hours, 7 days or a year before the measurement.
#' @param creatinine patientid, samplingdate, samplingtime and c_value (umol/L) of creatinine (NPU.KREA).
#'   A missing sampling time is set to 00:00:00; measurements without a valid date are left out.
#' @return one row per measurement with baselines, aki_def, aki_stage, aki_episode and akd (acute kidney
#'   disease: creatinine still elevated 7-90 days after the onset)
build_AE_AKI_measurements = function(creatinine){
  AKI = creatinine %>%
    select(patientid, samplingdate, samplingtime, c_value) %>%
    distinct() %>%
    filter(!is.na(c_value)) %>%
    mutate(samplingtime = as.character(samplingtime),
           samplingtime = if_else(is.na(samplingtime) | samplingtime == '', '00:00:00', samplingtime),
           datetime = ymd_hms(paste(as.character(as.Date(samplingdate)), samplingtime), quiet = TRUE),
           samplingtime = hms::as_hms(samplingtime), # same type as samplingtime in all_laboratory_measurements
           c_value = as.numeric(c_value)) %>%
    filter(!is.na(datetime)) %>%
    arrange(patientid, datetime)

  data.table::setDT(AKI)
  data.table::setorder(AKI, patientid, datetime)

  # restrict rolling windows by time
  AKI[, base_recent := data.table::shift(c_value, 1), by = patientid]
  AKI[, base_min_48h := sapply(seq_len(.N), function(i) {
    min(c_value[datetime >= datetime[i] - hours(48) & datetime < datetime[i]], na.rm = TRUE)
  }), by = patientid]
  AKI[, base_min_7d := sapply(seq_len(.N), function(i) {
    min(c_value[datetime >= datetime[i] - days(7) & datetime < datetime[i]], na.rm = TRUE)
  }), by = patientid]
  AKI[, base_mean_7d365 := sapply(seq_len(.N), function(i) {
    vals = c_value[datetime >= datetime[i] - days(365) & datetime[i] - days(7) <= datetime]
    if (length(vals) > 0) mean(vals, na.rm = TRUE) else NA_real_
  }), by = patientid]

  AKI_baselines = as.data.frame(AKI)

  # detect AKI
  AKI_detected = AKI_baselines %>%
    mutate(aki_def_48h = c_value >= base_min_48h + 26.5,
           aki_def_recent = c_value >= base_recent * 1.5,
           aki_def_7d = c_value >= base_min_7d * 1.5,
           aki_def_year = c_value >= base_mean_7d365 * 1.5,
           aki_def = aki_def_48h | aki_def_7d | aki_def_recent | aki_def_year) # true if at least one is true

  # AKI stage
  AKI_stages = AKI_detected %>%
    mutate(aki_stage = case_when(
      c_value >= 353.6 ~ 3,
      c_value / base_min_7d >= 3 ~ 3,
      c_value / base_min_7d >= 2 ~ 2,
      c_value / base_min_7d >= 1.5 ~ 1,
      c_value / base_mean_7d365 >= 3 ~ 3,
      c_value / base_mean_7d365 >= 2 ~ 2,
      c_value / base_mean_7d365 >= 1.5 ~ 1,
      c_value / base_recent >= 3 ~ 3,
      c_value / base_recent >= 2 ~ 2,
      c_value / base_recent >= 1.5 ~ 1,
      c_value >= base_min_48h + 26.5 ~ 1,
      TRUE ~ NA_real_))

  # AKI episodes: the first time AKI appears after a non-AKI state
  AKI_episodes = AKI_stages %>%
    arrange(patientid, datetime) %>%
    group_by(patientid) %>%
    mutate(aki_def = replace_na(aki_def, FALSE),
           new_aki = aki_def & !lag(aki_def, default = FALSE),
           aki_episode = cumsum(new_aki),
           aki_start_time = if_else(new_aki == TRUE, datetime, as.POSIXct(NA))) %>%
    ungroup()

  # AKD: creatinine still elevated 7-90 days after the onset
  AKI_episodes %>%
    group_by(patientid, aki_episode) %>%
    mutate(akd = if_else(aki_def &
                           datetime >= first(aki_start_time) + days(7) &
                           datetime <= first(aki_start_time) + days(90), 1, 0)) %>%
    ungroup()
}

#### all_AKI ####
#' The table all_AKI: one row per creatinine measurement with the baselines, the criteria, the stage and the
#' episode. An empty window gives a missing baseline (not Inf).
#' @param AKI_measurements the result of build_AE_AKI_measurements()
format_all_AKI = function(AKI_measurements){
  AKI_measurements %>%
    transmute(patientid, samplingdate = as.Date(samplingdate), samplingtime, datetime, c_value,
              base_recent,
              base_min_48h = if_else(is.infinite(base_min_48h), NA_real_, base_min_48h),
              base_min_7d = if_else(is.infinite(base_min_7d), NA_real_, base_min_7d),
              base_mean_7d365,
              aki_def_48h, aki_def_7d, aki_def_year, aki_def_recent, aki_def,
              aki_stage, aki_episode, new_aki, aki_start_time, akd) %>%
    arrange(patientid, datetime)
}

#### Events ####
#' One event per AKI episode.
#' @param AKI_measurements the result of build_AE_AKI_measurements()
build_AE_AKI = function(AKI_measurements){
  AKI_measurements %>%
    filter(aki_episode > 0) %>%
    group_by(patientid, aki_episode) %>%
    summarise(onset = min(datetime[new_aki]),
              start_date = as.Date(min(datetime[new_aki])),
              event_value = aki_stage[new_aki][1],
              max_value = max(aki_stage[aki_def & datetime <= min(datetime[new_aki]) + days(7)], na.rm = TRUE),
              .groups = 'drop') %>%
    transmute(patientid,
              event_id = aki_episode,
              start_date,
              end_date = as.Date(NA),
              duration_days = as.numeric(NA),
              event_value = as.numeric(event_value),
              max_value = as.numeric(ifelse(is.infinite(max_value), NA, max_value)),
              event_type = 'ae_AKI') %>%
    arrange(patientid, start_date)
}

#### Run ####
# Loads the creatinine measurements of all patients in all_patients and builds the AKI measurements and events
# once. The dalycare package must be loaded first. Nothing runs when this file is sourced.
#   AKI = run_AE_AKI_all()
#   AKI$events    the ae_AKI events for all_comorbidity_and_ae (one per episode)
#   AKI$all_AKI   the table all_AKI: one row per creatinine measurement, like the old AE_AKI table
run_AE_AKI_all = function(){
  load_npu_common()
  load_dataset('all_patients')
  load_dataset('all_laboratory_measurements', NPU.KREA, column = 'analysiscode')
  creatinine = all_laboratory_measurements_subset %>%
    filter(patientid %in% all_patients$patientid)
  rm(all_laboratory_measurements_subset)
  measurements = build_AE_AKI_measurements(creatinine)
  list(events = build_AE_AKI(measurements),
       all_AKI = format_all_AKI(measurements))
}

# Only the events (used when the measurements are not needed)
run_AE_AKI = function() run_AE_AKI_all()$events
