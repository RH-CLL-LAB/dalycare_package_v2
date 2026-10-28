#' @title
#' helpers
#' @author
#' christian brieghel
#' @description  
#' functions that dplyr::mutate (base::transform) data

parse_sundk_date = function(x){
  #' @description
  #' robustly parses SUNDK date columns whose raw format/type has changed
  #' silently between deliveries (e.g. character dd-mm-yyyy in one delivery,
  #' ISO character or Date/POSIXct in another). Tries, per value: pass through
  #' if already Date/POSIXct, else ISO (as_date), else Danish dmy.
  #' @examples
  #' parse_sundk_date(c("2026-09-17", "17-09-2026", NA))
  #' parse_sundk_date(as_date("2026-09-17"))

  if (inherits(x, 'Date')) return(x)
  if (inherits(x, 'POSIXt')) return(as_date(x))

  parsed = suppressWarnings(as_date(x)) # try ISO first
  still_na = is.na(parsed) & !is.na(x)
  if (any(still_na)) {
    parsed[still_na] = suppressWarnings(dmy(x[still_na]))
  }
  parsed
}

go_live = function(){
  #' dataset defines SP go-live dates at Herlev Hospital (HGH), Rosdkilde Hospital (ROS), 
  #' Rigshopitalet (RH)
  #' @examples
  #' go_live()
  #' load_dataset('SUNDK_CLL')
  #' CLL_clean = SUNDK_CLL %>% clean_SUNDK_CLL() %>% left_join(go_live())
  #' CLL_clean$date_golive
  
  tibble(hospital_id = c('HER', 'RH', 'ROS'),
         date_golive = as_date(c("2016-05-21", "2016-11-05", "2017-11-25")))
}


clear_ram = function(){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

print_data = function(data){
  for(i in 1:length(names(data))){
    print_color(paste0('\n', names(data)[i], ': '), 'black') 
    print_color(paste0(data[,i][!is.na(data[,i])], collapse = ', '), 'blue')
  }
}

#### transformers ####
diff_hours = function(date_time_start, date_time_end) {as.numeric(difftime({{date_time_end}}, {{date_time_start}}, units = 'hours'))}
diff_days = function(date_start, date_end) {as.numeric(difftime({{date_end}},{{date_start}}, units = 'days'))}
diff_years = function(date_start, date_end) {as.numeric(difftime({{date_end}},{{date_start}}, units = 'days'))/365.25}

diff_hours_lag = function(date) {as.numeric(difftime({{date}}, lag({{date}}), units = 'hours'))}
diff_days_lag = function(date) {as.numeric(difftime({{date}}, lag({{date}}), units = 'days'))}
diff_years_lag = function(date) {as.numeric(difftime({{date}}, lag({{date}}), units = 'days'))/365.25}

str_between = function(string, from_pattern1, to_pattern2){
  str_split_fixed(str_split_fixed({{string}}, {{to_pattern2}}, 2)[,1], {{from_pattern1}}, 2)[,2]
}

change_month <- function(x) {
  #' Transforms 3-letter months (x) codes to numeric months
  #' @example 
  #' data$date %>% head # 2010JAN02 
  #' data %>% mutate(date = change_month(date)) # 2010-01-02
  
  x <- gsub('JAN', '-01-', x)
  x <- gsub('FEB', '-02-', x)
  x <- gsub('MAR', '-03-', x)
  x <- gsub('APR', '-04-', x)
  x <- gsub('MAY', '-05-', x)
  x <- gsub('JUN', '-06-', x)
  x <- gsub('JUL', '-07-', x)
  x <- gsub('AUG', '-08-', x)
  x <- gsub('SEP', '-09-', x)
  x <- gsub('OCT', '-10-', x)
  x <- gsub('NOV', '-11-', x)
  x <- gsub('DEC', '-12-', x)}

AA = function(aa){
  #' Transforms 3-letter amino acid (aa) codes to 1-letter codes
  #' @example 
  #' mutate(aa1_code = AA(aa3_code))
  
  aa = gsub('Ala', 'A', aa)
  aa = gsub('Arg', 'R', aa)
  aa = gsub('Asn', 'N', aa)
  aa = gsub('Asp', 'D', aa)
  aa = gsub('Cys', 'C', aa)
  aa = gsub('Glu', 'E', aa)
  aa = gsub('Gln', 'Q', aa)
  aa = gsub('Gly', 'G', aa)
  aa = gsub('His', 'H', aa)
  aa = gsub('Ile', 'I', aa)
  aa = gsub('Leu', 'L', aa)
  aa = gsub('Lys', 'K', aa)
  aa = gsub('Met', 'M', aa)
  aa = gsub('Phe', 'F', aa)
  aa = gsub('Pro', 'P', aa)
  aa = gsub('Ser', 'S', aa)
  aa = gsub('Thr', 'T', aa)
  aa = gsub('Trp', 'W', aa)
  aa = gsub('Tyr', 'Y', aa)
  aa = gsub('Val', 'V', aa)
}


is_odd <- function(x) x %% 2 != 0

cut_year = function(time, by = 0.25){
  cut({{time}}, 
      seq(ceiling(min({{time}})), floor(max({{time}})), by = {{by}}),
      labels = c(seq(ceiling(min({{time}})), floor(max({{time}})), by = {{by}}))[-length(seq(ceiling(min({{time}})), floor(max({{time}})), by = {{by}}))]*12) 
}

#### filters ####
str_detect_subset = function(x, text){
  x[x %>% str_detect(text) %>% which(.==T)]
}

filter_str_detect = function(data, string, pattern, negate = FALSE){
  data %>% 
    filter(str_detect({{string}}, str_flatten({{pattern}}, '|'), negate = negate))
}

filter_sentence = function(data, string = notat_text, pattern, extra_sentence = 0){
  data %>% 
    separate_rows({{string}}, sep = '\\.\\s*') %>%
    slice({
      tmp <- grep(str_flatten({{pattern}}, '|'), {{string}}, ignore.case = TRUE)
      sort(unique(c(tmp-{{extra_sentence}}, tmp, tmp + {{extra_sentence}})))
    })
}

#### counters ####
head2 = function(data){data %>% head(2)}

n_patients = function(data){data %>% pull(patientid) %>% n_distinct()}

nrow_npatients = function(data){
  if(nrow(data) == n_distinct(data$patientid)){
    print_color('Wide format:', 'black')
  }else{  
    print_color('\nLong format:\n', 'black')
  }
  print_color(paste('\n', data %>% nrow(), 'rows'), 'black')
  print_color(paste('\n', data %>% pull(patientid) %>% n_distinct(), 'patients\n'), 'black')}
