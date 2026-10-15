### This script defines ATC antineoplastic treatment codes from Codes_ATC
## Sys.Date() "2025-12-28" 
## author: christian brieghel

#### ATC loop ####
print_color('Loads antineoplastic ATC codes in a set of vectors starting with "ATC."', 'black')

if(!exists('Codes_ATC')){
  load_dataset('Codes_ATC')
}

Codes_antineoplastics = Codes_ATC %>% filter_str_detect(atc, '^L') %>% 
  mutate(class_name = gsub(' ', '_', class_name),
         class_name = gsub("\\(|\\)|\\,|\\.|\\-|\\'", '', class_name)) %>% 
  select(atc, class_name) %>% 
  group_by(class_name) %>% 
  summarise(atc = paste0(atc, collapse = ' ')) %>%  
  ungroup()
for (i in 1:nrow(Codes_antineoplastics)){
  assign( paste0("ATC.", Codes_antineoplastics[i,1]),  unlist(str_split(Codes_antineoplastics[i,2], ' ')))
}

#### Immunotherapies ####
ATC.talquetamab = 'L01FX29'#9/12-25

#### Cell therapies ####

ATC.axicel = c('L01XL03', 'L01XX70')
ATC.ciltacel = 'L01XL05'
ATC.lisocel = 'L01XL08'
ATC.brexucel = 'L01XL06'
ATC.idecel = 'L01XL07'
ATC.tisacel = 'L01XL04'
ATC.CART = c(ATC.axicel, ATC.ciltacel, ATC.lisocel, ATC.brexucel, ATC.idecel, ATC.tisacel)

#### ATC groups #####
ATC.BTKi = Codes_ATC %>% filter_str_detect(class_name, 'brutinib') %>% pull(atc) %>% unique() %>% sort
ATC.immunotherapy = Codes_ATC %>% filter_str_detect(class_name, 'mab$') %>% pull(atc) %>% unique() %>% sort
ATC.antiCD20 = Codes_ATC %>% filter(class_name %in% c('rituximab', 'ofatumumab', 'obinutuzumab', 'Obinutuzumab')) %>% pull(atc) %>% unique() %>% sort
ATC.small_molecules = Codes_ATC %>% filter_str_detect(class_name, 'nib$') %>% pull(atc) %>% unique() %>% sort
ATC.CLL_chemotherapy = Codes_ATC %>% filter(class_name %in% c('chlorambucil', 'fludarabine','bendamustine')) %>% pull(atc) %>% unique() %>% sort
ATC.CLL_immunotherapy = Codes_ATC %>% filter(class_name %in% c('rituximab', 'ofatumumab', 'Obinutuzumab', 'obinutuzumab')) %>% pull(atc) %>% unique() %>% sort
ATC.CLL_targeted = Codes_ATC %>% filter(class_name %in% c('ibrutinib', 'acalabrutinib', 'zanubrutinib', 'venetoclax', 'idelalisib')) %>% pull(atc) %>% unique() %>% sort
ATC.CLL_treatment = c(ATC.CLL_chemotherapy, ATC.CLL_immunotherapy, ATC.CLL_targeted)

# End of script