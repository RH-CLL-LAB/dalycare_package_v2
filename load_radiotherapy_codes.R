### Identifies radio therapy treatment codes
# author: christian brieghel
# date: 06/12-2025

RT_excluded = c('BWGA', 'D', 'AFB', 'ZZ0153A4')
RT_disease = c(
  # 'AFB06F3', # CML and MPN with RT
  #              'AFB05F3', # AML and MDS wtih RT
  'AFB04F3', # MM with RT
  'AFB03F3') # Lymphoma and CLL with RT

# Codes_all %>% filter_str_detect(code, paste0('^', RT)) # doesn't work

SKS.radiotherapy = Codes_all %>% 
  filter(type == 'sks') %>% 
  filter_str_detect(text, 'strålebehandling')  %>% 
  filter_str_detect(code, paste0('^', RT_exclude), negate = T) %>% 
  bind_rows(Codes_all %>% filter(code %in% RT_disease)) %>% 
  pull(code) %>% 
  sort
