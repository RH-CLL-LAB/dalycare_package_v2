### This script defines SKS antineoplastic treatment from SKS B_CODES
## Sys.Date() "2025-08-18"
## author: christian brieghel
# load_dataset('CODES_B') # used search words:
# antivira, antimyk, antibio, antipro, infektion

#Antibiotics
SKS.antibiotics_local_ophtalmologic = 'BCHY0'
SKS.antibiotics_local_gyneocologic = 'BJHG0'
SKS.antibiotics_local_dermatologic = 'BNHG'
SKS.antibiotics_local = 'BNHG0'
SKS.antibiotics_systemic = 'BPH' #Systemisk antibiotikabehandling af infektionssygdomme
SKS.antibiotics_penicillins = 'BPHB'
SKS.antibiotics_others = 'BPHY'

SKS.antibiotics = c(SKS.antibiotics_local_ophtalmologic, SKS.antibiotics_local_gyneocologic, SKS.antibiotics_local_dermatologic, SKS.antibiotics_local,
                   SKS.antibiotics_systemic, SKS.antibiotics_penicillins, SKS.antibiotics_others, SKS.antibiotics_others)

#Antivirals
SKS.antivirals_vaginal = 'BJHG01'
SKS.antivirals_local = 'BNHG01'
SKS.antivirals_other = 'BPHM'
SKS.antivirals_other_NOS = 'BPHM9'
SKS.antivirals_direct_active = 'BPHM92'

SKS.antivirals  = c(SKS.antivirals_vaginal,SKS.antivirals_local, SKS.antivirals_other, SKS.antivirals_other_NOS, SKS.antivirals_direct_active)

#Antimycotics/antifungals
SKS.antimycotics_vaginal = 'BJHG02'
SKS.antimycotics_specific = 'BNHA'
SKS.antimycotics_local = 'BNHA0'
SKS.antimycotics_systemic = c('BPHG', 'BPHG0') 
SKS.antimycotics = c(SKS.antimycotics_vaginal, SKS.antimycotics_specific, SKS.antimycotics_local, SKS.antimycotics_systemic)

# No antihelmintics identified

#Other infectives
SKS.antibiotics_local_eye = 'BCHY00' #Lokalbehandling af bakteriel infektion i øje
SKS.antivirals_local_eye = 'BCHY01' #Lokalbehandling af virusinfektion i øje
SKS.antinfective_combined_w_steroid = 'BNHJ2' #Lokalbehandling med glukortikoid i kombination med antiinfektivum
SKS.antinfective_medication_and_immunization = 'BP' #Medikamentel behandling af infektionssygdomme, immunisering m.m.

SKS.antiinfectives = c(SKS.antibiotics_local_eye, SKS.antivirals_local_eye, SKS.antinfective_combined_w_steroid, SKS.antinfective_medication_and_immunization)

#### All antimircobials ####
SKS.antimicrobials = c(SKS.antibiotics, SKS.antivirals, SKS.antimycotics, SKS.antiinfectives)
SKS.antimicrobials_systemic = c(SKS.antibiotics_systemic, SKS.antimycotics_systemic, SKS.antimycotics, SKS.antiinfectives)

# load_dataset('SDS_t_sksube', SKS.antimicrobials, 'c_opr')
