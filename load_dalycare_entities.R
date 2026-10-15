## Defines DALY-CARE entities
print_color('Loads DALY-CARE entities in a set of vectors starting with "ICD10."\n', 'black')
ICD10.HL = c('DC811', 'DC812', 'DC813', 'DC814', 'DC817', 'DC819') # excluding NLP-HL  == 'DC810',
ICD10.FL = c('DC829', 'DC820', 'DC821', 'DC822', 'DC827')
ICD10.SLL = 'DC830'
ICD10.LPL = c('DC830B', 'DC880')
ICD10.NMZL = 'DC830C'
ICD10.SMZL = 'DC830D'
ICD10.EMZL = c('DC884', 'DC884A', 'DC884B', 'DC884C')
ICD10.MZL = c(ICD10.NMZL, ICD10.SMZL, ICD10.EMZL)
ICD10.MCL = 'DC831'
ICD10.DLBCL = 'DC833'
ICD10.LBL = c('DC835', 'DC825A', 'DC835B')
ICD10.BL = 'DC837'
ICD10.PTCL = 'DC844'
ICD10.BCL_UNS = 'DC851'
ICD10.Lymphoma_UNS = c('DC857', 'DC859')
ICD10.AITL = 'DC865'
ICD10.ALCL = c('DC845A', 'DC846', 'DC846A', 'DC847')
ICD10.MM = 'DC900'
ICD10.PCL = 'DC901'
ICD10.SolM = c('DC902', 'DC902A', 'DC903')

ICD10.CLL = 'DC911'
ICD10.RT = 'DC911B'
ICD10.HCL = 'DC914'
ICD10.Leukemia_UNS = c('DC917', 'DC919')
ICD10.MGUS = c('DD472', 'DD472A', 'DD472B')
ICD10.MBL = 'DD479B'	
ICD10.UNS = c(ICD10.BCL_UNS, ICD10.Lymphoma_UNS, ICD10.Leukemia_UNS)

#### Amyloidosis extended 2026-09-23: research lead approved 11 additional
#### ICD-10 codes beyond the original 'DE858A' (AL-amyloidose) that was part
#### of the published method paper (see help_info.R and ICD10.DALYCARE
#### below). Mirrored in the Python pipeline's SPECIFIC_CODES (diagnoses.py).
ICD10.AMYLOIDOSIS = c('DE859',   # Amyloidose UNS
                       'DI680',  # Cerebral amyloidangiopati
                       'DE854',  # Lokaliseret amyloidose
                       'DE858',  # Anden amyloidose
                       'DE851A', # Amyloidose med polyneuropati
                       'DE853',  # Sekundaer systemisk amyloidose
                       'DE858A', # AL-amyloidose -- original method paper code
                       'DL990',  # Amyloidosis cutis UNS
                       'DL990A', # Lichen-amyloidose
                       'DL990B', # Makulaer amyloidose
                       'DL990C', # Nodulaer amyloidose
                       'DM144')  # Artropati ved amyloidose

ICD10.BCL = c('DC82', 'DC83', 
              'DC850','DC851', 'DC852', 'DC853', 'DC854', 'DC855', 'DC856', 'DC858', # excluding 'DC857', 'DC859', 
              'DC880', 'DC884')
ICD10.TCL = c(ICD10.AITL, ICD10.PTCL, ICD10.ALCL)
ICD10.PCD = c(ICD10.MM, ICD10.PCL, ICD10.SolM)
ICD10.DALYCARE = c('DC81', 'DC82', 'DC83', 'DC84', 'DC85', 'DC86', 'DC87', 'DC88', 'DC89', 'DC90',
                   'DC911', 'DC912', 'DC913', 'DC914', 'DC915', 'DC916', 'DC917', 'DC918', 'DC919',
                   'DC951', 'DC957', 'DC959',
                   'DD472', 'DD479B', ICD10.AMYLOIDOSIS) # amyloidosis extended 2026-09-23, see ICD10.AMYLOIDOSIS above
