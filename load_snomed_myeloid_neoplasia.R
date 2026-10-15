## This script defines a set of SNOMED codes as myeloid neoplasia and gathers codes into larger groups
## Author: christian brieghel
## Date: 26/11-25


#### Myelodysplastic neoplasm (MDS) ####

# Myelodysplasia with single lineage dysplasia

SNOMED.MDS_SLD = 'M99803'

# Myelodysplasia with ringsideroblasts and single lineage dysplasia

SNOMED.MDS_RS_SLD = 'M99823'

# Myelodysplasia with excess blasts

SNOMED.MDS_EB = 'M99833'

# Myelodysplasia with multilineage dysplasia

SNOMED.MDS_MLD ='M99853'

# Myelodysplasia with del5q

SNOMED.MDS_del5q = 'M99863'

# Myelodysplasia UNS

SNOMED.MDS_UNS = 'M99893'

# Myelodysplasia with multilineage dysplasia and ringsideroblasts

SNOMED.MDS_RS_MLD = 'M99933'

##### Gather MDS #####

SNOMED.MDS = c(SNOMED.MDS_del5q, SNOMED.MDS_EB, SNOMED.MDS_SLD, SNOMED.MDS_MLD, SNOMED.MDS_RS_SLD, SNOMED.MDS_RS_MLD, SNOMED.MDS_UNS)
# SNOMED.MDS %>%  length()

#### CMML ####
# Chronic myelomonocytic leukemia

SNOMED.CMML = 'M99453'

#### Acute myeloid leukemia ####

SNOMED.AML_UNS = 'M98613'

# Acute myeloid leukemia with t(6;9); DEK-NUP214

SNOMED.AML_t6_9 = 'M98653'

# Acute myeloid leukemia with t(3;3) or GATA2

SNOMED.AML_t3_3 = 'M98693'

# Acute myeloid leukemia with germline CEBPA mutation

SNOMED.AML_CEBPA = 'M986A3'

# Acute myeloid leukemia with NPM1 mutation

SNOMED.AML_NPM1 = 'M986H3'

# Acute myeloid leukemia with biallelic CEPBA mutation

SNOMED.AML_CEPBA_biallelic = 'M986I3'

# Acute myeloid leukemia with inv(16) or t(16;16)

SNOMED.AML_inv16 = 'M98713'

# Acute myeloid leukemia, minimally differentiated

SNOMED.AML_M0  = 'M98723'

# Acute myeloid leukemia without maturation

SNOMED.AML_M1 = 'M98733'

# Acute myeloid leukemia with maturation

SNOMED.AML_M2 = 'M98743'

# Acute myeloid leukemia with MDS related changes

SNOMED.AML_MRC = 'M98953'


# terapi relateret AML tAML 

SNOMED.tAML = 'M99203'

# Acute myeloid leukemia with t(8;21); RUNX1-RUNX1T1

SNOMED.AML_t8_21 = 'M98963'

# Acute myeloid leukemia with t(9;11); KMT2A-MLLT3

SNOMED.AML_t9_11 = 'M98973'

# Acute myeloid leukemia with t(1;22); RBM15-MKL1

SNOMED.AML_t1_22 = 'M99113'

# Myeloid sarcoma

SNOMED.Myeloid_sarcoma  = 'M99303'

# Acute megakaryoblastic leukemia

SNOMED.AML_M7 ='M99103'

# Acute monoblastic/monocytic leukemia

SNOMED.AML_M5 = 'M98913'

# Acute  myelomonocytic leukemia

SNOMED.AML_M4 = 'M98673'

# Acute promyelocytic leukaemia with t(15;17); PML-RARa

SNOMED.AML_M3 = 'M98663'

# akut erytroid leukæmi
SNOMED.AML_M6 = 'M98403'

##### Gather AML #####
SNOMED.AML  = c(SNOMED.AML_M0, SNOMED.AML_M1, SNOMED.AML_M2, SNOMED.AML_M3, SNOMED.AML_M4, SNOMED.AML_M5, SNOMED.AML_M6, SNOMED.AML_M7,  
                SNOMED.AML_CEBPA, SNOMED.AML_CEPBA_biallelic, SNOMED.AML_NPM1, SNOMED.AML_MRC,
                SNOMED.AML_inv16, SNOMED.AML_t1_22, SNOMED.AML_t3_3, SNOMED.AML_t6_9, SNOMED.AML_t8_21, SNOMED.AML_t9_11,
                SNOMED.AML_UNS, SNOMED.tAML)

#### Essentiel trombocytosis ####

# Essentiel trombocytosis

SNOMED.ET  = 'M99623'

#### Polycythemia vera ####

SNOMED.PV = 'M99503'

#### Myelofibrosis ####

# Panmyeloid proliferation with myelofibrosis

SNOMED.PMF_panmyeloid = 'M99313'

# Myelofibrosis

SNOMED.PMF_UNS = 'M99613'

SNOMED.PMF = c(SNOMED.PMF_panmyeloid, SNOMED.PMF_UNS)

##### Gather myeloproliferative neoplasms (MPN) #####
SNOMED.MPN = c(SNOMED.ET, SNOMED.PV, SNOMED.PMF)

#### CML ####

# Chronic myeloid leukemia BCR-ABL1 positiv

SNOMED.CML_Ph_pos = 'M98753'

# Chronic myeloid leukemia BCR-ABL1 negative

SNOMED.CML_Ph_neg = 'M98763'

SNOMED.CML = c(SNOMED.CML_Ph_pos, SNOMED.CML_Ph_neg)

#### Gather myeloid neoplasms ####

SNOMED.Myeloid_neoplasms = c(SNOMED.MDS, SNOMED.AML, SNOMED.CML, SNOMED.CMML, SNOMED.MPN) %>% sort

cat('\nLoads SKS myeloid neoplasia codes in a set of vectors starting with "SKS."')
