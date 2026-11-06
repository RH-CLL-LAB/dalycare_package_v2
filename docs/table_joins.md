# Joining tables without a patient id

Most released tables have a `patientid` column. Some SDS tables do not: they are
linked to a patient through a key in a parent table. This page lists those links.

| Table (no patientid) | Key in table | Parent (has patientid) | Key in parent | Notes |
|---|---|---|---|---|
| `SDS_diagnoser` | `dw_ek_kontakt` | `SDS_kontakter` | `dw_ek_kontakt` | LPR3 diagnoses |
| `SDS_resultater` | `dw_ek_kontakt` | `SDS_kontakter` | `dw_ek_kontakt` | LPR3 |
| `SDS_forloebsmarkoerer` | `dw_ek_forloeb` | `SDS_kontakter` | `dw_ek_forloeb` | LPR3 |
| `SDS_procedurer_andre` | `dw_ek_forloeb` | `SDS_forloeb` | `dw_ek_forloeb` | LPR3 |
| `SDS_procedurer_kirurgi` | `dw_ek_forloeb` | `SDS_forloeb` | `dw_ek_forloeb` | LPR3; `CLL_CI()` joins it through `dw_ek_kontakt` to `SDS_kontakter` |
| `SDS_dimpatologiskdiagnose` | `dw_ek_rekvisition` | `SDS_fctrekvisition` | `dw_ek_rekvisition` | pathology |
| `SDS_fctpatologiskprocedure` | `dw_ek_rekvisition` | `SDS_fctrekvisition` | `dw_ek_rekvisition` | pathology |
| `SDS_t_diag` | `v_recnum` | `SDS_t_adm` | `k_recnum` | LPR2 diagnoses |
| `SDS_t_sksopr` | `v_recnum` | `SDS_t_adm` | `k_recnum` | LPR2 procedures |
| `SDS_t_sksube` | `v_recnum` | `SDS_t_adm` | `k_recnum` | LPR2 examinations |
| `SDS_t_udtilsgh` | `v_recnum` | `SDS_t_adm` | `k_recnum` | LPR2 |
| `SDS_t_psyk_diag` | `v_recnum` | `SDS_t_psyk_adm` | `k_recnum` | LPR2 psychiatry |
| `SDS_t_psyk_sksopr` | `v_recnum` | `SDS_t_psyk_adm` | `k_recnum` | LPR2 psychiatry |
| `SDS_t_psyk_sksube` | `v_recnum` | `SDS_t_psyk_adm` | `k_recnum` | LPR2 psychiatry |
| `SDS_t_psyk_udtilsgh` | `v_recnum` | `SDS_t_psyk_adm` | `k_recnum` | LPR2 psychiatry |

Example (LPR2 diagnoses with the patient id):

```r
load_dataset('SDS_t_adm')
load_dataset('SDS_t_diag')
diag_with_patient = SDS_t_diag %>%
  inner_join(SDS_t_adm %>% select(patientid, k_recnum), by = c('v_recnum' = 'k_recnum'))
```

Some tables are reference or lookup tables with no patient data (for example
`SDS_koder`, `SDS_laegemiddel`, `SDS_dimnpu`).

## Data dictionaries

- SDS: <https://www.esundhed.dk/Dokumentation?rid=5&tid=7>
- SUNDK: <https://www.db-dokumentation-sundk.dk/Public/Default.aspx?ReturnUrl=%2f&db2=-9>
- SP: no data dictionary exists. List a table's columns with `names(load_dataset('table_name'))`.
