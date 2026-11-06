# dalycare_package_v2

R package for working with the DALYCARE data release on NGC. It loads the released tables, holds lookup tables and helper functions, and documents the curated tables.

## Using the package

```r
source('/ngc/projects2/dalyca_r/data/dalycare_r_package/load_dalycare_package.R')  # current release
use_version('20260901')                  # optional: lock code and data to a date
load_dataset('all_patients')             # load a table
load_dataset('all_patients', value = ids, column = 'patientid')   # filtered, gives all_patients_subset
table_info()                             # list documented tables
table_info('all_patients')               # purpose, sources, one row, columns, caveats
```

## Joining tables

Most released tables have a `patientid` column. Some tables (SDS and SP) do not: they are
linked to a patient through a key in a parent table. The links are listed below.

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
| `SP_Journalnotater_Del2` | `note_id` | `SP_Journalnotater_Del1` | `note_id` | SP clinical notes |

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
- SP: none exists. Use `table_info()` for the documented tables, or `names()` on a loaded table to see its columns.

## Contents

| Path | Content |
|---|---|
| `load_dalycare_package.R`, `load_functions.R`, `help_info.R` | Entry point, loading of functions and lookup tables, help text |
| `functions/` | Analysis and helper functions (`load_dataset()`, comorbidity indices, prognostic indices, CTCAE, plots, ...) |
| `clean_SUNDK/`, `clean_SDS/` | Cleaning functions for the SUNDK registries and the pathology data |
| `core_scripts/` | Logic for the derived tables built by `dalycare_core` (comorbidity and adverse events, all_IPI, ...) |
| `table_info/` | One YAML file per documented table, read by `table_info()` |
| `docs/` | Longer documentation of derived tables and the help file for the package |
| `tests/test_package.R` | For package developers only: tests to run on the development version before release |
| `release_package.R` | For package developers only: promotes `development/` to `current/` and archives the previous release |

The pipeline that produces the import tables is `dalycare_pipeline`; the derived (core) tables are built by `dalycare_core`.

## Release (for package developers only)

This section is only relevant if you maintain the package. Users load the package with `load_dalycare_package.R` and never run the tests or the release.

Run `tests/test_package.R` on the development version, then `release_package()` to promote it to `current/` (the previous `current/` is archived under `old/<date>/`).
