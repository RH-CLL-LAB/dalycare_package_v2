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

Some SDS tables have no `patientid` and must be joined to a parent table through a key (e.g. `SDS_t_diag` to `SDS_t_adm`). The full list is in [`docs/table_joins.md`](docs/table_joins.md).

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
| `docs/` | Longer documentation of derived tables, table joins (`table_joins.md`) and the help file for the package |
| `tests/test_package.R` | Tests to run on the development version before release |
| `release_package.R` | Promotes `development/` to `current/` and archives the previous release |

The pipeline that produces the import tables is `dalycare_pipeline`; the derived (core) tables are built by `dalycare_core`.

## Release

Run `tests/test_package.R` on the development version, then `release_package()` to promote it to `current/` (the previous `current/` is archived under `old/<date>/`).
