# Functions that build the derived tables of CORE

The tables that `dalycare_core` builds are made by ordinary package functions in `functions/`. They are loaded with the package, so researchers can run them directly, and the core pipeline calls the same functions. There is one function per table or part of a table. Each has a matching description in `table_info/` and, where useful, in `docs/`.

| Function (file in `functions/`) | Table | Documentation |
|---|---|---|
| `all_IPI()` (`all_IPI.R`) | `all_IPI` | `docs/all_IPI.md` |
| `AE_infections()` (`AE_infections.R`) | `all_comorbidity_and_ae` (infection events) | `docs/all_comorbidity_and_ae.md` |
| `AE_CTCAE()` (`AE_CTCAE.R`) | `all_comorbidity_and_ae` (CTCAE lab adverse events) | `docs/all_comorbidity_and_ae.md` |
| `AE_AKI()` and `AE_AKI_all()` (`AE_AKI.R`) | `all_comorbidity_and_ae` (acute kidney injury, one row per episode) and `all_AKI` (one row per creatinine measurement) | `docs/all_comorbidity_and_ae.md` |
| `AE_T2D()` (`AE_T2D.R`) | `all_comorbidity_and_ae` (type 2 diabetes onset) | `docs/all_comorbidity_and_ae.md` |
| `AE_hypertension()` (`AE_hypertension.R`) | `all_comorbidity_and_ae` (hypertension) | `docs/all_comorbidity_and_ae.md` |

## Structure

The logic of a table lives in a `build_*()` function (for example `build_all_IPI()`) that only takes data in and returns the table: no paths, no loading and no writing, so the code can be read, copied into your own script, or called from a pipeline. The function with the name of the table (for example `all_IPI()`) loads the data and returns the table. Nothing is saved by these functions: saving to `core/` is done by the core pipeline.
