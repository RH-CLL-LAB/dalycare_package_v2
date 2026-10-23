# core_scripts

Scripts that build new tables for CORE (as opposed to `functions/`, which holds reusable functions). Each script should have a matching description in `table_info/` and, where useful, in `docs/`.

| Script | Table | Documentation |
|---|---|---|
| `all_IPI.R` | `all_IPI` | `docs/all_IPI.md` |
| `AE_infections.R` | `all_comorbidity_and_ae` (infection events) | `docs/all_comorbidity_and_ae.md` |
| `AE_CTCAE.R` | `all_comorbidity_and_ae` (CTCAE lab adverse events) | `docs/all_comorbidity_and_ae.md` |
| `AE_AKI.R` | `all_comorbidity_and_ae` (acute kidney injury) | `docs/all_comorbidity_and_ae.md` |
| `AE_T2D.R` | `all_comorbidity_and_ae` (type 2 diabetes onset) | `docs/all_comorbidity_and_ae.md` |
| `AE_hypertension.R` | `all_comorbidity_and_ae` (hypertension) | `docs/all_comorbidity_and_ae.md` |

## Structure of a core script

The logic of a table lives in a function (for example `build_all_IPI()`) that only takes data in and returns the table: no paths, no loading and no writing, so the code can be read, copied into your own script, or called from a pipeline. `run_*()` functions load the data and return the table. Nothing runs when a script is sourced, and saving to `core/` is done by the core pipeline.
