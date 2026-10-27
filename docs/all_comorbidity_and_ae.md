# all_comorbidity_and_ae

Comorbidities and adverse events (AE) as events in one table, built from several parts. Column definitions and caveats are in `table_info/all_comorbidity_and_ae.yaml`. This page describes the parts that are built so far.

## Output

One row per event, with the same columns for every part.

| Column | Content |
|---|---|
| `patientid` | Patient id |
| `event_id` | Counts the events of one patient within one `event_type`, in date order |
| `start_date` | Start of the event |
| `end_date` | End of the event (empty when not defined) |
| `duration_days` | Duration (empty when not defined) |
| `event_value` | Value of the event: 1 for events without grade, otherwise the grade or stage |
| `max_value` | Highest value during the event (empty when not defined) |
| `event_type` | Which part and definition the event comes from |

## Parts built

### Infections (`core_scripts/AE_infections.R`)

`run_AE_infections()` loads the data for all patients in `all_patients` and returns the events of the 5 infection definitions:

| event_type | Definition |
|---|---|
| `BC+IV_AB` | Definition 1: IV antibiotics (at least 24 hours) within 48 hours of a blood culture |
| `IV_AB` | Definition 2: IV antibiotics for at least 24 hours, independent of blood culture |
| `BC_drawn` | Definition 3: blood culture drawn, independent of antibiotics |
| `any_AB_pre_or_adm` | Definition 4: any antibiotics prescribed or administered for at least 24 hours, excluding the prophylactic antimicrobials Aciclovir, Sulfotrim and Valaciclovir |
| `BC_OR_any_AB` | Definition 5: blood culture or any antibiotics (definition 3 or 4) |

Infections of one definition are separated by more than 7 days. The date of an infection is the first administration (or the date of the blood culture). Prescribed medicine is not known to be taken, and the durations of prescriptions are not necessarily correct.

The earlier separate `IVAB_Infection` event was identical to `IV_AB` when built on the same patients and is not a separate event type anymore. In the old SQL table it covered all patients in `SP_AdministreretMedicin`, not only `all_patients`.

Sources: `SP_AlleProvesvar` (blood cultures, via `load_blood_culture_SP()`), `SP_AdministreretMedicin` and `SP_OrdineretMedicin`. The coverage period differs between these sources, so the period is not the same for all definitions.

### CTCAE adverse events (`core_scripts/AE_CTCAE.R`)

`run_AE_CTCAE()` grades the laboratory measurements (`all_laboratory_measurements`) with the CTCAE v5.0 functions in `functions/CTCAE/` and summarises the grades to events with `summarize_AE_events()`. The same 22 types as in the old table: `ae_ALAT_increased`, `ae_ASAT_increased`, `ae_BASP_increased`, `ae_INR_increased`, `ae_anemia`, `ae_bilirubin_increased`, `ae_creatinine_increased`, `ae_eosinophilia`, `ae_haptoglobin_decreased`, `ae_hypercalcemia`, `ae_hyperkalemia`, `ae_hypermagnesemia`, `ae_hypernatremia`, `ae_hypertriglyceridemia`, `ae_hypoalbuminemia`, `ae_hypocalcemia`, `ae_hypoglycemia`, `ae_lymphocyte_count_decreased`, `ae_lymphocyte_count_increased`, `ae_neutrophil_count_decreased`, `ae_platelets_decreased` and `ae_wbc_decreased`. `AE_leukocytosis()` and `AE_hemoglobin_increased()` were not part of the old table and are not included.

An event is a run of the same positive grade. It ends at the first measurement with grade 0 or at a measurement with another positive grade, which starts a new event. An event that is never followed by a lower grade ends at the last measurement of the patient. `event_value` is the grade the event started with, `max_value` the highest grade during the event, and `duration_days` is `end_date - start_date + 1`. Measurements without a grade are left out. `AE_anemia()` and `AE_hypoalbuminemia()` return the grade `'3-4'` for the most severe values, which is counted as grade 3. In the old workflow `'3-4'` became a missing value that the C++ program read as grade 0, so the most severe measurements ended the event and were not counted.

This replaces the C++ program `process_ae_files` and the R script `AE_combined.R`. The R version is the one in `function_summarize_events_v2.R`. It has not been checked against the C++ program for every case, so compare the numbers with the old table before relying on it.

Comparison with the old SQL table (built on older data): the new table has 7-14% more events per type. Most of it is data from 2024-2026, which the old table did not have complete. For ALAT and hyperkalemia the years 2008-2022 are 2-7% higher in the new table. Anemia (and hypoalbuminemia) now include the most severe grade `'3-4'` as grade 3, which the old table did not, so those two types are expected to differ from the old table.

### AKI (`core_scripts/AE_AKI.R`)

`run_AE_AKI_all()` uses the creatinine measurements (NPU.KREA) and the KDIGO-based definition by Tereza Fait Kadlec. A measurement is AKI if creatinine is at least 26.5 umol/L above the minimum of the past 48 hours, or at least 1.5 times the minimum of the past 7 days, the mean of the past year (the 7 most recent days excluded) or the most recent value. The last criterion is not part of the official definition: it was added because the data may not have enough measurements to use the 48 hour and 7 day windows. State this in publications. The criterion is kept (confirmed): AKI in this table therefore deviates from the official KDIGO definition, and the `ae_AKI` events include measurements flagged only by this fourth criterion.

One event (`ae_AKI`) per AKI episode, an episode being the first AKI measurement after a measurement without AKI. `start_date` is the date of that measurement, `event_value` is the stage at that measurement and `max_value` the highest stage within 7 days. `end_date` and `duration_days` are empty. The function `build_AE_AKI_measurements()` returns every creatinine measurement with baselines, criteria, stage, episode and AKD (creatinine still elevated 7-90 days after the onset). `run_AE_AKI_all()` builds this once and returns both the events (`$events`, for this table) and the per-measurement table (`$all_AKI`), which the core pipeline releases as its own table `all_AKI` (like the old `AE_AKI`). The original script made one row per measurement and no events; the event per episode is a choice made here. A possible alternative is the stage per measurement summarised like the CTCAE types (an idea commented out in `CTCAE_v2.R`), which would give an end date and a duration. This is a different definition from `functions/AE_AKI.R`; the old SQL table `AE_AKI` was built with the definition used here.

Comparison with the old SQL table: the number of episodes per year of onset is 1-2% higher in the new table for 2008-2023, with almost the same split by stage. 2024 is 12% higher and 2025-2026 are only complete in the new table (newer data). `all_laboratory_measurements$samplingdate` is a date-time, so the date is taken with `as.Date()` before the sampling time is added.

### Type 2 diabetes (`core_scripts/AE_T2D.R`)

`run_AE_T2D()`: one event (`T2D onset`) per patient in `all_patients`. Candidates have a diagnosis starting with DE11 or a prescription of an anti-diabetic drug (ATC A10B), from `SDS_epikur` and `SDS_ekokur`. The old script used `SDS_epikur` twice; `SDS_ekokur` is now included. Excluded are women aged 20-39 with only metformin (A10BA02) prescriptions and no DE11 diagnosis, patients with insulin (A10A) or a DE10 diagnosis before the age of 30, and patients with a DE10 diagnosis before the T2D onset who never had a non-insulin anti-diabetic drug. `start_date` is the earlier of the first diagnosis and the first prescription.

Comparison with the old SQL table: 99.8% of the old patients are in the new table, the new table has 10% more patients (8,174 against 7,420), and 95% of the patients have the same onset date. The rest have an earlier date in the new table. `SDS_ekokur` adds only one patient, so the difference comes from newer and longer diagnosis and prescription data.

As in the old script, a patient is only kept if there is both a diagnosis date and a prescription date. The condition was written for "T2D at CLL diagnosis" and with missing dates it removes patients that have only a diagnosis or only a prescription.

### Hypertension (`core_scripts/AE_hypertension.R`)

`run_AE_hypertension()` by Noomi: one event (`hypertension event`) per patient, found before the first malignant diagnosis (`view_definition_first_diagnosis_malignant()`), so only for patients with such a diagnosis. One of three conditions is enough: diagnosis codes DI10-DI13 or DI15 on at least 2 dates at least 7 days apart; at least 2 prescriptions of first-choice antihypertensives with at least one within 365 days before the diagnosis; or prescriptions of at least 2 different ATC groups (3 characters) of antihypertensives within 365 days before the diagnosis. `start_date` is the earliest date of any condition.

Comparison with the old SQL table: 99.7% of the old patients are in the new table, the new table has 7.6% more patients (35,908 against 33,377), and 98.5% of the patients have the same date.

## Population

Infections, AKI and T2D: all patients in `all_patients`. CTCAE adverse events and hypertension: patients with a first malignant DALYCARE diagnosis, as in the old scripts. In `CTCAE_v2.R` only ALAT, ASAT and BASP were restricted to these patients (the other types used all laboratory data); here all types use the malignant group. The whole cohort has a malignant diagnosis, so the result is the same.

## Not built yet

The IPI is a separate table, `all_IPI`.
