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

`run_AE_infections()` loads the data for all patients in `all_patients` and returns the events of two parts:

| event_type | Definition |
|---|---|
| `IVAB_Infection` | Infection treated with intravenous antibiotics: `AE_infection()` on administered antibiotics (`ATC_AB()`), at least 1 day of IV antibiotics, infections separated by more than 7 days. `duration_days` is the number of days of IV antibiotics. |
| `BC+IV_AB` | Definition 1: IV antibiotics (at least 24 hours) within 48 hours of a blood culture |
| `IV_AB` | Definition 2: IV antibiotics for at least 24 hours, independent of blood culture |
| `BC_drawn` | Definition 3: blood culture drawn, independent of antibiotics |
| `any_AB_pre_or_adm` | Definition 4: any antibiotics prescribed or administered for at least 24 hours, excluding the prophylactic antimicrobials Aciclovir, Sulfotrim and Valaciclovir |
| `BC_OR_any_AB` | Definition 5: blood culture or any antibiotics (definition 3 or 4) |

Infections of one definition are separated by more than 7 days. The date of an infection is the first administration (or the date of the blood culture). Prescribed medicine is not known to be taken, and the durations of prescriptions are not necessarily correct.

Sources: `SP_AlleProvesvar` (blood cultures, via `load_blood_culture_SP()`), `SP_AdministreretMedicin` and `SP_OrdineretMedicin`. The coverage period differs between these sources, so the period is not the same for all definitions.

## Population

Infections: all patients in `all_patients`. The CTCAE adverse events (planned) are restricted to patients with a first malignant DALYCARE diagnosis, as in the old scripts.

## Not built yet

CTCAE (lab-based), AKI, hypertension and type 2 diabetes. The IPI is a separate table, `all_IPI`.
