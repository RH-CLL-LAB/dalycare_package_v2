# all_IPI

`all_IPI` is a table in CORE with one prognostic index group per patient and disease, calculated for the SUNDK registries (CLL, DaMyDa, LYFO). It is built by `core_scripts/all_IPI.R` (derived from `IPI_eop_2026.R`, last updated 2026-10-08). Column definitions and caveats are in `table_info/all_IPI.yaml`.

## Output

Long format, one row per patient and disease. A patient with two diseases appears in two rows, so join on `c("patientid", "disease")`.

| Column | Content |
|---|---|
| `patientid` | Patient id |
| `IPI` | Risk group, ordered factor: Low, Intermediate, High, Very high |
| `index` | Index behind the group: CLL-IPI, R-ISS, R-IPI, FLIPI2, MIPI, IPSSWM, MALT-IPI or IPS |
| `disease` | CLL, MM, DLBCL, FL, MCL, LPL, MZL or cHL |

Patients without a score (`NA`) are removed. The script does not save the table: it is written to `core/` by the core pipeline, as `all_IPI.csv` (semicolon separated).

## Index used per disease

| Disease | Index | Source of the score | Levels |
|---|---|---|---|
| CLL | CLL-IPI | `CLL_IPI()` (adjusted, see below) | Low to Very high |
| MM | R-ISS | Calculated in the script from ISS, LDH and FISH | Low, Intermediate, High |
| DLBCL | R-IPI | `RIPI_diagnosis` from `clean_SUNDK_LYFO()`: the registry IPI score grouped as 0, 1-2, 3 or more | Low, Intermediate, High |
| FL | FLIPI2 | `FLIPI2_diagnosis` from `clean_SUNDK_LYFO()`: the registry FLIPI2 score grouped as 0, 1-2, 3 or more | Low, Intermediate, High |
| MCL | MIPI | `MIPI()` | |
| LPL (WM) | IPSSWM | `IPSSWM()` | |
| MZL | MALT-IPI | `MALT_IPI()` | |
| cHL | IPS | `IPS()`, score grouped in two | Low, High |

## Inputs

- `SUNDK_CLL`, `SUNDK_DaMyDa`, `SUNDK_LYFO` and `all_patients`
- An IGHV IMGT dataset and pathology answers from `SDS_fctrekvisition` (CLL IGHV)
- `all_laboratory_measurements`: albumin, B2M, LDH, IgM, haemoglobin, leukocytes and lymphocytes

## Logic

1. Each registry is cleaned with its `clean_SUNDK_*` function.
2. Laboratory values supplement the registry only where the registry value is missing: the measurement closest to the diagnosis date within +/- 30 days is used (`slice_closest_value()`).
3. CLL: IGHV is taken from the registry, else the IMGT dataset, else pathology. B2M is supplemented from the laboratory data. After `CLL_IPI()`, the group is set to High or Very high if the score is already decided with one component missing (the `IPI_score_minus_B2M` and `IPI_score_minus_IGHV` scores).
4. MM: ISS is the registry value, else calculated from albumin and B2M. LDH thresholds depend on age. The R-ISS combines ISS with high LDH or high-risk FISH (t(4;14), t(14;16), del(17p)). Only subtype DC900 is included.
5. Lymphoma subtypes: haemoglobin, leukocytes, lymphocytes, LDH and IgM are supplemented the same way, then the subtype-specific index is calculated.
6. The eight tables are stacked and `IPI` is set as a factor with the four levels above.

Scores are not imputed: a missing component gives `NA` and the row is dropped. For DLBCL and FL the registry's own score is grouped in `clean_SUNDK_LYFO()` and used as a finished value, so it cannot be seen whether it was based on missing or supplemented data. Laboratory supplements may include up to 30 days of immortality bias.

## Relation to `functions/aaIPI.R`

`aaIPI.R` collects age-adjusted and other variants of IPI-type scores (for example `aaCLL_IPI` and `aaFLIPI2`). It is only one of many prognostic scores in the package (R-ISS, R2-ISS, IPSS and others have their own functions) and is **not** used for `all_IPI`. `all_IPI` uses the functions listed in the table above.

## Running the script

Check `PACKAGE_DIR` at the top of the run section of `core_scripts/all_IPI.R` and run it on the server where the datasets are available. This builds `all_IPI` in your session without saving it.
