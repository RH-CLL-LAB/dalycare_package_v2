# core_scripts

Scripts that build new tables for CORE (as opposed to `functions/`, which holds reusable functions). Each script should have a matching description in `table_info/` and, where useful, in `docs/`.

| Script | Table | Documentation |
|---|---|---|
| `all_IPI.R` | `all_IPI` | `docs/all_IPI.md` |

## Structure of a core script

The logic of a table lives in a function (for example `build_all_IPI()`) that only takes data in and returns the table: no paths, no loading and no writing, so the code can be read, copied into your own script, or called from a pipeline. The code at the bottom of the script loads the data and writes the table when the script is run directly.
