# Data (not included)

The biometric records belong to the La Raya Research and Production Centre (CIP La Raya), Universidad Nacional del Altiplano de Puno, and are **not publicly available**. They can be requested from the corresponding author (vibanez@unap.edu.pe) and are shared with the permission of CIP La Raya.

To reproduce the analysis, place the file `BASE DATOS LLAMAS_CHACU_KARA_1_a_5_year_VRI_2026.xlsx` in this folder (or set the environment variable `LLAMAS_XLSX` to its path). The code reads the sheet `viq_todo`, range `A19:Q259`.

## Data dictionary

240 records, one per animal: 2 breeds × 2 sexes × 5 ages × 12 animals, single biometric survey (2015).

| Column | Description | Unit |
|---|---|---|
| `Nro` | Sequential record number | – |
| `SEXO` | Sex (`MACHO`, `HEMBRA`) | – |
| `VARIE` | Breed (`KARA`, `CHACU`) | – |
| `EDAD` | Age class | years (1–5) |
| `ALCRU` | Withers height | cm |
| `ALGRU` | Rump height | cm |
| `PETO` | Heart girth | cm |
| `PEAB` | Abdominal circumference | cm |
| `LARDO` | Back length | cm |
| `LACU` | Body length | cm |
| `PROF` | Chest depth | cm |
| `AMPLI` | Chest width | cm |
| `ALAB` | Belly height | cm |
| `LAGRU` | Rump length | cm |
| `ANGRU` | Rump width | cm |
| `PECAN` | Fore cannon circumference | cm |
| `PEVI` | Live weight (response) | kg |
