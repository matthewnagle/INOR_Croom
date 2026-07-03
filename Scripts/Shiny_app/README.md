# INOR Arthroplasty Explorer

This is the **complete app folder**. Keep these together after unzipping:

- `app.R`
- `INOR_query_tool.R`
- `run_app.R`
- `RawData/`

## What changed in this revision

The app now includes:

- consultant filters on **PROMs**, **Complications**, and **Surgical times**
- implant-classification filters on those same tabs:
  - fixation type (`Cemented`, `Uncemented`, `Hybrid`, etc.)
  - knee system
  - acetabular component
  - femoral component
- a complications display toggle for:
  - **absolute numbers**
  - **rates (% of eligible cases)**
- a **BMI histogram** on the **Cases & demographics** tab

## Important note about the dummy extracts

Your dummy files do **not** share linked `FORM_RESPONSE_GROUP_ID` values across all datasets.

That means:

- the new consultant / implant filters are fully wired into the app
- but on the dummy files they may only show `All` or produce empty linked summaries
- once you replace the dummy files with real linked extracts, those filters should populate automatically

## How to run

```r
setwd("/path/to/INOR_shiny_app_complete_v2")
shiny::runApp()
```

or:

```r
setwd("/path/to/INOR_shiny_app_complete_v2")
source("run_app.R")
```

## Bundle contents

- `app.R` — Shiny UI and server
- `INOR_query_tool.R` — query and classification logic
- `run_app.R` — convenience launcher
- `RawData/` — the dummy CSV files you supplied
- `reference/Implant_queries.R`
- `reference/Summary_data.R`

## Complication rates: denominator used

When **Rates** is selected on the complications tab, the app uses:

- **numerator** = affected cases with the filtered complication(s)
- **denominator** = eligible post-op cases after the selected source / joint / consultant / implant / presentation / side filters

So the rate is shown as:

`affected cases / eligible cases * 100`

## Using real registry exports

Replace the CSV files inside `RawData/` with your real exports using the same filenames, or edit the paths at the top of `INOR_query_tool.R`.

The app is designed around `FORM_RESPONSE_GROUP_ID` as the case-level key for one joint replacement episode.
