# INOR Arthroplasty Explorer

This is the **complete app folder**. Keep these together after unzipping:

- `app.R`
- `INOR_query_tool.R`
- `run_app.R`
- `RawData/`

## What changed in this revision

The **PROMs** tab is now split into two views:

- **Scores by stage** — the original cross-sectional view (mean score at each stage)
- **Pre-op vs post-op change** — paired analysis of how much each case actually improved

### Pre-op vs post-op change

Each follow-up score is paired back to that case's own pre-op baseline, so the
change is a within-patient difference rather than a comparison of two different
cohorts. Pairing is on `FORM_RESPONSE_GROUP_ID` **and side**, so a bilateral
patient is never baselined against the other knee or hip.

Where a case has repeat submissions at the same stage, the **last** pre-op score
is used (closest to surgery) and the **first** score at a follow-up stage (closest
to the nominal review point).

`Change = follow-up score - pre-op score`, so a positive change is an improvement
for all three instruments (OKS 0-48, OHS 0-48, EQ-5D index up to 1.0).

The view shows:

- paired case count, mean pre-op, mean follow-up, and mean change with a 95% CI
  and a paired t-test
- the proportion improved, unchanged, worse, meeting the MCID, and meeting PASS
- a distribution of change and a pre-op vs follow-up scatter against the line of
  no change
- mean change with confidence intervals at every follow-up stage in the extract
- the same change breakdown by whichever **Compare scores by** grouping is
  selected (consultant, surgeon grade, fixation, knee system, components)
- a per-case table of both scores, the change, days between, and the case
  metadata — downloadable as CSV

### PASS (patient acceptable symptom state)

PASS asks a different question from the MCID: not "did this patient gain enough?"
but "is this patient's state acceptable now?" It is an absolute post-op score, so
it needs no pre-op baseline and appears on **both** views.

- **Scores by stage** — headline PASS rate, PASS rate at each stage, the score
  distribution against the threshold, and PASS rate by the selected comparison
  group
- **Pre-op vs post-op change** — PASS rate among the paired cohort, a `PASS (%)`
  column in the by-stage and by-group tables, a `Met PASS` column per case, and a
  cross-tab of the two definitions

That cross-tab matters: a patient starting from a very low baseline can clear the
MCID comfortably and still fall short of an acceptable state. On the knee dummy
data at 6 months, 74% met both, but 12% met the MCID while remaining below PASS.

The headline PASS rate and the group comparison use **follow-up records only**.
Pooling pre-op records (where almost nobody is in an acceptable state) would drag
every rate down, and would let a group's mix of stages masquerade as a difference
in outcome. Pre-op still appears in the by-stage breakdown as a reference point.

### Threshold defaults

Both thresholds are editable in the sidebar. The defaults are:

| Instrument | MCID | PASS |
|---|---|---|
| Oxford Knee Score | 7 | 30 |
| Oxford Hip Score | 8 | 40 |
| EQ-5D index | 0.074 | 0.80 |

MCIDs are the individual-patient (ROC-derived) values from Beard et al.,
*J Clin Epidemiol* 2015;68(1):73-79, since the app reports the proportion of
individual patients achieving a meaningful improvement. Their group-level
equivalents are larger (9 for OKS, 11 for OHS). The EQ-5D value is from Walters &
Brazier, *Qual Life Res* 2005.

PASS defaults are the 12-month values from Ingelsrud et al., *Acta Orthop*
2020;92(1):85-90 (OKS: 27 at 3 months, 30 at 12 and 24 months) and Galea et al.,
*Acta Orthop* 2020;91(4):372-377 (OHS: 34 at 3 months, 40 at 1 year, 39 at
2 years). For the EQ-5D index, reported thresholds run 0.68 to 0.85 depending on
the value set (Conner-Spady et al., *Qual Life Res* 2022).

**Published thresholds drift with follow-up length, case mix, and value set**, so
treat these as starting points and set them to whatever your registry has agreed.
The sidebar shows the timepoint-specific published values for the selected
instrument.

Things to be aware of:

- The **Stage** filter does not apply to this view. The paired analysis always
  needs both the pre-op and the follow-up row for a case.
- Cases that cannot be paired are excluded and counted in the banner at the top
  of the view, so a small paired cohort is never mistaken for a complete one.
- `Extraordinary presentation` records are now visible in the Stage filter (they
  were previously dropped), but they are excluded from the change analysis
  because they are unscheduled reviews rather than a fixed follow-up point.

The case trajectory lookup also gains a **Change from pre-op** column, baselined
per side.

The app also includes:

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
