# INOR Arthroplasty Explorer

This is the **complete app folder**. Keep these together after unzipping:

- `app.R`
- `INOR_query_tool.R`
- `run_app.R`
- `RawData/`

## What changed in this revision

The **PROMs** tab is now split into three views:

- **Scores by stage** — the original cross-sectional view (mean score at each stage)
- **Pre-op vs post-op change** — paired analysis of how much each case actually improved
- **Trends** — rolling averages over consecutive cases, for monitoring drift over time
- **Monitoring** — funnel plot and CUSUM, for comparing units and detecting change

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
every rate down. Pre-op still appears in the by-stage breakdown as a reference
point.

#### Why the two tabs report different PASS rates

They are correct but measure different cohorts, and the difference is signposted
in both places:

| | Scores by stage | Pre-op vs post-op change |
|---|---|---|
| Stages | every selected follow-up stage, broken out separately | the one selected follow-up stage |
| Cases | every follow-up record | only cases that also have a pre-op baseline |
| Repeat submissions | all counted | collapsed to one per case / side / stage |

PASS climbs with time since surgery (79.6% at 6 months, 88.9% at 2 years on the
knee dummy data), so **never compare a pooled figure against a single-stage one**.
The group comparison on the Scores by stage tab is therefore broken out by stage
rather than pooled — otherwise a consultant with proportionally more 2-year
follow-ups would score better purely through case mix.

Compared stage-for-stage, the two tabs agree to within about 2 percentage points
on the dummy data. What remains is the paired-only restriction: follow-up records
with no pre-op baseline count on the Scores by stage tab but cannot appear in a
paired analysis.

### Trends

A rolling average over consecutive cases, for spotting drift that a single
pooled number hides.

Cases are ordered by **procedure date** and each point averages that case plus
the preceding cases in the window, so the x axis is operative sequence rather
than calendar time. The window is **right-aligned**: a point is never influenced
by surgery that had not happened yet, and the series only starts once a full
window exists rather than opening on an average of two or three cases.

Controls:

- **Rolling window (cases)** — 5 up to half the available cases, capped at 400
- **Metrics to plot** — mean follow-up score, mean pre-op score, mean change,
  Met MCID (%), PASS rate (%); each gets its own panel with its own y scale
- **Plot against** — case sequence or procedure date
- Series split by the **Compare scores by** selection, capped at the 8 groups
  with the most cases

Alongside the chart:

- metric tiles comparing the **latest window against the first**, per metric
- a per-series table of the same first-vs-latest comparison
- the full per-case rolling series, downloadable as CSV

The cohort is the **paired** one — cases with both a pre-op baseline and a score
at the selected follow-up stage — so every metric on the chart describes the same
set of patients and the MCID and PASS lines are directly comparable.

Two things to read carefully:

- Grouped by consultant with **case sequence** on the x axis, each series is
  numbered from its own first case, so the lines are *not* aligned in time. That
  is what you want for a per-surgeon learning curve; switch to **procedure date**
  to compare calendar periods.
- The first-vs-latest table is a crude comparison of two windows, not a test for
  trend. A window of 50 on a noisy measure will swing several points on chance
  alone. Read it next to the chart, not instead of it. For a proper test of
  change over time, use the CUSUM on the Monitoring view.

### Monitoring: funnel plot and CUSUM

Both charts work on the paired cohort at the selected follow-up stage, and both
carry the same caveat in the app itself.

> **Neither chart adjusts for case mix.** A signal means a group or a run differs
> from the pooled cohort under the current filters — not that care was worse. A
> surgeon operating on patients with lower baseline function, more complex
> deformity, or higher comorbidity drifts towards a signal for those reasons
> alone. Both are prompts to look, never conclusions.

#### Funnel plot

Each unit (consultant, surgeon grade, fixation type, component — whatever
**Compare scores by** is set to) is one point: volume on the x axis, outcome on
the y. Control limits narrow as volume rises, so a small unit has to be far from
the centre before it means anything.

- Outcomes: Met PASS, Met MCID, mean change, mean follow-up score
- Limits at 95% and 99.8% (either, or both)
- Proportions use **exact binomial** limits rather than the normal
  approximation, which misbehaves badly at the low-volume end of a funnel —
  exactly where registry units sit. The limit line is stepped as a result; that
  is honest, not a rendering artefact.
- Continuous outcomes use the pooled SD, which assumes common variance across
  units
- All four outcomes run "higher is better", so a point above the upper limit is
  flagged as *better* than the cohort, and below the lower limit as *worse*

#### CUSUM

A Bernoulli log-likelihood-ratio CUSUM (Steiner et al., *Biostatistics* 2000),
monitoring failure to achieve a yes/no outcome — Met PASS or Met MCID. Continuous
outcomes are not charted; the funnel handles those.

The upper chart accumulates evidence that the failure odds have **risen** by the
chosen factor, the lower that they have **fallen**. Cases are ordered by
procedure date. Crossing the control limit `h` raises a signal, and the chart
**resets to zero** after each one — without the reset a single crossing leaves
the chart flagged for every later case, so one detection reads as hundreds and
later changes are masked.

Configurable: the odds ratio to detect (default 2), the control limit `h`
(default 5), and the baseline failure rate (defaults to the filtered cohort's
own observed rate).

**Read the false-alarm rate before acting on a crossing.** The app simulates the
in-control average run length for your settings and states it in plain words —
"a false signal would still be expected roughly every N cases". That number moves
a great deal with `h`: at a 20% baseline failure rate and an odds ratio of 2,
`h = 3` gives a false signal about every 530 cases, `h = 4` about every 1,400,
`h = 5` about every 4,300, and `h = 6` more than every 20,000. A limit chosen
without looking at this is arbitrary.

### Patient demographic filters

The PROMs sidebar filters on **sex, age at procedure, and BMI**. These apply to
every PROMs view — scores by stage, change, trends and monitoring — because they
sit in the shared filter chain, and a cohort composition line above the tabs
reports what the current filters actually selected (n, mean age, % female, mean
BMI, and how many records have no value on file).

Where the values come from matters, because coverage differs:

| Field | Source | Coverage on the dummy data |
|---|---|---|
| Sex | the PROM record's own `Sex at Birth`, falling back to the case record | 100% |
| Age | case record's age at surgery, else computed from the PROM date of birth | 100% |
| BMI | case record only — PROM extracts carry no BMI | ~85% |

Because BMI is only ~85% covered, each range filter has an explicit **include
records with no value** checkbox, on by default. A slider alone would silently
delete every case with nothing on file, which on BMI would quietly drop a sixth
of the cohort.

Age is age at procedure where the record links to the case table. Where it does
not, it falls back to age at the PROM event — which for a 5-year review is five
years older than age at surgery. Same caveat as the date filter.

**Sex, age band and BMI band are also available under Compare scores by**, so
every existing comparison — mean score, change, MCID, PASS, funnel, CUSUM,
rolling trend — works across them with no extra controls. Bands are:

- Age: under 50, 50-59, 60-69, 70-79, 80 and over
- BMI: WHO categories, with the obesity classes kept separate (30-34.9, 35-39.9,
  40+) because operative risk and PROM gain differ across them

Banded groupings sort in clinical order rather than alphabetically.

On the knee dummy data this immediately shows a PASS gradient across BMI at
6 months — 84.3% healthy weight, 81.0% overweight, 79.4% obese I, 76.0% obese II,
68.3% obese III — while mean change stays roughly flat. Patients with higher BMI
gain about as much, but start and finish lower.

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

## Surgical times: rolling trend

The **Surgical times** tab is split into two views:

- **Distribution and comparison** — the original histogram, comparison chart and
  tables
- **Rolling trend** — a rolling average over consecutive cases, the same
  mechanism as the PROMs Trends view

Cases are ordered by procedure date and each point averages that case plus the
preceding cases in the window. The window is right-aligned, so no point is
influenced by an operation that had not happened yet, and a series only starts
once a full window exists.

Metrics: mean duration, median duration, variability (SD), and the percentage of
**long cases** over a threshold you set (default 120 minutes). Median and SD are
worth watching alongside the mean — a stable mean with rising SD means the
list has become less predictable even though average theatre time has not moved.

Controls mirror the PROMs trend: window size in cases, which metrics to plot,
case sequence or procedure date on the x axis, and series split by the
**Compare duration by** selection. Metric tiles compare the latest window against
the first, and the full per-case rolling series downloads as CSV.

The same caution applies as on the PROMs trend: grouped by consultant with case
sequence on the x axis, each series is numbered from its own first case, so the
lines are not aligned in time. That is what you want for a per-surgeon learning
curve; switch to procedure date to compare calendar periods.

## Complication rates: denominator used

When **Rates** is selected on the complications tab, the app uses:

- **numerator** = affected cases with the filtered complication(s)
- **denominator** = eligible post-op cases after the selected source / joint / consultant / implant / presentation / side filters

So the rate is shown as:

`affected cases / eligible cases * 100`

## Using real registry exports

Replace the CSV files inside `RawData/` with your real exports using the same filenames, or edit the paths at the top of `INOR_query_tool.R`.

The app is designed around `FORM_RESPONSE_GROUP_ID` as the case-level key for one joint replacement episode.
