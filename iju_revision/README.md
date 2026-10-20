# IJU Major Revision — Statistical Response Analyses

R scripts and console output for the statistical analyses supporting the response to the
International Journal of Urology major-revision reviewer comments (dNLR/MLR prognostic
classification in metastatic clear-cell RCC).

**Patient-level data is intentionally not included in this repository.** `scripts/` and
`output/` contain only code and aggregate/summary results (counts, HRs, C-indices, etc.);
no row-level patient data is committed.

## Layout

- `scripts/` — R scripts, run with base R 4.3.3 + the `survival` package (and `rms` for
  `02b_ALC_median.R`). `helpers.R` implements a from-scratch Heagerty–Zheng KM-method
  time-dependent ROC (Youden-optimal cutoff), used throughout since `survivalROC`/`timeROC`
  were not installable in the analysis environment.
- `output/` — console output from each script, one log per script, same filename stem.

## Scripts

| Script | Reviewer comment(s) | Purpose |
|---|---|---|
| `01_load.R` | — | Loads `data_24m.xlsx` into `d.rds`; reproduces key reported summary stats as a sanity check. |
| `02_ALC.R` | R2-2 | First attempt: ROC-derived ALC cutoff. Degenerate (AUC 0.435, 183/9 split) — kept for the audit trail; superseded by `02b`. |
| `02b_ALC_median.R` | R2-2 | ALC analysis using the literature median cutoff: univariate/multivariable HRs, the decisive test (ALC added to the dNLR-MLR model), C-index comparison. |
| `03_crp_vs_dnlrmlr.R` | R2-4 / R3-6 | Paired bootstrap (B=1000) comparing IMDC-dNLR-MLR vs IMDC-CRP C-index, with the percentile-CI definition later reused for the nested bootstraps. |
| `04_overlap_regimen.R` | R1-1 / R3-5 / R2-3 | dNLR/MLR overlap (Jaccard, kappa); dNLR vs IMDC-neutrophilia overlap; regimen-stratified C-index refresh. |
| `05_sensitivity_18mo.R` | R3-4 | Re-derives cutoffs at 18 vs 24 months; classification agreement. |
| `06_NRI.R` | R1-1 | Continuous NRI/IDI for MLR added to IMDC+dNLR and vice versa, with bootstrap CI. |
| `07_nested_boot.R` | R1-2 / R3-3 | Fully nested bootstrap (B=1000): cutoff derivation + marker significance screening + model selection all re-run inside each resample. Reports model-selection frequency and the nested optimism-corrected C-index. |
| `08_nested_per_model.R` | R2-4 reviewer request | Per-model nested bootstrap (B=1000 each): model structure (which marker/combination) held fixed, but each model's own cutoff is re-derived inside every resample. Run separately for all 9 candidate models (IMDC alone, 6 single markers, 2 combinations). |
| `EZR_nested_bootstrap_1marker.R` | — | Self-contained, EZR-pasteable version of `08`'s method for one model at a time (running all markers in a single loop was reported to freeze EZR). Change the `MARKERS` variable at the top and re-run per model. |

## CI definition (important)

All nested-bootstrap 95% CIs here use the same definition as `03_crp_vs_dnlrmlr.R`
(the original, fixed-cutoff comparison): for each resample *b*,

```
optimism_b   = train_C_b - test_C_b      # train_C_b: apparent C on the resample itself
                                          # test_C_b:  that resample's model evaluated on the original data
corrected_b  = apparent_C_full - optimism_b
```

and the 95% CI is the 2.5/97.5 percentile of `corrected_b` across resamples. By
construction, `mean(corrected_b)` equals the reported point estimate exactly, so the CI is
centered on the number actually reported — not on the (related but distinct) distribution
of `test_C_b` alone.
