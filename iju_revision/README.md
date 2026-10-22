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
- `figures/` — the dNLR/MLR overlap Venn diagram (R/VennDiagram and pptxgenjs versions).
- `deliverables/` — the generated Word/Excel response documents and the scripts that built them.

## Scripts

| Script | Reviewer comment(s) | Purpose |
|---|---|---|
| `01_load.R` | — | Loads `data_24m.xlsx` into `d.rds`; reproduces key reported summary stats as a sanity check. |
| `02_ALC.R` | R2-2 | First attempt: ROC-derived ALC cutoff. Degenerate (AUC 0.435, 183/9 split) — kept for the audit trail; superseded by `02b`. |
| `02b_ALC_median.R` | R2-2 | ALC analysis using the literature median cutoff: univariate/multivariable HRs, the decisive test (ALC added to the dNLR-MLR model), C-index comparison. |
| `03_crp_vs_dnlrmlr.R` | R2-4 / R3-6 | Paired bootstrap (B=1000) comparing IMDC-dNLR-MLR vs IMDC-CRP C-index (apparent-level, cutoffs fixed), with the percentile-CI definition later reused for the nested bootstraps. |
| `04_overlap_regimen.R` | R1-1 / R3-5 / R2-3 | dNLR/MLR overlap (Jaccard, kappa); dNLR vs IMDC-neutrophilia overlap; regimen-stratified C-index refresh. |
| `05_sensitivity_18mo.R` | R3-4 | Re-derives cutoffs at 18 vs 24 months; classification agreement. |
| `06_NRI.R` | R1-1 | Continuous NRI/IDI for MLR added to IMDC+dNLR and vice versa, with bootstrap CI. |
| `07_nested_boot.R` | R1-2 / R3-3 | Fully nested bootstrap (B=1000): cutoff derivation + marker significance screening + model selection all re-run inside each resample. Reports model-selection frequency and the nested optimism-corrected C-index. |
| `08_nested_per_model.R` | R2-4 reviewer request | Per-model nested bootstrap (B=1000 each): model structure (which marker/combination) held fixed, but each model's own cutoff is re-derived inside every resample. Run for all 9 candidate models. |
| `08c_nested_per_model_with_slope.R` | R2-4 | Same as `08`, extended to also compute the nested optimism-corrected calibration slope (8 models matching the published Supplementary Table 4, PIV excluded). |
| `09_nested_per_model_regimen.R` | R2-3 | Reuses `08`'s per-model nested bootstrap, run separately within the ICI-ICI (n=104) and ICI-TKI (n=88) regimen subgroups (cutoffs/coefficients resampled from within that regimen only). |
| `10_nested_full_pipeline_regimen.R` | R2-3 | Reuses `07`'s full-pipeline nested bootstrap (including marker significance screening and model selection), run separately within each regimen subgroup, to directly test whether model-selection frequency is less stable at smaller regimen-level sample sizes than at the pooled n=192. |
| `11_crp_interaction.R` | R2-3 / R2-4 | Classification × regimen interaction test (likelihood ratio test) for IMDC-CRP, plus direct CRP vs dNLR-MLR comparison restricted to the ICI-TKI subgroup only. |
| `12_dnlr_mlr_interaction.R` | R2-3 | Same interaction test for IMDC-dNLR alone and IMDC-MLR alone. |
| `13_all_interaction.R` | R2-3 | Classification × regimen interaction test for all 8 candidate models in one run (only IMDC-CRP reaches significance, p=0.036). |
| `14_cutoff_ci.R` | Supplementary Table 1 | Bootstrap 95% CI for the ROC-derived cutoff value itself (not just downstream performance), for all 8 markers. NLR and CRP show genuine bimodal cutoff distributions under resampling; the other markers are comparatively stable. |
| `15_pairwise_vs_dnlrmlr.R` | R2-4 / R3-6 | Paired bootstrap (apparent-level, cutoffs fixed) comparing IMDC-dNLR-MLR against all 7 other candidate models (not just CRP) in one run. |
| `15b_pairwise_piv.R` | R2-4 / R3-6 | Same apparent-level paired bootstrap, extended to IMDC-PIV (missing from `15`'s original run), to complete the 8-model comparison set. |
| `16_nested_paired_vs_dnlrmlr.R` | R2-4 / R3-6 | Same comparison as `15`/`15b` but fully nested: cutoffs for **both** models in each pair are re-derived within every resample before differencing. B=500, run against all 8 other candidates (including PIV). Result: the apparent-level comparison (`15`/`15b`) shows IMDC-dNLR-MLR significantly ahead of all 8; the fully nested version only retains significance against IMDC alone and IMDC-PIV — a direct illustration of the optimism that a fixed-cutoff comparison hides (cf. Altman et al., *J Natl Cancer Inst* 1994, on the dangers of "optimal" cutpoints). |
| `17_alc_nested.R` | R2-2 | Per-model nested bootstrap C-index for IMDC-ALC, using the median cutoff (re-derived per resample, not a degenerate ROC search) for consistency with `02b`. |
| `18_full_pipeline_with_alc.R` | R2-2 | Re-runs `07`'s full-pipeline nested bootstrap with ALC added as a 9th single-marker candidate (median cutoff, low=high-risk direction), to see how often ALC or an ALC-containing choice would be selected if it had been a candidate marker from the start. |
| `19_table4_with_alc.R` / `19b_table4_full_format.R` | R2-2 | Reproduces the published Table 4 multivariable Cox models (classification + regimen + bone metastasis + nephrectomy + age) for OS and PFS, with ALC added as an additional covariate. ALC is non-significant in both fully-adjusted models (OS p=0.81, PFS p=0.78) and leaves every other covariate's HR essentially unchanged. `19b` adds clean HR (95% CI) / p formatting for all covariates. |
| `EZR_nested_bootstrap_1marker.R` | — | Self-contained, EZR-pasteable version of `08`'s method for one model at a time (running all markers in a single loop was reported to freeze EZR). Change the `MARKERS` variable at the top and re-run per model. |

## Key cross-cutting finding: regimen × classification interaction

Of the 8 candidate models, only **IMDC-CRP** shows a statistically significant classification ×
regimen interaction (p=0.036, likelihood ratio test). IMDC-dNLR-MLR (p=0.812), IMDC-dNLR alone
(p=0.890), and IMDC-MLR alone (p=0.941) do not — i.e. the dNLR/MLR-based classifications'
prognostic effect is consistent regardless of ICI-ICI vs ICI-TKI regimen, while CRP's is not.
This is used both to justify pooling regimens for model selection (R2-3) and as a principled,
mechanism-level reason (not just a numerical one) to prefer dNLR-MLR over CRP (R2-4/R3-6),
ahead of the confirmatory pairwise C-index comparisons in `15`/`16`.

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

Paired model-comparison bootstraps (`03`, `15`, `16`) instead take the percentile CI of the
per-resample *difference* between the two models' out-of-resample performance — the standard
approach for a formal "is model A significantly better than model B" test.
