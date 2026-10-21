import openpyxl
from openpyxl.styles import Font, Alignment, PatternFill, Border, Side

wb = openpyxl.Workbook()
wb.remove(wb.active)

FONT = "Arial"
header_font = Font(name=FONT, bold=True, color="FFFFFF", size=11)
header_fill = PatternFill(start_color="2F5D53", end_color="2F5D53", fill_type="solid")
title_font = Font(name=FONT, bold=True, size=13)
body_font = Font(name=FONT, size=11)
bold_font = Font(name=FONT, bold=True, size=11)
note_font = Font(name=FONT, italic=True, size=9, color="555555")
thin = Side(style="thin", color="BFBFBF")
border = Border(left=thin, right=thin, top=thin, bottom=thin)
center = Alignment(horizontal="center", vertical="center", wrap_text=True)
left = Alignment(horizontal="left", vertical="center", wrap_text=True)

def write_sheet(name, title, note, headers, rows, col_widths, highlight_rows=None):
    ws = wb.create_sheet(name)
    ws["A1"] = title
    ws["A1"].font = title_font
    ws.merge_cells(start_row=1, start_column=1, end_row=1, end_column=len(headers))
    r = 2
    if note:
        ws.cell(row=2, column=1, value=note).font = note_font
        ws.merge_cells(start_row=2, start_column=1, end_row=2, end_column=len(headers))
        r = 3
    hdr_row = r + 1
    for j, h in enumerate(headers, start=1):
        c = ws.cell(row=hdr_row, column=j, value=h)
        c.font = header_font; c.fill = header_fill; c.alignment = center; c.border = border
    rr = hdr_row + 1
    highlight_rows = highlight_rows or set()
    for idx, row in enumerate(rows):
        is_hi = idx in highlight_rows
        for j, v in enumerate(row, start=1):
            c = ws.cell(row=rr, column=j, value=v)
            c.font = bold_font if (is_hi and j == 1) else (bold_font if is_hi else body_font)
            c.border = border
            c.alignment = left if j == 1 else center
            if is_hi:
                c.fill = PatternFill(start_color="E4EDE9", end_color="E4EDE9", fill_type="solid")
        rr += 1
    for j, w in enumerate(col_widths, start=1):
        ws.column_dimensions[chr(64+j)].width = w
    ws.freeze_panes = ws.cell(row=hdr_row+1, column=1).coordinate
    return ws

# ---- Sheet 1: R1-1 overlap + NRI/IDI ----
write_sheet(
    "R1-1_overlap_NRI",
    "R1-1: dNLR / MLR overlap and incremental value (NRI, IDI)",
    "n=192. NRI/IDI: continuous, 24-month OS. 95% CI from B=500 bootstrap (percentile of apparent-minus-optimism per resample, see README).",
    ["Metric", "Value", "95% CI", "Note"],
    [
        ["Neither marker-high (n, %)", "81 (42.2%)", "", ""],
        ["dNLR-only (n, %)", "21 (10.9%)", "", ""],
        ["MLR-only (n, %)", "38 (19.8%)", "", ""],
        ["Both marker-high (n, %)", "52 (27.1%)", "", ""],
        ["Jaccard index", 0.468, "", "both / (dNLR-only + MLR-only + both)"],
        ["Cohen's kappa", 0.376, "", "fair–moderate agreement"],
        ["NRI: MLR added to IMDC+dNLR", 0.510, "0.123 to 0.851", "excludes 0, significant"],
        ["NRI: dNLR added to IMDC+MLR", 0.670, "−0.402 to 1.010", "includes 0, not significant"],
        ["IDI: MLR added to IMDC+dNLR", 0.0024, "", ""],
        ["IDI: dNLR added to IMDC+MLR", 0.0213, "", ""],
    ],
    [32, 16, 20, 42],
)

# ---- Sheet 2: R1-2/R3-3 full pipeline nested bootstrap ----
ws2 = write_sheet(
    "R1-2_R3-3_full_pipeline",
    "R1-2 / R3-3: Fully nested bootstrap (cutoff + marker screening + model selection re-run within each resample)",
    "B=1,000. 'Selected' = model with highest apparent C-index within that resample.",
    ["Model", "Times selected (of 1,000)", "Selection frequency"],
    [
        ["IMDC-dNLR-MLR", 413, "41.3%"],
        ["IMDC-CRP", 339, "33.9%"],
        ["IMDC-dNLR", 86, "8.6%"],
        ["IMDC-MLR", 83, "8.3%"],
        ["IMDC-NLR-MLR", 33, "3.3%"],
        ["IMDC-NLR", 23, "2.3%"],
        ["IMDC-SIRI", 12, "1.2%"],
        ["IMDC-PIV", 6, "0.6%"],
        ["IMDC-PLR", 3, "0.3%"],
        ["IMDC alone", 2, "0.2%"],
    ],
    [20, 24, 20],
    highlight_rows={0},
)
r = ws2.max_row + 2
ws2.cell(row=r, column=1, value="Summary statistic").font = header_font
ws2.cell(row=r, column=1).fill = header_fill
ws2.cell(row=r, column=2, value="Value").font = header_font
ws2.cell(row=r, column=2).fill = header_fill
ws2.merge_cells(start_row=r, start_column=2, end_row=r, end_column=3)
for lbl, val in [
    ("Apparent C-index (originally-selected model, dNLR-MLR, full data)", "0.7182"),
    ("Mean optimism (full selection process)", "0.0385"),
    ("Nested optimism-corrected C-index", "0.680"),
    ("95% CI (nested-corrected)", "0.615 to 0.738"),
    ("For comparison: naive (model-fixed) bootstrap corrected C", "0.705–0.711"),
]:
    r += 1
    ws2.cell(row=r, column=1, value=lbl).font = body_font
    ws2.cell(row=r, column=1).border = border
    c = ws2.cell(row=r, column=2, value=val); c.font = body_font; c.border = border
    ws2.merge_cells(start_row=r, start_column=2, end_row=r, end_column=3)

# ---- Sheet 3: Supplementary Table 4 revised (per-model nested, C-index only; see separate file for + calibration slope) ----
write_sheet(
    "sTable4_nested_Cindex",
    "Supplementary Table 4 (revised): per-model nested bootstrap, C-index",
    "B=1,000 per model. Cutoff and coefficients both re-derived within each resample; model structure fixed per row. 95% CI = percentile of (apparent C − optimism_b).",
    ["Model", "Apparent C-index", "Optimism-corrected C-index", "95% CI lower", "95% CI upper"],
    [
        ["IMDC", 0.6216, 0.6213, 0.5556, 0.6812],
        ["IMDC-NLR", 0.6727, 0.6509, 0.5841, 0.7150],
        ["IMDC-dNLR", 0.6890, 0.6653, 0.5958, 0.7309],
        ["IMDC-MLR", 0.6870, 0.6651, 0.6011, 0.7283],
        ["IMDC-SIRI", 0.6646, 0.6412, 0.5675, 0.7096],
        ["IMDC-CRP", 0.6979, 0.6737, 0.6058, 0.7318],
        ["IMDC-NLR-MLR", 0.6842, 0.6642, 0.5980, 0.7245],
        ["IMDC-dNLR-MLR", 0.7182, 0.6984, 0.6323, 0.7563],
    ],
    [18, 18, 24, 15, 15],
    highlight_rows={7},
)

# ---- Sheet 4: R2-2 ALC ----
write_sheet(
    "R2-2_ALC",
    "R2-2: Absolute lymphocyte count (ALC) analysis",
    "Median cutoff used (ALC ROC cutoff was degenerate: AUC=0.435, 183/9 split).",
    ["Analysis", "Result"],
    [
        ["ALC cutoff used", "Median = 0.835 ×10³/μL (balanced 96/96 split)"],
        ["Spearman: ALC vs dNLR", "ρ = −0.439 (p<0.001)"],
        ["Spearman: ALC vs MLR", "ρ = −0.491 (p<0.001)"],
        ["Univariate: ALC-low HR (OS)", "2.03 (95% CI 1.21–3.41, p=0.008)"],
        ["Multivariable (+IMDC): ALC-low HR", "1.81 (95% CI 1.08–3.06, p=0.026)"],
        ["Decisive test: ALC added to dNLR-MLR grp4 — ALC-low HR", "1.10 (95% CI 0.62–1.93, p=0.75); dNLR-MLR group HRs essentially unchanged"],
        ["Individual terms (IMDC+dNLR_ge+MLR_ge+ALC) — ALC-low HR", "1.07 (p=0.84)"],
        ["C-index: IMDC alone (apparent/corrected)", "0.622 / 0.625"],
        ["C-index: IMDC+ALC (apparent/corrected)", "0.664 / 0.659"],
        ["C-index: IMDC-dNLR-MLR (apparent/corrected)", "0.718 / 0.714"],
        ["C-index: IMDC-dNLR-MLR+ALC (apparent/corrected)", "0.721 / 0.709 (adding ALC does not improve, slightly worsens corrected C)"],
    ],
    [46, 56],
)

# ---- Sheet 5: R2-3 regimen-stratified nested bootstrap ----
write_sheet(
    "R2-3_regimen_nested",
    "R2-3: Regimen-stratified nested bootstrap (per-model, cutoffs + coefficients re-derived WITHIN each regimen subgroup)",
    "B=1,000 per model per regimen. ICI-ICI n=104, ICI-TKI n=88. Model structure fixed per row; cutoff/coefficients resampled from within that regimen only.",
    ["Model", "ICI-ICI corrected C", "ICI-ICI 95% CI", "ICI-TKI corrected C", "ICI-TKI 95% CI"],
    [
        ["IMDC", 0.644, "0.556–0.734", 0.582, "0.475–0.676"],
        ["IMDC-NLR", 0.667, "0.590–0.751", 0.593, "0.492–0.689"],
        ["IMDC-dNLR", 0.669, "0.587–0.746", 0.598, "0.480–0.709"],
        ["IMDC-MLR", 0.661, "0.575–0.735", 0.652, "0.541–0.759"],
        ["IMDC-SIRI", 0.663, "0.576–0.744", 0.582, "0.482–0.677"],
        ["IMDC-CRP", 0.663, "0.582–0.734", 0.691, "0.589–0.771"],
        ["IMDC-NLR-MLR", 0.660, "0.578–0.741", 0.671, "0.570–0.760"],
        ["IMDC-dNLR-MLR", 0.696, "0.617–0.771", 0.670, "0.572–0.765"],
    ],
    [18, 20, 20, 20, 20],
    highlight_rows={7},
)
ws5 = wb["R2-3_regimen_nested"]
note_row = ws5.max_row + 2
note_text = ("IMPORTANT CAVEAT: these numbers test whether EACH FIXED model's cutoff/coefficients are reproducible "
             "within a regimen subgroup. They do NOT test whether MODEL SELECTION ITSELF (choosing among the 8 "
             "candidates) is stable within each regimen — that full-pipeline selection-frequency analysis has only "
             "been run on the pooled n=192 cohort (see R1-2_R3-3_full_pipeline sheet), not separately per regimen.")
ws5.cell(row=note_row, column=1, value=note_text).font = note_font
ws5.merge_cells(start_row=note_row, start_column=1, end_row=note_row, end_column=5)
ws5.row_dimensions[note_row].height = 45

# ---- Sheet 6: R2-4/R3-6 CRP comparison ----
write_sheet(
    "R2-4_R3-6_CRP_comparison",
    "R2-4 / R3-6: IMDC-dNLR-MLR vs IMDC-CRP direct comparison",
    "Paired bootstrap, B=1,000, each resample's two models evaluated out-of-resample on the original data.",
    ["Metric", "IMDC-dNLR-MLR", "IMDC-CRP"],
    [
        ["Apparent C-index", 0.7182, 0.6954],
        ["Optimism-corrected C-index (paired bootstrap)", 0.7105, 0.6805],
        ["Nested per-model corrected C-index (95% CI)", "0.698 (0.632–0.756)", "0.674 (0.606–0.732)"],
    ],
    [42, 24, 24],
)
ws6 = wb["R2-4_R3-6_CRP_comparison"]
r = ws6.max_row + 2
ws6.cell(row=r, column=1, value="Paired difference (dNLR-MLR minus CRP)").font = bold_font
ws6.cell(row=r, column=2, value="Mean = 0.025, 95% CI 0.0065 to 0.0687 (excludes 0)").font = body_font
ws6.merge_cells(start_row=r, start_column=2, end_row=r, end_column=3)

# ---- Sheet 7: R3-4 18-month sensitivity ----
write_sheet(
    "R3-4_18mo_sensitivity",
    "R3-4: Cutoff sensitivity, 18-month vs 24-month OS",
    "",
    ["Marker", "24-month cutoff (AUC)", "18-month cutoff (AUC)"],
    [
        ["NLR", "2.50 (0.629)", "2.50 (0.614)"],
        ["dNLR", "2.29 (0.680)", "2.29 (0.662)"],
        ["MLR", "0.38 (0.646)", "0.38 (0.629)"],
        ["CRP", "4.00 (0.730)", "4.00 (0.720)"],
    ],
    [16, 24, 24],
)
ws7 = wb["R3-4_18mo_sensitivity"]
r = ws7.max_row + 2
ws7.cell(row=r, column=1, value="dNLR+MLR classification agreement (18-month vs 24-month cutoffs)").font = bold_font
ws7.cell(row=r, column=2, value="100% (192/192 patients, identical group assignment)").font = body_font
ws7.merge_cells(start_row=r, start_column=2, end_row=r, end_column=3)

# ---- Sheet 8: R3-5 neutrophil overlap ----
write_sheet(
    "R3-5_neutrophil_overlap",
    "R3-5: dNLR vs IMDC neutrophilia criterion overlap",
    "",
    ["Metric", "Value"],
    [
        ["Simple agreement (dNLR-high vs IMDC neutrophilia flag)", "68.8%"],
        ["Cohen's kappa", "0.226 (fair agreement)"],
        ["IMDC-neutrophilia-positive patients (n)", "19 / 192"],
        ["Of those, also dNLR-high", "16 / 19 (84.2%)"],
        ["dNLR-high patients (n)", "73 / 192"],
        ["Of those, NOT captured by IMDC neutrophilia flag", "57 / 73 (78.1%)"],
        ["Spearman: ANC (continuous) vs dNLR (continuous)", "ρ = 0.649 (p<0.001)"],
    ],
    [50, 40],
)

wb.save("all_results_summary.xlsx")
print("saved, sheets:", wb.sheetnames)
