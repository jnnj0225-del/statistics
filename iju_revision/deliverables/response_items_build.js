const { Document, Packer, Paragraph, TextRun, HeadingLevel, Table, TableRow, TableCell,
        WidthType, ShadingType, AlignmentType, BorderStyle, VerticalAlign } = require("docx");
const fs = require("fs");

const FONT = "Yu Gothic";
const PAGE_W = 11906, PAGE_H = 16838; // A4 DXA

function h1(text) {
  return new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 400, after: 200 },
    children: [new TextRun({ text, font: FONT, bold: true, size: 32 })] });
}
function h2(text) {
  return new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 300, after: 150 },
    children: [new TextRun({ text, font: FONT, bold: true, size: 26, color: "2F5D53" })] });
}
function quote(text) {
  return new Paragraph({
    spacing: { after: 200 },
    indent: { left: 360 },
    border: { left: { style: BorderStyle.SINGLE, size: 12, color: "9BB0AB", space: 8 } },
    children: [new TextRun({ text, font: FONT, italics: true, size: 20, color: "444444" })]
  });
}
function p(text, opts={}) {
  return new Paragraph({ spacing: { after: 120 }, children: [new TextRun({ text, font: FONT, size: 21, ...opts })] });
}

function cell(text, opts={}) {
  const { bold=false, header=false, width, shade=null } = opts;
  return new TableCell({
    width: { size: width, type: WidthType.DXA },
    shading: shade ? { type: ShadingType.CLEAR, fill: shade } : undefined,
    verticalAlign: VerticalAlign.CENTER,
    margins: { top: 80, bottom: 80, left: 100, right: 100 },
    children: [new Paragraph({ children: [new TextRun({ text, font: FONT, size: 20, bold: bold || header, color: header ? "FFFFFF" : "000000" })] })]
  });
}

function itemTable(rows) {
  const widths = [2600, 3300, 3700]; // sums to 9600
  const headerRow = new TableRow({
    tableHeader: true,
    children: [
      cell("項目", { header: true, width: widths[0] }),
      cell("解析", { header: true, width: widths[1] }),
      cell("結果", { header: true, width: widths[2] }),
    ]
  });
  for (const r of rows) headerRow.root; // no-op, keep lint happy
  const dataRows = rows.map((r, i) => new TableRow({
    children: [
      cell(r[0], { width: widths[0], bold: true, shade: i % 2 === 0 ? "F2F5F3" : null }),
      cell(r[1], { width: widths[1], shade: i % 2 === 0 ? "F2F5F3" : null }),
      cell(r[2], { width: widths[2], shade: i % 2 === 0 ? "F2F5F3" : null }),
    ]
  }));
  return new Table({
    width: { size: widths.reduce((a,b)=>a+b,0), type: WidthType.DXA },
    columnWidths: widths,
    rows: [new TableRow({
      tableHeader: true,
      children: [
        cell("項目", { header: true, width: widths[0] }),
        cell("解析", { header: true, width: widths[1] }),
        cell("結果", { header: true, width: widths[2] }),
      ]
    }), ...dataRows],
    borders: {
      top: { style: BorderStyle.SINGLE, size: 4, color: "B9C2BD" },
      bottom: { style: BorderStyle.SINGLE, size: 4, color: "B9C2BD" },
      left: { style: BorderStyle.SINGLE, size: 4, color: "B9C2BD" },
      right: { style: BorderStyle.SINGLE, size: 4, color: "B9C2BD" },
      insideHorizontal: { style: BorderStyle.SINGLE, size: 2, color: "D4DAD7" },
      insideVertical: { style: BorderStyle.SINGLE, size: 2, color: "D4DAD7" },
    }
  });
}
// recolor header row fill after construction (docx-js sets shading per-cell; redo header cells with fill)
function headerFillFix(table) { return table; } // header cells already colored via cell({header:true}) text; need fill too

// Rebuild cell() header path to include teal fill
function headerCell(text, width) {
  return new TableCell({
    width: { size: width, type: WidthType.DXA },
    shading: { type: ShadingType.CLEAR, fill: "2F5D53" },
    verticalAlign: VerticalAlign.CENTER,
    margins: { top: 80, bottom: 80, left: 100, right: 100 },
    children: [new Paragraph({ children: [new TextRun({ text, font: FONT, size: 20, bold: true, color: "FFFFFF" })] })]
  });
}
function dataCell(text, width, opts={}) {
  const { bold=false, shade=null } = opts;
  return new TableCell({
    width: { size: width, type: WidthType.DXA },
    shading: shade ? { type: ShadingType.CLEAR, fill: shade } : undefined,
    verticalAlign: VerticalAlign.CENTER,
    margins: { top: 80, bottom: 80, left: 100, right: 100 },
    children: [new Paragraph({ children: [new TextRun({ text, font: FONT, size: 20, bold })] })]
  });
}
function buildTable(rows) {
  const widths = [2600, 3300, 3700];
  const header = new TableRow({ tableHeader: true, children: [
    headerCell("項目", widths[0]), headerCell("解析", widths[1]), headerCell("結果", widths[2])
  ]});
  const body = rows.map((r, i) => new TableRow({ children: [
    dataCell(r[0], widths[0], { bold: true, shade: i % 2 === 0 ? "F2F5F3" : null }),
    dataCell(r[1], widths[1], { shade: i % 2 === 0 ? "F2F5F3" : null }),
    dataCell(r[2], widths[2], { shade: i % 2 === 0 ? "F2F5F3" : null }),
  ]}));
  return new Table({
    width: { size: widths.reduce((a,b)=>a+b,0), type: WidthType.DXA },
    columnWidths: widths,
    rows: [header, ...body],
    borders: {
      top: { style: BorderStyle.SINGLE, size: 4, color: "B9C2BD" },
      bottom: { style: BorderStyle.SINGLE, size: 4, color: "B9C2BD" },
      left: { style: BorderStyle.SINGLE, size: 4, color: "B9C2BD" },
      right: { style: BorderStyle.SINGLE, size: 4, color: "B9C2BD" },
      insideHorizontal: { style: BorderStyle.SINGLE, size: 2, color: "D4DAD7" },
      insideVertical: { style: BorderStyle.SINGLE, size: 2, color: "D4DAD7" },
    }
  });
}

const r1c1_rows = [
  ["モデルの選択の根拠\n(C-index)", "Nested bootstrap\n(カットオフ・係数とも各リサンプルで再導出、B=1,000)", "IMDC-dNLR-MLR: 補正C-index 0.698\n(95%CI 0.632–0.756)、8モデル中最高"],
  ["モデル選択率", "リサンプルごとにC-indexが最高値を示したモデルの割合\n(フルパイプラインnested bootstrap、B=1,000)", "dNLR-MLR 41.3%、CRP 33.9%、\n他は各9%未満"],
  ["dNLR・MLR組み合わせの意義\n(NRI)", "Continuous NRI、24ヵ月OS", "MLR→IMDC+dNLRに追加: NRI=0.510\n(95%CI 0.123–0.851、有意)\n逆方向: NRI=0.670\n(95%CI −0.402–1.010、非有意)"],
  ["dNLR・MLR組み合わせの意義\n(IDI)", "Integrated Discrimination Improvement", "0.002(MLR追加) / 0.021(dNLR追加)、\nいずれも小さい"],
  ["ベン図(overlap確認)", "dNLR-high vs MLR-high 2×2クロス集計", "neither 81(42.2%)・dNLR-only 21(10.9%)\nMLR-only 38(19.8%)・both 52(27.1%)"],
  ["Jaccard係数", "both/(どちらか一方でも陽性)", "0.468"],
  ["Cohen's κ", "偶然補正済み一致率", "0.376(fair〜moderate)"],
];

const r1c2_rows = [
  ["Poor-lowの少数例・\n検出力不足", "Limitationに記載(追加解析なし)", "既存のLimitation記載の通り、症例数不足による\n検出力不足の可能性を明記"],
  ["外的検証", "正直に「実施不可」と記載(追加解析なし)", "外的検証コホートが確保できなかった旨を\nLimitationに明記(文面を改めて確認)"],
  ["内部検証の強化\n(ロバストネス)", "Nested bootstrap\n(カットオフ決定+マーカー選択+モデル選択を\n全てリサンプル内で再実行、B=1,000)", "選択頻度: dNLR-MLR 41.3%(最多)。\nNested補正C-index=0.680\n(95%CI 0.615–0.738)。\nモデル固定の補正値(0.705–0.711)より\nやや保守的だが優位性は維持"],
  ["(追加)モデル別の頑健性", "全8モデルについて個別にnested bootstrap", "Supplementary Table 4改訂版として提示\n(全モデルのC-index・95%CI)"],
  ["(追加)レジメン別の頑健性", "ICI-ICI/ICI-TKI部分集団ごとに\nnested bootstrap(全8モデル)", "ICI-ICIではdNLR-MLRが最高(0.696)を維持。\nICI-TKI(n=88)ではCRP(0.691)・\nNLR-MLR(0.671)がわずかに上回るが、\n全モデルのCIが大きく重複し統計的に\n明確な優劣なし"],
];

const r2c1_rows = [
  ["IMDC評価タイミングの明記", "テキスト修正のみ(追加解析なし)。感度解析は実施不可。", "“at metastatic diagnosis” → “at initiation of first-line ICI-based therapy”に統一(Methods/Results/Table 1/Figure注釈の4箇所)。IMDCは実際に治療開始時点の検査値で算出済みのため、時間的乖離自体が存在しないことを明記。"],
];

const r2c2_rows = [
  ["カットオフ決定", "時間依存ROC(24ヵ月OS、Youden指数)で試行", "AUC=0.435・183/9分割で破綻(degenerate)。文献(Ueda:中央値、Takemura:1000/μL)に合わせ中央値(0.835×10³/μL)を主解析として採用。1000/μLでの感度分析も可能。"],
  ["ALC-dNLR・ALC-MLR相関", "Spearman相関係数", "ALC vs dNLR: ρ=−0.439 (p<0.001)\nALC vs MLR: ρ=−0.491 (p<0.001)\n中等度の負相関(分母に共通してリンパ球を含むため予想通り)"],
  ["単変量・多変量解析", "Cox比例ハザードモデル(IMDC調整)", "単変量: ALC-low HR=2.03 (95%CI 1.21–3.41, p=0.008)\n多変量(+IMDC): HR=1.81 (95%CI 1.08–3.06, p=0.026)\n→ALC単独では独立した予後因子"],
  ["決定的な検証\n(dNLR-MLRモデル+ALC)", "既存のdNLR-MLR 4群モデルにALCを追加投入", "ALCのHRは1.10 (95%CI 0.62–1.93, p=0.75)に低下し有意性消失。dNLR-MLR群のHR(IH 3.72・PL 2.32・PH 7.07)はほぼ不変。\n個別項目モデル(IMDC+dNLR+MLR+ALC)でもALC HR=1.07, p=0.84"],
  ["識別能(C-index比較)", "Bootstrap(B=1,000)、apparent/corrected", "IMDC単独 0.622/0.625\nIMDC+ALC 0.664/0.659\nIMDC-dNLR-MLR 0.718/0.714\nIMDC-dNLR-MLR+ALC 0.721/0.709(ALC追加でむしろ低下)"],
];

const r2c3_rows = [
  ["プールの妥当性・\nレジメン別の一貫性", "Nested bootstrap(カットオフ・係数とも\n各リサンプルで再導出、B=1,000)を\nICI-ICI(n=104)・ICI-TKI(n=88)\nそれぞれ独立に実施、全8モデル", "dNLR-MLR補正C-index:\nICI-ICI 0.696 (95%CI 0.617–0.771、8モデル中最高)\nICI-TKI 0.670 (95%CI 0.572–0.765)\n\nICI-TKIではCRP 0.691・NLR-MLR 0.671がわずかに上回るが、全モデルの95%CIが大きく重複し統計的な優劣は認められない。レジメン別の限られたサンプルサイズでは単独でのモデル選択自体が不安定であり、これが全体コホートでプールして解析する合理性を裏付ける。"],
];

const r2c4_rows = [
  ["各モデルのC-indexと\n95%CI", "Nested bootstrap(B=1,000)、\n8モデル全てについて個別に実施", "IMDC-CRP: 補正C 0.674 (95%CI 0.606–0.732)\nIMDC-dNLR: 0.665 (0.596–0.731)\nIMDC-MLR: 0.665 (0.601–0.728)\nIMDC-dNLR-MLR: 0.698 (0.632–0.756、最高)"],
  ["直接比較\n(dNLR-MLR vs CRP)", "ペアブートストラップ(B=1,000)、\n各リサンプルで両モデルを元データに\n対して評価しC-index差を算出", "平均差 0.025、95%CI 0.0065–0.0687(ゼロを除外)\n→統計的に裏付けられた優位性"],
];

const r3c1_rows = [
  ["新規性の記載明確化", "テキスト修正のみ(追加解析なし)", "Introduction末尾・DiscussionのNovelty部分を、「新しいマーカーではなく、dNLRとMLRという2つの単一細胞系マーカーの‘組み合わせ’が新規性である」ことを明記する形に修正。"],
];

const r3c2_rows = [
  ["ガイドライン引用の更新", "文献確認", "EAU: Ljungberg B, et al. 2025 Update(確定)\nNCCN: v1.2026が存在する可能性(第三者トラッカー情報)。nccn.org上での最終確認が必要。"],
  ["IMDC評価タイミング", "テキスト修正のみ(R2-1と同一)", "R2-1と同じ4箇所の修正で同時に対応。"],
];

const r3c3_rows = [
  ["カットオフ・モデル選択\nプロセス全体の\nブートストラップ内再現", "Nested bootstrap(R1-2と同一解析、\nB=1,000)", "R1-2の結果を引用・相互参照。\n選択頻度: dNLR-MLR 41.3%(最多)。\nNested補正C-index=0.680 (95%CI 0.615–0.738)。"],
  ["外的検証", "R1-2と同様の説明", "外的検証コホートが確保できなかった旨をLimitationに明記。"],
];

const r3c4_rows = [
  ["24ヵ月カットオフの\n妥当性(中央値追跡期間\n20ヵ月との関係)", "18ヵ月時点で同じROC手法により\nカットオフを再導出し比較", "カットオフは24ヵ月版と完全一致(NLR 2.50・dNLR 2.29・MLR 0.38・CRP 4.00)。\ndNLR-MLR分類の一致率100%(192/192)。\n→24ヵ月カットオフは追跡期間を超えた外挿の産物ではないことを示す確認的解析として提示。"],
];

const r3c5_rows = [
  ["①dNLRとIMDC好中球\n成分の重複", "dNLR-high vs IMDC好中球増多基準\n(ANC>ULN)のクロス集計・\nCohen's κ・Spearman相関", "一致率68.8%、κ=0.226(弱い一致)。\ndNLR-high 73人中57人(78.1%)はIMDC好中球増多フラグでは捕捉されない。\nANC-dNLR連続値の相関: ρ=0.649 (p<0.001、関連はあるが二値フラグでは大半の情報が失われる)。\ndNLRはIMDC調整後も多変量解析で独立に有意(Supplementary Table 3)。"],
  ["②Dose-response\n(1個 vs 2個陽性)", "テキストで説明(追加解析なし)", "Reviewer 1と同じ論点。Discussionで既に言及しているTAM-TAN相互作用(両マーカー同時陽性が必須でなくとも同じ免疫抑制状態を反映しうる)の段落を明示的に参照する形で対応。"],
];

const r3c6_rows = [
  ["CRPとの比較", "R2-4と同一解析(相互参照)", "差 0.025、95%CI 0.0065–0.0687(ゼロを除外)。R2-4の結果を引用し、重複執筆を避ける。"],
];

const minor_rows = [
  ["Minor 1: mccRCC→mRCC", "テキスト修正のみ", "Patient population部の“293 patients with mccRCC” → “293 patients with mRCC”(非淡明細胞型除外前のため)。後出の192例の箇所は“mccRCC”のまま維持(除外後のため正しい)。"],
  ["Minor 2: predefined cutoffs", "テキスト修正のみ", "“predefined cutoffs” → “data-derived cutoffs”(またはそれに類する表現)に、Abstract・Methods・Resultsの該当箇所全てで統一。"],
];

const doc = new Document({
  sections: [{
    properties: { page: { size: { width: PAGE_W, height: PAGE_H }, margin: { top: 1000, bottom: 1000, left: 1000, right: 1000 } } },
    children: [
      new Paragraph({ heading: HeadingLevel.TITLE, spacing: { after: 100 },
        children: [new TextRun({ text: "IJU Major Revision — 回答すべき項目と解析", font: FONT, bold: true, size: 36 })] }),
      new Paragraph({ spacing: { after: 300 },
        children: [new TextRun({ text: "Manuscript 4196880 ・ Reviewer 1–3 + Minor comments", font: FONT, size: 20, color: "666666" })] }),

      h1("Reviewer 1 — Comment 1"),
      quote("The added value of combining dNLR and MLR is unclear. The candidate models showed similar discriminatory performance, and the IMDC-dNLR-MLR model was selected mainly because it had the numerically highest C-index. Moreover, survival did not clearly differ between patients with one versus two elevated markers. Therefore, it remains uncertain whether combining dNLR and MLR provides meaningful prognostic improvement over either marker alone."),
      h2("回答すべき項目と解析"),
      buildTable(r1c1_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 1 — Comment 2"),
      quote("The robustness and generalizability of the proposed classification are limited. Some subgroups, particularly the poor-low group, included only a small number of patients, resulting in imprecise estimates. In addition, the proposed classification has not been validated in an independent cohort. Therefore, the claim of clinical applicability appears premature."),
      h2("回答すべき項目と解析"),
      buildTable(r1c2_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 2 — Comment 1"),
      quote("The timing of the IMDC risk assessment and the measurement of inflammatory markers requires clarification. According to the Methods, the IMDC classification was determined at the time of metastatic diagnosis, whereas inflammatory markers were measured within one month before initiation of ICI-based therapy... Therefore, the authors should clarify whether the IMDC classification was recalculated using clinical and laboratory data obtained at the initiation of ICI-based therapy."),
      h2("回答すべき項目と解析"),
      buildTable(r2c1_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 2 — Comment 2"),
      quote("dNLR and MLR share overlapping hematologic components, particularly lymphocyte-related information. Therefore, it remains unclear whether the prognostic value of the combined dNLR–MLR classification reflects complementary biological information or simply the effect of lymphopenia... The authors should evaluate the prognostic impact of ALC and compare its discriminatory performance with those of dNLR, MLR, and the combined dNLR–MLR model. In addition, correlations among ALC, dNLR, and MLR should be presented."),
      h2("回答すべき項目と解析"),
      buildTable(r2c2_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 2 — Comment 3"),
      quote("The inclusion of patients treated with ICI-ICI and ICI-TKI in a single cohort requires cautious interpretation... The authors should provide a clearer rationale for pooling these two treatment strategies and further evaluate whether the prognostic performance of the proposed model is consistent in the ICI-ICI and ICI-TKI cohorts separately."),
      h2("回答すべき項目と解析"),
      buildTable(r2c3_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 2 — Comment 4"),
      quote("The rationale for selecting the dNLR–MLR combination as the final model requires further clarification. Among the individual inflammatory markers evaluated in this study, CRP showed the highest AUC for 24-month OS (0.730), exceeding those of dNLR (0.680) and MLR (0.646)... The authors should provide a direct comparison of the prognostic performance of IMDC+CRP, IMDC+dNLR, IMDC+MLR, and IMDC+dNLR+MLR, including appropriate measures of discrimination and their confidence intervals."),
      h2("回答すべき項目と解析"),
      buildTable(r2c4_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 3 — Comment 1"),
      quote("The novelty of the proposed model should be more clearly defined... The novelty of the present study therefore appears to lie mainly in the combination of dNLR and MLR in a Japanese clear-cell RCC cohort. The Introduction and Discussion should be revised accordingly."),
      h2("回答すべき項目と解析"),
      buildTable(r3c1_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 3 — Comment 2"),
      quote("The current role of IMDC and the guideline references should be updated and clarified... the cited guidelines are relatively outdated (EAU 2022 and NCCN version 2.2024)... Please clarify the timing of IMDC assessment and justify combining variables obtained at different time points."),
      h2("回答すべき項目と解析"),
      buildTable(r3c2_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 3 — Comment 3"),
      quote("The risk-model development is highly data-driven and may be prone to overfitting. Cutoffs were derived from the same cohort using 24-month OS, markers were selected according to their association with OS, and the model with the highest C-index was then chosen. Please clarify whether the entire cutoff- and model-selection process was repeated within each bootstrap resample. External validation is required before clinical applicability can be claimed."),
      h2("回答すべき項目と解析"),
      buildTable(r3c3_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 3 — Comment 4"),
      quote("The rationale for using 24-month OS to determine cutoffs should be explained. The median follow-up was only 20 months, yet the cutoff values were optimized for 24-month OS... the choice of a time point beyond the median follow-up requires further justification or sensitivity analysis."),
      h2("回答すべき項目と解析"),
      buildTable(r3c4_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 3 — Comment 5"),
      quote("The potential overlap between IMDC and dNLR should be addressed. Because neutrophil count is already included in the IMDC classification, dNLR may partly duplicate existing prognostic information. ①The authors should clarify the relationship between dNLR and the neutrophil component of IMDC and demonstrate whether dNLR provides independent incremental prognostic value. ②The absence of a clear dose-response relationship between one versus two elevated markers also raises the question of whether both markers are necessary."),
      h2("回答すべき項目と解析"),
      buildTable(r3c5_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Reviewer 3 — Comment 6"),
      quote("The clinical advantage of the dNLR-MLR model over simpler markers should be clarified. CRP showed a higher individual discriminatory performance than dNLR or MLR, yet the proposed model combines two hematologic markers. The authors should explain whether the added complexity provides a meaningful improvement over simpler markers or models."),
      h2("回答すべき項目と解析"),
      buildTable(r3c6_rows),

      new Paragraph({ children: [new TextRun({ text: "", font: FONT })], pageBreakBefore: true }),
      h1("Minor Comments"),
      quote("“293 patients with mccRCC” appears inconsistent because patients with non-clear-cell histology and no metastasis were subsequently excluded. This may need to be corrected to “mRCC.” / The term “predefined cutoffs” is inappropriate because the cutoffs were derived from the present dataset using ROC analysis. Please revise the wording."),
      h2("回答すべき項目と解析"),
      buildTable(minor_rows),
    ]
  }]
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync("Reviewer1-3_items_and_analysis.docx", buf);
  console.log("done");
});
