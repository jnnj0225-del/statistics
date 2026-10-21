const pptxgen = require("pptxgenjs");

const pres = new pptxgen();
pres.layout = "LAYOUT_WIDE"; // 13.333 x 7.5 in, matches uploaded Figure files (12192000 x 6858000 EMU)

const FONT = "Arial";
const COL_dNLR = "3B8FD6"; // blue  - matches uploaded Figure2/3 palette
const COL_MLR  = "E05A6E"; // red/pink - matches uploaded Figure2/3 palette
const BLACK = "000000";

const slide = pres.addSlide();
slide.background = { color: "FFFFFF" };

// Title
slide.addText("Overlap of dNLR-high and MLR-high patients (n = 192)", {
  x: 0.5, y: 0.35, w: 12.33, h: 0.6,
  fontFace: FONT, fontSize: 22, bold: true, color: BLACK, align: "center", isTextBox: true, margin: 0
});

// Two overlapping ellipses (Venn circles)
const cy = 3.85;        // vertical center of circles
const r = 2.3;          // radius of each circle
const sep = 2.6;        // distance between the two circle centers (< 2r, so they overlap)
const cx0 = 6.6665;     // horizontal center of the slide
const mlrCx = cx0 - sep / 2;   // MLR circle center x (left)
const dnlrCx = cx0 + sep / 2;  // dNLR circle center x (right)
const ellW = r * 2, ellH = r * 2;

// MLR circle (left)
slide.addShape(pres.ShapeType.ellipse, {
  x: mlrCx - r, y: cy - r, w: ellW, h: ellH,
  fill: { color: COL_MLR, transparency: 45 },
  line: { color: BLACK, width: 1.5 }
});

// dNLR circle (right)
slide.addShape(pres.ShapeType.ellipse, {
  x: dnlrCx - r, y: cy - r, w: ellW, h: ellH,
  fill: { color: COL_dNLR, transparency: 45 },
  line: { color: BLACK, width: 1.5 }
});

// Count labels
const lblW = 1.2;
slide.addText("38", { x: mlrCx - r * 0.55 - lblW / 2, y: cy - 0.35, w: lblW, h: 0.7, fontFace: FONT, fontSize: 26, bold: true, color: BLACK, align: "center", isTextBox: true, margin: 0 });
slide.addText("52", { x: cx0 - lblW / 2, y: cy - 0.35, w: lblW, h: 0.7, fontFace: FONT, fontSize: 26, bold: true, color: BLACK, align: "center", isTextBox: true, margin: 0 });
slide.addText("21", { x: dnlrCx + r * 0.55 - lblW / 2, y: cy - 0.35, w: lblW, h: 0.7, fontFace: FONT, fontSize: 26, bold: true, color: BLACK, align: "center", isTextBox: true, margin: 0 });

// Set labels
slide.addText("MLR-high", { x: mlrCx - r - 0.1, y: cy + r - 0.45, w: 2.3, h: 0.5, fontFace: FONT, fontSize: 20, bold: true, color: COL_MLR, align: "left", isTextBox: true, margin: 0 });
slide.addText("dNLR-high", { x: dnlrCx + r - 2.2, y: cy + r - 0.45, w: 2.3, h: 0.5, fontFace: FONT, fontSize: 20, bold: true, color: COL_dNLR, align: "right", isTextBox: true, margin: 0 });

// Footer stats, two lines, below the circles
const footY = cy + r + 0.35;
slide.addText("neither = 81 (42.2%)     dNLR-only = 21 (10.9%)     MLR-only = 38 (19.8%)     both = 52 (27.1%)", {
  x: 0.5, y: footY, w: 12.33, h: 0.4, fontFace: FONT, fontSize: 14, color: BLACK, align: "center", isTextBox: true, margin: 0
});
slide.addText("Jaccard index = 0.468          Cohen’s κ = 0.376", {
  x: 0.5, y: footY + 0.42, w: 12.33, h: 0.4, fontFace: FONT, fontSize: 14, color: BLACK, align: "center", isTextBox: true, margin: 0
});

pres.writeFile({ fileName: "dNLR_MLR_overlap_venn.pptx" }).then(() => console.log("done"));
