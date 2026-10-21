suppressMessages(library(VennDiagram))
suppressMessages(library(grid))
suppressMessages(library(ragg))

n_dNLR_total <- 21 + 52
n_MLR_total  <- 38 + 52
n_both       <- 52

FONT <- "Liberation Sans"   # Arial-metric-compatible substitute (matches uploaded Figure files, which use Arial)
COL_dNLR <- "#3B8FD6"       # blue  -- matches the study's own Figure2/3 palette
COL_MLR  <- "#E05A6E"       # red/pink -- matches the study's own Figure2/3 palette

build_grob <- function() {
  venn.diagram(
    x = list("dNLR-high" = 1:n_dNLR_total,
             "MLR-high"  = c(1:n_both, (n_dNLR_total+1):(n_dNLR_total + (n_MLR_total - n_both)))),
    filename = NULL,
    fill = c(COL_dNLR, COL_MLR),
    alpha = 0.55,
    col = "#000000",
    lwd = 1.5,
    cex = 1.4,
    fontface = "bold",
    fontfamily = FONT,
    cat.cex = 1.3,
    cat.fontface = "bold",
    cat.fontfamily = FONT,
    cat.col = c(COL_dNLR, COL_MLR),
    cat.dist = 0.05,
    margin = 0.08
  )
}

v <- build_grob()

draw_full <- function() {
  grid.newpage()
  pushViewport(viewport(y = 0.56, height = 0.82))
  grid.draw(v)
  popViewport()
  grid.text("Overlap of dNLR-high and MLR-high patients (n = 192)",
             x = 0.5, y = 0.985, gp = gpar(fontfamily = FONT, fontface = "bold", fontsize = 15, col = "black"))
  grid.text("neither = 81 (42.2%)     dNLR-only = 21 (10.9%)",
             x = 0.5, y = 0.105, gp = gpar(fontfamily = FONT, fontsize = 11.5, col = "black"))
  grid.text("MLR-only = 38 (19.8%)     both = 52 (27.1%)",
             x = 0.5, y = 0.065, gp = gpar(fontfamily = FONT, fontsize = 11.5, col = "black"))
  grid.text("Jaccard index = 0.468          Cohen's κ = 0.376",
             x = 0.5, y = 0.02, gp = gpar(fontfamily = FONT, fontsize = 11.5, col = "black"))
}

agg_png("dNLR_MLR_overlap_venn.png", width = 2100, height = 1900, res = 300, background = "white")
draw_full()
dev.off()

cairo_pdf("dNLR_MLR_overlap_venn.pdf", width = 7, height = 6.33, family = FONT)
draw_full()
dev.off()

cat("done\n")
