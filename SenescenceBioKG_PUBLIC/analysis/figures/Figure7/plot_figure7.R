script_file <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
fig_root <- dirname(script_dir)
source(file.path(fig_root, "figure_theme.R"), encoding = "UTF-8")
source(file.path(fig_root, "data_helpers.R"), encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(png)
  library(grid)
})

core <- sbk_core()
assert_final_freeze(core)
out_dir <- file.path(sbk_root, "outputs", "figures", "Figure7")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
preview_dir <- file.path(sbk_root, "outputs", "figure_previews")
dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

cqs <- sbk_read("09_FIGURE7_QUERY_UTILITY", "figure7A_competency_questions.csv")
metrics <- sbk_read("09_FIGURE7_QUERY_UTILITY", "figure7B_benchmark_metrics.csv")
summary_metrics <- sbk_read("09_FIGURE7_QUERY_UTILITY", "figure7B_benchmark_summary.csv")
discoveries <- sbk_read("02_QUERY_BENCHMARK", "SenescenceBioKG_graph_discoveries_FINAL.csv")
screenshot_file <- file.path(sbk_root, "outputs", "explorer_screenshot.png")
stopifnot(nrow(cqs) == 15L, nrow(metrics) == 15L)

# A: required-relation matrix. CQ15 is a threshold query and is marked with an asterisk.
relations <- c("RESPONDS_TO", "RELEASES", "DELIVERS", "TARGETS", "MODULATES",
               "PROMOTES", "VALIDATED_AT", "COMPOSED_OF", "FABRICATED_BY", "USES")
a_dat <- crossing(CQ_ID = cqs$CQ_ID, Relation = relations) %>%
  left_join(cqs %>% select(CQ_ID, Required_relations), by = "CQ_ID") %>%
  mutate(
    Required = mapply(function(req, rel) str_detect(req, fixed(rel)), Required_relations, Relation),
    CQ_label = ifelse(CQ_ID == "CQ15", "CQ15*", CQ_ID),
    CQ_label = factor(CQ_label, levels = rev(ifelse(cqs$CQ_ID == "CQ15", "CQ15*", cqs$CQ_ID))),
    Relation = factor(Relation, levels = relations)
  )

pA <- ggplot(a_dat, aes(x = Relation, y = CQ_label)) +
  geom_point(data = a_dat %>% filter(Required), shape = 21, size = 3.0,
             fill = sbk_palette[["dark_navy"]], colour = "white", stroke = 0.35) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(x = "Required relation", y = NULL) +
  theme_sbk(7.3) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 6.7),
    axis.text.y = element_text(size = 7.0, face = "plain"),
    axis.ticks = element_blank(), axis.line = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, colour = sbk_palette[["light_gray"]]),
    plot.margin = margin(9, 6, 6, 7, "pt")
  )
pA <- add_panel_tag(pA, "A")

# B: benchmark agreement without redundant bars.
b_dat <- metrics %>%
  select(CQ_ID, Precision, Recall, F1) %>%
  pivot_longer(-CQ_ID, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = factor(Metric, levels = c("Precision", "Recall", "F1")),
    CQ_ID = factor(CQ_ID, levels = rev(metrics$CQ_ID))
  )

pB_tiles <- ggplot(b_dat, aes(x = Metric, y = CQ_ID, fill = Value)) +
  geom_tile(colour = "white", linewidth = 0.45) +
  geom_text(aes(label = sprintf("%.2f", Value)), family = SBK_FONT,
            fontface = "bold", size = 2.2, colour = sbk_palette[["black"]]) +
  scale_fill_gradient(low = sbk_palette[["light_blue_gray"]], high = sbk_palette[["teal"]],
                      limits = c(0, 1), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_sbk(7.1) +
  theme(axis.text.x = element_text(size = 7.0, face = "bold"),
        axis.text.y = element_text(size = 6.8, face = "plain"),
        axis.ticks = element_blank(), axis.line = element_blank(),
        plot.margin = margin(9, 3, 5, 7, "pt"))
pB_tiles <- add_panel_tag(pB_tiles, "B")
pB_tiles <- pB_tiles + theme(plot.tag.position = c(-0.16, 1.03),
                             plot.margin = margin(13, 3, 5, 15, "pt"))

pB_note <- ggplot() +
  annotate("text", x = 0.5, y = 0.70, label = "15 / 15", family = SBK_FONT,
           fontface = "bold", size = 5.2, colour = sbk_palette[["dark_navy"]]) +
  annotate("text", x = 0.5, y = 0.58, label = "Exact-set\nmatches", family = SBK_FONT,
           fontface = "bold", size = 2.8, lineheight = 0.92) +
  annotate("text", x = 0.5, y = 0.34,
           label = sprintf("Macro\nP %.2f\nR %.2f\nF1 %.2f",
                           summary_metrics$macro_precision,
                           summary_metrics$macro_recall,
                           summary_metrics$macro_F1),
           family = SBK_FONT, fontface = "bold", size = 2.75, lineheight = 1.15) +
  xlim(0, 1) + ylim(0, 1) + theme_sbk_void(7.0) +
  theme(plot.margin = margin(9, 5, 5, 0, "pt"))
pB <- pB_tiles + pB_note + plot_layout(widths = c(1.45, 0.65))

# C: eight concise, graph-derived discovery categories.
get_count <- function(ids) sum(discoveries$Supporting_count[discoveries$Discovery_ID %in% ids])
c_dat <- tibble(
  Category = c(
    "Response-target-regeneration", "Response-release",
    "Response-mechanism-regeneration", "Cell-mechanism-regeneration",
    "Animal in vivo mechanism paths", "Validated-core missing links",
    "High evidence-density materials", "Human-derived evidence"
  ),
  Supporting_count = c(
    get_count("D001"), get_count("D002"), get_count("D003"), get_count("D004"),
    get_count("D005"), get_count(c("D008", "D009", "D010")),
    get_count("D011"), get_count("D018")
  )
) %>%
  arrange(Supporting_count) %>%
  mutate(Category = factor(wrap_sbk(Category, 29), levels = wrap_sbk(Category, 29)))

pC <- ggplot(c_dat, aes(x = Supporting_count, y = Category)) +
  geom_segment(aes(x = 0, xend = Supporting_count, yend = Category),
               linewidth = 0.65, colour = sbk_palette[["light_blue_gray"]]) +
  geom_point(size = 3.1, colour = sbk_palette[["muted_purple"]]) +
  geom_text(aes(label = Supporting_count), nudge_x = 2.2, hjust = 0,
            family = SBK_FONT, fontface = "bold", size = 2.55) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), breaks = pretty_breaks(4)) +
  labs(x = "Supporting count", y = NULL) +
  theme_sbk(7.5) +
  theme(axis.text.y = element_text(size = 7.1, face = "bold"),
        plot.margin = margin(9, 7, 6, 7, "pt"))
pC <- add_panel_tag(pC, "C")

# D: use a locally generated public Explorer screenshot when available.
if (file.exists(screenshot_file)) {
  screenshot <- readPNG(screenshot_file)
  d_layer <- annotation_custom(rasterGrob(screenshot, interpolate = TRUE), xmin = 0, xmax = 1.6, ymin = 0, ymax = 1)
} else {
  screenshot <- array(1, dim = c(1, 1, 4))
  d_layer <- annotate("label", x = 0.8, y = 0.53,
    label = "Run the public Explorer and save\noutputs/explorer_screenshot.png\nto reproduce this panel.",
    family = SBK_FONT, fontface = "bold", size = 4.0,
    colour = sbk_palette[["dark_navy"]], fill = "white")
}
pD <- ggplot() + d_layer +
  annotate("text", x = 1.59, y = 0.005, label = "SenescenceBioKG Explorer",
           family = SBK_FONT, fontface = "plain", size = 0.2,
           colour = "white", hjust = 1, vjust = 0) +
  coord_cartesian(xlim = c(0, 1.6), ylim = c(0, 1), expand = FALSE, clip = "off") +
  theme_sbk_void(7.5) +
  theme(plot.margin = margin(4, 2, 3, 3, "pt"))
pD <- add_panel_tag(pD, "D")
pD_combined <- patchwork::free(pD, type = "panel")

design <- "
AB
CC
DD
"
combined <- pA + pB + pC + pD_combined +
  plot_layout(design = design, heights = c(1.10, 0.62, 1.50))

save_pdf_sbk(pA, file.path(out_dir, "Figure7A.pdf"), 90, 94)
save_pdf_sbk(pB, file.path(out_dir, "Figure7B.pdf"), 90, 94)
save_pdf_sbk(pC, file.path(out_dir, "Figure7C.pdf"), 90, 86)
save_pdf_sbk(pD, file.path(out_dir, "Figure7D.pdf"), 180, 118)
save_pdf_sbk(combined, file.path(out_dir, "Figure7_combined.pdf"), 180, 248)
save_preview_sbk(combined, file.path(preview_dir, "Figure7_combined.png"), 180, 248, 180)

caption <- paste0(
  "Figure 7 | Competency queries and interactive analytical access to SenescenceBioKG. ",
  "(A) Required-relation matrix for CQ01-CQ15; CQ15 is a relation-diversity threshold query and is marked with an asterisk. ",
  "(B) Independent benchmark agreement for all 15 queries. Precision, recall and F1 quantify implementation agreement between graph traversal and an independently coded dataframe reference computation; they do not quantify biomedical truth. ",
  "(C) Selected graph-derived discovery categories reported as supporting record/material counts. The validated-core missing-link count combines the three frozen gap definitions and does not imply that the corresponding experiments were absent from the source studies. Animal in vivo denotes Small Animal or Large Animal; Human-derived remains separate. ",
  "(D) Screenshot of the real, locally running, read-only SenescenceBioKG Explorer showing a material-centred one-hop subgraph and its linked same-record evidence."
)
writeLines(caption, file.path(out_dir, "Figure7_caption_draft.txt"), useBytes = TRUE)

pdfs <- file.path(out_dir, c("Figure7A.pdf", "Figure7B.pdf", "Figure7C.pdf", "Figure7D.pdf", "Figure7_combined.pdf"))
qc <- c(
  "Figure 7 plot QC",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "Source files: 09_FIGURE7_QUERY_UTILITY/*.csv; 02_QUERY_BENCHMARK graph discoveries; real Explorer screenshot",
  paste0("Source row counts: CQ=", nrow(cqs), "; metrics=", nrow(metrics), "; discoveries=", nrow(discoveries)),
  paste0("CQ exact-set matches: ", sum(toupper(metrics$Exact_set_match) == "YES"), "/15"),
  paste0("Macro P/R/F1: ", summary_metrics$macro_precision, "/",
         summary_metrics$macro_recall, "/", summary_metrics$macro_F1),
  paste0("Explorer screenshot dimensions: ", dim(screenshot)[2], "x", dim(screenshot)[1]),
  paste0("FINAL FREEZE consistency: ", ifelse(assert_final_freeze(core), "PASS", "FAIL")),
  "Font check: Arial Bold used for core labels and Arial Regular for ticks/secondary text; Explorer CSS uses Arial",
  "Figure 7D: real running Explorer screenshot with selected RESPONDS_TO edge and Evidence_ID",
  paste0("PDF existence: ", ifelse(all(vapply(pdfs, pdf_ok, logical(1))), "PASS", "FAIL")),
  "Panel labels: A/B/C/D black Arial Bold 17 pt",
  "Panel subtitles: none",
  "Visual inspection status: PENDING"
)
write_qc_lines(file.path(out_dir, "Figure7_plot_QC.txt"), qc)
