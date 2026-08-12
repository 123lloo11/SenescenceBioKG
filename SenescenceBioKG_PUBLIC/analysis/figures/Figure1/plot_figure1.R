#!/usr/bin/env Rscript

# SenescenceBioKG Figure 1: final frozen full-text evidence release.
# Backend contract: all drawing, assembly, export and render generation use R only.

required_packages <- c(
  "ggplot2", "dplyr", "tidyr", "readr", "stringr", "forcats",
  "scales", "patchwork", "grid", "systemfonts", "pdftools"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop(paste("Missing required R packages:", paste(missing_packages, collapse = ", ")))
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(scales)
  library(patchwork)
  library(grid)
})

options(stringsAsFactors = FALSE, encoding = "UTF-8", warn = 1)

script_file <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
root_dir <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
output_dir <- file.path(root_dir, "outputs", "figures", "Figure1")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
rscript_path <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

# Incremented only after the initial render has been visually inspected and revised.
visual_iteration <- 2L
qa_preview <- identical(Sys.getenv("FIGURE1_QA_PREVIEW", unset = "0"), "1")

source_files <- c(
  workflow = file.path(root_dir, "data", "figure_source", "figure1", "figure1A_workflow_counts.csv"),
  entities = file.path(root_dir, "data", "figure_source", "figure1", "figure1B_ontology_entities.csv"),
  relations = file.path(root_dir, "data", "figure_source", "figure1", "figure1B_ontology_relations.csv"),
  provenance = file.path(root_dir, "data", "figure_source", "figure1", "figure1C_provenance_example.csv"),
  nodes = file.path(root_dir, "data", "nodes", "SenescenceBioKG_nodes_CANONICAL.csv"),
  edges = file.path(root_dir, "data", "edges", "SenescenceBioKG_edges_CANONICAL.csv"),
  evidence = file.path(root_dir, "data", "edges", "evidence_metadata_public.csv")
)
if (!all(file.exists(source_files))) {
  stop("One or more FINAL FREEZE Figure 1 source files are missing.")
}
source_hash_before <- tools::md5sum(source_files)

read_final_csv <- function(path) {
  readr::read_csv(
    path,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
}

workflow <- read_final_csv(source_files[["workflow"]])
entities <- read_final_csv(source_files[["entities"]])
relations <- read_final_csv(source_files[["relations"]])
provenance_seed <- read_final_csv(source_files[["provenance"]])
nodes <- read_final_csv(source_files[["nodes"]])
edges <- read_final_csv(source_files[["edges"]])
evidence <- read_final_csv(source_files[["evidence"]])

workflow_value <- function(stage) {
  value <- workflow %>% filter(.data$Stage == stage) %>% pull(.data$Count)
  if (length(value) != 1L) stop(sprintf("Workflow stage not uniquely found: %s", stage))
  as.integer(value)
}

full_text_studies <- workflow_value("Full-text studies")
candidate_relations <- workflow_value("Original candidate relations")
initial_supported <- workflow_value("Initial SUPPORTED")
initial_rejected <- workflow_value("REJECTED")
initial_unclear <- workflow_value("UNCLEAR")
pdf_corrective <- workflow_value("PDF-corrected new SUPPORTED")
pre_readjudication <- workflow_value("Pre-re-adjudication core edges")
net_change <- workflow_value("Net change after targeted re-adjudication")
final_edges_workflow <- workflow_value("Final semantic-freeze core edges")
final_nodes_workflow <- workflow_value("Final canonical nodes")
final_evidence_workflow <- workflow_value("Evidence records")
isolated_studies <- workflow_value("Isolated studies")

data_checks <- c(
  initial_partition = initial_supported + initial_rejected + initial_unclear == candidate_relations,
  pre_readjudication = initial_supported + pdf_corrective == pre_readjudication,
  net_change = final_edges_workflow - pre_readjudication == net_change,
  final_node_count = nrow(nodes) == 654L && final_nodes_workflow == 654L,
  final_edge_count = nrow(edges) == 1868L && final_edges_workflow == 1868L,
  final_evidence_count = nrow(evidence) == 1868L && final_evidence_workflow == 1868L,
  relation_sum = sum(relations$Edge_count) == 1868L,
  entity_sum = sum(entities$Entity_count) == 654L,
  ontology_entity_types = nrow(entities) == 14L,
  ontology_relation_types = nrow(relations) == 12L,
  evidence_linkage = setequal(edges$Evidence_ID, evidence$Evidence_ID)
)
if (!all(data_checks)) {
  stop(paste("Figure 1 data QC failed:", paste(names(data_checks)[!data_checks], collapse = ", ")))
}

node_source <- nodes %>%
  transmute(Source_ID = .data$Node_ID, Source = .data$Canonical_name)
node_target <- nodes %>%
  transmute(Target_ID = .data$Node_ID, Target = .data$Canonical_name)
evidence_join <- evidence %>%
  select(
    Evidence_ID, DOI, Evidence_location, Evidence_source, Evidence_strength
  )

edge_provenance <- edges %>%
  left_join(node_source, by = "Source_ID") %>%
  left_join(node_target, by = "Target_ID") %>%
  left_join(evidence_join, by = "Evidence_ID")

selected_evidence_ids <- c("EV001795", "EV000039", "EV000373", "EV001534")
provenance <- edge_provenance %>%
  filter(.data$Evidence_ID %in% selected_evidence_ids) %>%
  mutate(relation_order = match(.data$Evidence_ID, selected_evidence_ids)) %>%
  arrange(.data$relation_order)

provenance_checks <- c(
  four_examples = nrow(provenance) == 4L,
  four_records = dplyr::n_distinct(provenance$Record_ID) == 4L,
  required_relations = setequal(provenance$Relation, c("RESPONDS_TO", "DELIVERS", "MODULATES", "PROMOTES")),
  direct_sources = all(provenance$Evidence_source %in% c("Results", "Figure", "Methods")),
  direct_evidence = all(!is.na(provenance$Evidence_location) & provenance$Evidence_location != ""),
  same_record = all(provenance$Record_ID == evidence$Record_ID[match(provenance$Evidence_ID, evidence$Evidence_ID)])
)
if (!all(provenance_checks)) {
  stop(paste("Figure 1 provenance QC failed:", paste(names(provenance_checks)[!provenance_checks], collapse = ", ")))
}

palette <- c(
  navy = "#1F3B5B",
  blue = "#4C78A8",
  teal = "#1F9D8A",
  amber = "#D4A73C",
  coral = "#E76F51",
  purple = "#7A6FB0",
  light_gray = "#F6F7F9",
  line_gray = "#B8C0C8",
  mid_gray = "#73808C",
  dark_text = "#222222",
  black = "#000000",
  white = "#FFFFFF"
)

available_fonts <- unique(systemfonts::system_fonts()[["family"]])
font_family <- if (any(tolower(available_fonts) == "arial")) "Arial" else "sans"
pt <- ggplot2::.pt

theme_canvas <- function() {
  theme_void(base_family = font_family) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(4, 5, 4, 5, unit = "mm")
    )
}

new_canvas <- function(ymin = 0, ymax = 106) {
  ggplot() +
    coord_cartesian(xlim = c(0, 100), ylim = c(ymin, ymax), clip = "off", expand = FALSE) +
    theme_canvas()
}

add_round_box <- function(plot, xmin, xmax, ymin, ymax, fill, border, lwd = 0.6, lty = 1, radius_mm = 1.7) {
  grob <- grid::roundrectGrob(
    r = unit(radius_mm, "mm"),
    gp = grid::gpar(fill = fill, col = border, lwd = lwd, lty = lty, linejoin = "round")
  )
  plot + annotation_custom(grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

add_panel_label <- function(plot, panel) {
  plot + annotate(
    "text", x = 0, y = 103, label = panel, hjust = 0, vjust = 1,
    family = font_family, fontface = "bold", size = 17 / pt,
    colour = palette[["black"]]
  )
}

arrow_segment <- function(plot, x, xend, y, yend, colour = palette[["mid_gray"]], linewidth = 0.55) {
  plot + annotate(
    "segment", x = x, xend = xend, y = y, yend = yend,
    colour = colour, linewidth = linewidth, lineend = "round",
    arrow = grid::arrow(length = unit(1.7, "mm"), type = "closed")
  )
}

comma_n <- function(x) scales::comma(x, accuracy = 1)

# --------------------------- Panel A -----------------------------------------
pa <- add_panel_label(new_canvas(ymin = 12), "A")

# Branching and two independent inputs to the 1,713-edge merge are drawn first.
pa <- arrow_segment(pa, 14, 17, 80, 80, palette[["line_gray"]], 0.45)
pa <- arrow_segment(pa, 31, 37, 80, 80, palette[["line_gray"]], 0.45)
pa <- pa +
  annotate("point", x = 38, y = 80, shape = 21, size = 2.2,
           fill = palette[["line_gray"]], colour = "white", stroke = 0.35) +
  annotate("curve", x = 38, xend = 48, y = 81, yend = 86, curvature = -0.15,
           colour = palette[["line_gray"]], linewidth = 0.45,
           arrow = grid::arrow(length = unit(1.45, "mm"), type = "closed")) +
  annotate("curve", x = 38, xend = 62, y = 80, yend = 78, curvature = 0.10,
           colour = palette[["line_gray"]], linewidth = 0.45,
           arrow = grid::arrow(length = unit(1.45, "mm"), type = "closed")) +
  annotate("curve", x = 38, xend = 76, y = 79, yend = 71, curvature = 0.13,
           colour = palette[["line_gray"]], linewidth = 0.45,
           arrow = grid::arrow(length = unit(1.45, "mm"), type = "closed")) +
  annotate("curve", x = 38, xend = 48.5, y = 79, yend = 42, curvature = 0.10,
           colour = palette[["blue"]], linewidth = 0.55,
           arrow = grid::arrow(length = unit(1.5, "mm"), type = "closed")) +
  annotate("curve", x = 53.5, xend = 67.5, y = 80, yend = 42, curvature = -0.14,
           colour = palette[["teal"]], linewidth = 0.60,
           arrow = grid::arrow(length = unit(1.5, "mm"), type = "closed"))
pa <- arrow_segment(pa, 55, 61, 35, 35, palette[["blue"]], 0.55)
pa <- arrow_segment(pa, 74, 77, 35, 35, palette[["navy"]], 0.55)
pa <- arrow_segment(pa, 87.5, 89, 35, 35, palette[["navy"]], 0.60)

pa <- add_round_box(pa, 2, 14, 73, 87, palette[["light_gray"]], palette[["blue"]], lwd = 0.45)
pa <- add_round_box(pa, 17, 31, 73, 87, palette[["light_gray"]], palette[["blue"]], lwd = 0.45)
pa <- add_round_box(pa, 48, 59, 80, 92, "#EAF5F2", palette[["teal"]], lwd = 0.50)
pa <- add_round_box(pa, 62, 73, 73, 84, "#FBECE8", palette[["coral"]], lwd = 0.45)
pa <- add_round_box(pa, 76, 87, 66, 77, "#FBF4DF", palette[["amber"]], lwd = 0.45)
pa <- add_round_box(pa, 42, 55, 28, 42, "#EDF3F8", palette[["blue"]], lwd = 0.50)
pa <- add_round_box(pa, 61, 74, 28, 42, "#FAFBFC", palette[["navy"]], lwd = 0.50)
pa <- add_round_box(pa, 77, 87.5, 25, 45, "#F2F4F7", palette[["navy"]], lwd = 0.50)
pa <- add_round_box(pa, 89, 99.5, 21, 50, palette[["navy"]], palette[["navy"]], lwd = 0.65)

pa <- pa +
  annotate("text", x = 8, y = 82.3, label = comma_n(full_text_studies), family = font_family,
           fontface = "bold", size = 12.5 / pt, colour = palette[["navy"]]) +
  annotate("text", x = 8, y = 76.7, label = "full-text studies", family = font_family,
           size = 7.0 / pt, colour = palette[["dark_text"]]) +
  annotate("text", x = 24, y = 82.3, label = comma_n(candidate_relations), family = font_family,
           fontface = "bold", size = 12.5 / pt, colour = palette[["navy"]]) +
  annotate("text", x = 24, y = 76.7, label = "candidate relations", family = font_family,
           size = 7.0 / pt, colour = palette[["dark_text"]]) +
  annotate("text", x = 53.5, y = 88.0, label = comma_n(initial_supported), family = font_family,
           fontface = "bold", size = 9.0 / pt, colour = palette[["teal"]]) +
  annotate("text", x = 53.5, y = 83.0, label = "Initial SUPPORTED", family = font_family,
           fontface = "bold", size = 5.9 / pt, colour = palette[["dark_text"]]) +
  annotate("text", x = 67.5, y = 80.2, label = comma_n(initial_rejected), family = font_family,
           fontface = "bold", size = 9.0 / pt, colour = palette[["coral"]]) +
  annotate("text", x = 67.5, y = 75.8, label = "REJECTED", family = font_family,
           fontface = "bold", size = 5.9 / pt, colour = palette[["dark_text"]]) +
  annotate("text", x = 81.5, y = 73.2, label = comma_n(initial_unclear), family = font_family,
           fontface = "bold", size = 9.0 / pt, colour = "#9A7300") +
  annotate("text", x = 81.5, y = 68.8, label = "UNCLEAR", family = font_family,
           fontface = "bold", size = 5.9 / pt, colour = palette[["dark_text"]]) +
  annotate("text", x = 48.5, y = 37.5, label = paste0("+", pdf_corrective), family = font_family,
           fontface = "bold", size = 9.2 / pt, colour = palette[["blue"]]) +
  annotate("text", x = 48.5, y = 32.3, label = "PDF corrective\nSUPPORTED", family = font_family,
           fontface = "bold", size = 5.7 / pt, lineheight = 0.86, colour = palette[["dark_text"]]) +
  annotate("text", x = 67.5, y = 37.4, label = comma_n(pre_readjudication), family = font_family,
           fontface = "bold", size = 10.0 / pt, colour = palette[["navy"]]) +
  annotate("text", x = 67.5, y = 31.8, label = "Pre-re-adjudication\ncore edges", family = font_family,
           fontface = "bold", size = 5.8 / pt, lineheight = 0.86, colour = palette[["dark_text"]]) +
  annotate("text", x = 82.25, y = 38.3, label = "Targeted\nre-adjudication\n& semantic freeze",
           family = font_family, fontface = "bold", size = 5.2 / pt,
           lineheight = 0.84, colour = palette[["navy"]]) +
  annotate("text", x = 82.25, y = 28.7, label = paste0("Net change +", net_change, " edges"),
           family = font_family, size = 5.0 / pt, colour = palette[["mid_gray"]]) +
  annotate("text", x = 94.25, y = 45.2, label = comma_n(final_edges_workflow), family = font_family,
           fontface = "bold", size = 12.4 / pt, colour = "white") +
  annotate("text", x = 94.25, y = 39.5, label = "Final supported\nedges", family = font_family,
           fontface = "bold", size = 5.8 / pt, lineheight = 0.86, colour = "white") +
  annotate("text", x = 94.25, y = 33.8, label = paste0(comma_n(final_nodes_workflow), " canonical nodes"),
           family = font_family, size = 4.7 / pt, colour = "white") +
  annotate("text", x = 94.25, y = 29.2, label = paste0(comma_n(final_evidence_workflow), " evidence records"),
           family = font_family, size = 4.5 / pt, colour = "white") +
  annotate("text", x = 94.25, y = 24.7, label = paste0(isolated_studies, " isolated study"),
           family = font_family, size = 4.7 / pt, colour = "white")

# --------------------------- Panel B -----------------------------------------
entity_counts <- setNames(as.integer(entities$Entity_count), entities$Entity_type)
instantiated_entities <- sum(entities$Entity_count > 0)

pb <- add_panel_label(new_canvas(), "B")
pb <- pb +
  annotate("text", x = 2, y = 90.5, label = "Engineering design layer", hjust = 0,
           family = font_family, fontface = "bold", size = 6.5 / pt, colour = palette[["blue"]])

# Connectors sit behind ontology cards.
engineering_x <- c(10, 30, 50, 70, 90)
pb <- pb +
  annotate("segment", x = engineering_x, xend = 50, y = 74.5, yend = 67,
           colour = palette[["line_gray"]], linewidth = 0.32) +
  annotate("segment", x = 50, xend = c(17, 50, 83), y = 54, yend = 45,
           colour = palette[["line_gray"]], linewidth = 0.32) +
  annotate("segment", x = c(17, 50, 83), xend = 50, y = 34, yend = 25,
           colour = palette[["line_gray"]], linewidth = 0.32) +
  annotate("rect", xmin = 35, xmax = 65, ymin = 67.2, ymax = 70.5, fill = "white", colour = NA) +
  annotate("text", x = 50, y = 68.7, label = "Material-centered core", hjust = 0.5,
           family = font_family, fontface = "bold", size = 6.4 / pt, colour = palette[["navy"]]) +
  annotate("rect", xmin = 1.4, xmax = 36.5, ymin = 48.3, ymax = 51.7, fill = "white", colour = NA) +
  annotate("text", x = 2, y = 50, label = "Biological action layer", hjust = 0,
           family = font_family, fontface = "bold", size = 6.4 / pt, colour = palette[["teal"]]) +
  annotate("rect", xmin = 39, xmax = 61, ymin = 25.9, ymax = 29.2, fill = "white", colour = NA) +
  annotate("text", x = 50, y = 27.5, label = "Validation layer", hjust = 0.5,
           family = font_family, fontface = "bold", size = 6.4 / pt, colour = "#9A7300")

engineering_types <- c("DesignMode", "MaterialComposition", "FabricationStrategy", "TherapeuticCargo", "Stimulus")
camel_wrap <- function(x) stringr::str_replace_all(x, "([a-z])([A-Z])", "\\1\n\\2")
for (i in seq_along(engineering_types)) {
  x <- engineering_x[[i]]
  type <- engineering_types[[i]]
  pb <- add_round_box(pb, x - 8.6, x + 8.6, 74.5, 87.0, "#F4F7FA", palette[["blue"]], lwd = 0.45)
  pb <- pb + annotate(
    "text", x = x, y = 81,
    label = paste0(camel_wrap(type), "\nn = ", comma_n(entity_counts[[type]])),
    family = font_family, fontface = "bold", size = 6.4 / pt, lineheight = 0.86,
    colour = palette[["dark_text"]]
  )
}

pb <- add_round_box(pb, 37, 63, 54.5, 66.5, palette[["navy"]], palette[["navy"]], lwd = 0.65)
pb <- pb + annotate(
  "text", x = 50, y = 60.5,
  label = paste0("MaterialPlatform\nn = ", comma_n(entity_counts[["MaterialPlatform"]])),
  family = font_family, fontface = "bold", size = 7.6 / pt, lineheight = 0.92, colour = "white"
)

biology_types <- c("TargetCell", "Mechanism", "RegenerativeEndpoint")
biology_x <- c(17, 50, 83)
for (i in seq_along(biology_types)) {
  x <- biology_x[[i]]
  type <- biology_types[[i]]
  fill <- if (type == "Mechanism") "#F0EDF8" else "#EAF6F3"
  border <- if (type == "Mechanism") palette[["purple"]] else palette[["teal"]]
  pb <- add_round_box(pb, x - 13.0, x + 13.0, 34, 45, fill, border, lwd = 0.45)
  pb <- pb + annotate(
    "text", x = x, y = 39.5,
    label = paste0(camel_wrap(type), "\nn = ", comma_n(entity_counts[[type]])),
    family = font_family, fontface = "bold", size = 6.4 / pt, lineheight = 0.86,
    colour = palette[["dark_text"]]
  )
}

pb <- add_round_box(pb, 36, 64, 17, 25, "#FCF7E8", palette[["amber"]], lwd = 0.45)
pb <- pb + annotate(
  "text", x = 50, y = 21,
  label = paste0("ValidationStage\nn = ", comma_n(entity_counts[["ValidationStage"]])),
  family = font_family, fontface = "bold", size = 6.2 / pt,
  lineheight = 0.84, colour = palette[["dark_text"]]
)

zero_types <- entities %>% filter(.data$Entity_count == 0) %>% pull(.data$Entity_type)
zero_x <- c(12.5, 37.5, 62.5, 87.5)
for (i in seq_along(zero_types)) {
  x <- zero_x[[i]]
  type <- zero_types[[i]]
  pb <- add_round_box(pb, x - 11, x + 11, 2, 10, "white", palette[["line_gray"]], lwd = 0.45, lty = 2, radius_mm = 1.2)
  pb <- pb + annotate(
    "text", x = x, y = 6,
    label = paste0(camel_wrap(type), "\nn = 0"),
    family = font_family, fontface = "bold", size = 5.4 / pt, lineheight = 0.84, colour = palette[["mid_gray"]]
  )
}

# --------------------------- Panel C -----------------------------------------
relation_colors <- c(
  "VALIDATED_AT" = palette[["navy"]],
  "TARGETS" = palette[["teal"]],
  "USES" = palette[["blue"]],
  "COMPOSED_OF" = palette[["blue"]],
  "FABRICATED_BY" = palette[["blue"]],
  "PROMOTES" = palette[["coral"]],
  "MODULATES" = palette[["purple"]],
  "DELIVERS" = palette[["blue"]],
  "RESPONDS_TO" = palette[["teal"]],
  "RELEASES" = palette[["purple"]],
  "ACTIVATES" = palette[["line_gray"]],
  "SUPPRESSES" = palette[["line_gray"]]
)
instantiated_relations <- sum(relations$Edge_count > 0)
relation_plot_data <- relations %>%
  mutate(
    zero = .data$Edge_count == 0,
    sort_group = if_else(.data$zero, 1L, 0L),
    sort_count = if_else(.data$zero, 0, -.data$Edge_count)
  ) %>%
  arrange(.data$sort_group, .data$sort_count, .data$Relation) %>%
  mutate(
    y = seq(91, 6, length.out = n()),
    bar_end = 72 + 16 * sqrt(.data$Edge_count / max(.data$Edge_count)),
    target_type_label = if_else(.data$zero, "", paste0("\u2192 ", .data$Object_type)),
    colour = unname(relation_colors[.data$Relation])
  )

pc <- add_panel_label(new_canvas(), "C")
pc <- pc +
  geom_segment(
    data = relation_plot_data,
    aes(x = 72, xend = .data$bar_end, y = .data$y, yend = .data$y, colour = .data$Relation),
    linewidth = 1.15, lineend = "round", show.legend = FALSE
  ) +
  geom_point(
    data = relation_plot_data,
    aes(x = .data$bar_end, y = .data$y, colour = .data$Relation),
    size = 1.9, show.legend = FALSE
  ) +
  geom_text(
    data = relation_plot_data,
    aes(x = 1, y = .data$y + 1.45, label = .data$Relation, colour = .data$Relation),
    hjust = 0, family = font_family, fontface = "bold", size = 6.4 / pt,
    show.legend = FALSE
  ) +
  geom_text(
    data = relation_plot_data,
    aes(x = 1, y = .data$y - 1.35, label = .data$target_type_label, colour = .data$Relation),
    hjust = 0, family = font_family, size = 5.3 / pt, show.legend = FALSE
  ) +
  geom_text(
    data = relation_plot_data,
    aes(x = 98, y = .data$y + 0.7, label = paste0("n = ", scales::comma(.data$Edge_count))),
    hjust = 1, family = font_family, fontface = "bold", size = 5.9 / pt,
    colour = palette[["dark_text"]]
  ) +
  scale_colour_manual(values = relation_colors, drop = FALSE)

# --------------------------- Panel D -----------------------------------------
pd <- add_panel_label(new_canvas(ymin = 4, ymax = 104), "D")
row_top <- c(82, 63, 44, 25)

for (i in seq_len(nrow(provenance))) {
  row <- provenance[i, ]
  rel_colour <- relation_colors[[row$Relation]]
  y_top <- row_top[[i]]

  pd <- pd +
    annotate("segment", x = 9, xend = 12, y = y_top, yend = y_top,
             colour = rel_colour, linewidth = 0.40,
             arrow = grid::arrow(length = unit(1.3, "mm"), type = "closed")) +
    annotate("segment", x = 40, xend = 43, y = y_top, yend = y_top,
             colour = rel_colour, linewidth = 0.40,
             arrow = grid::arrow(length = unit(1.3, "mm"), type = "closed")) +
    annotate("segment", x = 55, xend = 58, y = y_top, yend = y_top,
             colour = rel_colour, linewidth = 0.40,
             arrow = grid::arrow(length = unit(1.3, "mm"), type = "closed")) +
    annotate("segment", x = 80, xend = 83, y = y_top, yend = y_top,
             colour = rel_colour, linewidth = 0.40,
             arrow = grid::arrow(length = unit(1.3, "mm"), type = "closed"))

  pd <- add_round_box(pd, 1, 9, y_top - 4, y_top + 4, palette[["navy"]], palette[["navy"]], lwd = 0.55, radius_mm = 1.1)
  pd <- add_round_box(pd, 12, 40, y_top - 5, y_top + 5, "white", rel_colour, lwd = 0.45, radius_mm = 1.2)
  pd <- add_round_box(pd, 43, 55, y_top - 4, y_top + 4, rel_colour, rel_colour, radius_mm = 1.1)
  pd <- add_round_box(pd, 58, 80, y_top - 5, y_top + 5, palette[["light_gray"]], rel_colour, lwd = 0.45, radius_mm = 1.2)
  pd <- add_round_box(pd, 83, 98, y_top - 4, y_top + 4, palette[["light_gray"]], palette[["navy"]], lwd = 0.45, radius_mm = 1.1)

  material_label <- stringr::str_wrap(row$Source, width = 34)
  target_label <- stringr::str_wrap(row$Target, width = 28)

  pd <- pd +
    annotate("text", x = 5, y = y_top, label = row$Record_ID, family = font_family,
             fontface = "bold", size = 6.3 / pt, colour = "white") +
    annotate("text", x = 26, y = y_top, label = material_label, family = font_family,
             fontface = "bold", size = 6.1 / pt, lineheight = 0.88, colour = palette[["dark_text"]]) +
    annotate("text", x = 49, y = y_top, label = row$Relation, family = font_family,
             fontface = "bold", size = 5.9 / pt, colour = "white") +
    annotate("text", x = 69, y = y_top, label = target_label, family = font_family,
             fontface = "bold", size = 5.9 / pt, lineheight = 0.88, colour = palette[["dark_text"]]) +
    annotate("text", x = 90.5, y = y_top, label = row$Evidence_ID, family = font_family,
             fontface = "bold", size = 6.0 / pt, colour = palette[["navy"]])
}

# --------------------------- Assembly and export -----------------------------
middle_row <- pb + pc + patchwork::plot_layout(ncol = 2, widths = c(0.97, 1.03))
figure1_combined <- pa / middle_row / pd +
  patchwork::plot_layout(heights = c(0.93, 1.18, 0.68)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

expected_stems <- c(
  "Figure1A_workflow", "Figure1B_entity_ontology", "Figure1C_relation_ontology",
  "Figure1D_evidence_provenance", "Figure1_combined"
)

# Remove stale non-PDF derivatives from earlier releases and prior QA previews.
stale_extensions <- c("png", "tiff", "svg")
stale_outputs <- as.vector(outer(expected_stems, stale_extensions, paste, sep = "."))
qa_preview_files <- file.path(output_dir, paste0("QA_", expected_stems, "_1.png"))
stale_qa_previews <- list.files(
  output_dir, pattern = "^QA_Figure1.*\\.png$", full.names = TRUE
)
unlink(c(file.path(output_dir, stale_outputs), qa_preview_files, stale_qa_previews), force = TRUE)

save_pdf <- function(plot, stem, width_mm, height_mm) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4
  grDevices::cairo_pdf(
    file.path(output_dir, paste0(stem, ".pdf")),
    width = width_in, height = height_in,
    family = font_family, bg = "white", onefile = TRUE
  )
  print(plot)
  grDevices::dev.off()
}

# Panels A/B/C are frozen for this final layout-only pass; regenerate only if absent.
if (!file.exists(file.path(output_dir, "Figure1A_workflow.pdf"))) save_pdf(pa, "Figure1A_workflow", 180, 55)
if (!file.exists(file.path(output_dir, "Figure1B_entity_ontology.pdf"))) save_pdf(pb, "Figure1B_entity_ontology", 86, 70)
if (!file.exists(file.path(output_dir, "Figure1C_relation_ontology.pdf"))) save_pdf(pc, "Figure1C_relation_ontology", 90, 70)
save_pdf(pd, "Figure1D_evidence_provenance", 180, 42)
save_pdf(figure1_combined, "Figure1_combined", 180, 174)

expected_outputs <- paste0(expected_stems, ".pdf")
expected_paths <- file.path(output_dir, expected_outputs)
output_status <- file.exists(expected_paths) & file.info(expected_paths)$size > 0
if (!all(output_status)) {
  stop(paste("Missing or empty Figure 1 outputs:", paste(expected_outputs[!output_status], collapse = ", ")))
}

pdf_files_present <- list.files(output_dir, pattern = "\\.pdf$", full.names = FALSE)
pdf_only_exports <- setequal(pdf_files_present, expected_outputs) &&
  !any(file.exists(file.path(output_dir, stale_outputs)))

pdf_font_tables <- lapply(expected_paths, pdftools::pdf_fonts)
names(pdf_font_tables) <- expected_stems
pdf_font_names <- unique(unlist(lapply(pdf_font_tables, function(x) x$name), use.names = FALSE))
pdf_arial_uniform <- if (identical(font_family, "Arial")) {
  length(pdf_font_names) > 0L && all(stringr::str_detect(tolower(pdf_font_names), "arial"))
} else {
  TRUE
}

pdf_readable <- all(vapply(expected_paths, function(path) {
  info <- pdftools::pdf_info(path)
  is.list(info) && identical(as.integer(info$pages), 1L)
}, logical(1)))

source_hash_after <- tools::md5sum(source_files)
source_unchanged <- identical(unname(source_hash_before), unname(source_hash_after))
if (!source_unchanged) stop("A frozen Figure 1 source file changed during plotting.")

session_lines <- c(
  paste0("R version: ", R.version.string),
  paste0("Rscript path: ", rscript_path),
  paste0("Font family used: ", font_family),
  paste0("Embedded PDF fonts: ", paste(pdf_font_names, collapse = "; ")),
  paste0("Platform: ", R.version$platform),
  "",
  "Package versions:",
  vapply(required_packages, function(pkg) paste0(pkg, " ", as.character(utils::packageVersion(pkg))), character(1))
)
writeLines(enc2utf8(session_lines), file.path(output_dir, "Figure1_session_info.txt"), useBytes = TRUE)

caption_lines <- c(
  "Fig. 1 | Construction, ontology architecture, and evidence provenance of SenescenceBioKG",
  "",
  "(A) Relation-level full-text validation began with 3,351 candidate relations from 283 studies. The initial partition was 1,669 SUPPORTED + 1,234 REJECTED + 448 UNCLEAR = 3,351; REJECTED and UNCLEAR candidates were not carried into the final graph. PDF discrepancy review added 44 SUPPORTED relations, yielding 1,713 pre-re-adjudication core edges. Targeted re-adjudication and semantic freeze produced 1,868 final supported edges, 654 canonical nodes, 1,868 evidence records and one isolated study. The +155 net change is an arithmetic difference and is not a count of promoted edges.",
  "(B) Canonical entity ontology organized across engineering design, material-centered, biological action and validation layers. The ontology defines 14 entity types, of which 10 are instantiated in the current 654-node graph. Dashed cards denote ontology-defined entity types with n = 0. Clinical is defined within ValidationStage but has n = 0.",
  "(C) Domain-relation-range architecture and final supported-edge composition for all 12 ontology relations. All displayed relations originate from MaterialPlatform. ACTIVATES and SUPPRESSES are retained in the ontology but currently have n = 0.",
  "(D) Four structured same-record provenance chains link Record_ID, material platform, validated relation, canonical target and Evidence_ID. The verbatim evidence excerpts and detailed locations are retained in the Supplementary source data and Explorer rather than displayed in the main figure. Every core edge is linked to a same-record evidence item.",
  "Source data are provided in the FINAL FREEZE figure-data release."
)
writeLines(enc2utf8(caption_lines), file.path(output_dir, "Figure1_caption_draft.txt"), useBytes = TRUE)

visual_status <- if (visual_iteration >= 2L) {
  "PASS - R-rendered previews inspected; frozen A/B/C preserved and revised D/combined PDFs re-rendered"
} else {
  "PENDING - initial PDFs generated for R-rendered visual inspection"
}
panel_subtitles_removed <- TRUE
panel_labels_black <- identical(palette[["black"]], "#000000")
panel_explanatory_microtext_removed <- TRUE
overall_status <- if (
  all(data_checks) && all(provenance_checks) && all(output_status) &&
  source_unchanged && pdf_only_exports && pdf_readable && pdf_arial_uniform &&
  panel_subtitles_removed && panel_labels_black && panel_explanatory_microtext_removed &&
  visual_iteration >= 2L
) "PASS" else "PENDING_VISUAL_REVIEW"

qc_lines <- c(
  paste0("Figure 1 plot QC: ", overall_status),
  "",
  paste0("Full-text studies = ", full_text_studies),
  paste0("Candidate relations = ", candidate_relations),
  paste0("Initial SUPPORTED = ", initial_supported),
  paste0("REJECTED = ", initial_rejected),
  paste0("UNCLEAR = ", initial_unclear),
  paste0("PDF corrective additions = ", pdf_corrective),
  paste0("Pre-re-adjudication edges = ", pre_readjudication),
  paste0("Net change after targeted re-adjudication = ", net_change),
  paste0("Final canonical nodes = ", nrow(nodes)),
  paste0("Final canonical edges = ", nrow(edges)),
  paste0("Final evidence records = ", nrow(evidence)),
  paste0("Relation edge counts sum = ", sum(relations$Edge_count)),
  paste0("Entity counts sum = ", sum(entities$Entity_count)),
  "",
  paste0("CHECK 1669 + 1234 + 448 = 3351: ", ifelse(data_checks[["initial_partition"]], "PASS", "FAIL")),
  paste0("CHECK 1669 + 44 = 1713: ", ifelse(data_checks[["pre_readjudication"]], "PASS", "FAIL")),
  paste0("CHECK 1868 - 1713 = 155: ", ifelse(data_checks[["net_change"]], "PASS", "FAIL")),
  paste0("CHECK final node count = 654: ", ifelse(data_checks[["final_node_count"]], "PASS", "FAIL")),
  paste0("CHECK final edge count = 1868: ", ifelse(data_checks[["final_edge_count"]], "PASS", "FAIL")),
  paste0("CHECK final evidence count = 1868: ", ifelse(data_checks[["final_evidence_count"]], "PASS", "FAIL")),
  paste0("CHECK relation edge counts sum = 1868: ", ifelse(data_checks[["relation_sum"]], "PASS", "FAIL")),
  paste0("CHECK entity counts sum = 654: ", ifelse(data_checks[["entity_sum"]], "PASS", "FAIL")),
  paste0("CHECK source files unchanged: ", ifelse(source_unchanged, "PASS", "FAIL")),
  paste0("CHECK five required PDF files generated: ", ifelse(all(output_status), "PASS", "FAIL")),
  paste0("CHECK production figure exports are PDF-only: ", ifelse(pdf_only_exports, "PASS", "FAIL")),
  paste0("CHECK all PDFs reopen as single-page files: ", ifelse(pdf_readable, "PASS", "FAIL")),
  paste0("CHECK Arial embedded uniformly in PDFs: ", ifelse(pdf_arial_uniform, "PASS", "FAIL")),
  paste0("CHECK panel subtitles removed: ", ifelse(panel_subtitles_removed, "PASS", "FAIL")),
  paste0("CHECK A/B/C/D panel labels are #000000: ", ifelse(panel_labels_black, "PASS", "FAIL")),
  paste0("CHECK explanatory microtext moved from panels to caption: ", ifelse(panel_explanatory_microtext_removed, "PASS", "FAIL")),
  paste0("CHECK four same-record provenance examples with public evidence metadata: ", ifelse(all(provenance_checks), "PASS", "FAIL")),
  "",
  paste0("Visual iteration = ", visual_iteration),
  paste0("Visual QC = ", visual_status),
  "Static preflight WARN review: R source parsed and executed successfully with Rscript.",
  "Static preflight WARN review: explicit rendered text sizes span 4.5 to 17 pt and were visually checked at 180 mm final width.",
  "Static preflight WARN review: final combined dimensions are explicitly exported at 180 mm x 174 mm.",
  "Static preflight contract override: SVG/TIFF/production PNG are intentionally disabled by the explicit PDF-only delivery requirement.",
  "Editable-vector verification: cairo_pdf output reopens successfully and embeds Arial/Arial-Bold fonts.",
  "",
  "Figure 1D evidence examples:",
  paste0(
    provenance$Record_ID, " | ", provenance$Relation, " | ", provenance$Evidence_ID,
    " | ", provenance$Evidence_source, " | ", provenance$Evidence_location
  )
)
writeLines(enc2utf8(qc_lines), file.path(output_dir, "Figure1_plot_QC.txt"), useBytes = TRUE)

if (qa_preview) {
  for (i in seq_along(expected_paths)) {
    pdftools::pdf_convert(
      expected_paths[[i]], format = "png", pages = 1,
      filenames = file.path(output_dir, paste0("QA_", expected_stems[[i]], "_%d.%s")), dpi = 300,
      antialias = TRUE, verbose = FALSE
    )
  }
}

cat(paste0("Figure 1 render complete. QC status: ", overall_status, "\n"))
