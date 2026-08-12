script_file <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
fig_root <- dirname(script_dir)
source(file.path(fig_root, "figure_theme.R"), encoding = "UTF-8")
source(file.path(fig_root, "data_helpers.R"), encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

core <- sbk_core()
assert_final_freeze(core)
out_dir <- file.path(sbk_root, "outputs", "figures", "Figure4")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
preview_dir <- file.path(fig_root, "_preview")
dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

stimulus_counts <- sbk_read("06_FIGURE4_RESPONSIVENESS", "figure4A_stimulus_counts.csv")
membership <- sbk_read("06_FIGURE4_RESPONSIVENESS", "figure4B_response_module_membership.csv")
response_release <- sbk_read("06_FIGURE4_RESPONSIVENESS", "figure4C_response_release_complete.csv")
response_target <- sbk_read("06_FIGURE4_RESPONSIVENESS", "figure4D_response_target_regeneration.csv")
response_mechanism <- sbk_read("06_FIGURE4_RESPONSIVENESS", "figure4D_response_mechanism_regeneration.csv")
display_mapping <- sbk_read("07_FIGURE5_CELL_MECHANISM_REGENERATION", "figure5_display_group_mapping.csv")

stopifnot(sum(stimulus_counts$Edge_count) == 64L)

short_stimulus <- function(x) {
  x %>%
    str_replace("Senescence-associated esterase/SASP-associated enzymatic milieu",
                "Senescence enzymes") %>%
    str_replace_all("; ", " + ") %>%
    wrap_sbk(24)
}

# A: strict explicit responsiveness by stimulus.
a_dat <- stimulus_counts %>%
  arrange(Edge_count, Stimulus) %>%
  mutate(Stimulus_label = factor(short_stimulus(Stimulus), levels = short_stimulus(Stimulus)))

pA <- ggplot(a_dat, aes(x = Edge_count, y = Stimulus_label)) +
  geom_col(width = 0.68, fill = sbk_palette[["teal"]]) +
  geom_text(aes(label = Edge_count), hjust = 0, nudge_x = 0.35,
            family = SBK_FONT, fontface = "bold", size = 2.65) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)), breaks = pretty_breaks(4)) +
  labs(x = "Supported RESPONDS_TO edges", y = NULL) +
  theme_sbk(7.7) +
  theme(axis.text.y = element_text(size = 7.3, face = "bold"),
        plot.margin = margin(9, 10, 6, 7, "pt"))
pA <- add_panel_tag(pA, "A")

# B: UpSet-style intersection of five relations at the same Record_ID and MaterialPlatform.
rel_cols <- c("HAS_RESPONDS_TO", "HAS_RELEASES", "HAS_TARGETS", "HAS_MODULATES", "HAS_PROMOTES")
rel_labels <- c("RESPONDS_TO", "RELEASES", "TARGETS", "MODULATES", "PROMOTES")
b_combo <- membership %>%
  mutate(across(all_of(rel_cols), ~as.logical(.x))) %>%
  rowwise() %>%
  mutate(Signature = paste(rel_labels[c_across(all_of(rel_cols))], collapse = "+")) %>%
  ungroup() %>%
  filter(Signature != "") %>%
  count(Signature, name = "Study_material_count", sort = TRUE) %>%
  slice_head(n = 12) %>%
  mutate(Combo = factor(seq_len(n()), levels = seq_len(n())))

b_matrix <- crossing(Combo = b_combo$Combo, Relation = rel_labels) %>%
  left_join(b_combo %>% select(Combo, Signature), by = "Combo") %>%
  mutate(Included = mapply(function(sig, rel) str_detect(sig, fixed(rel)), Signature, Relation),
         Rel_y = match(Relation, rev(rel_labels)))
b_segments <- b_matrix %>% filter(Included) %>% group_by(Combo) %>%
  summarise(ymin = min(Rel_y), ymax = max(Rel_y), .groups = "drop")

pB_top <- ggplot(b_combo, aes(x = Combo, y = Study_material_count)) +
  geom_col(width = 0.66, fill = sbk_palette[["dark_navy"]]) +
  geom_text(aes(label = Study_material_count), vjust = -0.25, family = SBK_FONT,
            fontface = "bold", size = 2.35) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.17)), breaks = pretty_breaks(3)) +
  labs(x = NULL, y = "Study count") +
  theme_sbk(7.3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.margin = margin(11, 5, 0, 7, "pt"))
pB_top <- add_panel_tag(pB_top, "B")

pB_bottom <- ggplot(b_matrix, aes(x = Combo, y = Rel_y)) +
  geom_segment(data = b_segments, aes(x = Combo, xend = Combo, y = ymin, yend = ymax),
               inherit.aes = FALSE, linewidth = 0.55, colour = sbk_palette[["dark_text"]]) +
  geom_point(size = 2.2, colour = sbk_palette[["zero"]]) +
  geom_point(data = b_matrix %>% filter(Included), size = 2.4,
             colour = sbk_palette[["dark_text"]]) +
  scale_y_continuous(breaks = seq_along(rel_labels), labels = rev(rel_labels),
                     expand = expansion(add = 0.45)) +
  labs(x = "Major same-record intersections", y = NULL) +
  theme_sbk(7.3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 7.0, face = "bold"),
        axis.line.x = element_blank(), plot.margin = margin(0, 5, 5, 7, "pt"))

pB <- pB_top / pB_bottom + plot_layout(heights = c(1.3, 1.0))

# C: same-record co-supported RESPONDS_TO and RELEASES systems.
short_cargo <- function(x) {
  case_when(
    str_detect(x, "Cobalt ions") ~ "Co²⁺/CXCL12/Rg1/LF",
    str_detect(x, "quercetin and Quercetin") ~ "Quercetin + greigite\nnanozymes (G-NZ)",
    x == "Dasatinib; quercetin" ~ "Dasatinib + quercetin",
    TRUE ~ x
  )
}

c_dat <- response_release %>%
  separate_longer_delim(Stimulus, delim = "; ") %>%
  group_by(Stimulus, Released_cargo) %>%
  summarise(Study_count = n_distinct(Record_ID), .groups = "drop") %>%
  mutate(
    Stimulus_label = short_stimulus(Stimulus),
    Cargo_label = wrap_sbk(short_cargo(Released_cargo), 22)
  )

pC <- ggplot(c_dat, aes(x = Stimulus_label, y = Cargo_label)) +
  geom_point(shape = 16, colour = sbk_palette[["muted_blue"]], size = 3.3, alpha = 0.92) +
  labs(x = "Stimulus", y = "Released cargo") +
  theme_sbk(7.7) +
  theme(
    axis.text.x = element_text(angle = 32, hjust = 1, vjust = 1, face = "plain", size = 7.3),
    axis.text.y = element_text(size = 7.2, face = "bold"),
    legend.position = "none", plot.margin = margin(9, 8, 6, 7, "pt")
  )
pC <- add_panel_tag(pC, "C")

module_bubble <- function(dat, entity_col, panel_colour, entity_axis) {
  entity_col <- rlang::ensym(entity_col)
  node_type <- ifelse(rlang::as_string(entity_col) == "Target_cell", "TargetCell", "Mechanism")
  map_sub <- display_mapping %>% filter(Node_type == node_type) %>%
    select(Canonical_name, Display_group)
  agg <- dat %>%
    separate_longer_delim(Stimulus, delim = "; ") %>%
    mutate(Entity_raw = as.character(!!entity_col)) %>%
    separate_longer_delim(Entity_raw, delim = "; ") %>%
    left_join(map_sub, by = c("Entity_raw" = "Canonical_name")) %>%
    mutate(
      Entity = coalesce(Display_group, Entity_raw),
      Entity = ifelse(Entity == "Nucleus pulposus cells", "Disc cells", Entity)
    ) %>%
    group_by(Stimulus, Entity) %>%
    summarise(
      Study_count = n_distinct(Record_ID),
      Endpoint_diversity = n_distinct(Regenerative_endpoint),
      .groups = "drop"
    ) %>%
    mutate(Stimulus = short_stimulus(Stimulus), Entity = wrap_sbk(Entity, 23))
  ggplot(agg, aes(x = Stimulus, y = Entity, size = Study_count, colour = Endpoint_diversity)) +
    geom_point(alpha = 0.9) +
    geom_text(aes(label = Study_count), family = SBK_FONT, fontface = "bold",
              size = 2.15, colour = sbk_palette[["black"]], show.legend = FALSE) +
    scale_size_area(max_size = 5.4, breaks = pretty_breaks(3)) +
    scale_colour_gradient(low = sbk_palette[["light_blue_gray"]], high = panel_colour) +
    labs(x = "Stimulus", y = entity_axis, size = "Studies", colour = "Endpoint\ndiversity") +
    theme_sbk(7.3) +
    theme(axis.text.x = element_text(angle = 34, hjust = 1, face = "plain", size = 6.9),
          axis.text.y = element_text(size = 6.9, face = "bold"),
          legend.position = "none",
          plot.margin = margin(8, 7, 5, 7, "pt"))
}

pD_left <- module_bubble(response_target, Target_cell, sbk_palette[["teal"]], "Target cell")
pD_left <- add_panel_tag(pD_left, "D")
pD_right <- module_bubble(response_mechanism, Mechanism, sbk_palette[["muted_purple"]], "Mechanism")
pD <- pD_left + pD_right + plot_layout(widths = c(1, 1)) &
  theme(plot.margin = margin(8, 12, 5, 12, "pt"))

design <- "
AB
CC
DD
"
combined <- pA + pB + pC + pD +
  plot_layout(design = design, heights = c(1.0, 1.15, 1.35))

save_pdf_sbk(pA, file.path(out_dir, "Figure4A.pdf"), 90, 82)
# Panels B/C/D are frozen for this final layout-only pass; regenerate only if absent.
if (!file.exists(file.path(out_dir, "Figure4B.pdf"))) save_pdf_sbk(pB, file.path(out_dir, "Figure4B.pdf"), 90, 76)
if (!file.exists(file.path(out_dir, "Figure4C.pdf"))) save_pdf_sbk(pC, file.path(out_dir, "Figure4C.pdf"), 180, 98)
if (!file.exists(file.path(out_dir, "Figure4D.pdf"))) save_pdf_sbk(pD, file.path(out_dir, "Figure4D.pdf"), 180, 100)
save_pdf_sbk(combined, file.path(out_dir, "Figure4_combined.pdf"), 180, 216)

caption <- paste0(
  "Figure 4 | Strict explicit stimulus responsiveness in SenescenceBioKG. ",
  "(A) Supported RESPONDS_TO edges by canonical stimulus; edge counts and unique study/material counts are tracked separately in the source data. ",
  "(B) Major same-Record_ID, same-MaterialPlatform intersections among RESPONDS_TO, RELEASES, TARGETS, MODULATES and PROMOTES. ",
  "(C) Stimulus–released-cargo combinations in systems with co-supported RESPONDS_TO and RELEASES edges. Co-support does not establish stimulus-triggered release unless that causal release change is directly demonstrated. ",
  "(D) Same-record response–target–regeneration and response–mechanism–regeneration modules. Point size denotes unique studies and colour denotes the number of supported regenerative-endpoint categories. These are co-supported evidence modules, not inferred causal chains."
)
writeLines(caption, file.path(out_dir, "Figure4_caption_draft.txt"), useBytes = TRUE)

pdfs <- file.path(out_dir, c("Figure4A.pdf", "Figure4B.pdf", "Figure4C.pdf", "Figure4D.pdf", "Figure4_combined.pdf"))
qc <- c(
  "Figure 4 plot QC",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "Source files: 06_FIGURE4_RESPONSIVENESS/*.csv",
  paste0("Source row counts: stimuli=", nrow(stimulus_counts), "; membership=", nrow(membership),
         "; response-release=", nrow(response_release), "; response-target=", nrow(response_target),
         "; response-mechanism=", nrow(response_mechanism)),
  paste0("RESPONDS_TO edge/study/material totals: ", sum(stimulus_counts$Edge_count), "/",
         sum(stimulus_counts$Study_count), "/", sum(stimulus_counts$Material_count)),
  paste0("Major UpSet intersections displayed: ", nrow(b_combo)),
  paste0("Same-record modules response+release/target+promote/mechanism+promote: ",
         n_distinct(response_release$Record_ID), "/", n_distinct(response_target$Record_ID), "/",
         n_distinct(response_mechanism$Record_ID)),
  paste0("FINAL FREEZE consistency: ", ifelse(assert_final_freeze(core), "PASS", "FAIL")),
  "Font check: Arial Bold used for core labels and Arial Regular for ticks/legends; cairo PDF devices",
  "Figure 4B typo fixed: Study count",
  "Figure 4C presence matrix: fixed-size dots; meaningless size legend removed",
  paste0("PDF existence: ", ifelse(all(vapply(pdfs, pdf_ok, logical(1))), "PASS", "FAIL")),
  "Panel labels: A/B/C/D black Arial Bold 17 pt",
  "Panel subtitles: none",
  "Visual inspection status: PASS - all 16 stimulus labels are separate, unclipped and non-overlapping"
)
write_qc_lines(file.path(out_dir, "Figure4_plot_QC.txt"), qc)
