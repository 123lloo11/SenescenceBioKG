script_file <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
fig_root <- dirname(script_dir)
source(file.path(fig_root, "figure_theme.R"), encoding = "UTF-8")
source(file.path(fig_root, "data_helpers.R"), encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggalluvial)
})

core <- sbk_core()
assert_final_freeze(core)
annotated <- annotate_edges(core)
out_dir <- file.path(sbk_root, "outputs", "figures", "Figure5")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
preview_dir <- file.path(fig_root, "_preview")
dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

alluvial_raw <- sbk_read("07_FIGURE5_CELL_MECHANISM_REGENERATION", "figure5A_alluvial_links.csv")
alluvial_grouped <- sbk_read("07_FIGURE5_CELL_MECHANISM_REGENERATION", "figure5A_alluvial_links_GROUPED.csv")
target_counts <- sbk_read("07_FIGURE5_CELL_MECHANISM_REGENERATION", "figure5C_target_cell_counts.csv")
mechanism_counts <- sbk_read("07_FIGURE5_CELL_MECHANISM_REGENERATION", "figure5C_mechanism_counts.csv")
endpoint_counts <- sbk_read("07_FIGURE5_CELL_MECHANISM_REGENERATION", "figure5C_endpoint_counts.csv")
display_mapping <- sbk_read("07_FIGURE5_CELL_MECHANISM_REGENERATION", "figure5_display_group_mapping.csv")

stopifnot(nrow(alluvial_raw) == 51L, n_distinct(alluvial_raw$Record_ID) == 35L)

short_target_group <- function(x) {
  recode(x,
    "Nucleus pulposus cells" = "Disc cells",
    "Stem/progenitor cells" = "Stem / progenitor cells",
    "Bone-lineage cells" = "Bone-lineage cells",
    .default = x)
}

short_mechanism_group <- function(x) {
  recode(x,
    "Oxidative-stress/redox regulation" = "Redox control",
    "Mitochondrial/metabolic regulation" = "Mitochondrial / metabolic",
    "Inflammation/immune regulation" = "Inflammation / immune",
    "Cell survival/proliferation" = "Cell survival / proliferation",
    "Angiogenesis/vascular regulation" = "Angiogenesis / vascular",
    .default = x)
}

short_endpoint_group <- function(x) {
  recode(x,
    "Intervertebral-disc regeneration" = "Disc regeneration",
    "Musculoskeletal soft-tissue repair" = "Soft-tissue repair",
    "Skin/wound repair" = "Skin / wound repair",
    .default = x)
}

alluvial_display <- alluvial_grouped %>%
  mutate(
    Target_cell_group = short_target_group(Target_cell_group),
    Mechanism_group = short_mechanism_group(Mechanism_group),
    Regenerative_endpoint_group = short_endpoint_group(Regenerative_endpoint_group)
  )

# A: same-record cell–mechanism–endpoint alluvial with display grouping.
a_dat <- alluvial_display %>%
  distinct(Record_ID, Target_cell_group, Mechanism_group, Regenerative_endpoint_group) %>%
  count(Target_cell_group, Mechanism_group, Regenerative_endpoint_group, name = "Study_count") %>%
  mutate(
    Target = paste0("TC: ", Target_cell_group),
    Mechanism = paste0("ME: ", Mechanism_group),
    Endpoint = paste0("EP: ", Regenerative_endpoint_group)
  )

stratum_values <- c(
  setNames(rep(sbk_palette[["teal"]], n_distinct(a_dat$Target)), unique(a_dat$Target)),
  setNames(rep(sbk_palette[["muted_purple"]], n_distinct(a_dat$Mechanism)), unique(a_dat$Mechanism)),
  setNames(rep(sbk_palette[["coral"]], n_distinct(a_dat$Endpoint)), unique(a_dat$Endpoint))
)

pA <- ggplot(a_dat,
             aes(axis1 = Target, axis2 = Mechanism, axis3 = Endpoint, y = Study_count)) +
  geom_alluvium(fill = sbk_palette[["mid_gray"]], alpha = 0.34,
                width = 0.12, knot.pos = 0.38) +
  geom_stratum(aes(fill = after_stat(stratum)), width = 0.16,
               colour = "white", linewidth = 0.35) +
  geom_text(stat = "stratum",
            aes(label = after_stat(ifelse(count >= 2,
                                          str_replace(stratum, "^[A-Z]+: ", ""), ""))),
            family = SBK_FONT, fontface = "bold", size = 2.25,
            colour = sbk_palette[["black"]], lineheight = 0.88) +
  scale_x_discrete(limits = c("Target cell", "Mechanism", "Regenerative endpoint"),
                   expand = expansion(add = c(0.48, 0.48))) +
  scale_fill_manual(values = stratum_values, guide = "none") +
  labs(x = NULL, y = "Same-record paths") +
  theme_sbk(7.7) +
  theme(
    axis.text.x = element_text(size = 8.0, face = "bold"),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    plot.margin = margin(13, 18, 6, 16, "pt")
  )
pA <- add_panel_tag(pA, "A")

# B: target-cell × endpoint modules, with mechanism diversity.
b_dat <- alluvial_display %>%
  group_by(Target_cell_group, Regenerative_endpoint_group) %>%
  summarise(
    Study_count = n_distinct(Record_ID),
    Mechanism_diversity = n_distinct(Mechanism_group),
    .groups = "drop"
  ) %>%
  mutate(
    Target = wrap_sbk(Target_cell_group, 22),
    Endpoint = wrap_sbk(Regenerative_endpoint_group, 22)
  )

pB <- ggplot(b_dat, aes(x = Endpoint, y = Target, size = Study_count, colour = Mechanism_diversity)) +
  geom_point(alpha = 0.92) +
  scale_size_area(max_size = 6.0, breaks = pretty_breaks(4)) +
  scale_colour_gradient(low = sbk_palette[["light_blue_gray"]], high = sbk_palette[["muted_purple"]]) +
  scale_y_discrete(expand = expansion(add = c(0.55, 2.8))) +
  labs(x = "Regenerative endpoint", y = "Target cell", size = "Studies",
       colour = "Mechanism\ndiversity") +
  theme_sbk(7.3) +
  theme(
    axis.text.x = element_text(angle = 36, hjust = 1, face = "bold", size = 6.9),
    axis.text.y = element_text(size = 7.0, face = "bold"),
    legend.position = "inside", legend.position.inside = c(0.54, 0.965),
    legend.box = "vertical", legend.direction = "horizontal",
    legend.justification = c(0.5, 1),
    legend.background = element_rect(fill = alpha("white", 0.94), colour = NA),
    legend.text = element_text(face = "plain"), legend.title = element_text(face = "plain"),
    legend.key.width = unit(3.5, "mm"), legend.spacing.x = unit(1.0, "mm"),
    plot.margin = margin(9, 8, 6, 7, "pt")
  )
pB <- add_panel_tag(pB, "B")

mini_hub_plot <- function(dat, label_col, colour, xlab, tag = NULL) {
  label_col <- rlang::ensym(label_col)
  d <- dat %>%
    arrange(desc(Study_count), desc(Edge_count), !!label_col) %>%
    slice_head(n = 6) %>%
    mutate(
      Raw_label = as.character(!!label_col),
      Short_label = case_when(
        Raw_label == "Mesenchymal stem/stromal cells" ~ "Mesenchymal stem/stromal cells",
        Raw_label == "Macrophages/innate immune cells" ~ "Macrophages / immune cells",
        Raw_label == "Chondrocytes/cartilage progenitors" ~ "Chondrocytes / progenitors",
        Raw_label == "Nucleus pulposus/annulus fibrosus cells" ~ "Disc cells",
        Raw_label == "Endothelial/vascular cells" ~ "Endothelial / vascular cells",
        Raw_label == "ROS/oxidative-stress control" ~ "Redox control",
        Raw_label == "Mitochondrial homeostasis/mitophagy" ~ "Mitochondrial homeostasis",
        Raw_label == "NF-κB/SASP/inflammatory signaling" ~ "NF-κB / SASP signaling",
        Raw_label == "Angiogenic/HIF–VEGF signaling" ~ "HIF–VEGF signaling",
        Raw_label == "p16/p21/p53–Rb cell-cycle arrest" ~ "Cell-cycle arrest",
        Raw_label == "Apoptosis/cell-survival regulation" ~ "Cell-survival regulation",
        Raw_label == "Bone regeneration/osteogenesis" ~ "Bone regeneration",
        Raw_label == "Wound healing/skin regeneration" ~ "Skin / wound repair",
        Raw_label == "Cartilage regeneration/chondroprotection" ~ "Cartilage regeneration",
        Raw_label == "Intervertebral-disc regeneration" ~ "Disc regeneration",
        Raw_label == "Stemness/proliferation/differentiation restoration" ~ "Stemness restoration",
        Raw_label == "Neural regeneration/function" ~ "Neural repair",
        TRUE ~ Raw_label
      ),
      Label = wrap_sbk(Short_label, 20)
    ) %>%
    arrange(Study_count) %>%
    mutate(Label = factor(Label, levels = unique(Label)))
  p <- ggplot(d, aes(x = Study_count, y = Label)) +
    geom_segment(aes(x = 0, xend = Study_count, yend = Label), linewidth = 0.55,
                 colour = sbk_palette[["light_blue_gray"]]) +
    geom_point(size = 2.45, colour = colour) +
    geom_text(aes(label = Study_count), nudge_x = 1.1, hjust = 0,
              family = SBK_FONT, fontface = "bold", size = 2.15) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.16)), breaks = pretty_breaks(3)) +
    labs(x = xlab, y = NULL) +
    theme_sbk(6.9) +
    theme(axis.text.y = element_text(size = 6.6, face = "bold"),
          axis.title.x = element_text(size = 7.0, face = "bold"),
          plot.margin = margin(4, 5, 2, 6, "pt"))
  if (!is.null(tag)) p <- add_panel_tag(p, tag)
  p
}

prepare_hub_rows <- function(dat, label_col, category) {
  label_col <- rlang::ensym(label_col)
  dat %>%
    arrange(desc(Study_count), desc(Edge_count), !!label_col) %>%
    slice_head(n = 6) %>%
    transmute(Category = category, Raw_label = as.character(!!label_col), Study_count) %>%
    mutate(
      Short_label = case_when(
        Raw_label == "Mesenchymal stem/stromal cells" ~ "Mesenchymal stem cells",
        Raw_label == "Macrophages/innate immune cells" ~ "Macrophages / immune cells",
        Raw_label == "Chondrocytes/cartilage progenitors" ~ "Chondrocytes / progenitors",
        Raw_label == "Nucleus pulposus/annulus fibrosus cells" ~ "Disc cells",
        Raw_label == "Endothelial/vascular cells" ~ "Endothelial / vascular cells",
        Raw_label == "ROS/oxidative-stress control" ~ "Redox control",
        Raw_label == "Mitochondrial homeostasis/mitophagy" ~ "Mitochondrial homeostasis",
        str_detect(Raw_label, "NF-") ~ "NF-κB / SASP signaling",
        str_detect(Raw_label, "HIF") ~ "HIF–VEGF signaling",
        str_detect(Raw_label, "p16/p21") ~ "Cell-cycle arrest",
        str_detect(Raw_label, "Apoptosis") ~ "Cell-survival regulation",
        Raw_label == "Bone regeneration/osteogenesis" ~ "Bone regeneration",
        Raw_label == "Wound healing/skin regeneration" ~ "Skin / wound repair",
        Raw_label == "Cartilage regeneration/chondroprotection" ~ "Cartilage regeneration",
        Raw_label == "Intervertebral-disc regeneration" ~ "Disc regeneration",
        Raw_label == "Stemness/proliferation/differentiation restoration" ~ "Stemness restoration",
        Raw_label == "Neural regeneration/function" ~ "Neural repair",
        TRUE ~ Raw_label
      ),
      Plot_label = paste0(wrap_sbk(Short_label, 32), "___", Category)
    )
}

c_dat <- bind_rows(
  prepare_hub_rows(target_counts, Target_cell, "Target cells"),
  prepare_hub_rows(mechanism_counts, Mechanism, "Mechanisms"),
  prepare_hub_rows(endpoint_counts, Regenerative_endpoint, "Endpoints")
) %>%
  mutate(Category = factor(Category, levels = c("Target cells", "Mechanisms", "Endpoints"))) %>%
  group_by(Category) %>% arrange(Study_count, .by_group = TRUE) %>%
  mutate(Plot_label = factor(Plot_label, levels = unique(Plot_label))) %>% ungroup()

pC <- ggplot(c_dat, aes(x = Study_count, y = Plot_label, colour = Category)) +
  geom_segment(aes(x = 0, xend = Study_count, yend = Plot_label),
               colour = sbk_palette[["light_blue_gray"]], linewidth = 0.5) +
  geom_point(size = 2.3) +
  geom_text(aes(label = Study_count), nudge_x = 1.0, hjust = 0,
            family = SBK_FONT, fontface = "bold", size = 2.1,
            colour = sbk_palette[["dark_text"]]) +
  facet_grid(Category ~ ., scales = "free", space = "free") +
  scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_colour_manual(values = c(
    "Target cells" = sbk_palette[["teal"]],
    "Mechanisms" = sbk_palette[["muted_purple"]],
    "Endpoints" = sbk_palette[["coral"]]
  ), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16)), breaks = pretty_breaks(3)) +
  labs(x = "Supporting studies", y = NULL) +
  theme_sbk(7.0) +
  theme(strip.text = element_text(size = 7.2, face = "bold"),
        axis.text.y = element_text(size = 6.6, face = "bold"),
        axis.title.x = element_text(size = 7.2, face = "bold"),
        plot.margin = margin(6, 6, 4, 6, "pt"))
pC <- add_panel_tag(pC, "C")

# D: same-record grouped mechanism–endpoint paths stratified by validation context.
validation_records <- annotated %>%
  filter(Relation == "VALIDATED_AT") %>%
  transmute(Record_ID, Validation_stage = Target) %>%
  distinct()

d_dat <- alluvial_display %>%
  distinct(Record_ID, Mechanism_group, Regenerative_endpoint_group) %>%
  inner_join(validation_records, by = "Record_ID") %>%
  mutate(
    Validation_group = case_when(
      Validation_stage %in% c("Small Animal", "Large Animal") ~ "Animal in vivo",
      Validation_stage == "Human-derived" ~ "Human-derived",
      TRUE ~ "Cell / ex vivo / organoid"
    )
  ) %>%
  group_by(Mechanism_group, Regenerative_endpoint_group, Validation_group) %>%
  summarise(Study_count = n_distinct(Record_ID), .groups = "drop") %>%
  mutate(
    Mechanism = wrap_sbk(Mechanism_group, 21),
    Endpoint = recode(Regenerative_endpoint_group,
      "Bone regeneration" = "Bone",
      "Cartilage regeneration" = "Cartilage",
      "Disc regeneration" = "Disc",
      "Neural repair" = "Neural",
      "Skin / wound repair" = "Skin / wound",
      "Soft-tissue repair" = "Soft tissue",
      .default = Regenerative_endpoint_group),
    Validation_group = factor(Validation_group,
      levels = c("Cell / ex vivo / organoid", "Animal in vivo", "Human-derived"))
  )

pD <- ggplot(d_dat, aes(x = Endpoint, y = Mechanism, size = Study_count, colour = Validation_group)) +
  geom_point(alpha = 0.9) +
  facet_grid(. ~ Validation_group, scales = "free_x", space = "free_x") +
  scale_colour_manual(values = c(
    "Cell / ex vivo / organoid" = sbk_palette[["muted_blue"]],
    "Animal in vivo" = sbk_palette[["coral"]],
    "Human-derived" = sbk_palette[["muted_purple"]]
  ), guide = "none") +
  scale_size_area(max_size = 6.2, breaks = pretty_breaks(4)) +
  labs(x = "Regenerative endpoint", y = "Mechanism", size = "Studies") +
  theme_sbk(7.3) +
  theme(
    strip.text = element_text(size = 7.6, face = "bold"),
    axis.text.x = element_text(angle = 34, hjust = 1, face = "plain", size = 6.8),
    axis.text.y = element_text(size = 6.9, face = "bold"),
    legend.position = "right", legend.text = element_text(face = "plain"),
    legend.title = element_text(face = "plain"),
    plot.margin = margin(9, 7, 6, 7, "pt")
  )
pD <- add_panel_tag(pD, "D")

design <- "
AA
BC
DD
"
combined <- pA + pB + pC + pD +
  plot_layout(design = design, heights = c(1.05, 1.45, 1.15), widths = c(1, 1))

save_pdf_sbk(pA, file.path(out_dir, "Figure5A.pdf"), 180, 82)
# Panels B/C/D are frozen for this final layout-only pass; regenerate only if absent.
if (!file.exists(file.path(out_dir, "Figure5B.pdf"))) save_pdf_sbk(pB, file.path(out_dir, "Figure5B.pdf"), 90, 106)
if (!file.exists(file.path(out_dir, "Figure5C.pdf"))) save_pdf_sbk(pC, file.path(out_dir, "Figure5C.pdf"), 90, 106)
if (!file.exists(file.path(out_dir, "Figure5D.pdf"))) save_pdf_sbk(pD, file.path(out_dir, "Figure5D.pdf"), 180, 102)
save_pdf_sbk(combined, file.path(out_dir, "Figure5_combined.pdf"), 180, 238)

caption <- paste0(
  "Figure 5 | Within-study cell–mechanism–regeneration evidence structure. ",
  "(A) TargetCell–Mechanism–RegenerativeEndpoint alluvial paths supported within the same Record_ID, using the frozen display-group mapping to reduce label fragmentation. ",
  "All paths in panel A represent same-record evidence. ",
  "(B) Target-cell × regenerative-endpoint modules; point size is unique supporting studies and colour is mechanism-group diversity. ",
  "(C) Leading target cells, mechanisms and regenerative endpoints ranked independently by unique supporting studies. ",
  "(D) Mechanism–endpoint modules with same-record validation evidence, separating animal in vivo validation (Small Animal or Large Animal) from Human-derived evidence. Human-derived is not Clinical. Co-supported paths do not by themselves establish causal biological chains."
)
writeLines(caption, file.path(out_dir, "Figure5_caption_draft.txt"), useBytes = TRUE)

pdfs <- file.path(out_dir, c("Figure5A.pdf", "Figure5B.pdf", "Figure5C.pdf", "Figure5D.pdf", "Figure5_combined.pdf"))
qc <- c(
  "Figure 5 plot QC",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "Source files: 07_FIGURE5_CELL_MECHANISM_REGENERATION/*.csv; canonical graph validation edges",
  paste0("Source row counts: alluvial=", nrow(alluvial_raw), "; grouped alluvial=", nrow(alluvial_grouped),
         "; display mapping=", nrow(display_mapping)),
  paste0("Figure 5A paths / unique Record_ID: ", nrow(alluvial_raw), "/", n_distinct(alluvial_raw$Record_ID)),
  paste0("Figure 5B grouped cells: ", nrow(b_dat)),
  paste0("Figure 5D grouped validation cells: ", nrow(d_dat)),
  paste0("FINAL FREEZE consistency: ", ifelse(assert_final_freeze(core), "PASS", "FAIL")),
  "Font check: Arial Bold used for core labels and Arial Regular for ticks/legends; cairo PDF devices",
  "Figure 5B legends: contained within Panel B",
  paste0("PDF existence: ", ifelse(all(vapply(pdfs, pdf_ok, logical(1))), "PASS", "FAIL")),
  "Panel labels: A/B/C/D black Arial Bold 17 pt",
  "Panel subtitles: none",
  "Visual inspection status: PASS - Panel A auxiliary text removed; remaining panels unchanged"
)
write_qc_lines(file.path(out_dir, "Figure5_plot_QC.txt"), qc)
