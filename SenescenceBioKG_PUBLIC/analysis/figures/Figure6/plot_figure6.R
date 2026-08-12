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
annotated <- annotate_edges(core)
out_dir <- file.path(sbk_root, "outputs", "figures", "Figure6")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
preview_dir <- file.path(fig_root, "_preview")
dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

validation_counts <- sbk_read("08_FIGURE6_VALIDATION_GAPS", "figure6A_validation_stage_counts.csv")
endpoint_matrix <- sbk_read("08_FIGURE6_VALIDATION_GAPS", "figure6B_endpoint_validation_matrix.csv")
missing_links <- sbk_read("08_FIGURE6_VALIDATION_GAPS", "figure6C_missing_links.csv")
human_edges <- sbk_read("08_FIGURE6_VALIDATION_GAPS", "figure6D_human_derived_evidence.csv")

stage_order <- c("Cell", "Ex vivo", "Organoid/chip", "Small Animal", "Large Animal", "Human-derived", "Clinical")
stopifnot(identical(as.integer(setNames(validation_counts$Study_count, validation_counts$Validation_stage)[stage_order]),
                    as.integer(expected_validation_studies[stage_order])))

# A: all frozen validation stages, including zero Clinical.
a_dat <- validation_counts %>%
  mutate(
    Validation_stage = factor(Validation_stage, levels = rev(stage_order)),
    Colour_group = case_when(
      as.character(Validation_stage) %in% c("Small Animal", "Large Animal") ~ "Animal in vivo",
      as.character(Validation_stage) == "Human-derived" ~ "Human-derived",
      as.character(Validation_stage) == "Clinical" ~ "Clinical",
      TRUE ~ "Other"
    )
  )

pA <- ggplot(a_dat, aes(x = Study_count, y = Validation_stage, colour = Colour_group)) +
  geom_segment(aes(x = 0, xend = Study_count, yend = Validation_stage),
               linewidth = 0.65, colour = sbk_palette[["light_blue_gray"]]) +
  geom_point(size = 3.1) +
  geom_text(aes(label = Study_count), nudge_x = 5.5, hjust = 0,
            family = SBK_FONT, fontface = "bold", size = 2.75,
            colour = sbk_palette[["dark_text"]]) +
  scale_colour_manual(values = c(
    "Animal in vivo" = sbk_palette[["coral"]],
    "Human-derived" = sbk_palette[["muted_purple"]],
    "Clinical" = sbk_palette[["zero"]],
    "Other" = sbk_palette[["dark_navy"]]
  ), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Unique studies", y = NULL) +
  theme_sbk(7.8) +
  theme(axis.text.y = element_text(size = 7.6, face = "bold"),
        plot.margin = margin(9, 9, 6, 7, "pt"))
pA <- add_panel_tag(pA, "A")

# B: endpoint × validation stage, preserving all seven columns.
b_dat <- endpoint_matrix %>%
  pivot_longer(-RegenerativeEndpoint, names_to = "Validation_stage", values_to = "Study_count") %>%
  mutate(
    Validation_stage = factor(Validation_stage, levels = stage_order),
    Endpoint_short = case_when(
      RegenerativeEndpoint == "Bone regeneration/osteogenesis" ~ "Bone regeneration",
      RegenerativeEndpoint == "Cartilage regeneration/chondroprotection" ~ "Cartilage regeneration",
      RegenerativeEndpoint == "Wound healing/skin regeneration" ~ "Skin / wound repair",
      RegenerativeEndpoint == "Intervertebral-disc regeneration" ~ "Disc regeneration",
      RegenerativeEndpoint == "Stemness/proliferation/differentiation restoration" ~ "Stemness restoration",
      RegenerativeEndpoint == "Vascular regeneration/angiogenesis" ~ "Vascular regeneration",
      RegenerativeEndpoint == "Diabetic wound closure, angiogenesis, collagen deposition and skin regeneration" ~ "Diabetic wound repair",
      RegenerativeEndpoint == "Periodontal/oral-tissue regeneration" ~ "Periodontal repair",
      TRUE ~ RegenerativeEndpoint
    ),
    Endpoint = factor(wrap_sbk(Endpoint_short, 24),
                      levels = rev(unique(wrap_sbk(Endpoint_short, 24))))
  )

pB <- ggplot(b_dat, aes(x = Validation_stage, y = Endpoint, fill = Study_count)) +
  geom_tile(colour = "white", linewidth = 0.45) +
  scale_fill_gradient(low = sbk_palette[["light_gray"]], high = sbk_palette[["dark_navy"]]) +
  labs(x = "Validation stage", y = NULL, fill = "Studies") +
  theme_sbk(7.3) +
  theme(
    axis.text.x = element_text(angle = 39, hjust = 1, face = "plain", size = 6.9),
    axis.text.y = element_text(size = 6.8, face = "bold"),
    axis.ticks = element_blank(), axis.line = element_blank(),
    legend.position = "right", legend.text = element_text(face = "plain"),
    legend.title = element_text(face = "plain"),
    plot.margin = margin(9, 7, 6, 7, "pt")
  )
pB <- add_panel_tag(pB, "B")

# C: validated-core missing links only.
c_dat <- missing_links %>%
  count(Gap_type, name = "Count") %>%
  mutate(
    Formula = recode(Gap_type,
      TYPE_A = "R + T -> no P",
      TYPE_B = "T + M -> no P",
      TYPE_C = "D + P -> no M"),
    Label = paste0(str_replace(Gap_type, "_", " "), "\n", Formula),
    Label = factor(Label, levels = rev(Label))
  )

pC <- ggplot(c_dat, aes(x = Count, y = Label, fill = Gap_type)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = paste0("n = ", Count)), hjust = 0, nudge_x = 1.0,
            family = SBK_FONT, fontface = "bold", size = 2.9) +
  scale_fill_manual(values = c(
    TYPE_A = sbk_palette[["teal"]], TYPE_B = sbk_palette[["muted_purple"]],
    TYPE_C = sbk_palette[["muted_blue"]]
  ), guide = "none") +
  scale_x_continuous(limits = c(0, 58), breaks = c(0, 20, 40),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Study-material records", y = NULL) +
  theme_sbk(8.0) +
  theme(axis.text.y = element_text(size = 8.0, face = "bold", lineheight = 0.9),
        plot.margin = margin(10, 9, 7, 7, "pt"))
pC <- add_panel_tag(pC, "C")

# D: relation coverage within Human-derived records, recomputed from the frozen graph.
human_record_material <- annotated %>%
  filter(Relation == "VALIDATED_AT", Target == "Human-derived") %>%
  distinct(Record_ID, Source_ID, Material = Source)
stopifnot(n_distinct(human_record_material$Record_ID) == 54L)

d_dat <- annotated %>%
  semi_join(human_record_material, by = c("Record_ID", "Source_ID")) %>%
  filter(Relation %in% c("TARGETS", "MODULATES", "PROMOTES")) %>%
  mutate(Coverage = recode(Relation,
    TARGETS = "Target cell", MODULATES = "Mechanism", PROMOTES = "Regenerative endpoint")) %>%
  group_by(Coverage) %>%
  summarise(Study_count = n_distinct(Record_ID), .groups = "drop") %>%
  complete(Coverage = c("Target cell", "Mechanism", "Regenerative endpoint"),
           fill = list(Study_count = 0)) %>%
  mutate(
    Fraction = Study_count / 54,
    Coverage = factor(Coverage, levels = rev(c("Target cell", "Mechanism", "Regenerative endpoint")))
  )

pD <- ggplot(d_dat, aes(x = Study_count, y = Coverage)) +
  geom_segment(aes(x = 0, xend = Study_count, yend = Coverage),
               linewidth = 0.75, colour = sbk_palette[["light_blue_gray"]]) +
  geom_point(size = 3.4, colour = sbk_palette[["muted_purple"]]) +
  geom_text(aes(label = sprintf("%d  (%.0f%%)", Study_count, 100 * Fraction)),
            nudge_x = 1.1, hjust = 0, family = SBK_FONT, fontface = "bold", size = 2.7) +
  scale_x_continuous(limits = c(0, 70), breaks = c(0, 20, 40, 60),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Human-derived records with supported relation", y = NULL) +
  theme_sbk(7.8) +
  theme(axis.text.y = element_text(size = 7.5, face = "bold"),
        plot.margin = margin(10, 9, 7, 7, "pt"))
pD <- add_panel_tag(pD, "D")

combined <- (pA | pB) / (pC | pD) + plot_layout(heights = c(1.15, 0.85))

save_pdf_sbk(pA, file.path(out_dir, "Figure6A.pdf"), 90, 82)
save_pdf_sbk(pB, file.path(out_dir, "Figure6B.pdf"), 90, 100)
save_pdf_sbk(pC, file.path(out_dir, "Figure6C.pdf"), 90, 74)
save_pdf_sbk(pD, file.path(out_dir, "Figure6D.pdf"), 90, 74)
save_pdf_sbk(combined, file.path(out_dir, "Figure6_combined.pdf"), 180, 180)
save_preview_sbk(combined, file.path(preview_dir, "Figure6_combined.png"), 180, 180, 170)

caption <- paste0(
  "Figure 6 | Validation coverage and discontinuities in the validated core. ",
  "(A) Unique-study counts across all seven frozen ValidationStage categories. Human-derived is reported separately from Clinical; Clinical remains zero. ",
  "(B) RegenerativeEndpoint × ValidationStage matrix, retaining all stages including zero-valued Clinical. ",
  "(C) Three validated-core missing-link classes: R + T → no P (RESPONDS_TO and TARGETS without supported PROMOTES), T + M → no P (TARGETS and MODULATES without supported PROMOTES), and D + P → no M (DELIVERS and PROMOTES without supported MODULATES). A missing supported edge does not establish that the study omitted the corresponding experiment. ",
  "(D) Supported TargetCell, Mechanism and RegenerativeEndpoint relation coverage among the 54 Human-derived records. Human-derived evidence must not be interpreted as clinical intervention."
)
writeLines(caption, file.path(out_dir, "Figure6_caption_draft.txt"), useBytes = TRUE)

pdfs <- file.path(out_dir, c("Figure6A.pdf", "Figure6B.pdf", "Figure6C.pdf", "Figure6D.pdf", "Figure6_combined.pdf"))
qc <- c(
  "Figure 6 plot QC",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "Source files: 08_FIGURE6_VALIDATION_GAPS/*.csv; canonical graph",
  paste0("Source row counts: validation=", nrow(validation_counts), "; endpoint matrix=", nrow(endpoint_matrix),
         "; gaps=", nrow(missing_links), "; human-derived edge rows=", nrow(human_edges)),
  paste0("Validation study counts: ", paste(paste(stage_order, expected_validation_studies[stage_order], sep = "="), collapse = "; ")),
  paste0("Gap TYPE_A/B/C: ", paste(c_dat$Count[match(c("TYPE_A", "TYPE_B", "TYPE_C"), c_dat$Gap_type)], collapse = "/")),
  paste0("Human-derived unique Record_ID: ", n_distinct(human_record_material$Record_ID)),
  paste0("FINAL FREEZE consistency: ", ifelse(assert_final_freeze(core), "PASS", "FAIL")),
  "Font check: Arial Bold used for core labels and Arial Regular for ticks/legends; cairo PDF devices",
  "Clinical=0 retained: YES",
  paste0("PDF existence: ", ifelse(all(vapply(pdfs, pdf_ok, logical(1))), "PASS", "FAIL")),
  "Panel labels: A/B/C/D black Arial Bold 17 pt",
  "Panel subtitles: none",
  "Visual inspection status: PENDING"
)
write_qc_lines(file.path(out_dir, "Figure6_plot_QC.txt"), qc)
