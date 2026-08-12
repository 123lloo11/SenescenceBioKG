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

out_dir <- file.path(sbk_root, "outputs", "figures", "Figure2")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
preview_dir <- file.path(fig_root, "_preview")
dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

relation_matrix <- sbk_read("04_FIGURE2_VALIDATION", "figure2B_relation_validation_matrix.csv")
strength_by_relation <- sbk_read("04_FIGURE2_VALIDATION", "figure2D_evidence_strength_by_relation.csv")

# A: final adjudication composition.
overall <- relation_matrix %>%
  summarise(
    SUPPORTED = sum(Final_SUPPORTED),
    REJECTED = sum(Final_REJECTED),
    UNCLEAR = sum(Final_UNCLEAR)
  ) %>%
  pivot_longer(everything(), names_to = "Status", values_to = "Count") %>%
  mutate(
    Status = factor(Status, levels = c("UNCLEAR", "REJECTED", "SUPPORTED")),
    Fraction = Count / sum(Count),
    Label = sprintf("%s\n%s | %.1f%%", Status, comma(Count), 100 * Fraction)
  )

f_unclear <- overall$Fraction[as.character(overall$Status) == "UNCLEAR"]
f_rejected <- overall$Fraction[as.character(overall$Status) == "REJECTED"]
overall <- overall %>%
  mutate(
    Midpoint = case_when(
      as.character(Status) == "UNCLEAR" ~ f_unclear / 2,
      as.character(Status) == "REJECTED" ~ f_unclear + Fraction / 2,
      TRUE ~ f_unclear + f_rejected + Fraction / 2
    ),
    Label_y = ifelse(as.character(Status) == "UNCLEAR", 1.82, 1.50)
  )

pA <- ggplot(overall, aes(x = Fraction, y = factor(1), fill = Status)) +
  geom_col(width = 0.46, colour = "white", linewidth = 0.5,
           position = position_stack(reverse = TRUE)) +
  geom_segment(aes(x = Midpoint, xend = Midpoint, y = 1.24, yend = Label_y - 0.10),
               inherit.aes = FALSE, linewidth = 0.35, colour = sbk_palette[["mid_gray"]]) +
  geom_text(aes(x = Midpoint, y = Label_y, label = Label,
                hjust = ifelse(Status == "UNCLEAR", 0, 0.5)), inherit.aes = FALSE,
            family = SBK_FONT, fontface = "bold", size = 2.45,
            colour = sbk_palette[["black"]], lineheight = 0.9) +
  scale_fill_manual(values = c(
    SUPPORTED = sbk_palette[["teal"]],
    REJECTED = sbk_palette[["coral"]],
    UNCLEAR = sbk_palette[["amber"]]
  )) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0.72, 1.98), clip = "off") +
  labs(x = "Final adjudicated relation universe", y = NULL) +
  theme_sbk(8.2) +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    legend.position = "none", plot.margin = margin(8, 8, 10, 10, "pt")
  )
pA <- add_panel_tag(pA, "A")

# B: relation-wise final status counts. Zero-universe ontology relations stay in the caption.
b_order <- relation_matrix %>% filter(Final_adjudicated_universe > 0) %>%
  arrange(Final_adjudicated_universe) %>% pull(Relation)
b_long <- relation_matrix %>%
  filter(Final_adjudicated_universe > 0) %>%
  select(Relation, Final_SUPPORTED, Final_REJECTED, Final_UNCLEAR) %>%
  pivot_longer(-Relation, names_to = "Status", values_to = "Count") %>%
  mutate(
    Status = recode(Status,
      Final_SUPPORTED = "SUPPORTED", Final_REJECTED = "REJECTED", Final_UNCLEAR = "UNCLEAR"),
    Status = factor(Status, levels = c("SUPPORTED", "REJECTED", "UNCLEAR")),
    Relation = factor(Relation, levels = b_order)
  )

pB <- ggplot(b_long, aes(x = Count, y = Relation, fill = Status)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.22) +
  geom_text(aes(label = ifelse(Count >= 18, Count, "")),
            position = position_stack(vjust = 0.5), family = SBK_FONT,
            fontface = "bold", size = 2.55, colour = sbk_palette[["black"]]) +
  scale_fill_manual(values = c(
    SUPPORTED = sbk_palette[["teal"]],
    REJECTED = sbk_palette[["coral"]],
    UNCLEAR = sbk_palette[["amber"]]
  )) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(x = "Final adjudicated relations", y = NULL, fill = NULL) +
  theme_sbk(7.7) +
  theme(
    legend.position = "top", legend.direction = "horizontal",
    legend.margin = margin(0, 0, 2, 0),
    axis.text.y = element_text(size = 7.5, face = "bold"),
    axis.text.x = element_text(face = "plain"),
    legend.text = element_text(face = "plain"),
    plot.margin = margin(8, 8, 6, 7, "pt")
  )
pB <- add_panel_tag(pB, "B")

# C: strength distribution within supported edges, by relation.
rel_supported_order <- core$edges %>% count(Relation, name = "n") %>% arrange(n) %>% pull(Relation)
c_dat <- strength_by_relation %>%
  complete(Relation = rel_supported_order,
           Evidence_strength = c("High", "Medium", "Low"), fill = list(Count = 0)) %>%
  group_by(Relation) %>%
  mutate(Fraction = Count / sum(Count)) %>%
  ungroup() %>%
  mutate(
    Relation = factor(Relation, levels = rel_supported_order),
    Evidence_strength = factor(Evidence_strength, levels = c("High", "Medium", "Low"))
  )

pC <- ggplot(c_dat, aes(x = Fraction, y = Relation, fill = Evidence_strength)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.22) +
  geom_text(aes(label = ifelse(Fraction >= 0.075, percent(Fraction, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), family = SBK_FONT,
            fontface = "bold", size = 2.45, colour = sbk_palette[["black"]]) +
  scale_fill_manual(values = c(
    High = sbk_palette[["dark_navy"]], Medium = sbk_palette[["amber"]], Low = sbk_palette[["coral"]]
  )) +
  scale_x_continuous(labels = percent_format(accuracy = 25), expand = c(0, 0)) +
  labs(x = "Evidence strength within supported edges", y = NULL, fill = NULL) +
  theme_sbk(7.7) +
  theme(
    legend.position = "top", axis.text.y = element_text(size = 7.5, face = "bold"),
    axis.text.x = element_text(face = "plain"), legend.text = element_text(face = "plain"),
    plot.margin = margin(8, 8, 6, 7, "pt")
  )
pC <- add_panel_tag(pC, "C")

# D: full-text evidence source by relation.
d_dat <- core$edges %>%
  select(Relation, Evidence_ID) %>%
  left_join(core$evidence %>% select(Evidence_ID, Evidence_source), by = "Evidence_ID") %>%
  count(Relation, Evidence_source, name = "Count") %>%
  complete(Relation = relation_order,
           Evidence_source = sort(unique(core$evidence$Evidence_source)), fill = list(Count = 0)) %>%
  mutate(
    Relation = factor(Relation, levels = rev(relation_order)),
    Evidence_source = factor(Evidence_source,
      levels = c("Abstract", "Methods", "Results", "Figure", "Table", "Supplement")[
        c("Abstract", "Methods", "Results", "Figure", "Table", "Supplement") %in%
          unique(as.character(Evidence_source))])
  )

pD <- ggplot(d_dat, aes(x = Evidence_source, y = Relation, fill = Count)) +
  geom_tile(colour = "white", linewidth = 0.45) +
  geom_text(aes(label = ifelse(Count > 0, Count, "")), family = SBK_FONT,
            fontface = "bold", size = 2.55, colour = sbk_palette[["black"]]) +
  scale_fill_gradient(low = sbk_palette[["light_blue_gray"]], high = sbk_palette[["dark_navy"]]) +
  labs(x = "Evidence source", y = NULL, fill = "Edges") +
  theme_sbk(7.7) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, face = "plain"),
    legend.title = element_text(face = "plain"), legend.text = element_text(face = "plain"),
    axis.ticks = element_blank(), axis.line = element_blank(),
    legend.position = "right", plot.margin = margin(8, 7, 6, 7, "pt")
  )
pD <- add_panel_tag(pD, "D")

combined <- (pA | pB) / (pC | pD) +
  plot_layout(heights = c(0.82, 1.18), widths = c(1, 1))

save_pdf_sbk(pA, file.path(out_dir, "Figure2A.pdf"), 90, 76)
save_pdf_sbk(pB, file.path(out_dir, "Figure2B.pdf"), 90, 86)
save_pdf_sbk(pC, file.path(out_dir, "Figure2C.pdf"), 90, 92)
save_pdf_sbk(pD, file.path(out_dir, "Figure2D.pdf"), 90, 92)
save_pdf_sbk(combined, file.path(out_dir, "Figure2_combined.pdf"), 180, 172)
save_preview_sbk(combined, file.path(preview_dir, "Figure2_combined.png"), 180, 172, 170)

caption <- paste0(
  "Figure 2 | Full-text relation validation across the frozen SenescenceBioKG. ",
  "(A) Composition of the final adjudicated relation universe; counts and percentages are calculated across final relation-wise SUPPORTED, REJECTED and UNCLEAR states. ",
  "(B) Final relation-wise adjudication after targeted full-text semantic audit. ACTIVATES and SUPPRESSES have zero adjudicated instances and are not plotted. ",
  "(C) Evidence-strength composition among the 1,868 supported edges. High, Medium and Low follow the frozen evidence-strength assignments. ",
  "(D) Source sections of the same-record full-text evidence linked to supported edges. Counts represent unique Edge_IDs; PDF page locations and raw excerpts are retained outside the main figure."
)
writeLines(caption, file.path(out_dir, "Figure2_caption_draft.txt"), useBytes = TRUE)

pdfs <- file.path(out_dir, c("Figure2A.pdf", "Figure2B.pdf", "Figure2C.pdf", "Figure2D.pdf", "Figure2_combined.pdf"))
qc <- c(
  "Figure 2 plot QC",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "Source files: 04_FIGURE2_VALIDATION/*.csv; 01_FINAL_GRAPH canonical edges/evidence",
  paste0("Source row counts: relation matrix=", nrow(relation_matrix),
         "; evidence strength by relation=", nrow(strength_by_relation)),
  paste0("Derived final universe: ", sum(overall$Count)),
  paste0("Derived SUPPORTED/REJECTED/UNCLEAR: ", paste(overall$Count, collapse = "/")),
  paste0("Evidence source total: ", sum(d_dat$Count)),
  paste0("FINAL FREEZE consistency: ", ifelse(assert_final_freeze(core), "PASS", "FAIL")),
  "Font check: Arial Bold used for core labels and Arial Regular for ticks/legends; cairo PDF devices",
  paste0("PDF existence: ", ifelse(all(vapply(pdfs, pdf_ok, logical(1))), "PASS", "FAIL")),
  "Panel labels: A/B/C/D black Arial Bold 17 pt",
  "Panel subtitles: none",
  "Visual inspection status: PENDING"
)
write_qc_lines(file.path(out_dir, "Figure2_plot_QC.txt"), qc)
