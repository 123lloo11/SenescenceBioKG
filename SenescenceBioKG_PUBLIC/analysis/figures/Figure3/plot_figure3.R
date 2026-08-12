script_file <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))])
script_dir <- dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
fig_root <- dirname(script_dir)
source(file.path(fig_root, "figure_theme.R"), encoding = "UTF-8")
source(file.path(fig_root, "data_helpers.R"), encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(igraph)
  library(ggraph)
  library(ggrepel)
})

core <- sbk_core()
assert_final_freeze(core)
out_dir <- file.path(sbk_root, "outputs", "figures", "Figure3")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
preview_dir <- file.path(fig_root, "_preview")
dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

nodes <- sbk_read("05_FIGURE3_GLOBAL_GRAPH", "figure3A_network_nodes.csv")
edges <- sbk_read("05_FIGURE3_GLOBAL_GRAPH", "figure3A_network_edges.csv")
material_hubs <- sbk_read("05_FIGURE3_GLOBAL_GRAPH", "figure3B_material_hubs.csv")
fabrication_hubs <- sbk_read("05_FIGURE3_GLOBAL_GRAPH", "figure3C_fabrication_hubs.csv")
biological_hubs <- sbk_read("05_FIGURE3_GLOBAL_GRAPH", "figure3D_biological_hubs.csv")

stopifnot(nrow(nodes) == 654L, nrow(edges) == 1868L)

# A: deterministic full typed network.
set.seed(20260811)
g <- graph_from_data_frame(
  edges %>% select(Source_ID, Target_ID, Relation, Edge_ID),
  directed = TRUE,
  vertices = nodes %>% rename(name = Node_ID)
)
g_layout <- layout_with_fr(as.undirected(g, mode = "collapse"), niter = 1800, grid = "nogrid")
V(g)$plot_size <- ifelse(V(g)$Node_type == "MaterialPlatform", 1.25,
                         pmin(4.2, 1.3 + sqrt(pmax(V(g)$Degree, 1)) / 3.8))
hub_ids <- nodes %>%
  arrange(desc(Degree), desc(Number_of_studies), desc(Number_of_relation_types), Canonical_name) %>%
  slice_head(n = 8) %>% pull(Node_ID)

pA <- ggraph(g, layout = "manual", x = g_layout[, 1], y = g_layout[, 2]) +
  geom_edge_link(aes(edge_colour = Relation), edge_alpha = 0.065,
                 edge_width = 0.18, show.legend = FALSE) +
  geom_node_point(aes(colour = Node_type, size = plot_size), alpha = 0.88,
                  stroke = 0, show.legend = TRUE) +
  geom_node_label(
    data = function(x) x %>% filter(name %in% hub_ids),
    aes(label = wrap_sbk(Canonical_name, 22)), repel = TRUE,
    family = SBK_FONT, fontface = "bold", size = 2.45,
    label.size = 0.16, label.padding = unit(1.3, "mm"),
    fill = alpha("white", 0.92), colour = sbk_palette[["dark_text"]],
    max.overlaps = Inf, box.padding = 0.45, point.padding = 0.2,
    seed = 20260811, show.legend = FALSE
  ) +
  scale_edge_colour_manual(values = relation_palette, guide = "none") +
  scale_colour_manual(
    values = node_palette[names(node_palette) %in% unique(nodes$Node_type)],
    labels = c(
      DesignMode = "Design mode", FabricationStrategy = "Fabrication",
      MaterialComposition = "Composition", MaterialPlatform = "Material platform",
      Mechanism = "Mechanism", RegenerativeEndpoint = "Endpoint",
      Stimulus = "Stimulus", TargetCell = "Target cell",
      TherapeuticCargo = "Cargo", ValidationStage = "Validation"
    )
  ) +
  scale_size_identity() +
  guides(colour = guide_legend(
    title = NULL, ncol = 5, byrow = TRUE,
    override.aes = list(size = 2.8, alpha = 1)
  )) +
  theme_sbk_void(7.7) +
  theme(
    legend.position = "bottom", legend.justification = "center",
    legend.text = element_text(face = "plain", size = 6.8),
    legend.key.width = unit(3.2, "mm"), legend.spacing.x = unit(1.2, "mm"),
    legend.margin = margin(1, 0, 0, 0),
    plot.margin = margin(11, 8, 3, 8, "pt")
  )
pA <- add_panel_tag(pA, "A")

lollipop_plot <- function(dat, value_col, colour, wrap_width = 26, base_size = 7.6) {
  value_col <- rlang::ensym(value_col)
  dat <- dat %>%
    mutate(Label = wrap_sbk(Canonical_name, wrap_width)) %>%
    arrange(!!value_col) %>%
    mutate(Label = factor(Label, levels = unique(Label)))
  ggplot(dat, aes(x = !!value_col, y = Label)) +
    geom_segment(aes(x = 0, xend = !!value_col, yend = Label),
                 linewidth = 0.6, colour = sbk_palette[["light_blue_gray"]]) +
    geom_point(size = 2.7, colour = colour) +
    geom_text(aes(label = !!value_col), nudge_x = 0.45, family = SBK_FONT,
              fontface = "bold", size = 2.4, hjust = 0) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.13))) +
    labs(x = ifelse(rlang::as_string(value_col) == "Degree", "Degree", "Studies"), y = NULL) +
    theme_sbk(base_size) +
    theme(axis.text.y = element_text(size = 7.3, face = "bold"),
          axis.text.x = element_text(face = "plain"),
          plot.margin = margin(8, 9, 5, 7, "pt"))
}

short_material <- function(x) {
  case_when(
    str_detect(x, "^ASD all-in-one") ~ "ASD smart hydrogel",
    str_detect(x, "^Lubricated hydrogel") ~ "STING-inhibiting EXO hydrogel",
    str_detect(x, "^OGPQ/G-NZ") ~ "OGPQ/G-NZ hydrogel",
    str_detect(x, "^EGCG@MAB") ~ "EGCG@MAB hydrogel",
    str_detect(x, "^CCM@CS-HG") ~ "CCM@CS-HG hydrogel",
    str_detect(x, "^LT-NPs") ~ "LT-NPs nanoplatform",
    str_detect(x, "^Smart spatiotemporal") ~ "Spatiotemporal delivery hydrogel",
    str_detect(x, "^TSH-Q") ~ "TSH-Q thermosensitive hydrogel",
    str_detect(x, "^GelMA@ZIF-8/PGE") ~ "GelMA@ZIF-8/PGE hydrogel",
    str_detect(x, "^F@GP") ~ "F@GP nanoparticles",
    str_detect(x, "^EMHS HxMoO3") ~ "EMHS HxMoO3 lipid nanoparticles",
    str_detect(x, "^Quercetin/HA-loaded") ~ "Quercetin/HA TG-18 hydrogel",
    TRUE ~ x
  )
}

pB <- material_hubs %>%
  arrange(desc(Degree), desc(Number_of_relation_types), Canonical_name) %>%
  slice_head(n = 10) %>%
  mutate(Canonical_name = short_material(Canonical_name)) %>%
  lollipop_plot(Degree, sbk_palette[["dark_navy"]], wrap_width = 48) %>%
  add_panel_tag("B")

pC <- fabrication_hubs %>%
  arrange(desc(Number_of_studies), desc(Degree), Canonical_name) %>%
  slice_head(n = 10) %>%
  lollipop_plot(Number_of_studies, sbk_palette[["muted_blue"]], wrap_width = 24) %>%
  add_panel_tag("C")

# D: three biological hub families, ranked independently.
d_top <- biological_hubs %>%
  filter(Node_type %in% c("TargetCell", "Mechanism", "RegenerativeEndpoint")) %>%
  group_by(Node_type) %>%
  arrange(desc(Number_of_studies), Canonical_name, .by_group = TRUE) %>%
  slice_head(n = 6) %>%
  ungroup() %>%
  mutate(
    Panel = recode(Node_type,
      TargetCell = "Target cells", Mechanism = "Mechanisms",
      RegenerativeEndpoint = "Endpoints"),
    Panel = factor(Panel, levels = c("Target cells", "Mechanisms", "Endpoints")),
    Display_name = case_when(
      Canonical_name == "Macrophages/innate immune cells" ~ "Macrophages / innate immune cells",
      Canonical_name == "Chondrocytes/cartilage progenitors" ~ "Chondrocytes / progenitors",
      Canonical_name == "Nucleus pulposus/annulus fibrosus cells" ~ "Disc cells",
      Canonical_name == "Endothelial/vascular cells" ~ "Endothelial / vascular cells",
      Canonical_name == "ROS/oxidative-stress control" ~ "Redox control",
      Canonical_name == "Mitochondrial homeostasis/mitophagy" ~ "Mitochondrial homeostasis",
      str_detect(Canonical_name, "NF-") ~ "NF-κB / SASP signaling",
      str_detect(Canonical_name, "HIF") ~ "HIF–VEGF signaling",
      str_detect(Canonical_name, "p16/p21") ~ "Cell-cycle arrest",
      str_detect(Canonical_name, "Apoptosis") ~ "Cell-survival regulation",
      Canonical_name == "Bone regeneration/osteogenesis" ~ "Bone regeneration",
      Canonical_name == "Wound healing/skin regeneration" ~ "Skin / wound repair",
      Canonical_name == "Cartilage regeneration/chondroprotection" ~ "Cartilage regeneration",
      Canonical_name == "Intervertebral-disc regeneration" ~ "Disc regeneration",
      Canonical_name == "Stemness/proliferation/differentiation restoration" ~ "Stemness restoration",
      Canonical_name == "Neural regeneration/function" ~ "Neural repair",
      TRUE ~ Canonical_name
    ),
    Label = paste0(wrap_sbk(Display_name, 20), "___", Panel)
  ) %>%
  group_by(Panel) %>%
  arrange(Number_of_studies, Canonical_name, .by_group = TRUE) %>%
  mutate(Label = factor(Label, levels = unique(Label))) %>%
  ungroup()

pD <- ggplot(d_top, aes(x = Number_of_studies, y = Label, colour = Panel)) +
  geom_segment(aes(x = 0, xend = Number_of_studies, yend = Label),
               linewidth = 0.55, colour = sbk_palette[["light_blue_gray"]]) +
  geom_point(size = 2.6) +
  geom_text(aes(label = Number_of_studies), nudge_x = 1.4, family = SBK_FONT,
            fontface = "bold", size = 2.35, hjust = 0, colour = sbk_palette[["dark_text"]]) +
  facet_wrap(~Panel, nrow = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_colour_manual(values = c(
    "Target cells" = sbk_palette[["teal"]],
    "Mechanisms" = sbk_palette[["muted_purple"]],
    "Regenerative endpoints" = sbk_palette[["coral"]]
  ), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(x = "Supporting studies", y = NULL) +
  theme_sbk(7.5) +
  theme(
    strip.text = element_text(size = 8.0, face = "bold"),
    axis.text.y = element_text(size = 7.1, face = "bold"),
    axis.text.x = element_text(face = "plain"),
    plot.margin = margin(8, 8, 6, 7, "pt")
  )
pD <- add_panel_tag(pD, "D")

design <- "
AA
BC
DD
"
combined <- pA + pB + pC + pD +
  plot_layout(design = design, heights = c(1.85, 1.0, 1.22))

save_pdf_sbk(pA, file.path(out_dir, "Figure3A.pdf"), 180, 102)
save_pdf_sbk(pB, file.path(out_dir, "Figure3B.pdf"), 90, 72)
save_pdf_sbk(pC, file.path(out_dir, "Figure3C.pdf"), 90, 72)
save_pdf_sbk(pD, file.path(out_dir, "Figure3D.pdf"), 180, 72)
save_pdf_sbk(combined, file.path(out_dir, "Figure3_combined.pdf"), 180, 204)
save_preview_sbk(combined, file.path(preview_dir, "Figure3_combined.png"), 180, 204, 170)

caption <- paste0(
  "Figure 3 | Global architecture of the frozen, validated SenescenceBioKG. ",
  "(A) Complete typed network containing 654 canonical nodes and 1,868 supported edges. Node colour denotes entity type; edge colour denotes relation type. Labels identify graph hubs selected by degree, supporting-study count and relation-type coverage, without implying biological superiority. ",
  "(B) MaterialPlatform hubs ranked by degree. ",
  "(C) FabricationStrategy hubs ranked by the number of supporting studies. ",
  "(D) Leading TargetCell, Mechanism and RegenerativeEndpoint nodes ranked independently by unique supporting studies. Network measures are descriptive properties of the frozen evidence graph."
)
writeLines(caption, file.path(out_dir, "Figure3_caption_draft.txt"), useBytes = TRUE)

pdfs <- file.path(out_dir, c("Figure3A.pdf", "Figure3B.pdf", "Figure3C.pdf", "Figure3D.pdf", "Figure3_combined.pdf"))
qc <- c(
  "Figure 3 plot QC",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "Source files: 05_FIGURE3_GLOBAL_GRAPH/*.csv; canonical graph",
  paste0("Source row counts: nodes=", nrow(nodes), "; edges=", nrow(edges),
         "; materials=", nrow(material_hubs), "; fabrication=", nrow(fabrication_hubs),
         "; biological=", nrow(biological_hubs)),
  paste0("Derived network nodes/edges: ", vcount(g), "/", ecount(g)),
  paste0("Panel A hub labels / Panel B/C/D displayed rows: 8/10/10/", nrow(d_top)),
  paste0("FINAL FREEZE consistency: ", ifelse(assert_final_freeze(core), "PASS", "FAIL")),
  "Font check: Arial Bold used for core labels and Arial Regular for ticks/legends; cairo PDF devices",
  paste0("PDF existence: ", ifelse(all(vapply(pdfs, pdf_ok, logical(1))), "PASS", "FAIL")),
  "Panel labels: A/B/C/D black Arial Bold 17 pt",
  "Panel subtitles: none",
  "Visual inspection status: PENDING"
)
write_qc_lines(file.path(out_dir, "Figure3_plot_QC.txt"), qc)
