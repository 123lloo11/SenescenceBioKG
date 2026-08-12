suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

explorer_node_colours <- c(
  MaterialPlatform = "#264653", MaterialComposition = "#7B94A2",
  FabricationStrategy = "#8A7F73", DesignMode = "#7C8B76",
  Stimulus = "#2A9D8F", TherapeuticCargo = "#457B9D",
  TargetCell = "#2A9D8F", Mechanism = "#7567B8",
  RegenerativeEndpoint = "#E76F51", ValidationStage = "#264653"
)

explorer_relation_colours <- c(
  RESPONDS_TO = "#2A9D8F", TARGETS = "#2A9D8F",
  DELIVERS = "#457B9D", RELEASES = "#457B9D",
  MODULATES = "#7567B8", PROMOTES = "#E76F51",
  VALIDATED_AT = "#264653", COMPOSED_OF = "#6F8794",
  FABRICATED_BY = "#8A7F73", USES = "#7C8B76"
)

build_vis_payload <- function(dat, edge_rows, max_edges = 180L) {
  if (nrow(edge_rows) == 0) return(list(nodes = tibble(), edges = tibble(), truncated = FALSE))

  truncated <- nrow(edge_rows) > max_edges
  if (truncated) {
    keep_sources <- edge_rows %>% count(Source_ID, sort = TRUE) %>% slice_head(n = 12) %>% pull(Source_ID)
    edge_rows <- edge_rows %>% filter(Source_ID %in% keep_sources) %>% slice_head(n = max_edges)
  }

  node_ids <- unique(c(edge_rows$Source_ID, edge_rows$Target_ID))
  vis_nodes <- dat$nodes %>%
    filter(Node_ID %in% node_ids) %>%
    transmute(
      id = Node_ID,
      label = str_wrap(Canonical_name, width = 24),
      title = paste0("<b>", htmltools::htmlEscape(Canonical_name), "</b><br>", Node_type),
      group = Node_type,
      color = unname(explorer_node_colours[Node_type]),
      shape = ifelse(Node_type == "MaterialPlatform", "hexagon", "dot"),
      size = ifelse(Node_type == "MaterialPlatform", 28, 19),
      font.size = ifelse(Node_type == "MaterialPlatform", 18, 14),
      font.face = "bold"
    )

  vis_edges <- edge_rows %>%
    transmute(
      id = Edge_ID, from = Source_ID, to = Target_ID,
      label = Relation,
      title = paste0("<b>", Relation, "</b><br>", Record_ID, " | ", Evidence_ID),
      color = unname(explorer_relation_colours[Relation]),
      width = 1.5, arrows = "to",
      font.size = 11, font.align = "middle"
    )

  list(nodes = vis_nodes, edges = vis_edges, truncated = truncated)
}
