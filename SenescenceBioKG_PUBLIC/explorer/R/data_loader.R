suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

read_sbk_csv <- function(root, ...) {
  read_csv(file.path(root, ...), show_col_types = FALSE,
           locale = locale(encoding = "UTF-8"), na = c("", "NA"))
}

collapse_targets <- function(target, relation, wanted) {
  values <- sort(unique(target[relation == wanted & !is.na(target) & target != ""]))
  if (length(values) == 0) "" else paste(values, collapse = "; ")
}

load_sbk_explorer_data <- function(root) {
  nodes <- read_sbk_csv(root, "data", "nodes", "SenescenceBioKG_nodes_CANONICAL.csv")
  edges <- read_sbk_csv(root, "data", "edges", "SenescenceBioKG_edges_CANONICAL.csv")
  evidence <- read_sbk_csv(root, "data", "edges", "evidence_metadata_public.csv")

  stopifnot(nrow(nodes) == 654L, nrow(edges) == 1868L, nrow(evidence) == 1868L)

  source_names <- nodes %>%
    select(Source_ID = Node_ID, Material = Canonical_name, Source_node_type = Node_type)
  target_names <- nodes %>%
    select(Target_ID = Node_ID, Target = Canonical_name, Target_node_type = Node_type)

  edge_view <- edges %>%
    left_join(source_names, by = "Source_ID") %>%
    left_join(target_names, by = "Target_ID") %>%
    left_join(evidence %>%
                select(Evidence_ID, DOI, Evidence_location, Evidence_source,
                       Evidence_strength, Evidence_excerpt_available_locally),
              by = "Evidence_ID")

  material_table <- edge_view %>%
    group_by(Record_ID, Source_ID, Material) %>%
    summarise(
      DesignMode = collapse_targets(Target, Relation, "USES"),
      Stimulus = collapse_targets(Target, Relation, "RESPONDS_TO"),
      TargetCell = collapse_targets(Target, Relation, "TARGETS"),
      Mechanism = collapse_targets(Target, Relation, "MODULATES"),
      RegenerativeEndpoint = collapse_targets(Target, Relation, "PROMOTES"),
      ValidationStage = collapse_targets(Target, Relation, "VALIDATED_AT"),
      .groups = "drop"
    ) %>%
    arrange(Material, Record_ID)

  responds_sources <- edge_view %>% filter(Relation == "RESPONDS_TO") %>% distinct(Source_ID)
  default_material <- edge_view %>%
    semi_join(responds_sources, by = "Source_ID") %>%
    count(Source_ID, Material, name = "Degree") %>%
    arrange(desc(Degree), Material) %>%
    slice_head(n = 1) %>%
    pull(Material)

  cqs <- read_sbk_csv(root, "data", "queries", "competency_questions.csv")
  cq_results <- read_sbk_csv(root, "data", "queries", "query_results_final.csv")
  cq_metrics <- read_sbk_csv(root, "data", "queries", "cq_benchmark_metrics.csv")

  list(
    root = root,
    nodes = nodes,
    edges = edges,
    evidence = evidence,
    edge_view = edge_view,
    material_table = material_table,
    cqs = cqs,
    cq_results = cq_results,
    cq_metrics = cq_metrics,
    default_material = default_material
  )
}
