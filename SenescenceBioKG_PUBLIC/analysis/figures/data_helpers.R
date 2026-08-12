suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
})

helper_dir <- dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE))
repo_root <- normalizePath(file.path(helper_dir, "..", ".."), winslash = "/", mustWork = TRUE)
sbk_root <- repo_root

public_path_map <- c(
  `01_FINAL_GRAPH/SenescenceBioKG_nodes_CANONICAL.csv` = "data/nodes/SenescenceBioKG_nodes_CANONICAL.csv",
  `01_FINAL_GRAPH/SenescenceBioKG_edges_CANONICAL.csv` = "data/edges/SenescenceBioKG_edges_CANONICAL.csv",
  `01_FINAL_GRAPH/SenescenceBioKG_evidence_FINAL.csv` = "data/edges/evidence_metadata_public.csv",
  `02_QUERY_BENCHMARK/SenescenceBioKG_graph_discoveries_FINAL.csv` = "data/queries/graph_discoveries_final.csv",
  `02_QUERY_BENCHMARK/SenescenceBioKG_query_results_FINAL.csv` = "data/queries/query_results_final.csv",
  `03_FIGURE1_CONSTRUCTION` = "data/figure_source/figure1",
  `04_FIGURE2_VALIDATION` = "data/figure_source/figure2",
  `05_FIGURE3_GLOBAL_GRAPH` = "data/figure_source/figure3",
  `06_FIGURE4_RESPONSIVENESS` = "data/figure_source/figure4",
  `07_FIGURE5_CELL_MECHANISM_REGENERATION` = "data/figure_source/figure5",
  `08_FIGURE6_VALIDATION_GAPS` = "data/figure_source/figure6",
  `09_FIGURE7_QUERY_UTILITY` = "data/figure_source/figure7",
  `10_SUPPLEMENTARY_DATA` = "data/figure_source/supplementary"
)

sbk_resolve <- function(...) {
  parts <- c(...); key <- paste(parts, collapse = "/")
  if (key %in% names(public_path_map)) return(file.path(sbk_root, public_path_map[[key]]))
  first <- parts[[1]]
  if (first %in% names(public_path_map)) return(file.path(sbk_root, public_path_map[[first]], parts[-1]))
  file.path(sbk_root, parts)
}

sbk_read <- function(...) {
  read_csv(sbk_resolve(...), show_col_types = FALSE,
           locale = locale(encoding = "UTF-8"), na = c("", "NA"))
}

sbk_core <- function() list(
  nodes = sbk_read("01_FINAL_GRAPH", "SenescenceBioKG_nodes_CANONICAL.csv"),
  edges = sbk_read("01_FINAL_GRAPH", "SenescenceBioKG_edges_CANONICAL.csv"),
  evidence = sbk_read("01_FINAL_GRAPH", "SenescenceBioKG_evidence_FINAL.csv")
)

expected_relation_counts <- c(COMPOSED_OF=213L, DELIVERS=71L, FABRICATED_BY=162L,
  MODULATES=88L, PROMOTES=107L, RELEASES=61L, RESPONDS_TO=64L, TARGETS=313L,
  USES=222L, VALIDATED_AT=567L)
expected_validation_studies <- c("Cell"=280L, "Ex vivo"=6L, "Organoid/chip"=6L,
  "Small Animal"=217L, "Large Animal"=4L, "Human-derived"=54L, "Clinical"=0L)

assert_final_freeze <- function(core = sbk_core()) {
  stopifnot(nrow(core$nodes)==654L, nrow(core$edges)==1868L, nrow(core$evidence)==1868L)
  stopifnot(n_distinct(core$nodes$Node_ID)==654L, n_distinct(core$edges$Edge_ID)==1868L)
  stopifnot(n_distinct(core$evidence$Evidence_ID)==1868L)
  rel <- core$edges %>% count(Relation); actual <- setNames(rel$n, rel$Relation)[names(expected_relation_counts)]
  stopifnot(identical(as.integer(actual), as.integer(expected_relation_counts)))
  stopifnot(all(core$edges$Evidence_ID %in% core$evidence$Evidence_ID))
  stopifnot(all(core$edges$Source_ID %in% core$nodes$Node_ID), all(core$edges$Target_ID %in% core$nodes$Node_ID))
  invisible(TRUE)
}

annotate_edges <- function(core = sbk_core()) {
  src <- core$nodes %>% select(Source_ID=Node_ID, Source=Canonical_name, Source_node_type=Node_type)
  tgt <- core$nodes %>% select(Target_ID=Node_ID, Target=Canonical_name, Target_node_type=Node_type)
  core$edges %>% left_join(src, by="Source_ID") %>% left_join(tgt, by="Target_ID") %>%
    left_join(core$evidence %>% select(Evidence_ID, Evidence_location, Evidence_source, DOI), by="Evidence_ID")
}

relation_order <- c("USES","COMPOSED_OF","FABRICATED_BY","RESPONDS_TO","DELIVERS","RELEASES","TARGETS","MODULATES","PROMOTES","VALIDATED_AT")
write_qc_lines <- function(path, lines) { dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE); writeLines(enc2utf8(lines), path, useBytes=TRUE) }
pdf_ok <- function(path, expected_pages=1L) file.exists(path) && file.info(path)$size > 1000 && identical(as.integer(pdftools::pdf_info(path)$pages), as.integer(expected_pages))
hash_files <- function(paths) tools::md5sum(paths)
