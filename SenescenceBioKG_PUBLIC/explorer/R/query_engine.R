suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

filter_sbk_edges <- function(dat, material = "", relation = "", stimulus = "",
                             target_cell = "", mechanism = "", endpoint = "",
                             validation = "", record_id = "") {
  e <- dat$edge_view
  keys <- dat$material_table %>% select(Record_ID, Source_ID, Material)

  if (!is.null(material) && nzchar(material)) {
    keys <- keys %>% filter(Material == material)
  }
  if (!is.null(record_id) && nzchar(record_id)) {
    keys <- keys %>% filter(Record_ID == record_id)
  }

  apply_target_filter <- function(key_tbl, value, relation_name) {
    if (is.null(value) || !nzchar(value)) return(key_tbl)
    allowed <- e %>%
      filter(Relation == relation_name, Target == value) %>%
      distinct(Record_ID, Source_ID)
    semi_join(key_tbl, allowed, by = c("Record_ID", "Source_ID"))
  }

  keys <- apply_target_filter(keys, stimulus, "RESPONDS_TO")
  keys <- apply_target_filter(keys, target_cell, "TARGETS")
  keys <- apply_target_filter(keys, mechanism, "MODULATES")
  keys <- apply_target_filter(keys, endpoint, "PROMOTES")
  keys <- apply_target_filter(keys, validation, "VALIDATED_AT")

  out <- e %>% semi_join(keys, by = c("Record_ID", "Source_ID"))
  if (!is.null(relation) && nzchar(relation)) out <- out %>% filter(Relation == relation)
  out
}

edge_detail_row <- function(dat, edge_id) {
  row <- dat$edge_view %>% filter(Edge_ID == edge_id) %>% slice_head(n = 1)
  if (nrow(row) == 0) return(NULL)
  row
}

cq_result <- function(dat, cq_id) {
  dat$cq_results %>% filter(CQ_ID == cq_id)
}

validate_cq_result_counts <- function(dat) {
  actual <- dat$cq_results %>% count(CQ_ID, name = "Actual_n")
  check <- dat$cq_metrics %>%
    select(CQ_ID, KG_result_n) %>%
    left_join(actual, by = "CQ_ID") %>%
    mutate(Actual_n = coalesce(Actual_n, 0L), Match = KG_result_n == Actual_n)
  check
}
