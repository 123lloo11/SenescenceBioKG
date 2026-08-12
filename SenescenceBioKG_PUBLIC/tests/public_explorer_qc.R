#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(dplyr) })
script_file <- sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))])
root <- normalizePath(file.path(dirname(script_file), ".."), winslash="/", mustWork=TRUE)
app_dir <- file.path(root, "explorer")
source(file.path(app_dir, "R", "data_loader.R"), encoding="UTF-8")
source(file.path(app_dir, "R", "query_engine.R"), encoding="UTF-8")
source(file.path(app_dir, "R", "network_helpers.R"), encoding="UTF-8")
dat <- load_sbk_explorer_data(root)
stopifnot(nrow(dat$nodes)==654L, nrow(dat$edges)==1868L, nrow(dat$evidence)==1868L)
stopifnot(!"Evidence_excerpt" %in% names(dat$evidence), n_distinct(dat$cqs$CQ_ID)==15L)
stopifnot(all(validate_cq_result_counts(dat)$Match))
f <- filter_sbk_edges(dat, material=dat$default_material)
p <- build_vis_payload(dat, f)
stopifnot(nrow(f)>0L, nrow(p$nodes)>0L, nrow(p$edges)>0L)
e <- edge_detail_row(dat, f$Edge_ID[[1]])
stopifnot(nzchar(e$Evidence_ID), nzchar(e$Evidence_location), "DOI" %in% names(e))

port <- httpuv::randomPort()
proc <- callr::r_bg(function(app_dir, port) {
  Sys.setenv(SBK_APP_DIR=app_dir)
  shiny::runApp(app_dir, host="127.0.0.1", port=port, launch.browser=FALSE)
}, args=list(app_dir=app_dir, port=port), supervise=TRUE)
on.exit(proc$kill(), add=TRUE)
ok <- FALSE
for (i in 1:30) {
  Sys.sleep(0.5)
  response <- try(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d", port)), silent=TRUE)
  if (!inherits(response, "try-error") && response$status_code==200L) { ok <- TRUE; break }
}
stopifnot(ok)
writeLines(c("Explorer public mode: PASS", "Private excerpt column absent: PASS", "Filters/network payload: PASS",
  "Evidence_ID/DOI/location fields: PASS", "CQ result counts: 15/15", "HTTP startup: PASS"),
  file.path(root, "outputs", "public_explorer_qc.txt"))
cat("Public Explorer QC: PASS\n")
