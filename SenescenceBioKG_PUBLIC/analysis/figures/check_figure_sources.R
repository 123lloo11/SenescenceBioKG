#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(readr); library(dplyr) })
script_file <- sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))])
root <- normalizePath(file.path(dirname(script_file), "..", ".."), winslash="/", mustWork=TRUE)
dirs <- file.path(root, "data", "figure_source", paste0("figure", 1:7))
stopifnot(all(dir.exists(dirs)))
files <- unlist(lapply(dirs, list.files, pattern="\\.csv$", full.names=TRUE))
stopifnot(length(files) > 0L)
rows <- vapply(files, function(f) nrow(read_csv(f, show_col_types=FALSE)), integer(1))
stopifnot(all(rows > 0L))
out <- file.path(root, "outputs"); dir.create(out, showWarnings=FALSE)
write_csv(tibble(File=sub(paste0("^", root, "/?"), "", normalizePath(files, winslash="/")), Rows=rows),
          file.path(out, "figure_source_read_check.csv"))
cat("Figure 1-7 source CSVs readable:", length(files), "files\n")
