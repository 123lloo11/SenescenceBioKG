#!/usr/bin/env Rscript
app_dir <- normalizePath(dirname(sub("^--file=", "", commandArgs(trailingOnly=FALSE)[grep("^--file=", commandArgs(trailingOnly=FALSE))])), winslash="/", mustWork=TRUE)
Sys.setenv(SBK_APP_DIR=app_dir)
shiny::runApp(app_dir, host="127.0.0.1", port=3838, launch.browser=interactive())
