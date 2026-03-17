#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(renv)
  library(knitr)
})

args <- commandArgs(trailingOnly = TRUE)
repo_root <- if (length(args) >= 1) args[[1]] else "."
out_bib <- if (length(args) >= 2) args[[2]] else file.path(repo_root, "article", "packages_auto.bib")

paths <- c(
  file.path(repo_root, "article", "manuscript.qmd"),
  file.path(repo_root, "article", "scripts", "build_validation_metrics.R"),
  file.path(repo_root, "data_generation", "scripts")
)

deps <- renv::dependencies(path = paths, progress = FALSE)
pkgs <- sort(unique(na.omit(deps$Package)))

knitr::write_bib(pkgs, out_bib)

cat(sprintf("Wrote %d package citations to %s\n", length(pkgs), out_bib))
