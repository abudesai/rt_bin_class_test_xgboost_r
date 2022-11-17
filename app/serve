#!/usr/bin/env Rscript

library(plumber)
# 'endpoints.R' is the location of the endpoints file
pr("endpoints.R") %>%
  pr_run(host = "0.0.0.0", port=8080)