library(usethis)
library(tidyverse)
library(nhanesA)
library(haven)

git_vaccinate()

nhanesTables("Q", 2013)

suffixes <- c("H", "I", "J")
modules <- list(
  demo  = "DEMO",
  DPQ   = "DPQ",
  FSQ   = "FSQ",
  SLQ   = "SLQ",
  PAQ   = "PAQ",
  HUQ   = "HUQ",
  SMQ   = "SMQ",
  ALQ   = "ALQ"
)

walk(suffixes, function(suf) {
  walk2(names(modules), modules, function(label, mod) {
    table_name <- paste0(mod, "-", suf)
    dest       <- glue::glue("data/raw/{table_name}.rds")
    if(!file.exists(dest)) {
      message("Fetching: ", table_name)
      tryCatch({
        df <- nhanes(table_name)
        saveRDS(df, dest)
        message("  Saved: ", dest)
      },error = function(e) {
        message("  FAILED:", table_name, "-", e$message)
      }
      )
    } else {
      message("Already exists, skipping: ", table_name)
    }
  })
})



