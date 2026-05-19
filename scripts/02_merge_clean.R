library(tidyverse)

file_pattern <- c(
  DEMO  = "^DEMO",
  DPQ   = "^DPQ",
  FSQ   = "^FSQ",
  SLQ   = "^SLQ",
  PAQ   = "^PAQ",
  HUQ   = "^HUQ",
  SMQ   = "^SMQ",
  ALQ   = "ALQ"
)

files <- map(file_pattern, function(pat) {
  list.files('data/raw', pattern = pat, full.names = TRUE)
})
str(files)

file_list <- map(files, function(f) {
  map_df(f, function(f) {
    read_rds(f) |> 
      mutate(across(everything(), as.character))
  })
})
str(file_list)

nhanes_merge <- file_list$DEMO |> 
  left_join(file_list$DPQ, join_by("SEQN")) |> 
  left_join(file_list$FSQ, join_by("SEQN")) |> 
  left_join(file_list$SLQ, join_by("SEQN")) |> 
  left_join(file_list$PAQ, join_by("SEQN")) |> 
  left_join(file_list$HUQ, join_by("SEQN")) |> 
  left_join(file_list$SMQ, join_by("SEQN")) |> 
  left_join(file_list$ALQ, join_by("SEQN")) 

nrow(nhanes_merge)
ncol(nhanes_merge)
