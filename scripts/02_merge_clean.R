library(tidyverse)

file_pattern <- c(
  DEMO  = "^DEMO",
  DPQ   = "^DPQ",
  FSQ   = "^FSQ",
  SLQ   = "^SLQ",
  PAQ   = "^PAQ",
  HUQ   = "^HUQ",
  SMQ   = "^SMQ",
  ALQ   = "^ALQ"
)

files <- map(file_pattern, function(pat) {
  list.files('data/raw', pattern = pat, full.names = TRUE)
})
str(files)

file_list <- map(files, function(f) {
  map_df(f, function(f) {
    read_rds(f) |> 
      mutate(
        cycle = str_extract(f, "[HIJ](?=\\.rds)"),
        across(everything(), as.character)
      )
  })
})

nhanes_merge <- file_list$DEMO |> 
  left_join(file_list$DPQ, join_by("SEQN")) |> 
  left_join(file_list$FSQ, join_by("SEQN")) |> 
  left_join(file_list$SLQ, join_by("SEQN")) |> 
  left_join(file_list$PAQ, join_by("SEQN")) |> 
  left_join(file_list$HUQ, join_by("SEQN")) |> 
  left_join(file_list$SMQ, join_by("SEQN")) |> 
  left_join(file_list$ALQ, join_by("SEQN")) 

str(nhanes_merge)

nhanes_merge <- nhanes_merge |> 
  rename(cycle = cycle.x) |> 
  select(-matches("cycle\\.y")) |> 
  select(-matches("cycle\\.x"))
nhanes_merge |> count(cycle)
ncol(nhanes_merge)

names(nhanes_merge)[names(nhanes_merge) %in% c("SLD012", "SLD010H")]

nhanes_clean <- nhanes_merge |> 
  select(
    SEQN, cycle, WTMEC2YR, SDMVPSU, SDMVSTRA, DPQ010:DPQ090, 
    RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, INDFMPIR, FSDAD,
    SLD010H, SLD012, PAQ605, PAQ620, HUQ090, SMQ020, SMQ040, 
    ALQ130
    ) |> 
  mutate(
    across(c(SEQN, WTMEC2YR, SDMVPSU, SDMVSTRA, RIDAGEYR, INDFMPIR,
             SLD010H, SLD012, ALQ130), as.numeric)
  ) |> 
  mutate(
    across(-c(SEQN, WTMEC2YR, SDMVPSU, SDMVSTRA, RIDAGEYR, INDFMPIR,
              SLD010H, SLD012, ALQ130), factor)
  ) |> 
  mutate(
    sleep_hr = coalesce(SLD010H, SLD012)
  ) |> 
  select(-c(SLD010H, SLD012))

nhanes_clean |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything()) |>
  arrange(desc(value)) |> 
  print(n = 27)

nhanes_clean |> count(RIDAGEYR < 18)

nhanes_clean <- nhanes_clean |> 
  filter(RIDAGEYR >= 18) 
nhanes_clean |> 
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything()) |>
  arrange(desc(value)) |> 
  print(n = 27)
