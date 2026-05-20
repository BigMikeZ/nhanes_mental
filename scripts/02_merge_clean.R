library(tidyverse)

# Read and join all data

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

# Clean data

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

nhanes_clean <- nhanes_clean |> 
  mutate(
    across(
      DPQ010:DPQ090, ~ case_when(
      . == "Not at all"              ~ 0,
      . == "Several days"            ~ 1,
      . == "More than half the days" ~ 2,
      . == "Nearly every day"        ~ 3,
      . == "Don't know"              ~ NA_real_,
      . == "Refused"                 ~ NA_real_
      )
    )
  ) |> 
  mutate(
    across(DPQ020:DPQ090, ~ if_else(DPQ010 == 0 & is.na(.), 0, .))
  ) |> 
  mutate(
    phq9_score = rowSums(across(DPQ010:DPQ090), na.rm = FALSE),
    depressed  = if_else(phq9_score >= 10, 1L, 0L)
  )

nhanes_clean |> 
  count(depressed)

nhanes_clean |> 
  ggplot(aes(x = phq9_score)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 10, color = "red", linetype = "dashed")

nhanes_clean |>
  select(RIAGENDR, RIDRETH3, DMDEDUC2, FSDAD, PAQ605, PAQ620, HUQ090, SMQ020, SMQ040) |>
  map(levels)

nhanes_clean_full <- nhanes_clean |> 
  mutate(
    across(
      c(DMDEDUC2, PAQ605, PAQ620, HUQ090, SMQ020),
      ~ if_else(. %in% c("Don't know", "Don't Know", "Refused"), NA, .)
      )
  ) |> 
  mutate(
    dmdeduc2_collapsed = case_when(
      DMDEDUC2 %in% c("Less than 9th grade", "9-11th grade (Includes 12th grade with no diploma)") ~ "Less than high school",
      DMDEDUC2 == "High school graduate/GED or equivalent"                                         ~ "High school/GED",
      DMDEDUC2 == "Some college or AA degree"                                                      ~ "Some college",
      DMDEDUC2 == "College graduate or above"                                                      ~ "College graduate+"
      )
  ) |> 
  mutate(
    dmdeduc2_collapsed = factor(
      dmdeduc2_collapsed,
      levels = c("College graduate+", "Some college", 
                 "High school/GED", "Less than high school")
      )
  ) |> 
  mutate(
    FSDAD = case_when(
      FSDAD == "AD full food security: 0"        ~ "Full",
      FSDAD == "AD marginal food security: 1-2"  ~ "Marginal",
      FSDAD == "AD low food security: 3-5"       ~ "Low",
      FSDAD == "AD very low food security: 6-10" ~ "Very low"
    )
  ) |> 
  mutate(
    fsdad_recoded = factor(
      FSDAD,
      levels = c("Full", "Marginal", "Low", "Very low"),
      ordered = TRUE
    )
  ) |> 
  mutate(
    smoking_status = case_when( 
      SMQ020 == "No"  & is.na(SMQ040)                            ~ "Never",
      SMQ020 == "Yes" & SMQ040 == "Not at all"                   ~ "Former",
      SMQ020 == "Yes" & SMQ040 %in% c("Some days", "Every day")  ~ "Current"
    )
  ) |> 
  mutate(
    smoking_status = factor(
      SMQ020,
      levels = c("Never", "Former", "Current"),
      ordered = TRUE
      )
  ) |>
  mutate(
    HUQ090 = case_when(
      HUQ090 %in% c("Don't know", "Refused")     ~  NA,
      HUQ090 == "Yes"                            ~  "Yes",
      HUQ090 == "No"                             ~  "No"
    )
  ) |>
  mutate(
    HUQ090 = factor(
      HUQ090,
      levels = c("Yes", "No"),
      ordered = TRUE
      )
  ) |> 
  mutate(
    ALQ130 = na_if(ALQ130, 999),
    ALQ130 = na_if(ALQ130, 777)
  ) |> 
  mutate(
    activity = case_when(
      PAQ605 == "No"  & PAQ620 == "No"               ~  "None",
      PAQ605 == "No"  & PAQ620 == "Yes"              ~  "Moderate",
      PAQ605 == "Yes" & PAQ620 %in% c("Yes", "No")   ~  "Vigorous"
    )
  ) |>
  mutate(
    activity = factor(
      activity,
      levels = c("Vigorous", "Moderate", "None"),
      ordered = TRUE
      )
  ) |> 
  mutate(
    sleep_hr = if_else(sleep_hr == 99, NA, sleep_hr)
  ) |> 
  mutate(
    WTMEC2YR = WTMEC2YR / 3
  ) |> 
  select(-c(SMQ020, SMQ040, DMDEDUC2, FSDAD, PAQ605, PAQ620)) 
  
saveRDS(nhanes_clean_full, "data/processed/nhanes_clean_full.rds")

nhanes_clean <- nhanes_clean_full |> 
  drop_na(depressed, RIDAGEYR, RIAGENDR, RIDRETH3, dmdeduc2_collapsed,
          INDFMPIR, fsdad_recoded, activity, HUQ090, 
          smoking_status, ALQ130, sleep_hr)
saveRDS(nhanes_clean, "data/processed/nhanes_clean.rds")

summary(nhanes_clean_full)
