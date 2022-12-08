library(data.table)
library(janitor)
library(randomForest)
library(tidyverse)

set.seed(2305) 

test_raw <- fread("data/test.csv") |> clean_names()

rate_features <- 
  test_raw |> 
  select(contains("rate")) |> 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) |> 
  mutate(across(where(is.character), as.factor))

numeric_features <- 
  test_raw |> 
  select(-contract_expiry) |> 
  select(-contains(c("id", "kit"))) |> 
  select_if(is.numeric)

test <- cbind(numeric_features, rate_features)

rf_model <- read_rds("models/random-forest-classifier.RDS")

rf_model |> 
  predict(test) |> 
  as_tibble() |> 
  rename(pred_class = "value") |> 
  rowid_to_column("id") |> 
  write_csv("prediction/test-prediction.csv")
