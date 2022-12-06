# dependencies ----
library(data.table)
library(nnet)
library(tidyverse)

set.seed(2305)  

# read data ----
train_raw <- fread("data/train.csv")

train_raw |> glimpse()
unique(train_raw[ , Preffered_Position])

# prepare train dataset ----
## transformations ----
train <- 
  train_raw |> 
  mutate(Preffered_Position = str_split(Preffered_Position, "/")) |> 
  unnest(Preffered_Position)

rate_features <- 
  train |> 
  select(contains("Rate")) |> 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) |> 
  mutate(across(where(is.character), as.factor))

numeric_features <- 
  train |> 
  select(-Contract_Expiry) |> 
  select(-contains(c("id", "Kit"))) |> 
  select_if(is.numeric)

## separate features and label ----

player_position <- train |> select(Name, Preffered_Position)
train <- cbind(player_position, rate_features, numeric_features)


## split train and validation ----
train_sample_ids <- sample.int(n=nrow(train), size=(.7*nrow(train)))
train <- train[train_sample_ids, ]
validation <- train[-train_sample_ids, ] 

# models ----
## model 1: multinomial regression ----
fit_multinom <- multinom(Preffered_Position ~ .,
                         data=train[ , !(names(train) == "Name")])
Y_val <- 
  fit_multinom |> 
  predict(validation)

Y_val_prob <- 
  fit_multinom |> 
  predict(validation, type = "prob") |>
  apply(1, max) 

cbind(validation, Y_val_prob)

cbind(validation, Y_val) |> 
  group_by(Name) |> 
  mutate(n = n()) |> 
  filter(n>1) |> 
  select(Name, Preffered_Position, Y_val, n) |> 
  View()
