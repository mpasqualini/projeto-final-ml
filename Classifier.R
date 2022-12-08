# dependencies ----
library(data.table)
library(e1071)
library(janitor)
library(nnet)
library(randomForest)
library(rpart)
library(tidyverse)

set.seed(2305)

# utils ----
loss <- function(pred, ground_truth){
  pred <- as.character(pred)
  return(mean(str_detect(ground_truth, pred, negate = TRUE)))
}

# read data ----
train_raw <- fread("data/train.csv") |> clean_names()

train_raw |> glimpse()
unique(train_raw[ , preffered_position])

# prepare train dataset ----
## transformations ----
train <- 
  train_raw |> 
  mutate(preffered_position = str_split(preffered_position, "/")) |> 
  unnest(preffered_position)

rate_features <- 
  train |> 
  dplyr::select(contains("rate")) |> 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) |> 
  mutate(across(where(is.character), as.factor))

numeric_features <- 
  train |> 
  select(-contract_expiry) |> 
  select(-contains(c("id", "kit"))) |> 
  select_if(is.numeric)

## separate features and label ----
player_position <- train |> select(name, preffered_position)
train <- cbind(player_position, rate_features, numeric_features)

## transform target ----
train <- train |> mutate(preffered_position = as.factor(preffered_position))

## split train and validation ----
train_sample_ids <- sample.int(n=nrow(train), size=(.7*nrow(train)))
train <- train[train_sample_ids, ]
validation <- train[-train_sample_ids, ] 

# models ----
## model 1: multinomial regression ----
fit_multinom <- multinom(preffered_position ~ .,
                         data=train[ , !(names(train) == "name")]
)

Y_val <- 
  fit_multinom |> 
  predict(validation)

Y_val_pred <- 
  cbind(validation, Y_val) |> 
  select(name, preffered_position, Y_val) |> 
  distinct(name, Y_val)

## model 2: random forest ----
fit_rf <- randomForest(preffered_position ~ .,
             data=train[ , !(names(train) == "name")], 
             ntree = 300
)

Y_val_rf <- 
  fit_rf |> 
  predict(validation)

Y_val_rf_pred <- 
  cbind(validation, Y_val_rf) |> 
  select(name, preffered_position, Y_val_rf) |> 
  distinct(name, Y_val_rf)


## model 3: naive bayes ----
fit_nb <- naiveBayes(preffered_position ~ .,
                     data=train[ , !(names(train) == "name")]
)

Y_val_nb <- 
  fit_nb |> 
  predict(validation)

Y_val_nb_pred <- 
  cbind(validation, Y_val_nb) |> 
  select(name, preffered_position, Y_val_nb) |> 
  distinct(name, Y_val_nb)

## model 4: svm ----
fit_svm <- svm(preffered_position ~ .,
               data=train[ , !(names(train) == "name")],
               kernel="linear"
)

Y_val_svm <- 
  fit_svm |> 
  predict(validation)

Y_val_svm_pred <- 
  cbind(validation, Y_val_svm) |> 
  select(name, preffered_position, Y_val_svm) |> 
  distinct(name, Y_val_svm)

## model 5: decision trees ----
fit_tree <- rpart(preffered_position ~ .,
                  data=train[ , !(names(train) == "name")],
                  method = "class"
)

best_cp <- fit_tree$cptable[which.min(fit_tree$cptable[, "xerror"]), "CP"]
fit_tree_cp <- prune(fit_tree, cp = best_cp)

Y_val_tree <- 
  fit_tree_cp |> 
  predict(validation, type = "class")

Y_val_tree_pred <- 
  cbind(validation, Y_val_tree) |> 
  dplyr::select(name, preffered_position, Y_val_tree) |> 
  distinct(name, Y_val_tree)

# gathering all models ----
ground_truth <- validation |>
  left_join(train_raw, by = "name") |> select(name, preffered_position.y)

predictions <- reduce(list(Y_val_nb_pred,
                           Y_val_svm_pred,
                           Y_val_pred,
                           Y_val_tree_pred,
                           Y_val_rf_pred), dplyr::left_join, by = "name")

results <- ground_truth |> left_join(predictions, by = "name") |> distinct()

results_loss <- 
  results |>
  select(starts_with("Y")) |> 
  map(loss, results$preffered_position.y) |> 
  enframe(name = "Modelo", value = "Risco estimado") |> 
  unnest(cols = "Risco estimado")

write_csv(results_loss, "models/loss-models.csv")
saveRDS(fit_rf, "models/random-forest-classifier.RDS")
