# dependencies ----
library(data.table)
library(e1071)
library(MASS)
library(nnet)
library(randomForest)
library(rpart)
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

## transform target ----

train <- train |> mutate(Preffered_Position = as.factor(Preffered_Position))


## split train and validation ----
train_sample_ids <- sample.int(n=nrow(train), size=(.7*nrow(train)))
train <- train[train_sample_ids, ]
validation <- train[-train_sample_ids, ] 

# models ----
## model 1: multinomial regression ----
fit_multinom <- multinom(Preffered_Position ~ .,
                         data=train[ , !(names(train) == "Name")]
)

Y_val <- 
  fit_multinom |> 
  predict(validation)

Y_val_pred <- 
  cbind(validation, Y_val) |> 
  select(Name, Preffered_Position, Y_val) |> 
  distinct(Name, Y_val)

## model 2: random forest ----

fit_rf <- randomForest(Preffered_Position ~ .,
             data=train[ , !(names(train) == "Name")], 
             ntree = 300
)

Y_val_rf <- 
  fit_rf |> 
  predict(validation)

Y_val_rf_pred <- 
  cbind(validation, Y_val_rf) |> 
  select(Name, Preffered_Position, Y_val_rf) |> 
  distinct(Name, Y_val_rf)


## model 3: naive bayes ----

fit_nb <- naiveBayes(Preffered_Position ~ .,
                     data=train[ , !(names(train) == "Name")]
)

Y_val_nb <- 
  fit_nb |> 
  predict(validation)

Y_val_nb_pred <- 
  cbind(validation, Y_val_nb) |> 
  select(Name, Preffered_Position, Y_val_nb) |> 
  distinct(Name, Y_val_nb)

## modelo 4: svm ----

fit_svm <- svm(Preffered_Position ~ .,
               data=train[ , !(names(train) == "Name")],
               kernel="linear"
)

Y_val_svm <- 
  fit_svm |> 
  predict(validation)

Y_val_svm_pred <- 
  cbind(validation, Y_val_svm) |> 
  select(Name, Preffered_Position, Y_val_svm) |> 
  distinct(Name, Y_val_svm)

## modelo 5: 

fit_tree <- rpart(Preffered_Position ~ .,
                  data=train[ , !(names(train) == "Name")],
                  method = "class"
)

# poda:
best_cp <- fit_tree$cptable[which.min(fit_tree$cptable[, "xerror"]), "CP"]
fit_tree_cp <- prune(fit, cp = best_cp)

Y_val_tree <- 
  fit_tree_cp |> 
  predict(validation, type = "class")

Y_val_tree_pred <- 
  cbind(validation, Y_val_tree) |> 
  dplyr::select(Name, Preffered_Position, Y_val_tree) |> 
  distinct(Name, Y_val_tree)

  