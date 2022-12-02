# dependencies ----
library(data.table)
library(nnet)
library(tidyverse)

# read data ----
train <- fread("data/train.csv")

train |> glimpse()

# check target variable ----
## TODO: check "Res" and "Sub" positions
unique(train[ , Club_Position])

train |> 
  ggplot(aes(x = Aggression, fill = Club_Position)) +
  geom_density()

train |> 
  ggplot(aes(x = Stamina)) +
  geom_density() +
  facet_wrap(~Club_Position)

train_features_long <- 
  train |> 
  pivot_longer(cols = c(Ball_Control,
                        Agility,
                        Dribbling, 
                        Reactions, 
                        Vision, 
                        Composure, 
                        Crossing,
                        Acceleration, 
                        Speed,
                        Finishing,
                        Stamina),
               names_to = "Feature_Name", 
               values_to = "Feature_Value") |> 
  select(Club_Position, Feature_Name, Feature_Value)

train_features_long |> 
  ggplot(aes(x = Feature_Value, color = Feature_Name)) +
  geom_density() +
  facet_wrap(~Club_Position)

setDT(train_features_long)
quantiles_features_position <- 
  train_features_long[, c(as.list(quantile(Feature_Value, 
                                           probs=c(0, .25, .5, .75, 1)))), 
                    by=.(Club_Position, Feature_Name)]  

train |> 
  group_by(Club_Position) |> 
  tally() |> 
  ggplot(aes(x = reorder(Club_Position, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Posição do jogador", y = "Frequência absoluta")

# transform variables ----

train <- train[ , Club_Position := as.factor(Club_Position)]

# model ----
## 1st model ----
### multinomial logistic regression w/ only numeric player features
X_train <- 
  train |> 
  select_if(is.numeric) |>
  select(-c(Country_Club_id, National_Kit, Club_Kit, Contract_Expiry,
            Weak_foot, Skill_Moves))

Y_train <- train |> select(Club_Position)

train_characteristics <- cbind(X_train, Y_train)

fit_nnet <- nnet(Club_Position ~ ., data = train_characteristics, 
                 weights = rep(1, 4532),
                 size = 1)

### test ----

test <- fread("data/test.csv")

X_test <- 
  test |> 
  select_if(is.numeric) |>
  select(-c(Contract_Expiry,
            Weak_foot, 
            Skill_Moves))

Y_predicted <- fit_nnet |> predict(X_test)
