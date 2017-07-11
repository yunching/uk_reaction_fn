library(MacrobondAPI)
library(tidyverse)
library(readr)
library(Rblpapi)
library(caret)
library(magrittr)
library(purrr)
set.seed(888)

# Macrobond API does not seem to be able to read Bloomberg data
#my_dataset <- FetchTimeSeries("gbrate0001", "ih:bl:bcmpukdg index")

# read data manually pasted into a csv file
my_data <- read_csv(file.path("C:", "Users", "ylim","Documents", "uk_reaction_fn", "my_data.csv"), na="NaN")

#filter data to just MPC dates

my_data %<>%
  filter(mpc_meeting_date == 1)

# filter for periods where all data are available
my_data %<>%
  filter(!is.na(dgi))

#calculate change in bank rate
my_data %<>%
  mutate(policy_move = bank_rate - lead(bank_rate, 1))

#function to classify bank rate movements
policy_type <- function(x) {
  if (is.na(x)){
    return(NA)
  } else if (x > 0){
    return("hike")
  } else if (x < 0) {
    return("cut")
  } else {
    return("hold")
  }
}


# work out policy decision type
my_data$policy_decision <- map_chr(my_data$policy_move, policy_type)
my_data$policy_decision <- factor(my_data$policy_decision)
my_data %<>%
  filter(!is.na(policy_decision))
summary(my_data)

#split data for training
train_index <- createDataPartition(my_data$policy_decision, p = 0.8, list = FALSE)
head(train_index)

my_data_train <- my_data[train_index, ]
my_data_test <- my_data[-train_index, ]

# TODO split data for time series

# train linear regression model
model_lm <- train(bank_rate ~ . - policy_move - policy_decision - date - mpc_meeting_date, data = my_data_train, method = "lm")
summary(model_lm)

model_lm_2 <- train(bank_rate ~ . - policy_move - policy_decision - date - mpc_meeting_date, data = my_data_train, method = "lm", preProcess = c("center", "scale"))
summary(model_lm_2)

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 500,
                              horizon = 1,
                              fixedWindow = FALSE)

model_lm_3 <- train(bank_rate ~ . - policy_move - policy_decision - date - mpc_meeting_date, data = my_data, 
                    method = "lm", trControl = myTimeControl, preProcess = c("center", "scale"))
summary(model_lm_3)

# train random forest model
model_rf <- train(policy_decision ~ . - policy_move - date, data = my_data_train, method = "ranger")
  