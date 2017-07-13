library(MacrobondAPI)
library(tidyverse)
library(Rblpapi)
library(caret)
library(magrittr)
library(lubridate)
set.seed(888)

# Macrobond API does not seem to be able to read Bloomberg data
#my_dataset <- FetchTimeSeries("gbrate0001", "ih:bl:bcmpukdg index")

# read data manually pasted into a csv file
my_data0 <- read_csv(file.path("C:", "Users", "ylim","Documents", "uk_reaction_fn", "my_data.csv"), na="NaN")
head(my_data0)

#arrange date in chronological order
my_data0$date <- lubridate::mdy(my_data0$date)
my_data0 %<>% arrange(date)
my_data <- my_data0

#EDA
summary(my_data)
c(min(my_data$date), max(my_data$date))
table(year(my_data$date))

#filter data to just MPC dates
my_data %<>%
  filter(mpc_meeting_date == 1)
table(year(my_data$date))

# filter for periods where all data are available
my_data %<>%
  filter(!is.na(dgi))

#re-check data after filtering for NAs
summary(my_data)
c(min(my_data$date), max(my_data$date))
table(year(my_data$date))

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
table(year(my_data$date), my_data$policy_decision)
c(min(my_data$date), max(my_data$date))

#split data for training
train_index <- createDataPartition(my_data$policy_decision, p = 0.8, list = FALSE)
head(train_index)

my_data_train <- my_data[train_index, ]
my_data_test <- my_data[-train_index, ]

# train linear regression model
model_lm <- train(bank_rate ~ . - policy_move - policy_decision - date - mpc_meeting_date, data = my_data, method = "lm")
summary(model_lm)
varImp(model_lm)

# preprocessing linear model does not improve model fit
model_lm_2 <- train(bank_rate ~ . - policy_move - policy_decision - date - mpc_meeting_date, data = my_data_train, method = "lm", preProcess = c("center", "scale"))
summary(model_lm_2)
varImp(model_lm_2)

#create time slices for random forest
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 30,
                              horizon = 1,
                              fixedWindow = TRUE,
                              classProbs = TRUE)

# train linear model using time slices - no major changes from first linear model
model_lm_3 <- train(bank_rate ~ . - policy_move - policy_decision - date - mpc_meeting_date, data = my_data, 
                    method = "lm", trControl = myTimeControl, preProcess = c("center", "scale"))
summary(model_lm_3)
varImp(model_lm_3)

model_lm_final <- train(bank_rate ~ core_inflation + headline_inflation + dgi + wages_yoy + real_estate_yoy + euro_area_growth_yoy, data = my_data, 
                        method = "lm")
summary(model_lm_final)


plot(model_lm_final$finalModel$fitted.values, type = "l")
str(model_lm_final$finalModel$fitted.values)
tmp <- dplyr::bind_cols(my_data, list(predicted=model_lm_final$finalModel$fitted.values))

tmp %<>% 
  select(date, actual = bank_rate, predicted = predicted) %>%
  gather(key = type, value = rate, -date)
  
ggplot(tmp, aes(x = date, y = rate, color = type)) + geom_line() + labs(x = "Date", y = "Bank Rate", col = "Type") 

latest_inputs <- list(core_inflation = 2.6,
                      headline_inflation = 2.9,
                      dgi = 1.8,
                      wages_yoy = 1.5,
                      consumption_yoy = 1.9,
                      ftse_100_yoy = 11.4,
                      real_estate_yoy = 2.2,
                      euro_area_growth_yoy = 0.6,
                      euro_area_comp_pmi = 52,
                      euro_stoxx_yoy = 16.9,
                      mpc_meeting_date = 1,
                      msci_wrld_yoy = 13.7
                      )

predict(model_lm_final, latest_inputs)

# train random forest model
grid <-  expand.grid(mtry = seq(4,7))

model_rf0 <- train(policy_decision ~ . -policy_move -date , data = my_data, method = "ranger", importance = "impurity", metric = "Kappa")
print(model_rf0)
confusionMatrix(model_rf0)
varImp(model_rf0)

model_rf <- train(policy_decision ~ . -policy_move -date -mpc_meeting_date -bank_rate, data = my_data, method = "ranger", metric = "Kappa", tuneGrid = grid, importance = "impurity")
print(model_rf)
summary(model_rf)
confusionMatrix(model_rf)
varImp(model_rf)
ranger::predictions(model_rf$finalModel)


# prediction with selected models

my_data %>% 
  filter

