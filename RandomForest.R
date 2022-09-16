dataset <- readRDS("sample_dataset.rds")
source("Source.R")
library(caret)
library(pROC)
library(ROSE)
#install.packages("randomForest")
#library(randomForest)

#ranger ima bolju (efikasniju) implementaciju RF-a
#install.packages("ranger")
library(ranger)
library(e1071)

dataset$ORIGIN = NULL
dataset$DEST=NULL
#RF ne moze da radi sa DEST i ORIGIN, pa cemo da ih izbacimo

set.seed(5)
indexes <- createDataPartition(dataset$DELAYED, p = 0.8, list = FALSE)
train_data <- dataset[indexes, ]
test_data <- dataset[-indexes, ]

set.seed(5)

rf1 <- ranger(
  DELAYED ~ .,
  data=train_data
)

rf1_pred <- predict(object = rf1, data = test_data)

rf1_cm = table(actual=test_data$DELAYED, predicted=rf1_pred$predictions)
rf1_cm

rf1_eval = getEvaluationMetrics(rf1_cm)
rf1_eval
# Accuracy Precision    Recall        F1 
#0.8326836 0.6109537 0.1630134 0.2573588 
##################### BALANSIRANJE ##############################

set.seed(5)
down_train <- downSample(x = train_data[, -7],
                     y = train_data$DELAYED)
colnames(down_train)[7]="DELAYED"
table(down_train$DELAYED)


###### podela train seta na 2 dela: 80% za trening i 20% za validaciju ##############
set.seed(5)
indexes2 <- createDataPartition(train_data$DELAYED, p = 0.8, list = FALSE)
train_data_balance <- train_data[indexes2, ]
validation_data <- train_data[-indexes2, ]

#### trazenje najboljeg oblika balanisranja:
set.seed(5)
up_train2 <- upSample(x = train_data_balance[, -7],
                     y = train_data_balance$DELAYED)
colnames(up_train2)[7]="DELAYED"
table(up_train2$DELAYED)

set.seed(5)
down_train2 <- downSample(x = train_data_balance[, -7],
                         y = train_data_balance$DELAYED)
colnames(down_train2)[7]="DELAYED"
table(down_train2$DELAYED)

set.seed(5)
rose_train2 <- ROSE(DELAYED ~ ., data  = train_data_balance)$data
colnames(rose_train2)[7]="DELAYED"
table(rose_train2$DELAYED)

set.seed(5)
rf_up <- ranger(
  DELAYED ~ .,
  data=up_train2
)

rf_up_pred <- predict(object = rf_up, data = validation_data)

rf_up_cm = table(actual=validation_data$DELAYED, predicted=rf_up_pred$predictions)
rf_up_cm

rf_up_eval = getEvaluationMetrics(rf_up_cm)
rf_up_eval
#Accuracy Precision    Recall        F1
#0.8055898 0.4246295 0.2616499 0.3237873 

set.seed(5)
rf_down <- ranger(
  DELAYED ~ .,
  data=down_train2
)

rf_down_pred <- predict(object = rf_down, data = validation_data)

rf_down_cm = table(actual=validation_data$DELAYED, predicted=rf_down_pred$predictions)
rf_down_cm

rf_down_eval = getEvaluationMetrics(rf_down_cm)
rf_down_eval
#Accuracy Precision    Recall        F1
#0.6555954 0.2842838 0.6168136 0.3891924 

set.seed(5)
rf_rose <- ranger(
  DELAYED ~ .,
  data=rose_train2
)

rf_rose_pred <- predict(object = rf_rose, data = validation_data)

rf_rose_cm = table(actual=validation_data$DELAYED, predicted=rf_rose_pred$predictions)
rf_rose_cm
rf_rose_eval = getEvaluationMetrics(rf_rose_cm)
rf_rose_eval
#Accuracy Precision    Recall        F1
#0.7002913 0.3059768 0.5399874 0.3906161 

data.frame(rbind(rf_up_eval, rf_down_eval, rf_rose_eval),
           row.names = c("RF_UP","RF_DOWN","RF_ROSE")) -> eval_df
eval_df

#RF_DOWN JE NAJBOLJI
##model 2 sa kompletnim train datasetom i down balansiranjem 
#

set.seed(5)
down_train <- downSample(x = train_data[, -7],
                         y = train_data$DELAYED)
colnames(down_train)[7]="DELAYED"

set.seed(5)

model2 <- ranger(
  DELAYED ~ .,
  data=down_train
)

model2_pred <- predict(object = model2, data = test_data)
model2_cm = table(actual=test_data$DELAYED, predicted=model2_pred$predictions)
model2_cm
model2_eval = getEvaluationMetrics(model2_cm)
model2_eval
#Accuracy Precision    Recall        F1 
#0.6525967 0.2825787 0.6195515 0.3881304 
####################################################################
###### 3. model, krosvalidacija za mtry

grid <- expand.grid(
  .mtry = 2:4,
  .splitrule = "gini",
  .min.node.size = 5
)

ctrl <- trainControl(method = "CV",
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

fit = train(x = train_data[,-7], 
            y = train_data$DELAYED,
            method = "ranger",
            metric="ROC",
            tuneGrid = grid,
            trControl = ctrl,
            importance="impurity")


fit$finalModel
fit$bestTune$mtry


fit_model <- ranger(
  DELAYED ~ .,
  data=down_train,
  mtry = fit$bestTune$mtry
)

fit_pred <- predict(object = fit_model, data = test_data)
cm = table(actual=test_data$DELAYED, predicted=fit_pred$predictions)
cm

eval_fit_model = getEvaluationMetrics(cm)
eval_fit_model

data.frame(rbind(rf1_eval, model2_eval, eval_fit_model),
           row.names = c("rf1_eval","rf_down_eval","eval_fit_model (opt mtry)")) -> eval_df_final
eval_df_final

# Accuracy Precision    Recall        F1
# rf1_eval                  0.8326836 0.6109537 0.1630134 0.2573588
# rf_down_eval              0.6533136 0.2837962 0.6230789 0.3899708
# eval_fit_model (opt mtry) 0.6530448 0.2845398 0.6278660 0.3916084



varImp(fit, scale = TRUE)
# Overall
# CRS_ARR_TIME 100.000
# CRS_DEP_TIME  98.766
# TAXI_OUT      88.479
# MONTH         11.834
# DAY            2.968
# OP_CARRIER     0.000
