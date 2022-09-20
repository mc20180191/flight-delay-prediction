dataset <- readRDS("sample_dataset.rds")

library(caret)
library(pROC)
library(ROSE)
library(e1071)
library(rpart)
library(rpart.plot)
source("Source.R")

set.seed(5)
indexes <- createDataPartition(dataset$DELAYED, p = 0.8, list = FALSE)
train_data <- dataset[indexes, ]
test_data <- dataset[-indexes, ]

prop.table(table(train_data$DELAYED))
#No        Yes
#0.8221167 0.1778833
################ TREE1 SA DEFAULT VREDNOSTIMA ###################
set.seed(5)
tree1 <- rpart(DELAYED ~ ., 
               data = train_data,
               method = "class",
               control = rpart.control(minsplit = 20, cp=0.01))
tree1

rpart.plot(tree1, extra = 106, split.fun=split.fun)


#Predikcije
tree1_pred <- predict(tree1, newdata = test_data, type = "class")

tree1_cm <- table(actual = test_data$DELAYED, predicted = tree1_pred)
tree1_cm

eval_tree1 <- getEvaluationMetrics(tree1_cm)
eval_tree1

tree1_pred_prob = predict(tree1, newdata = test_data)
tree1_auc = roc.curve(test_data$DELAYED, tree1_pred_prob[,2])$auc
tree1_auc

################ BALANSIRANJE i TREE2 #####################

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

#DOWN: 
set.seed(5)
train_down <- train(x = train_data[,-9],
                     y = train_data$DELAYED,
                     method = "rpart",
                     metric = "ROC",
                     trControl = ctrl)

#UP:
ctrl$sampling <- "up"
set.seed(5)
train_up <- train(x = train_data[,-9],
                   y = train_data$DELAYED,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl)


#ROSE:
ctrl$sampling <- "rose"
set.seed(5)
train_rose <- train(x = train_data[,-9],
                     y = train_data$DELAYED,
                     method = "rpart",
                     metric = "ROC",
                     trControl = ctrl)


#PRVI SKUP
ctrl$sampling <- NULL
set.seed(5)
train_original <- train(x = train_data[,-9],
                        y = train_data$DELAYED,
                         method = "rpart",
                         metric = "ROC",
                         trControl = ctrl)


models <- list(down = train_down,
               up = train_up,
               ROSE = train_rose,
               original = train_original)

train_resample = resamples(models)
summary(train_resample, metric = "ROC")

#Po medijani i sr vr je najbolji up

train_up$finalModel

rpart.plot(train_up$finalModel,  extra = 106, split.fun=split.fun)

##Predikcije:

tree2_pred <- predict(object = train_up$finalModel, newdata = test_data, type = "class")
tree2_cm <- table(actual=test_data$DELAYED, predicted=tree2_pred)
tree2_cm

eval_tree2 <- getEvaluationMetrics(tree2_cm)
eval_tree2

tree2_pred_prob = predict(train_up$finalModel, newdata = test_data)
tree2_auc = roc.curve(test_data$DELAYED, tree2_pred_prob[,2])$auc
tree2_auc

################### KROSVALIDACIJA + BALANSIRANJE za TREE3 ########################

cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by = 0.001))
ctrl2 <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")
set.seed(5)
down_cp <- train(x = train_data[,-9], 
                      y = train_data$DELAYED,
                      method = "rpart",
                      metric = "ROC",
                      trControl = ctrl2,
                      tuneGrid = cpGrid)

ctrl2$sampling = "up"

set.seed(5)
up_cp <- train(x = train_data[,-9], 
                 y = train_data$DELAYED,
                 method = "rpart",
                 metric = "ROC",
                 trControl = ctrl2,
                 tuneGrid = cpGrid)

ctrl2$sampling = "rose"
set.seed(5)
rose_cp <- train(x = train_data[,-9], 
               y = train_data$DELAYED,
               method = "rpart",
               metric = "ROC",
               trControl = ctrl2,
               tuneGrid = cpGrid)

ctrl2$sampling = NULL
set.seed(5)
orig_cp <- train(x = train_data[,-9], 
                 y = train_data$DELAYED,
                 method = "rpart",
                 metric = "ROC",
                 trControl = ctrl2,
                 tuneGrid = cpGrid)


modeli2 = list(down=down_cp,
               up=up_cp,
               rose=rose_cp,
               original=orig_cp)

resampling2=resamples(modeli2)

summary(resampling2, metric = "ROC")
#UP NAJBOLJI


up_cp$bestTune$cp #0.038
up_cp$finalModel
rpart.plot(up_cp$finalModel, extra = 106, split.fun=split.fun)


tree3_pred <- predict(object = up_cp$finalModel, newdata = test_data, type = "class")
tree3_cm <- table(actual=test_data$DELAYED, predicted=tree3_pred)
tree3_cm

eval_tree3 <- getEvaluationMetrics(tree3_cm)
eval_tree3

tree3_pred_prob = predict(up_cp$finalModel, newdata = test_data)
tree3_auc = roc.curve(test_data$DELAYED, tree3_pred_prob[,2])$auc
tree3_auc

data.frame(rbind(eval_tree1, eval_tree2, eval_tree3),
           row.names = c(paste("TREE_", 1:3, sep = ""))) -> eval_df

eval_df <- cbind(eval_df,
                 AUC = c(tree1_auc, tree2_auc, tree3_auc))
eval_df

#Accuracy Precision     Recall        F1       AUC
#TREE_1 0.8334454 0.7432432 0.09700176 0.1716069 0.5684331
#TREE_2 0.6940449 0.3002096 0.54119426 0.3861920 0.6776102
#TREE_3 0.6654568 0.2886498 0.60166289 0.3901323 0.6900624

###############################################################

varImp(up_cp, surrogates = FALSE, competes = TRUE, scale = TRUE)
# Overall
# CRS_DEP_TIME  100.00
# CRS_ARR_TIME   99.70
# DEST           98.77
# TAXI_OUT       98.61
# ORIGIN         78.05
# MONTH          58.06
# OP_CARRIER     30.57
# DAY             0.00