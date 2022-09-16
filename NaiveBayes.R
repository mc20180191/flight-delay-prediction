dataset <- readRDS("sample_dataset.rds")
source("Source.R")

library(e1071)
library(bnlearn)
library(caret)
library(ggplot2)
library(pROC)
library(ROSE)
#install.packages('naivebayes')
library(naivebayes)

qqnorm(dataset$CRS_DEP_TIME, pch = 1, frame = FALSE)
qqnorm(dataset$CRS_ARR_TIME, pch = 1, frame = FALSE)
qqnorm(dataset$TAXI_OUT, pch = 1, frame = FALSE)

numeric_vals = c(4:6)
summary(dataset[,numeric_vals])
#Opseg za Taxi out odskace, a za crs kolone je isti

ggplot(dataset, mapping = aes(x = CRS_DEP_TIME)) +
  geom_histogram() +
  theme_minimal()
#0-500, 500-1000, 1000-1500, do 2k, do 24k (5 podeoka)
ggplot(dataset, mapping = aes(x = CRS_ARR_TIME)) +
  geom_histogram() +
  theme_minimal()
#isto 5
ggplot(dataset, mapping = aes(x = TAXI_OUT)) +
  geom_histogram() +
  theme_minimal()
#mozda 3 ili 4

dataset$CRS_DEP_TIME = as.numeric(dataset$CRS_DEP_TIME)
dataset$CRS_ARR_TIME = as.numeric(dataset$CRS_ARR_TIME)

discretized <- discretize(data = dataset[,numeric_vals], method = 'quantile', breaks = c(5,5,5))


summary(discretized)

col_dif <- setdiff(names(dataset), names(discretized))
data_disc <- cbind(discretized, dataset[,col_dif])
str(data_disc)


set.seed(5)
indexes <- createDataPartition(data_disc$DELAYED, p = 0.8, list = FALSE)
train_data <- data_disc[indexes, ]
test_data <- data_disc[-indexes, ]

#### prvi model ###########
set.seed(5)
nb1 <- naiveBayes(DELAYED ~ ., data = train_data)
print(nb1)
nb1_pred <- predict(nb1, newdata = test_data, type = 'class')

nb1_cm = table(actual=test_data$DELAYED, predicted=nb1_pred)
nb1_cm

nb1_eval = getEvaluationMetrics(nb1_cm)
nb1_eval
#Accuracy Precision    Recall        F1 
#0.8063938 0.3822222 0.1445378 0.2097561

nb1_pred_prob <- predict(nb1, newdata = test_data, type = "raw")

nb1_roc <- roc(response = as.numeric(test_data$DELAYED),
               predictor = nb1_pred_prob[,2],
               levels = c(1, 2))
nb1_roc$auc
#0.695
nb1_auc = nb1_roc$auc
##### balansiranje ######################
set.seed(5)
#control
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")


laplace_param <- expand.grid(laplace = 1, usekernel= FALSE, adjust = 1)

#downSample
set.seed(5)
train_down <- train(x = train_data[,-9], 
                     y = train_data$DELAYED,
                     method = "naive_bayes",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid = laplace_param)

#up
ctrl$sampling <- "up"

set.seed(5)
train_up <- train(x =train_data[,-9], 
                   y = train_data$DELAYED,
                   method = "naive_bayes",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid = laplace_param)

#rose
ctrl$sampling <- "rose"

set.seed(5)
train_rose <- train(x = train_data[,-9], 
                     y = train_data$DELAYED,
                     method = "naive_bayes",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid = laplace_param)

ctrl$sampling <- NULL

set.seed(5)
train_orig <- train(x = train_data[,-9], 
                    y = train_data$DELAYED,
                    method = "naive_bayes",
                    metric = "ROC",
                    trControl = ctrl,
                    tuneGrid = laplace_param)

models <- list(down = train_down,
               up = train_up,
               ROSE = train_rose,
               original = train_orig)

train_resample = resamples(models)
summary(train_resample, metric = "ROC")

#UP NAJ

train_up$finalModel

######################################################

nb2_pred = predict(train_up$finalModel, newdata = test_data[,-9], type = 'class') ##JJ: dodato [,-9] da bi se izbegao warning
nb2_cm = table(actual=test_data$DELAYED, predicted=nb2_pred)
nb2_cm

nb2_eval = getEvaluationMetrics(nb2_cm)
nb2_eval
#Accuracy Precision    Recall        F1 
#0.6154765 0.2590529 0.6252101 0.3663220 

#verovatnoce
nb2_pred_prob <- predict(train_up$finalModel, newdata = test_data, type = "prob")
head(nb2_pred_prob)

nb2_roc <- roc(response = as.numeric(test_data$DELAYED),
               predictor = nb2_pred_prob[,2],
               levels = c(1, 2))
nb2_roc$auc
#0.66
nb2_auc = nb2_roc$auc
plot.roc(nb2_roc, print.thres = TRUE, print.thres.best.method = "youden")


nb2_coords <- coords(nb2_roc, ret = c("accuracy", "spec", "sens", "precision", "thr"), x = "local maximas")
nb2_coords

tresh = 0.45
nb3_pred <- ifelse(test = nb2_pred_prob[,2] >= tresh, yes = "Yes", no = "No")
nb3_pred <- as.factor(nb3_pred)

nb3_cm <- table(actual = test_data$DELAYED, predicted = nb3_pred)
nb3_cm
nb3_eval = getEvaluationMetrics(nb3_cm)
nb3_eval

#Cilj je bio veci sensitivity...

data.frame(rbind(nb1_eval, nb2_eval, nb3_eval),
           row.names = c(paste("NB_", 1:3, sep = ""))) -> eval_df
eval_df


eval_df <- cbind(eval_df,
                 AUC = c(nb1_auc, nb2_auc, "/"))
eval_df

# Accuracy Precision    Recall        F1               AUC
# NB_1 0.8169557 0.4544025 0.1456286 0.2205686 0.695001910929169
# NB_2 0.6326119 0.2762378 0.6578483 0.3890917 0.695646005012797
# NB_3 0.5862347 0.2619155 0.7296548 0.3854652                 /