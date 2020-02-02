#Loading packages
library(ggplot2)
library(DataExplorer)
library(lattice)
library(data.table)
library(mltools)
library(tidyverse)
library(caret)
library(ElemStatLearn)
library(gmodels)
library(DMwR)
library(class)

#loading zipcode data
zipcode_train = as.data.frame(ElemStatLearn::zip.train) 
zipcode_test = as.data.frame(ElemStatLearn::zip.test)

str(zipcode_train)
zipcode_train_sub = zipcode_train[which(zipcode_train$V1 == 2 | zipcode_train$V1 == 3),]
zipcode_test_sub = zipcode_test[which(zipcode_test$V1 == 2 | zipcode_test$V1 == 3),]

#Logistic regression
#Normalizing the data so that knn is not effected when distance is computed
zipcode_train_sub_scale <- as.data.frame(lapply(zipcode_train_sub[,-1],function(x) ((x - mean(x))/(max(x)-min(x)))))
zipcode_test_sub_scale <- as.data.frame(lapply(zipcode_test_sub[,-1],function(x) ((x - mean(x))/(max(x)-min(x)))))


v1 = zipcode_train_sub[,1]
zipcode_train_sub_scale = cbind(v1,zipcode_train_sub_scale)
zipcode_train_sub_scale$v1 = as.factor(zipcode_train_sub_scale$v1)
v1 = zipcode_test_sub[,1]
zipcode_test_sub_scale = cbind(v1,zipcode_test_sub_scale)
zipcode_test_sub_scale$v1 = as.factor(zipcode_test_sub_scale$v1)

summary(zipcode_train_sub_scale)

# Training Knn on training set and predicting on train dataset
set.seed(42)
i = 1
test_pred <- knn(train = zipcode_train_sub_scale,
                 test = zipcode_train_sub_scale,cl = zipcode_train_sub_scale$v1, k=i)
summary(test_pred)

train_error = list()
test_error = list()

for (i in c(1,3,5,7,9,11,13,15))
{
  train_pred <- knn(train = zipcode_train_sub_scale,
                   test = zipcode_train_sub_scale,cl = zipcode_train_sub_scale$v1, k=i)
  summary(train_pred)
  cat("for k value = ", i)
  train_table = CrossTable(x=zipcode_train_sub_scale$v1, y = train_pred, prop.chisq = FALSE)
  train_error[i] = train_table$prop.tbl[1,2] + train_table$prop.tbl[2,1]
}

for (i in c(1,3,5,7,9,11,13,15))
{
  test_pred <- knn(train = zipcode_train_sub_scale,
                    test = zipcode_test_sub_scale,cl = zipcode_train_sub_scale$v1, k=i)
  summary(test_pred)
  cat("for k value = ", i)
  test_table = CrossTable(x=zipcode_test_sub_scale$v1, y = test_pred, prop.chisq = FALSE)
  test_error[i] = test_table$prop.tbl[1,2] + test_table$prop.tbl[2,1]
}

train_error = train_error[lengths(train_error) != 0]
test_error = test_error[lengths(test_error) != 0]

train_error = as.data.frame(train_error)
test_error = as.data.frame(t(as.data.frame(test_error)))
k_frame = as.data.frame(c(1,3,5,7,9,11,13,15))
k_frame = cbind(k_frame, (train_error), test_error)
names(k_frame) = c("k_value","train_error_knn", "test_error_knn")

#Running linear regression
zipcode_train_sub_scale$v1 = as.numeric(as.character(zipcode_train_sub_scale))
train_data_lm = zipcode_train_sub_scale
model_lm <- lm(v1 ~., data = train_data_lm)

#predictions on train data
predictions_lm_train <- model_lm %>% predict(train_data_lm)
train_data_lm$v1_predict = predictions_lm_train
summary(predictions_lm_train)
train_data_lm$v1_predict = ifelse(train_data_lm$v1_predict<=1.5,2,3)
train_data_lm$v1_predict = as.factor(train_data_lm$v1_predict)
levels(train_data_lm$v1_predict)
train_lm_table = CrossTable(x=train_data_lm$v1, y = train_data_lm$v1_predict, prop.chisq = FALSE)
train_lm_table = train_lm_table$prop.tbl[1,2] + train_lm_table$prop.tbl[2,1]

#predictions on test data
test_data_lm = zipcode_test_sub_scale
predictions_lm_test <- model_lm %>% predict(test_data_lm)
test_data_lm$v1_predict = predictions_lm_test
summary(predictions_lm_test)
test_data_lm$v1_predict = ifelse(test_data_lm$v1_predict<=1.5,2,3)
test_data_lm$v1_predict = as.factor(test_data_lm$v1_predict)
levels(test_data_lm$v1_predict)
test_lm_table = CrossTable(x=test_data_lm$v1, y = test_data_lm$v1_predict, prop.chisq = FALSE)
test_lm_table = test_lm_table$prop.tbl[1,2] + test_lm_table$prop.tbl[2,1]

k_frame = cbind(k_frame,train_lm_table)
k_frame = cbind(k_frame,test_lm_table)
names(k_frame) = c("k_value","train_error_knn", "test_error_knn", "train_lm_error", "test_lm_error")
write.csv(k_frame,"knn_results.csv")
getwd()
