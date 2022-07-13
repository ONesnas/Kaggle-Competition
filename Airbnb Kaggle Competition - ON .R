
install.packages("Metrics")
###reading air bnb data

data <- read.csv("train.csv")
test_data <- read.csv("test.csv")
names(data)
names(test_data)
str(data)

### 1st submission (Linear regression)

model = lm(price~minimum_nights+review_scores_accuracy,data)

library(Metrics)
rmse(data$price, model$fitted.values)

test_data$predicted_price <- predict(model,test_data)

summary(model)

submissionFile = data.frame(id = test_data$id, price = test_data$predicted_price) 
write.csv(submissionFile, 'first_submissionON.csv',row.names = F)


### 2nd submission (Linear Regression)

sum(is.na(data))
which(is.na(data))
summary(data)
str(data)
data$calendar_updated <- as.factor(data$calendar_updated)
data$cancellation_policy <- as.factor(data$cancellation_policy)

data[is.na(data$security_deposit),"security_deposit"] <- 0
data[is.na(data)] <- 0
model_2= lm(price~minimum_nights+review_scores_accuracy+extra_people+guests_included+
              reviews_per_month+cancellation_policy+maximum_nights+security_deposit,data)

summary(model_2)
rmse(data$price, model_2$fitted.values)

test_data <- read.csv("test.csv")
test_data$calendar_updated <- as.factor(test_data$calendar_updated)
test_data$cancellation_policy <- as.factor(test_data$cancellation_policy)

test_data[is.na(test_data$security_deposit),"security_deposit"] <- 0
test_data[is.na(test_data)] <- 0

test_data$predicted_price <- predict(model_2,test_data)
submissionFile = data.frame(id = test_data$id, price = test_data$predicted_price) 
write.csv(submissionFile, 'second_submissionON.csv',row.names = F)

### 3rd submission (Decison Tree)
library(rpart)
str(data)
model_dt <- rpart(price~minimum_nights+review_scores_accuracy+extra_people+guests_included+
        reviews_per_month+cancellation_policy+maximum_nights+security_deposit+review_scores_rating+
         cleaning_fee,data)
predictions<- predict(model_dt, data)

summary(model_dt)

rmse(data$price, predictions)


test_data <- read.csv("test.csv")
test_data$calendar_updated <- as.factor(test_data$calendar_updated)
test_data$cancellation_policy <- as.factor(test_data$cancellation_policy)

test_data[is.na(test_data$security_deposit),"security_deposit"] <- 0
test_data[is.na(test_data)] <- 0

test_data$predicted_price <- predict(model_dt,test_data)
submissionFile = data.frame(id = test_data$id, price = test_data$predicted_price) 
write.csv(submissionFile, 'third_submissionON.csv',row.names = F)

### 4th Submission (Random Forest)

install.packages("randomForest")
library(randomForest)

model_rf <- randomForest(price~minimum_nights+review_scores_accuracy+extra_people+guests_included+
                    reviews_per_month
                    +maximum_nights+security_deposit+review_scores_rating+
                    cleaning_fee,data)
predictions<- predict(model_rf, data)
rmse(data$price, predictions)


test_data <- read.csv("test.csv")
test_data$calendar_updated <- as.factor(test_data$calendar_updated)
test_data$cancellation_policy <- as.factor(test_data$cancellation_policy)

test_data[is.na(test_data$security_deposit),"security_deposit"] <- 0
test_data[is.na(test_data)] <- 0
str(test_data)
test_data$predicted_price <- predict(model_rf,test_data)
submissionFile = data.frame(id = test_data$id, price = test_data$predicted_price) 
write.csv(submissionFile, 'fourth_submissionON.csv',row.names = F)


### Cleaning
data <- read.csv("train.csv")

###selecting only the desirable variables
str(data)
library(dplyr)
data$instant_bookable <- as.factor(data$instant_bookable)
data$has_availability <- as.factor(data$has_availability)
data$cancellation_policy <- as.factor(data$cancellation_policy)
req_data <- data %>% select(price,minimum_nights,review_scores_accuracy,extra_people,guests_included,
                  reviews_per_month
                ,maximum_nights,security_deposit,review_scores_rating,
                  cleaning_fee, number_of_reviews, instant_bookable, cancellation_policy)

summary(req_data)

### Removing all NA rows

req_data_full <- req_data[complete.cases(req_data),]
summary(req_data_full)

### Removing outliers

summary(req_data_full$minimum_nights)

req_data_full[req_data_full$minimum_nights > 10,"minimum_nights"] <- 10
req_data_full[req_data_full$maximum_nughts > 2000,"minimum_nights"] <- 2000
#install.packages("xgboost")

### 5th, 6th, 7th submission

library(xgboost)

labels <- req_data_full["price"]
req_data_full[,-1]
train <- data.matrix(req_data_full[,-1])
model_boosting <- xgboost(data = train,
        label = data.matrix(labels),
        booster = "gbtree", 
        objective = "reg:linear", 
        max.depth = 8, 
        eta = 0.7, 
        nthread = 2, 
        nround = 10, 
        min_child_weight = 5, 
        subsample = 0.5, 
        colsample_bytree = 1, 
        num_parallel_tree = 5)

test <- read.csv('test.csv')
test$instant_bookable <- as.factor(test$instant_bookable)
test$has_availability <- as.factor(test$has_availability)
test$cancellation_policy <- as.factor(test$cancellation_policy)
str(test)
req_test <- test %>% select(minimum_nights,review_scores_accuracy,extra_people,guests_included,
                            reviews_per_month
                            ,maximum_nights,security_deposit,review_scores_rating,
                            cleaning_fee, number_of_reviews,instant_bookable, cancellation_policy )

req_test[req_test$minimum_nights > 10,"minimum_nights"] <- 10
req_test[req_test$maximum_nughts > 2000,"minimum_nights"] <- 2000


req_test <- data.matrix(req_test)
#pred <- predict(mdel,test)

test$predicted_price <- predict(model_boosting,req_test)
submissionFile = data.frame(id = test$id, price = test$predicted_price) 
write.csv(submissionFile, 'check2_submissionON.csv',row.names = F)

### 8th submission (Random Forest with cleaned data)

install.packages("randomForest")
library(randomForest)

model_rf2 <- randomForest(price~minimum_nights+review_scores_accuracy+extra_people+guests_included+
                           reviews_per_month
                         +maximum_nights+security_deposit+review_scores_rating+
                           cleaning_fee,req_data_full)
predictions<- predict(model_rf2, req_data_full)
rmse(req_data_full$price, predictions)


test_data <- read.csv("test.csv")
test_data$calendar_updated <- as.factor(test_data$calendar_updated)
test_data$cancellation_policy <- as.factor(test_data$cancellation_policy)

test_data[is.na(test_data$security_deposit),"security_deposit"] <- 0
test_data[is.na(test_data)] <- 0
str(test_data)
test_data$predicted_pricerf2 <- predict(model_rf2,test_data)
submissionFile = data.frame(id = test_data$id, price = test_data$predicted_pricerf2) 
write.csv(submissionFile, '8.th_submissionON.csv',row.names = F)

### Attempted use of Tuned Forest Ranger Model
library(gbm)
set.seed(617)
boost = gbm(price~cleaning_fee+guests_included+security_deposit+extra_people+minimum_nights+reviews_per_month+review_scores_rating+maximum_nights,
            data=req_data_full,
            distribution="gaussian",
            n.trees = 500,
            interaction.depth = 2,
            shrinkage = 0.01)
pred = predict(boost,n.trees = 500)
rmse_boost_train = sqrt(mean((pred-req_data_full$price)^2)); rmse_boost_train

library(caret)
set.seed(617)
trControl = trainControl(method="cv",number=5)
tuneGrid = expand.grid(n.trees = 500, 
                       interaction.depth = c(1,2,3),
                       shrinkage = (1:100)*0.001,
                       n.minobsinnode=c(5,10,15))
str(req_data_full)
cvModel <- train(price~.,data=req_data_full,
                                          method="gbm",
                                          trControl=trControl, 
                                          tuneGrid=tuneGrid)
cvBoost = gbm(price~.,
              data=req_data_full,
              distribution="gaussian",
              n.trees=cvModel$bestTune$n.trees,
              interaction.depth=cvModel$bestTune$interaction.depth,
              shrinkage=cvModel$bestTune$shrinkage,
              n.minobsinnode = cvModel$bestTune$n.minobsinnode)
pred9 = predict(cvBoost,test,n.trees=500)
rmse_cv_boost = sqrt(mean((pred9-test$price)^2)) 
rmse_cv_boost


