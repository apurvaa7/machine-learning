#This script illustrates how to use Random Forests for classification
#Read training data
train <- read.csv("train_Nov30_withtfidf.csv")

library("randomForest")
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
train$teambestanswer <- factor(ifelse(train$teambestanswer==0,0,1))
train$title <- as.character(train$title)
train$questiondetails <- as.character(train$questiondetails)

#Train the model
train.RF <- randomForest(teambestanswer~askerbestanswer + voterbestanswer + tfidfmatching + cosine_dist_completequestion + stringlength_postcontent + keyworddist,data=train,ntree=10000,mtry=sqrt(8),
                         sampsize=0.6*nrow(train),nodesize=3,classwt=c(.14,.86),importance=TRUE)
#Display variables by importance
var.imp <- train.RF$importance
var.imp
##                                         0          1 MeanDecreaseAccuracy
## askerbestanswer               0.003638187 0.10682239          0.020868475
## voterbestanswer               0.017598702 0.13466453          0.037145773
## tfidfmatching                -0.002980902 0.00439330         -0.001753500
## cosine_dist_completequestion -0.001876392 0.01700550          0.001258580
## stringlength_postcontent     -0.014765705 0.10666677          0.005505945
## keyworddist                  -0.011856289 0.03095988         -0.004696049
##                              MeanDecreaseGini
## askerbestanswer                      7.893687
## voterbestanswer                     12.076517
## tfidfmatching                       10.115596
## cosine_dist_completequestion        48.207699
## stringlength_postcontent            59.771225
## keyworddist                         18.417911
#Model Validation
QAtest <- read.csv("test_Nov30_withtfidf.csv")
test <- QAtest
test$title <- as.character(test$title)
test$questiondetails <- as.character(test$questiondetails)
test$teambestanswer <- factor(ifelse(test$teambestanswer==0,0,1))

prediction <- predict(train.RF,test)

#compute results
result <- table(prediction,test$teambestanswer)
precision <- result[2,2]/(result[2,2] + result[2,1])
recall <- result[2,2]/(result[2,2] + result[1,2])
accuracy <- (result[1,1] + result[2,2])/sum(result)
result
##           
## prediction   0   1
##          0 536 102
##          1  33  34
precision
## [1] 0.5074627
recall
## [1] 0.25
accuracy
## [1] 0.8085106
