"
Project name: Assignment2 of FIT3152
Author: Chenlongjie Weng
Student number:29334152
"
# import the package we need at start
#install.packages("tree")
library(tree)
#install.packages("e1071")
library(e1071)
#install.packages(("ROCR"))
library(ROCR)
#install.packages("randomForest") 
library(randomForest) 
#install.packages("adabag") 
library(adabag)
#install.packages("rpart")
library(rpart) 
#install.packages("neuralnet") 
#library(neuralnet)
#detach("package:neuralnet", unload = TRUE)
library(car)
library(dplyr)

#####################################################################################
# First clear the work space and read file
setwd("C:/Users/WENGCHENLONGJIE/Desktop/FIT3152/Assignment2")
rm(list = ls())
WAUS <- read.csv("HumidPredict2023D.csv")
L <- as.data.frame(c(1:49))
set.seed(29334152) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

###################################################################################
# (1). Explore the data
MHT1=as.factor(WAUS$MHT)
summary(MHT1)

#####################################################################################
#（2). pre-processing to make the data set suitable for the model fitting
WAUS_original = WAUS #keep the original data set just in case
#remove all the variable I decide to remove as what I say in report
WAUS <- WAUS %>% dplyr::select(-c("Year")) #drop Year
#remove all the Na's
WAUS = WAUS[complete.cases(WAUS),]  
#show how many location I have after remove NAs. 
unique(WAUS$Location)
#then change the type of variables which should be a factor variables
WAUS$WindGustDir <- as.factor(WAUS$WindGustDir) #convert non-numeric variables to factor variables
WAUS$WindDir9am <- as.factor(WAUS$WindDir9am) #convert non-numeric variables to factors variables
WAUS$WindDir3pm <- as.factor(WAUS$WindDir3pm) #convert non-numeric variables to factors variables
WAUS$Cloud3pm <- as.factor(WAUS$Cloud3pm) #convert non-numeric variables to factors variables
WAUS$Cloud9am <- as.factor(WAUS$Cloud9am) #convert non-numeric variables to factors variables
WAUS$Location <- as.factor(WAUS$Location) #convert non-numeric variables to factors variables
WAUS$MHT = as.factor(WAUS$MHT) #set this be factor which means more humid or not
WAUS$RainToday = as.factor(WAUS$RainToday) #convert non-numeric variables to factors variables

#####################################################################################
# (3). Divide data into a 70% training and 30% test set
set.seed(29334152) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

#####################################################################################
# (4). create classification model by each technique
################ decision tree #################
WAUS.tree=tree(MHT~.,data = WAUS.train)
plot(WAUS.tree)
text(WAUS.tree, pretty = 1)

################ Naive Bayes #################
WAUS.bayes=naiveBayes(MHT~.,data = WAUS.train)

################ Bagging #################
WAUS.bag = bagging(MHT ~. , data = WAUS.train, mfinal=5) 

################ Boosting #################
WAUS.Boost <- boosting(MHT ~. , data = WAUS.train, mfinal=10)

################ Random Forest #################
WAUS.rf <- randomForest(MHT ~. , data = WAUS.train, na.action = na.exclude)

#####################################################################################
# (5). use test data to get Confusion matrix and report the accuracy of each model
set.seed(29334152)
################ decision tree #################
WAUS.pred = predict(WAUS.tree, WAUS.test, type = "class")
t1=table(actual=WAUS.test$MHT, predicted = WAUS.pred)
t1 
accuracy1 <- sum(diag(t1)) / sum(t1)  
accuracy1 #0.4561, 0.4386, 0.4269, 0.4211, 0.4327, 0.4444, 0.4152
accuracy1=(0.4561+0.4386+0.4269+0.4211+0.4327+0.4444+0.4152)/7 #0.4336

################ Naive Bayes #################
WAUS.predbayes = predict(WAUS.bayes, WAUS.test, type = "class")
t2=table(actual=WAUS.test$MHT, predicted = WAUS.predbayes)
t2 
accuracy2 <- sum(diag(t2)) / sum(t2)  
accuracy2 #0.5029

################ Bagging #################
WAUSpred.bag=predict.bagging(WAUS.bag,WAUS.test)
t3=table(observed=WAUS.test$MHT,predicted=WAUSpred.bag$class)
t3
accuracy3 <- sum(diag(t3)) / sum(t3)  
accuracy3 #0.5322

################ Boosting #################
WAUSpred.boost=predict.boosting(WAUS.Boost,WAUS.test)
#WAUSpred.boost$prob
t4=table(observed=WAUS.test$MHT,predicted=WAUSpred.boost$class)
accuracy4 <- sum(diag(t4)) / sum(t4)  
accuracy4 #0.5146

################ Random Forest #################
WAUSpredrf=predict(WAUS.rf,WAUS.test)
t5=table(Actual_Class=WAUS.test$MHT, Predicted_Class = WAUSpredrf)
accuracy5 <- sum(diag(t5)) / sum(t5)  
accuracy5 #0.5614+0.5673+0.5556
accuracy5 = (0.5614+0.5673+0.5556)/3 #0.5614


#####################################################################################
# (6). 
################ decision tree #################
WAUS.pred.tree = predict(WAUS.tree, WAUS.test, type = "vector")
WAUSDpred <- prediction(WAUS.pred.tree[,2], WAUS.test$MHT)
WAUSDperf <- performance(WAUSDpred,"tpr","fpr")
plot(WAUSDperf, main = "ROC Curve", col = "orange")
abline(0,1)
# calc auc
AUC.D = performance(WAUSDpred, "auc")
print(as.numeric(AUC.D@y.values)) #the area of decision tree 0.4662014


################ Naive Bayes #################
WAUSpred.bayes = predict(WAUS.bayes, WAUS.test, type = 'raw')
WAUSBpred <- prediction( WAUSpred.bayes[,2], WAUS.test$MHT)
WAUSBperf <- performance(WAUSBpred,"tpr","fpr")
plot(WAUSBperf, add=TRUE, col = "blueviolet")
# calc auc
AUC.N = performance(WAUSBpred, "auc")
print(as.numeric(AUC.N@y.values)) #the area of Naive Bayes 0.5432403

################ Bagging #################
WAUSBagpred <- prediction( WAUSpred.bag$prob[,2], WAUS.test$MHT)
WAUSBagperf <- performance(WAUSBagpred,"tpr","fpr")
plot(WAUSBagperf, add=TRUE, col = "blue")
# calc auc
AUC.B = performance(WAUSBagpred, "auc")
print(as.numeric(AUC.B@y.values)) #the area of decision tree 0.5476875

################ Boosting #################
WAUSBoostpred <- prediction( WAUSpred.boost$prob[,2], WAUS.test$MHT)
WAUSBoostperf <- performance(WAUSBoostpred,"tpr","fpr")
plot(WAUSBoostperf, add=TRUE, col = "red")
# calc auc
AUC.Boost = performance(WAUSBoostpred, "auc")
print(as.numeric(AUC.Boost@y.values)) #the area of Boosting 0.5191571


################ Random Forest #################
WAUSpred.rf <- predict(WAUS.rf, WAUS.test, type="prob")
WAUSFpred <- prediction( WAUSpred.rf[,2], WAUS.test$MHT)
WAUSFperf <- performance(WAUSFpred,"tpr","fpr")
plot(WAUSFperf, add=TRUE, col = "darkgreen")
# calc auc
AUC.F = performance(WAUSFpred, "auc")
print(as.numeric(AUC.F@y.values)) #the area of Random Forest 0.6208949

legend(0.85,0.44,legend = c("Decision Tree","Naive Bayes","Bagging","Boosting","Random Forest","X = Y"),
       lty = 1,col = c("orange","blueviolet","blue","red","darkgreen","black"),inset = c(0,0), cex = 0.5)



#####################################################################################
# (7). performance table for comparing result 
performance_table = data.frame(Classifier = c('Decision Tree','Naive Bayes','Bagging','Boosting','Random Forest'),
                                  Accuracy = c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5),
                                  AUC = c(0.4662014,0.5432403,0.5476875,0.5191571,0.6208949)) %>% arrange(desc(Accuracy))
print(performance_table)




#####################################################################################
# (8). determine the most important variables in predicting whether it will be more humid tomorrow or not
#Attribute importance for each one(bigger value is more important)
cat("\n#Decision Tree Attribute Importance\n")
print(summary(WAUS.tree))
cat("\n#Bagging Attribute Importance\n")
print(WAUS.bag$importance)
cat("\n#Boosting Attribute Importance\n")
print(WAUS.Boost$importance)
cat("\n#Random Forest Attribute Importance\n")
print(WAUS.rf$importance)


#####################################################################################
# (9). create a simple classifier (decision tree)
set.seed(29334152) #set seed to make sure each time get same result
cv.WAUS.tree = cv.tree(WAUS.tree, FUN = prune.misclass)
cv.WAUS.tree
#create the simple decision tree
simple.WAUS.tree = prune.misclass(WAUS.tree, best = 5)
summary(simple.WAUS.tree)
plot(simple.WAUS.tree)
text(simple.WAUS.tree, pretty = 1)
#then predict the simple decision tree model using test data
simple.WAUS.tree.pred = predict(simple.WAUS.tree, WAUS.test, type = "class")
#confusion matrix
simple.WAUS.tree.cm = table(actual = WAUS.test$MHT, predicted = simple.WAUS.tree.pred)
simple.WAUS.tree.cm
accuracy.simple <- sum(diag(simple.WAUS.tree.cm)) / sum(simple.WAUS.tree.cm)  
accuracy.simple #The Accuracy for Simple Decision Tree: 0.5847953
#then find simple decision tree confidence
simple.WAUS.tree.pred.confidence = predict(simple.WAUS.tree, WAUS.test, type = "vector")
#plot ROC curve for decision tree and simple decision tree
plot(WAUSDperf, main = "ROC Curve", col = "orange")
abline(0,1)
simple.WAUSDpred <- prediction(simple.WAUS.tree.pred.confidence[,2], WAUS.test$MHT)
simple.WAUSDperf <- performance(simple.WAUSDpred,"tpr","fpr")
plot(simple.WAUSDperf, add=T, col = "yellow")
legend(0.8,0.25,legend = c("Decision Tree","Simple Decision Tree","X = Y"),
       lty = 1,col = c("orange","yellow","black"),inset = c(0,0), cex = 0.5)
#calculate the AUC of simple decision tree ROC curve
AUC.SD = performance(simple.WAUSDpred, "auc")
print(as.numeric(AUC.SD@y.values)) #the area of simple decision tree: 0.5961275
#now create a table to easy compare each method
performance_table_simple = data.frame(Classifier = c('Decision Tree','Naive Bayes','Bagging','Boosting','Random Forest','Simple Decision Tree'),
                               Accuracy = c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy.simple),
                               AUC = c(0.4662014,0.5432403,0.5476875,0.5191571,0.6208949,0.5961275)) %>% arrange(desc(Accuracy))
print(performance_table_simple)


#####################################################################################
# (10).
set.seed(29334152)
WAUS.rf.best <- randomForest(MHT ~. , data = WAUS.train, na.action = na.exclude, ntree = 1300)
WAUSpredrf.best=predict(WAUS.rf.best,WAUS.test)
t5.best=table(Actual_Class=WAUS.test$MHT, Predicted_Class = WAUSpredrf.best)
accuracy5.best <- sum(diag(t5.best)) / sum(t5.best)  
accuracy5.best #0.6023
WAUSpred.rf.best <- predict(WAUS.rf.best, WAUS.test, type="prob")
WAUSFpred.best <- prediction( WAUSpred.rf.best[,2], WAUS.test$MHT)
WAUSFperf.best <- performance(WAUSFpred.best,"tpr","fpr")
plot(WAUSFperf, main = "ROC Curve", col = "orange")
plot(WAUSFperf.best, add=TRUE, col = "darkgreen")
abline(0,1)
legend(0.8,0.3,legend = c("Random Forest","Best Random Forest","X = Y"),
       lty = 1,col = c("orange","darkgreen","black"),inset = c(0,0), cex = 0.5)
# calc auc
AUC.F.best = performance(WAUSFpred.best, "auc")
print(as.numeric(AUC.F.best@y.values)) #the area of Random Forest 0.6327313
#now create a table to easy compare each method
performance_table_best = data.frame(Classifier = c('Decision Tree','Naive Bayes','Bagging','Boosting','Random Forest','Simple Decision Tree','Best Random Forest'),
                                      Accuracy = c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy.simple,accuracy5.best),
                                      AUC = c(0.4662014,0.5432403,0.5476875,0.5191571,0.6208949,0.5961275,0.6327313)) %>% arrange(desc(Accuracy))
print(performance_table_best)


#####################################################################################
# (11). create ANN and compare it with all other classifier
library(car)
library(neuralnet)
set.seed(29334152) #student ID as random seed

#first we omit less important variables
less.important = c("RainToday", "RISK_MM", "WindSpeed3pm")
# use dplyr:: to make sure the select method is from this package
# minus sign (-) is used in the select() function from the "dplyr" package to exclude specific columns from a data frame.
WAUS.important = WAUS_original %>% dplyr::select(-less.important)
#then we omit the no useful variables
WAUS.important <- WAUS.important %>% dplyr::select(-c("Year")) #drop Year

#below is convert the all the non numeric important variable to factors, 
str(WAUS.important) #use to make sure the most important variables: "WindDir3pm", "WindDir9am", "WindGustDir", "Cloud9am" in correct type
WAUS.important$WindGustDir <- as.factor(WAUS.important$WindGustDir) #convert non-numeric variables to factor variables
WAUS.important$WindDir9am <- as.factor(WAUS.important$WindDir9am) #convert non-numeric variables to factors variables
WAUS.important$WindDir3pm <- as.factor(WAUS.important$WindDir3pm) #convert non-numeric variables to factors variables

#remove all the Na's
WAUS.important = WAUS.important[complete.cases(WAUS.important),]  
WAUS.ANN = WAUS.important

#for now we can only contain the numeric variable then we can use it to fit ANN
#but WindGustDir+WindDir9am+WindDir3pm these three are not numeric
#we convert all categorical variables to binary variables, each indicating if the current
#data entry belongs to a category, being either 1 or 0.
#because these three are most important so we convert in to binary 
#then we can use it
WindDir_numberic = model.matrix(~WindGustDir+WindDir9am+WindDir3pm, data=WAUS.ANN) 
WAUS.ANN = cbind(WAUS.ANN, WindDir_numberic)

#Sampling the train and test data set
set.seed(29334152)
ind=sample(2,nrow(WAUS.ANN),replace = TRUE,prob = c(0.7,0.3)) # 70% train, 30% test
WAUS.ANN.train=WAUS.ANN[ind == 1,]; WAUS.ANN.test=WAUS.ANN[!ind==1,]

str(WAUS.ANN) #see all variables type, only use numeric variables
set.seed(29334152)
neural.model = neuralnet::neuralnet(MHT ~ Location + MinTemp + MaxTemp + Rainfall + Evaporation  
                                    + Sunshine + WindGustSpeed + WindSpeed9am + Pressure9am + Pressure3pm 
                                    + Cloud9am + Cloud3pm + Temp9am + Temp3pm + WindGustDirENE 
                                    + WindGustDirESE + WindGustDirN + WindGustDirNE + WindGustDirNNE 
                                    + WindGustDirNNW + WindGustDirNW + WindGustDirS
                                    + WindGustDirSE  + WindGustDirSSE + WindGustDirSSW 
                                    + WindGustDirSW + WindGustDirW + WindGustDirWNW + WindGustDirWSW 
                                    + WindDir9amENE + WindDir9amESE + WindDir9amN + WindDir9amNE 
                                    + WindDir9amNNE + WindDir9amNNW + WindDir9amNW + WindDir9amS 
                                    + WindDir9amSE + WindDir9amSSE + WindDir9amSSW + WindDir9amSW 
                                    + WindDir9amW + WindDir9amWNW + WindDir9amWSW + WindDir3pmENE 
                                    + WindDir3pmESE + WindDir3pmN + WindDir3pmNE + WindDir3pmNNE 
                                    + WindDir3pmNNW  + WindDir3pmNW + WindDir3pmS + WindDir3pmSE 
                                    + WindDir3pmSSE + WindDir3pmSSW + WindDir3pmSW + WindDir3pmW
                                    + WindDir3pmWNW + WindDir3pmWSW, data = WAUS.ANN.train, 
                                    hidden = c(9,5,4,3), #number of hidden neurons (vertices) in each layer
                                    linear.output = FALSE) #making a binary classification model

neural.predict = neuralnet::compute(neural.model, WAUS.ANN.test)

# now round these down to integers
neural.predict.label = as.data.frame(round(neural.predict$net.result,0))
neural.cm = table(observed = WAUS.ANN.test$MHT, predicted = neural.predict.label$V1)
neural.cm
#calculate the accuracy and print it out
neural.accuracy <- sum(diag(neural.cm)) / sum(neural.cm)  
neural.accuracy # 0.5914

#the ROC for the ANN
neural.prediction = ROCR::prediction(neural.predict$net.result, WAUS.ANN.test$MHT)
neural.performance = performance(neural.prediction, "tpr","fpr")

#plot ROC for all classifiers
plot(WAUSDperf, main = "ROC Curve", col = "orange")
abline(0,1)
plot(WAUSBperf, add=TRUE, col = "blueviolet")
plot(WAUSBagperf, add=TRUE, col = "blue")
plot(WAUSBoostperf, add=TRUE, col = "red")
plot(WAUSFperf, add=TRUE, col = "darkgreen")
plot(simple.WAUSDperf, add=T, col = "yellow")
plot(WAUSFperf.best, add=TRUE, col = "green")
plot(neural.performance,col = 'pink',add = TRUE) #the ANN here

legend(0.8,0.6,legend = c("Decision Tree","Naive Bayes","Bagging","Boosting","Random Forest","Simple Decision Tree","Best Random Forest","ANN","X = Y"),
       lty = 1,col = c("orange","blueviolet","blue","red","darkgreen","yellow","green","pink","black"),inset = c(0,0), cex = 0.45)

#calculate the AUC of ANN
neural.AUC = performance(neural.prediction, 'auc') #calculate AUC
neural.AUC = neural.AUC@y.values[[1]] #extract only the AUC value
neural.AUC #0.5708212

#then compare ANN with the classifiers we create before
performance_table_best = data.frame(Classifier = c('Decision Tree','Naive Bayes','Bagging','Boosting','Random Forest','Simple Decision Tree','Best Random Forest','ANN'),
                                    Accuracy = c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy.simple,accuracy5.best,neural.accuracy),
                                    AUC = c(0.4662014,0.5432403,0.5476875,0.5191571,0.6208949,0.5961275,0.6327313,0.5708212)) %>% arrange(desc(Accuracy))
print(performance_table_best)

detach(package:neuralnet) #detach the neuralnet package to avoid conflicts with other packages



#####################################################################################
# (12). use new model from web, which are XGBoost.
#install.packages("xgboost")
library(xgboost)
WAUS.XGboost = WAUS.important

WindDir_numberic = model.matrix(~WindGustDir+WindDir9am+WindDir3pm, data=WAUS.XGboost) 
WAUS.XGboost = cbind(WAUS.XGboost, WindDir_numberic)

WAUS.XGboost = WAUS.XGboost %>%
  dplyr::select(-c("WindGustDir",
                   "WindDir9am",
                   "WindDir3pm"))

#Sampling the train and test data set
set.seed(29334152) 
ind=sample(2,nrow(WAUS.XGboost),replace = TRUE,prob = c(0.7,0.3)) # 70% train, 30% test
WAUS.XGboost.train=WAUS.XGboost[ind == 1,]; WAUS.XGboost.test=WAUS.XGboost[!ind==1,]

#convert data frame to matrix
train.MHT = as.matrix(WAUS.XGboost.train$MHT)
train.other = as.matrix(WAUS.XGboost.train[,-15])
test.MHT = as.matrix(WAUS.XGboost.test$MHT)
test.other = as.matrix(WAUS.XGboost.test[,-15])

#build XGBoost model
WAUS.XGBoost.model = xgboost::xgboost(data = train.other, label = train.MHT, nrounds = 200, objective = "binary:logistic", verbose = 0, max_depth = 1, eta = 0.59, colsample_bytree = 0.6)

#test the model's performance
WAUS.XGBoost.pred = predict(WAUS.XGBoost.model, test.other)
prediction = as.numeric(WAUS.XGBoost.pred > 0.5) #here we see greater than 0.5 as 1, else 0
XGBoost.cm = table(predicted = prediction, Actual = test.MHT)
XGBoost.cm
#calculate accuracy
XGBoost.accuracy <- sum(diag(XGBoost.cm)) / sum(XGBoost.cm)  
XGBoost.accuracy # 1

#XGBoost
xgboost.pred <- prediction(WAUS.XGBoost.pred, WAUS.XGboost.test$MHT)
xgboost.perf <- ROCR::performance(xgboost.pred,"tpr","fpr")

#ROC curve of all classifier in one plot to compare
plot(WAUSDperf, main = "ROC Curve", col = "orange")
abline(0,1)
plot(WAUSBperf, add=TRUE, col = "blueviolet")
plot(WAUSBagperf, add=TRUE, col = "blue")
plot(WAUSBoostperf, add=TRUE, col = "red")
plot(WAUSFperf, add=TRUE, col = "darkgreen")
plot(simple.WAUSDperf, add=T, col = "yellow")
plot(WAUSFperf.best, add=TRUE, col = "green")
plot(neural.performance,col = 'pink',add = TRUE) #the ANN here
plot(xgboost.perf, col = 'lightblue',add = TRUE) #the XGBoost here

legend(0.8,0.6,legend = c("Decision Tree","Naive Bayes","Bagging","Boosting","Random Forest","Simple Decision Tree","Best Random Forest","ANN","XGBoost","X = Y"),
       lty = 1,col = c("orange","blueviolet","blue","red","darkgreen","yellow","green","pink","lightblue","black"),inset = c(0,0), cex = 0.45)

#calculate auc
xgboost_AUC = ROCR::performance(xgboost.pred, "auc")
xgboost_AUC <- as.numeric(format(xgboost_AUC@y.values[[1]])) 
xgboost_AUC #0.5414094

#the table to show how it performance in all classifier.
performance_table_best = data.frame(Classifier = c('Decision Tree','Naive Bayes','Bagging','Boosting','Random Forest','Simple Decision Tree','Best Random Forest','ANN','XGBoost'),
                                    Accuracy = c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy.simple,accuracy5.best,neural.accuracy,XGBoost.accuracy),
                                    AUC = c(0.4662014,0.5432403,0.5476875,0.5191571,0.6208949,0.5961275,0.6327313,0.5708212,xgboost_AUC)) %>% arrange(desc(Accuracy))
print(performance_table_best)

