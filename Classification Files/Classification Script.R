data <- read.csv("klasifikasimhs.csv")
head(data)
summary(data)
str(data)
data$Kelayakan.Keringanan.UKT <- ifelse(data$Kelayakan.Keringanan.UKT == "0","yes","no")
data$Kelayakan.Keringanan.UKT <- factor(data$Kelayakan.Keringanan.UKT,levels = c("yes","no"))
data$Tempat.Tinggal <- as.factor(data$Tempat.Tinggal)
str(data)

library(caret)
set.seed(300)
#Spliting data as training and test set. Using createDataPartition() function from caret. p is the percentage of data that goes to training. 
indxTrain <- createDataPartition(y = data$Kelayakan.Keringanan.UKT,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

#Checking distibution in original data and partitioned data
prop.table(table(training$Kelayakan.Keringanan.UKT)) * 100
prop.table(table(testing$Kelayakan.Keringanan.UKT)) * 100

trainX <- training[,names(training) != "Kelayakan.Keringanan.UKT"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)

# Train kNN Model
knnFit <- train(Kelayakan.Keringanan.UKT ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit

#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit)

knnPredict <- predict(knnFit,newdata = testing )

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$Kelayakan.Keringanan.UKT)
mean(knnPredict == testing$Kelayakan.Keringanan.UKT)

# Train Naive Bayes Model
NBFit <- train(Kelayakan.Keringanan.UKT ~ ., data = training, method = "naive_bayes", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of Naive Bayes fit
NBFit

#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(NBFit)

NBPredict <- predict(NBFit,newdata = testing )

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(NBPredict, testing$Kelayakan.Keringanan.UKT)
mean(NBPredict == testing$Kelayakan.Keringanan.UKT)
