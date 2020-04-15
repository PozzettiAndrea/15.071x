#1.1
census = read.csv("census.csv")
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
censusglm = glm( over50k ~ . , family="binomial", data=train)
summary(censusglm)
#1.2
predictTest = predict(censusglm, newdata=test, type = "response")
table(test$over50k, predictTest >= 0.5)
#1.3
table(train$over50k)
table(test$over50k)
#1.4
library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

#2.1, 2.2, 2.3
prp(censustree)
predictTest = predict(censustree, newdata=test, type = "class")
table(test$over50k, predictTest)
#2.6
library(ROCR)
predictTest = predict(censustree, newdata=test)
predictTest = predictTest[,2]
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

#3.1
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
censusrf = randomForest(over50k ~ . , data=trainSmall)
predictTest = predict(censusrf, newdata=test)
table(test$over50k, predictTest)
(9614+1050)/nrow(test)
#3.2
vu = varUsed(censusrf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))
#3.3
varImpPlot(censusrf)

#4.1
library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train( over50k ~ . , data=train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )
#4.2
model = rpart(over50k~., data=train, method="class", cp=0.002)
predictTest = predict(model, newdata=test, type="class")
table(test$over50k, predictTest)
#4.3
prp(model)

