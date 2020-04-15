#1.1
STATES = read.csv("STATESSimple.csv")
RegModel = lm(Life.Exp ~ ., data=STATES)
summary(RegModel)
#1.2
Predictions = predict(RegModel)
sum((STATES$Life.Exp - Predictions)^2)
sum(RegModel$residuals^2)
#1.3
RegModel2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=STATES)
summary(RegModel2)
#1.4
Predictions2 = predict(RegModel2)
sum((STATES$Life.Exp - Predictions2)^2)
SSE = sum(RegModel2$residuals^2)
SSE
#1.5
cor(STATES$Life.Exp, STATES$Income)
cor(STATES$Life.Exp, STATES$Illiteracy)
cor(STATES$Life.Exp, STATES$Area)

#2.1
library(rpart)
library(rpart.plot)
CARTmodel = rpart(Life.Exp ~ ., data=STATES)
prp(CARTmodel)
#2.2
PredictionsCART = predict(CARTmodel)
sum((STATES$Life.Exp - PredictionsCART)^2)
#2.3, 2.4, 2.5
CARTmodel2 = rpart(Life.Exp ~ ., data=STATES, minbucket=5)
prp(CARTmodel2)
PredictionsCART2 = predict(CARTmodel)
sum((STATES$Life.Exp - PredictionsCART2)^2)
#2.6, 2.7
CARTmodel3 = rpart(Life.Exp ~ Area, data=STATES, minbucket=1)
PredictionsCART3 = predict(CARTmodel3)
sum((STATES$Life.Exp - PredictionsCART3)^2)

#3.1
library(caret)
set.seed(111)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01) )
train(Life.Exp ~ ., data=STATES, method="rpart", trControl = fitControl, tuneGrid = cartGrid)
#3.2
CARTmodel4 = rpart(Life.Exp ~ ., data=STATES, cp=0.12)
prp(CARTmodel4)
#3.3, 3.4
PredictionsCART4 = predict(CARTmodel4)
sum((STATES$Life.Exp - PredictionsCART4)^2)
#3.5, 3.6, 3.7
set.seed(111)
train(Life.Exp ~ Area, data=STATES, method="rpart", trControl = fitControl, tuneGrid = cartGrid )
CARTmodel5 = rpart(Life.Exp ~ Area, data=STATES, cp=0.02)
prp(CARTmodel5)




PredictionsCART5 = predict(CARTmodel5)
sum((STATES$Life.Exp - PredictionsCART5)^2)
