#1.1
FluTrain = read.csv("FluTrain.csv")
which.max(FluTrain$ILI)
FluTrain$Week[303]
#1.2
hist(FluTrain$ILI)
#1.3, 2.1
plot(log(FluTrain$ILI), FluTrain$Queries)

#2.2
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)
#2.3
Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
FluTest = read.csv("FluTest.csv")
PredTest1 = predict(FluTrend1, newdata=FluTest)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

#3.1, 3.2
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]
#3.3
SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))

#4.1
install.packages("zoo")
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)
#4.2
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
#4.3, 4.4
FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)

#5.1
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)