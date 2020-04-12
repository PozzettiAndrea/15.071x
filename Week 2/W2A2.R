elantra = read.csv("elantra.csv")
ElantraTrain = subset(elantra,elantra$Year < 2013)
str(ElantraTrain)
ElantraTest = subset(elantra,elantra$Year > 2012)
str(ElantraTest)

#1.1
str(ElantraTrain)


#2.1
#What is the model R-squared? Note: In this problem, we will always be asking for the "Multiple R-Squared" of the model.
model1 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=ElantraTrain)
summary(model1)


#2.2
summary(model1)


#2.3
summary(model1)


#3.1
model2 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=ElantraTrain)
summary(model2)


#3.3
summary(model2)



#4.1

ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
model3 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)
summary(model3)


#5.1

cor(ElantraTrain$CPI_energy,ElantraTrain$Month)
cor(ElantraTrain$CPI_energy,ElantraTrain$Unemployment)
cor(ElantraTrain$CPI_energy,ElantraTrain$Queries)
cor(ElantraTrain$CPI_energy,ElantraTrain$CPI_all)


#5.2

cor(ElantraTrain$Queries,ElantraTrain$CPI_all)  #0.7536732
cor(ElantraTrain$Queries,ElantraTrain$Month)    #0.0158443
cor(ElantraTrain$Queries,ElantraTrain$Unemployment)  #-0.6411093
cor(ElantraTrain$Queries,ElantraTrain$CPI_energy)    #0.8328381


#6.1


model4 =  lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)


#6.2
PredictTest = predict(model4, newdata = ElantraTest)
SSE = sum((PredictTest - ElantraTest$ElantraSales) ^ 2)


#6.3

#The baseline is the mean of ElantraSales in the training set for every observation
mean(ElantraTrain$ElantraSales)


#6.4

SST = sum((mean(ElantraTrain$ElantraSales) - ElantraTest$ElantraSales)^2)


#6.5
max(abs(PredictTest - ElantraTest$ElantraSales))


#6.6
which.max(abs(PredictTest - ElantraTest$ElantraSales))


