#1.1
elantra = read.csv("elantra.csv")
ElantraTrain = subset(elantra,elantra$Year < 2013)
str(ElantraTrain)
ElantraTest = subset(elantra,elantra$Year > 2012)
str(ElantraTest)
str(ElantraTrain)

#2.1
#What is the model R-squared? Note: In this problem, we will always be asking for the "Multiple R-Squared" of the model.
MODEL1 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=ElantraTrain)
summary(MODEL1)
#2.2
summary(MODEL1)
#2.3
summary(MODEL1)

#3.1
MODEL2 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=ElantraTrain)
summary(MODEL2)
#3.3
summary(MODEL2)

#4.1
ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
MODEL3 =  lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)
summary(MODEL3)

#5.1
cor(ElantraTrain$CPI_energy,ElantraTrain$Month)
cor(ElantraTrain$CPI_energy,ElantraTrain$Unemployment)
cor(ElantraTrain$CPI_energy,ElantraTrain$Queries)
cor(ElantraTrain$CPI_energy,ElantraTrain$CPI_all)
#5.2
cor(ElantraTrain$Queries,ElantraTrain$CPI_all)
cor(ElantraTrain$Queries,ElantraTrain$Month)
cor(ElantraTrain$Queries,ElantraTrain$Unemployment)
cor(ElantraTrain$Queries,ElantraTrain$CPI_energy)

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


