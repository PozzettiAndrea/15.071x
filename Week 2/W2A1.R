#1.1, 1.2
climate = read.csv("climate_change.csv")
str(climate)
train = subset(climate,Year <=2006)
test = subset(climate,Year > 2006)
climateModel = lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data = train)

#2.2, 2.3
cor(train$N2O,train)
cor(train$CFC.11,train)
simplifiedModel = lm(Temp ~ MEI+TSI+Aerosols+N2O, data=train)
summary(simplifiedModel)
#2.4
finalClimateModel = step(climateModel)
summary(finalClimateModel)
#2.5
predictTest = predict(finalClimateModel, newdata=test)
SSE = sum((test$Temp - predictTest)^2)
SST = sum((test$Temp - mean(train$Temp))^2)
1-SSE/SST