#1.1
gerber = read.csv("gerber.csv")
table(gerber$voting)
#1.2
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)
#1.3
logMODEL = glm(voting ~ civicduty + neighbors + self+ hawthorne, data=gerber, family="binomial")
summary(logMODEL)
#1.4
predictLog = predict(logMODEL, type="response")
table(gerber$voting, predictLog>0.3)
#1.5
table(gerber$voting, predictLog>0.5)
#1.6
library(ROCR)
ROCRpred = prediction(predictLog, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

#2.1
CARTMODEL = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTMODEL)
#2.2, 2.3
CARTMODEL2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
#2.4
CARTMODEL3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTMODEL3)

#3.1
CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)
CARTsex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTcontrol, digits=6)
#3.2
prp(CARTsex,digits=6)
#3.3
LogMODELSex = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(LogMODELSex)
#3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogMODELSex, newdata=Possibilities, type="response")
#3.5
LogMODEL2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
#3.6, 3.7
predict(LogMODEL2, newdata=Possibilities, type="response")






