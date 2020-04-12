#1.1
LOANZ=read.csv("LOANZ.csv")
str(LOANZ)
table(LOANZ$not.fully.paid)
1533/(1533+8045)
#1.2
summary(LOANZ)
#1.3
missing = subset(LOANZ, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
table(missing$not.fully.paid)
#1.4
LOANZimputed=read.csv("LOANZ_imputed.csv")
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(LOANZ), "not.fully.paid")
imputed = complete(mice(LOANZ[vars.for.imputation]))
LOANZ[vars.for.imputation] = imputed

#2.1, 2.2
library(caTools)
set.seed(144)
spl = sample.split(LOANZ$not.fully.paid, 0.7)
train = subset(LOANZ, spl == TRUE)
test = subset(LOANZ, spl == FALSE)
mod = glm(not.fully.paid~., data=train, family="binomial")
summary(mod)
#2.3
test$predicted.risk = predict(mod, newdata=test, type="response")
table(test$not.fully.paid, test$predicted.risk > 0.5)
library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

#3.1
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
#3.2
pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate)
#3.3
prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)

#5.1
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)

#6.1
highInterest = subset(test, int.rate >= 0.15)
summary(highInterest$profit)
table(highInterest$not.fully.paid)
#6.2
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLOANZ = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLOANZ$profit)
table(selectedLOANZ$not.fully.paid)