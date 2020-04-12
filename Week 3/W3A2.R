#1.1
parole = read.csv("parole.csv")
nrow(parole)
#1.2, 2.1, 2.2
table(parole$violator)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole$state)
summary(parole$crime)
table(parole$state)
table(parole$crime)

#3.1, 3.2
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split==TRUE)
test = subset(parole, split==FALSE)
str(train)

#4.1, 4.2, 4.3
mod = glm(violator~., data=train, family="binomial")
summary(mod)

#5.1
predictions = predict(mod, newdata=test, type="response")
summary(predictions)
#5.2
table(test$violator, as.numeric(predictions >= 0.5))
#5.3, 5.4, 5.5
table(test$violator)
#5.6
library(ROCR)
pred = prediction(predictions, test$violator)
as.numeric(performance(pred, "auc")@y.values)