#1.1
LETTERS = read.csv("LETTERS_ABPR.csv")
str(LETTERS)
LETTERS$isB = as.factor(LETTERS$letter == "B")
set.seed(1000)
spl = sample.split(LETTERS$isB,SplitRatio=0.5)
train = subset(LETTERS,spl==TRUE)
test = subset(LETTERS,spl==FALSE)
table(test$isB)
#1.2
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predictions = predict(CARTb, newdata=test, type="class")
table(test$isB,predictions)
#1.3
library(randomForest)
set.seed(1000)
RFb = randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data=train)
RFb = randomForest(isB ~ . - letter, data=train)
predictions = predict(RFb, newdata=test)
table(test$isB,predictions)

#2.1
LETTERS$letter = as.factor( LETTERS$letter )
set.seed(2000)
spl = sample.split(LETTERS$letter, SplitRatio = 0.5)
train2 = subset(LETTERS, spl == TRUE)
test2 = subset(LETTERS, spl == FALSE)
table(train2$letter)
table(test2$letter)
#2.2
CARTletter = rpart(letter ~ . - isB, data=train2, method="class")
predictLetter = predict(CARTletter, newdata=test2, type="class")
table(test2$letter, predictLetter)
#2.3
set.seed(1000)
RFletter = randomForest(letter ~ . - isB, data=train2)
predictLetter = predict(RFletter, newdata=test2)
table(test2$letter, predictLetter)
