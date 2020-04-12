#1.1
state = read.csv("states.csv")
str(state)
data(state)
states= cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(states)
plot(states$x, states$y)
#1.2
tapply(states$HS.Grad, states$state.region, mean)
#1.3
boxplot(states$Murder ~ states$state.region)
#1.4
area = subset(states, states$state.region == "Northeast")
boxplot(area$Murder ~ area$state.abb)


#2.1, 2.2
model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area , data=state)
summary(model1)
#2.3
plot(states$Income, states$Life.Exp)

#3.1, 3.2
model1 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost , data=state)
#3.3
sort(predict(model1))
states$state.name [1]
states$state.name [which.min(states$Life.Exp)]
#3.4
sort(predict(model1))
states$state.name [47]
states$state.name [which.max(states$Life.Exp)]
#3.5
sort(abs(model1$residuals))
sort(abs(states$Life.Exp - predict(model1)))



