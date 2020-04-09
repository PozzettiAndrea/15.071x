
#1.1
poll = read.csv("AnonymityPoll.csv")
str(poll)
#1.2
table(poll$Smartphone)
#1.3
table(poll$State, poll$Region)
table(poll$State, poll$Region == "South")

#2.1
table(poll$Internet.Use, poll$Smartphone)
#2.2
summary(poll)
#2.3
limited = subset(poll,poll$Internet.Use == TRUE|poll$Smartphone == TRUE)

#3.1
summary(limited)
#3.2
summary(limited$Info.On.Internet)
#3.3
table(limited$Info.On.Internet)
#3.4
table(limited$Worry.About.Info)
#3.5
table(limited$Anonymity.Possible)
#3.6
table(limited$Tried.Masking.Identity)
#3.7
table(limited$Privacy.Laws.Effective)

#4.1
jpeg("img1_4_4.1")
hist(limited$Age)
dev.off()
#4.2
max(table(limited$Age, limited$Info.On.Internet))
#4.3
jitter(c(1, 2, 3))
jitter(c(1, 2, 3))
#4.4
jpeg("img1_4_4.4")
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
dev.off()
#4.5
tapply(limited$Info.On.Internet,limited$Smartphone,summary)
#4.6
tapply(limited$Tried.Masking.Identity,limited$Smartphone,summary)