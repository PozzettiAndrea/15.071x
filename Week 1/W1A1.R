#1.1, 1.2
mvt = read.csv("mvtWeek1.csv")
#1.3
mvt$ID[which.max(mvt$ID)]
#1.4
mvt$Beat[which.min(mvt$Beat)]
#1.5
summary(mvt$Arrest)
#1.6
nrow(subset(mvt, LocationDescription=="ALLEY"))

#2.1
mvt$Date[1]
#2.2
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
#2.3
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
which.min(table(mvt$Month))
which.max(table(mvt$Weekday))
#2.4
which.max(table(subset(mvt,Arrest == TRUE,Month)))

#3.1
hist(mvt$Date, breaks=100)
#3.2
boxplot(mvt$Date ~ mvt$Arrest)
#3.3
(table(mvt$Year))
table(subset(mvt, Year == 2001)$Arrest)
#3.4
table(subset(mvt, Year == 2007)$Arrest)
#3.5
table(subset(mvt, Year == 2012)$Arrest)

#4.1, 4.2
sort(table(mvt$LocationDescription))
Top5 = subset(mvt,LocationDescription =="STREET"|LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|LocationDescription == "ALLEY"|LocationDescription == "GAS STATION"|LocationDescription == "DRIVEWAY - RESIDENTIAL")
#4.3
Top5$LocationDescription = factor(Top5$LocationDescription)
a = prop.table(table(Top5$LocationDescription, Top5$Arrest),margin=1)   
sort(a[,"TRUE"],decreasing = TRUE)
#4.4
which.max(table(subset(Top5,LocationDescription == "GAS STATION",Weekday)))
#4.5
which.min(table(subset(Top5,LocationDescription == "DRIVEWAY - RESIDENTIAL",Weekday)))
