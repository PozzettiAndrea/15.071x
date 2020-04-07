DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Weekday = weekdays(DateConvert)
mvt$Month = months(DateConvert)
mvt$Year = year(DateConvert)
mvt$Date = DateConvert
min(mvt$Month,na.rm ==True)
table(mvt$Month)
table(mvt$Weekday)
str(mvt)
tapply(mvt$Arrest,mvt$Month,sum,na.rm=TRUE)
hist(mvt$Date, breaks=100)
boxplot(mvt$Year~mvt$Arrest)

ArrestYear = tapply(mvt$Arrest,mvt$Year,sum,na.rm=TRUE)
TotalYear = table(mvt$Year)
ArrestYear/TotalYear

sort(table(mvt$LocationDescription))
Top5 = subset(mvt, LocationDescription =="STREET" | LocationDescription =="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription =="GAS STATION" | LocationDescription =="DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription =="GAS STATION" | LocationDescription =="DRIVEWAY - RESIDENTIAL" | LocationDescription =="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription =="STREET" | LocationDescription =="ALLEY")
Top5$LocationDescription = factor(Top5$LocationDescription)

Top5_Arrest = tapply(Top5$Arrest, Top5$LocationDescription,sum)
Top5_All = table(Top5$LocationDescription)
Top5_Arrest/Top5_All

Top5_Gas = subset(Top5, LocationDescription == "GAS STATION")
table(Top5_Gas$Weekday)

Top5_Driveway<-subset(Top5,LocationDescription == "DRIVEWAY - RESIDENTIAL")
table(Top5_Driveway$Weekday)