#1.1
GE = read.csv("GEStock.csv")
IBM = read.csv("IBMStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
Boeing = read.csv("BoeingStock.csv")


GE$Date = as.Date(GE$Date, "%m/%d/%y")
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

str(IBM)
#1.2
summary(IBM$Date)
#1.3
summary(IBM$Date)
#1.4
summary(IBM$StockPrice)
#1.5
summary(GE$StockPrice)
#1.6, 1.7
summary(CocaCola$StockPrice)
#1.8
summary(Boeing$StockPrice)
#1.9
sd(ProcterGamble$StockPrice)

#2.1
jpeg("img1_2_2.1")
plot(CocaCola$Date, CocaCola$StockPrice,type="l")
dev.off()
#2.2, 2.3, 2.4
jpeg("img1_2_2.2")
plot(CocaCola$Date, CocaCola$StockPrice,type="l",col="red",lty=2)
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)
dev.off()

#3.1, 3.2
jpeg("img1_2_3.1")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col="blue")
lines(GE$Date[301:432],GE$StockPrice[301:432],col="green")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],col="purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col="orange")
#3.3
abline(v=as.Date(c("1997-09-01")), lwd=2)
#3.4
abline(v=as.Date(c("1997-11-01")), lwd=2)
dev.off()

#4.1
summary(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date),mean)
#4.2, 4.3
tapply(GE$StockPrice, months(GE$Date),mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date),mean)