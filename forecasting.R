library(fpp2)
library(hts)

#Chapter2

#seasonal plots
ggseasonplot(a10) #seasonplot(a10, year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)
ggmonthplot(a10) #monthplot(a10)
autoplot(elecdemand[,c(1,3)],facets=T) #time plots in dif wndws
qplot(Temperature, Demand, data=as.data.frame(elecdemand)) #scatterplot
ggAcf(window(elec,start=1980))
library(GGally); ggpairs(as.data.frame(visnights[,1:5]))

#simple methods
meanf(a10,3) #mean
snaive(a10,3) #last meaning with seasonal
rwf(a10,3,drift = TRUE) # last neaninf with drift

#transformation of DS
#1. Box-Cox transformations: w=log(y) if L=0, otherwise w=(y^L-1)/L
plot(BoxCox(elec,BoxCox.lambda(elec))) # size of the seasonal variation about the same across the whole series
#2. Calender adjustment
monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),14)
monthdays[26 + (4*12)*(0:2)] <- 29
plot(milk)
plot(milk/monthdays)

#Forecast accuracy
accuracy(meanf(a10,10))
Test<-window(x = hsales,start = c(1994,1)) 
Train<-window(x = hsales,end = c(1993,12))
(sum((naive(Train,23)$mean-Test)^2))^0.5

#residuals properties
res <- residuals(naive(dj)); plot(res);  Acf(res); hist(res)
Box.test(res,log(length(res)), fitdf=0, type="Lj") #ljung box test

#Time series cross-validation

e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))

e <- tsCV(goog200, forecastfunction=naive, h=8)
mse <- colMeans(e^2, na.rm = T)
data.frame(h = 1:8, MSE = mse) %>% ggplot(aes(x = h, y = MSE)) + geom_point()

#Chapter 5

uschange %>%as.data.frame() %>%  ggplot(aes(x=Income, y=Consumption)) +  geom_point() + geom_smooth(method="lm", se=FALSE)
M1<-tslm(Consumption~Income +Production +Savings+Unemployment,data = uschange)
autoplot(uschange[,'Consumption'], series="Data") +autolayer(fitted(M1))
checkresiduals(M1)

marathon %>%  splinef(lambda=0) %>% autoplot() #checkresiduals()
#bizdays() - N of workdays; 
#tslm(beer2 ~ trend + fourier(beer2, K=2)) - fourier insted of season

#Chapter 6

fit <- stl(elecequip, s.window=5) #ts decomposure @mstl@ @stlf@
plot(elecequip); lines(fit$time.series[,2],col="red",ylab="Trend") # trend, smoothing reg with s.window, "per" for default
lines(seasadj(fit),col="blue") #ts without seasonal component (trend + variation)

autoplot(stlf(fancy,method="rwdrift",lambda = "auto"),PI = F) #Forecast with stl
M1<-stl(fancy,s.window = 3); autoplot(fancy)+autolayer(M1$time.series[,2])+autolayer(seasadj(M1))#plot TS + trend + seasadj

autoplot(stlf(seasadj(mstl(ukcars)),etsmodel="AAN", damped=TRUE))

#MA
plot(elecsales, main="Residential electricity sales",ylab="GWh", xlab="Year")
lines(ma(elecsales,5),col="red") #movin average
library(locfit) ; lines(ts(spence.15(elecsales),start = 1989),col="blue") #weighted MA
#classical decomposure
fit <- decompose(elecequip, type="multiplicative")
#X-13 Arima decomposure
library(seasonal)
M1<-seas(a10,x11="")
series(M1, "forecast.forecasts")
autoplot(a10)+ autolayer(trendcycle(M1))+autolayer(seasadj(M1))+autolayer(seasonal(M1))

#X-13 for list of TS
dta <- list(fdeaths = fdeaths, mdeaths = mdeaths)
l1 <- lapply(dta, function(e) try(seas(e)))
is.err <- sapply(l1, class) == "try-error"
do.call(cbind, lapply(l1[!is.err], final))

#Chapter 8
a10 %>% log() %>% nsdiffs() # find N of seas dif (if enable). First plot, to sho if need log
a10 %>% log() %>% diff(lag=12) %>% ndiffs() #forecast
a10 %>% ggtsdisplay()
fit <- auto.arima(uschange[,1], xreg=uschange[,2])
forecast(fit, xreg=forecast(uschange[,2])$mean) %>% autoplot(xlim=c(2000,2018), PI = FALSE)+autolayer(forecast(auto.arima(uschange[,1])),PI=FALSE,col="red")

#Chapter 9

#Arima + regression
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"], MaxTempSq = elecdaily[, "Temperature"]^2, Workday = elecdaily[, "WorkDay"])
fit2 <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
forecast(fit2, xreg = cbind(rep(26,14), rep(26^2,14), c(0,1,0,0,1,1,1,1,1,0,0,1,1,1))) %>% autoplot(xlim=c(30,55),PI=FALSE)+autolayer(forecast(auto.arima(elecdaily[, "Demand"])),PI = FALSE,col="red")

#Arima+trend with 1 knot
x1 <- seq(length(huron)) # trend 1
x2 <- pmax(0, x1-45)  #knot, trend 2 
t <- seq(length(huron)+20) #H<-ts(huron,frequency = 5)
(fit <- auto.arima(huron, xreg=cbind(x1,x2)))#,fourier(H, K = 1)
fc <- forecast(fit, xreg=cbind(max(x1)+seq(20), max(x2)+seq(20)),h=20)#, fourier(H, K=1, h=20)
trend <- ts(coef(fit)["intercept"]+ coef(fit)["x1"]*t + coef(fit)["x2"]*pmax(0,t-45) , start=start(huron))
fc%>%autoplot()+autolayer(trend)

#Arima + fourier
cafe04 <- window(auscafe, start=2004)
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(cafe04, xreg = fourier(cafe04, K = i), seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit, xreg=fourier(cafe04, K=i, h=24))) +xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +ylab("") + ylim(1.5,4.7)
}
gridExtra::grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]], nrow=3)

#Arima + lagged predictors
Advert <- cbind(  AdLag0 = insurance[,2],AdLag1 = stats::lag(insurance[,2],-1),AdLag2 = stats::lag(insurance[,2],-2),AdLag3 = stats::lag(insurance[,2],-3)) %>%head(NROW(insurance))
a=NULL
for (i in 1:4) {
  fit <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:i], stationary=TRUE)
  a[i]=fit$aicc
}
(fit <- auto.arima(insurance[,1], xreg=Advert[,1:which.min(a)],stationary=TRUE))
insurance[,2]%>%stlf()%>%forecast(h=20)->F
forecast(fit, h=20, xreg=cbind(AdLag0 = F$mean,AdLag1 = c(Advert[40,1],F$mean[1:19])))%>%autoplot()

#Chapter 10

tourism.hts <- hts(visnights, characters = c(3, 5))
M1<-forecast(tourism.hts, method="bu", fmethod="ets")
M2<-forecast(tourism.hts, method="tdfp", fmethod="ets")

#Chapter 11

sunspotarea%>%nnetar(lambda = 0)%>%forecast(h=50)%>%autoplot() #nn for ds with differnt seasonality
#bootsrapping (bagging)
debitcards %>% baggedETS() %>% forecast(h=36)->Fc
debitcards %>% ets() %>% forecast(h=36)%>%autoplot(PI=FALSE, xlim= c(2005,2017))+autolayer(Fc,PI=FALSE,col="red")
#arima with multiple seasonality
y <- msts(x, seasonal.periods=c(7,365.25))
z <- fourier(y, K=c(2,5))
zf <- fourierf(y, K=c(2,5), h=100)
fit <- auto.arima(y, xreg=cbind(z,holiday), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf,holidayf), h=100)
#tbats
a10%>%auto.arima()%>%forecast()%>% autoplot(PI=FALSE, xlim=c(2005,2011))+autolayer(forecast(a10),PI=FALSE,col="red")+
  autolayer(forecast(tbats(a10)),PI=FALSE,col="green")+autolayer(forecast(nnetar(a10)),PI=FALSE,col="brown")

#Chapter 12
library(tsintermittent)

