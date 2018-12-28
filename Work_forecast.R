Forecast

install.packages(c("tidyverse", "shiny", "rmarkdown", "sparklyr", "ggplot2", "knitr", "tidyr", "readr",
                   "readxl", "lubridate", "stringr", "dplyr", "fpp2"))
library(fpp2)
library(hts)
library(xlsx)
library(readxl)
library(tidyverse)
library(reshape2)
# Прогноз розницы по группам без ГТС


LL<-as.matrix(read_excel("OSP2.xlsx",col_names = FALSE,skip = 1))
M<-LL[1:57,]
N<-LL[58:63,]

Y<-LL[1:45,1:126]
X<-LL[,127:252]

Prognoz<- function(dat, h=1, start=2014, Tr = FALSE, X=dat) {
    Trend=matrix(ncol = ncol(dat),nrow = nrow(dat))
    P1=P2=P3=P4=P5=matrix(ncol = ncol(dat),nrow = h)
    #forecast
    for (i in 1:ncol(dat)) {
        a<-ts(dat[,i],start = start, frequency = 12)
        Trend[,i]<-stlm(a)$stl[,2]
        P1[,i]<-forecast(stlm(a),h = h)$mean
        P2[,i]<-try(forecast(auto.arima(a+0.01,lambda = 0), h = h)$mean, TRUE)
        P3[,i]<-forecast(a, h = h)$mean
        P4[,i]<-forecast(tbats(a), h = h)$mean
        if (dim(X)==dim(dat)+c(h,0)){
            x<-ts(X[1:nrow(dat),i],start = start,frequency = 12)
            P5[,i]<-try(forecast(auto.arima(a+0.01,xreg = x+0.01,lambda = 0),xreg = X[(nrow(X)-h+1):nrow(X),i]+0.01,h=h)$mean, TRUE)
        }
    }
    Prog<-cbind(c(rep("Trend",12),rep("stl",h),rep("arima",h),rep("ets",h),rep("tbats",h),rep("xreg",h)),
                as.data.frame(rbind(tail(Trend,12),P1,P2,P3,P4,P5)))
    names(Prog)<-c("Тип",colnames(dat))
    if (Tr != TRUE) Prog<-tail(Prog,h*5)
    Prog
}
Prog<-Prognoz(dat = LL,start = 2014,h=14,Tr = TRUE)

Prog<-rbind(P1,P2,P3,P4,P5,P6)

############################################

for (i in 1:6) {
    P<-Prognoz(dat = LL[1:39+i,],start = 2015)
    assign(paste0("P",i),P)
}

cbind(rbind(P1[1:4,],P2[1:4,],P3[1:4,],P4[1:4,],P5[1:4,]),rep(1:5,each=4))%>%arrange(Тип)%>%t()->Prog #
colnames(Prog)<-Prog[1,]; Prog<-Prog[2:nrow(Prog),]

write.xlsx(Prog, "Test.xlsx")
