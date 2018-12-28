ibrary(fpp2)	1
library(hts)	
library(xlsx)	
library(readxl)	
# Прогноз розницы по группам без ГТС	


LL<-as.matrix(read_excel("OSP2.xlsx",col_names = FALSE))	
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
P1<-Prognoz(dat = Y[1:39,109:126],start = 2015,X = X[1:40,109:126])	
Prog<-rbind(P1,P2,P3,P4,P5,P6)	
Prog<-Prognoz(LL)	


write.xlsx(Prog, "Test.xlsx")	


#stepped trend	
LL<-as.matrix(read_excel("OSP2.xlsx"))	
M<-LL[1:57,]	
N<-LL[58:63,]	
Trend=SeasAdj=matrix(ncol = ncol(M),nrow = nrow(M))	
for (i in 1:ncol(M)) {	
    a<-ts(M[,i],start = 2014, frequency = 12)	
    Trend[,i]<-mstl(a)[,2]	
    SeasAdj[,i]<-seasadj(mstl(a))	
    # Trend[,i]<-stlm(a)$stl[,2]	
    # for (j in 1:(nrow(N)-1)) {	
    # if (N[j+1,i]>0 & N[j,i]>=24) {	
    # b<-ts(a[1:N[j,i]],start = 2014, frequency = 12)	
    # Tr<-stlm(b)$stl[,2]	
    # Trend[(N[j+1,i]+1):N[j,i],i]=Tr[(N[j+1,i]+1):N[j,i]]	
    # }	
    # }	
}	
write.xlsx(SeasAdj, "Test.xlsx")	
