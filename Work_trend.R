Trend UFO

PrXReg<- function(dat, h=1, start=2014, Tr = FALSE, X=dat) {
    P5=matrix(ncol = ncol(dat),nrow = h)
    #forecast
    for (i in 1:ncol(dat)) {
        a<-ts(dat[,i],start = start, frequency = 12)
        x<-ts(X[1:nrow(dat),i],start = start,frequency = 12)
        b<-try(forecast(auto.arima(a+0.01,xreg = x+0.01),xreg = X[(nrow(X)-h+1):nrow(X),i]+0.01,h=h)$mean, TRUE)#lambda = 0
        c<-forecast(a, h = h)$mean
        P5[,i]<- (if (class(b)=="ts") b
                  else c)
    }
    P5
}

TrendUFO<- function(path = "UFO.xlsx") {
    
    #read UFO
    UFO<-read_excel(path)
    melt(UFO, id = 1)%>%arrange(variable,desc(ОСП))->A
    A[is.na(A)]=0
    
    #count on values
    A[1,4]=1
    for (i in 2:nrow(A)) {
        A[i,4]=ifelse(A[i,3]==A[(i-1),3], A[(i-1),4]+1, 1 )
    }
    
    #grupping
    A[1,5]=1
    for (i in 2:nrow(A)) {
        A[i,5]={
            if (A[i,4] < 5 | i>(nrow(A)-2)) {
                if(A[i,4]==1) A[(i-1),5]+1
                else  A[(i-1),5]
            }
            else {
                if(A[i,4]==1 | (A[(i-1),5] == mean(A[(i-1),5]:A[(i-5),5]) & A[i,3]==A[(i+2),3]) ) A[(i-1),5]+1
                else  A[(i-1),5]      
            }
        }
    }
    
    #Read revenue
    gc<-as.matrix(read_excel(path,col_names = FALSE, n_max = 3,sheet = 2))
    TS<-as.matrix(read_excel(path,col_names = FALSE,skip = 3,sheet = 2))
    
    #prepring stepped trend
    A<-A%>%mutate(V6=rep(nrow(UFO):1,ncol(UFO)-1))
    A%>%group_by(variable,value,V5)%>%summarise(min(V6))->B
    A<-A%>%left_join(B)%>%select(-V6)
    A%>%mutate("2"=variable,variable=as.factor(ОСП))%>%select("2", variable, `min(V6)`)->df
    A%>%mutate("2"=variable,variable=as.factor(ОСП),U=value)%>%select("2", variable,U, V5)->df2
    DATA<-data.frame(t(TS)); names(DATA)<-UFO$ОСП
    DATA<-cbind(t(gc),DATA)
    C<-melt(DATA, id = 1:3)%>%left_join(df)
    reshape::cast(C[,c(1:4,6)],1+2+3~variable,sum)->TS2
    TS2<-t(as.matrix(TS2[,4:ncol(TS2)]))
    
    #stepped trend
    Trend=Trend2=SeasAdj=matrix(ncol = ncol(TS),nrow = nrow(TS))
    for (i in 1:ncol(TS)) {
        a<-ts(TS[,i],start = 2014, frequency = 12)
        Trend2[,i]=Trend[,i]<-mstl(a)[,2]
        SeasAdj[,i]<-seasadj(mstl(a))
        for (j in 1:(nrow(TS)-1)) {
            if (is.na(TS2[j,i]) == FALSE & TS2[j,i]>=24 & TS2[j,i]<TS2[(j+1),i]) {
                b<-ts(a[1:j],start = 2014, frequency = 12)
                Tr<-stlm(b)$stl[,2]
                Trend2[TS2[j,i]:j,i]=Tr[TS2[j,i]:j]
            }
        }
    }
    
    #reshaping
    D0<-data.frame(t(TS)); names(D0)<-UFO$ОСП; D0<-cbind(t(gc),D0)
    D1<-data.frame(t(Trend)); names(D1)<-UFO$ОСП; D1<-cbind(t(gc),D1)
    D2<-data.frame(t(Trend2)); names(D2)<-UFO$ОСП; D2<-cbind(t(gc),D2)
    D3<-data.frame(t(SeasAdj)); names(D3)<-UFO$ОСП; D3<-cbind(t(gc),D3)
    Tab<-cbind(melt(D1, id = 1:3),melt(D2, id = 1:3)[,5],melt(D3, id = 1:3)[,5],melt(D0, id = 1:3)[,5])%>%left_join(df2)
    names(Tab)<-c("Показатель","ОСП","Группа","Дата","Тренд","Тренд2","БезСез","Факт","УФО","Период")
    Tab<-Tab%>%arrange(ОСП,Группа,Показатель,Дата)%>%group_by(ОСП,Группа,Показатель)%>%mutate(МТ=max(Тренд))%>%
        ungroup()%>% group_by(ОСП,Группа,Показатель,Период,УФО)%>%
        mutate(оценка= round((last(Тренд)/mean(МТ)*0.4+last(Тренд2)/mean(МТ)*0.2+mean(БезСез)/mean(МТ)*0.4)*5.49,1))%>%
        mutate(оценка= ifelse(оценка<0,0,ifelse(оценка>5,5,оценка)))
    Tab
}

Tab<-TrendUFO()
Tab2<-Tab%>%summarise(first(Дата),last(Дата),sum(Факт), mean(оценка))#UFO_report

#matrixes for hts

GroupHTS<- function(Tab=Tab, h=3, GTS=TRUE) {
    Tab3<-Tab%>%filter(Показатель=="Осп, гр")
    reshape::cast(Tab3[,c(1:4,8)],Показатель+ОСП+Группа~Дата,sum)->Y1
    gc1<-as.matrix(t(as.matrix(Y1[,3]))); colnames(gc1)<-1:ncol(gc1)
    Y<-as.matrix(t(as.matrix(Y1[,4:ncol(Y1)]))); colnames(Y)<-1:ncol(Y)
    reshape::cast(Tab3[,c(1:4,12)],Показатель+ОСП+Группа~Дата,sum)->X
    X<-t(as.matrix(X[,4:ncol(X)]))
    a<-apply(X,2, function (x) seq(ifelse(tail(x,1)<2,tail(x,1)+0.3,ifelse(tail(x,1)<3.8,tail(x,1)+0.1,tail(x,1)+0)),
                                   ifelse(tail(x,1)<2,tail(x,1)+0.5,ifelse(tail(x,1)<3.8,tail(x,1)+0.2,tail(x,1)+0)), length.out = 3))
    X<-rbind(X, a) ;colnames(X)<-1:ncol(X)
    XS<-as.matrix(rowSums(X));YS<-as.matrix(rowSums(Y))
    if (GTS==TRUE) ProgHts(Y,gc1,h = h)
    else cbind(PrXReg(dat = YS, h = h, X = XS),PrXReg(dat = Y, h = h, X = X))
}

#GTS by group
spl<-split(Tab,as.factor(Tab$Группа)); Fc<-sapply(spl, GroupHTS)
R<-data.frame(rep(NA,85))
for (j in 1:4) {  b=NULL
for (i in seq(length(Fc)/4)) { a<- (if (class(Fc[[(i-1)*4+j]])=="factor") as.character(Fc[[(i-1)*4+j]])
                                    else Fc[[(i-1)*4+j]])
b<-c(b,a)  }
R<-cbind(R,b) };Fc<-R[,2:5]; Fc<-cbind(rep(unique(Tab$ОСП),5),Fc)
#simple GTS
Fc2<-GroupHTS(Tab = Tab); Fc2<-cbind(rep(unique(Tab$ОСП),each = 5),Fc2)

#consolidation
Cons<- function(Tab=Tab,h=3){
    Fc3<-GroupHTS(Tab = Tab,h=h,GTS = FALSE) 
    A<-hts(ts(Fc3[,2:ncol(Fc3)],start = 2014,frequency = 12))
    A1 <- ts(Fc3,start = 2014,frequency = 12)
    combinef(fcasts = A1, get_nodes(A), keep = "bottom")
}
Fc3<-sapply(spl, Cons)
Fc3%>%melt()%>%mutate(Var1=rep(1:3,85),Var3=rep(rep(unique(Tab$ОСП),each = 3),5))%>%dcast(Var3+Var2~Var1) ->Fc3
names(Fc)<-names(Fc3);names(Fc2)<-names(Fc3)
Fc_cons<-cbind(rbind(Fc,Fc2,Fc3),rep(1:3, each = nrow(Fc)))
write.xlsx(Fc_cons, "Test.xlsx")
