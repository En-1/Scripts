HTS

gc<-as.matrix(read_excel("OSP2.xlsx",col_names = FALSE, n_max = 1))
TS<-as.matrix(read_excel("OSP2.xlsx",col_names = FALSE,skip = 1))

rm(list=ls()[grep("Fc+[1-9]",ls())])

ProgHts<- function(dat, gc1=matrix(rep("A",ncol(dat)),nrow=1), h=1, start=2014) {
    TS<-ts(dat,start = start,frequency = 12)
    #gts forecast
    A<-gts(TS,gc1);A_l<-gts(TS+0.01,gc1) 
    F1<-try(forecast(A,method = "comb", weights = "wls",FUN = function(x) stlf(x),h=h)$bts,silent = TRUE)
    #F2<-try(forecast(A,method = "comb", weights = "wls",FUN = function(x) tbats(x),h=h)$bts,silent = TRUE)
    F3<-try(forecast(A_l,method = "comb", weights = "wls",h=h, lambda = 0)$bts,silent = TRUE)
    F4<-try(forecast(A_l,fmethod = "arima", method = "comb", weights = "wls",h=h, lambda = 0)$bts,silent = TRUE)
    Fc<-cbind(rep(1:h,3),as.data.frame(rbind(F1,F3,F4)))
    #mean positive
    for(i in 1:nrow(Fc)){
        for (j in 1:ncol(Fc)) {
            if (Fc[i,j]<0) {Fc[i,j]=0}
        }
    }
    spl<-split(Fc,as.factor(Fc[,1]))
    FcM<-sapply(spl, function(x) colMeans(x[2:ncol(x)]))
    #account for 30% of act sales for 8m 
    D<-colSums(dat[(nrow(dat)-7):nrow(dat),])
    #D2<-colSums(dat[(nrow(dat)-11):nrow(dat),])/4
    FcM<-cbind(gc1[1,],D,as.data.frame(FcM))
    names(FcM)=c("Group","D",paste0(rep("Pr",h),1:h))
    FcM%>% group_by(Group)%>%summarise_all(.funs = sum)%>%right_join(FcM,by = "Group")->FcM
    
    for (i in 1:h) FcM[,ncol(FcM)+1]=FcM[,2+i]/FcM[,2]*FcM[,3+h]
    for (i in 1:h) FcM[,ncol(FcM)+1]=FcM[,3+h+i]*0.7+FcM[,3+h*2+i]*0.3
    FcM[,c(1,(ncol(FcM)-h+1):ncol(FcM))]
}

Fc<-ProgHts(TS,gc,h = 1)
#Fc4<-ProgHts(TS[1:54,],gc)
#FC<-rbind(Fc1,Fc2,Fc3,Fc4,Fc5,Fc6,Fc7,Fc8)
write.xlsx(Fc, "Test.xlsx")

#########################################################

