pollutantmean<-function(dir,pol,id=1:332) {
    
    files_full <- list.files(dir, full.names=TRUE)
    dat <- data.frame()
    for (i in id) {
        dat <- rbind(dat, read.csv(files_full[i]))
    }
    mean(dat[,pol],na.rm = TRUE)
}

complete<-function(dir,id=1:332) {
    files_full <- list.files(dir, full.names=TRUE)
    nobs=NULL
    j=1
    for (i in id) {
        A=read.csv(files_full[i])
        nobs[j]=sum(!is.na(A[,2]))
        j=j+1
    }
    data.frame(id,nobs)
}


corr<-function(dir,thrh=0) {
    files_full <- list.files(dir, full.names=TRUE)
    dat <- data.frame()
    for (i in seq_along(files_full)) {
        dat <- rbind(dat, read.csv(files_full[i]))
    }
    id<-subset(dat, nobs>thrh)$id
    files_full <- list.files(dir, full.names=TRUE)
    cr=0
    j=1
    for (i in id) {
        A=read.csv(files_full[i])
        sulfate<-!is.na(A$sulfate)
        nitrate<-!is.na(A$nitrate)
        test<-sulfate+nitrate
        A1<-A[test==2,]
        cr[j]<-cor(A1$nitrate,A1$sulfate)
        j=j+1
    }
    cr
}
    