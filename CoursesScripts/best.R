best <- function(state, outcome) {
    M <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (!state%in%unique(M$State)) 
        stop("invalid state")
    if (!outcome%in%c("heart attack","heart failure","pneumonia"))  
        stop("invalid outcome") 
    if (outcome=="heart attack")
        data<-subset(M[,c(2,7,11)],State==state)
    if (outcome=="heart failure")
        data<-subset(M[,c(2,7,17)],State==state)
    if (outcome=="pneumonia")
        data<-subset(M[,c(2,7,23)],State==state)
    data[,3]<-as.numeric(data[,3])
    data<-data[!is.na(data[,3]),] 
    data<-data[order(data[,3],data[,1]),]
    data[1,1]
}

rankhospital <- function(state, outcome, num = "best") {
    M <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (!state%in%unique(M$State)) 
        stop("invalid state")
    if (!outcome%in%c("heart attack","heart failure","pneumonia"))  
        stop("invalid outcome") 
    if (outcome=="heart attack")
        data<-subset(M[,c(2,7,11)],State==state)
    if (outcome=="heart failure")
        data<-subset(M[,c(2,7,17)],State==state)
    if (outcome=="pneumonia")
        data<-subset(M[,c(2,7,23)],State==state)
    data[,3]<-as.numeric(data[,3])
    data<-data[!is.na(data[,3]),] 
    data<-data[order(data[,3],data[,1]),]
    if (num == "best") x=data[1,1]
    else if (num == "worst") x=tail(data,1)[1,1]
    else x=data[num,1]
    x
}

rankall <- function(outcome, num = "best") {
    M <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (!outcome%in%c("heart attack","heart failure","pneumonia"))  
        stop("invalid outcome") 
    if (outcome=="heart attack") data<-M[,c(2,7,11)]
        
    if (outcome=="heart failure") data<-M[,c(2,7,17)]
        
    if (outcome=="pneumonia") data<-M[,c(2,7,23)]
        
    data[,3]<-as.numeric(data[,3])
    data<-data[!is.na(data[,3]),] 
    data<-data[order(data[,2],data[,3],data[,1]),]
    A<-split(data,data$State)
    if (num == "best") x=cbind(sapply(A, function(x){x[1,1]}),sapply(A, function(x){x[1,2]}))
    else if (num == "worst") x=cbind(sapply(A, function(x){tail(x,1)[1,1]}),sapply(A, function(x){tail(x,1)[1,2]}))
    else x=cbind(sapply(A, function(x){x[num,1]}),sapply(A, function(x){x[1,2]}))
    x
}


