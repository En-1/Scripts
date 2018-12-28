
library(dplyr)
listD<-list.dirs("./data/UCI HAR Dataset")
listD<-listD[!grepl("Inertial Signals",listD)]
listF<-unlist(sapply(listD, list.files,full.names = TRUE))
listF<-listF[grep("txt$",listF)]
listN<-gsub("\\.(.*)","",gsub("(.*)/","",listF))
for (i in 1:length(listF)) {
    assign(listN[i], try(read.table(listF[i]),silent = TRUE))
}
DSet<-rbind(X_test,X_train)
namesL<-grepl("mean|std",features[,2])&!grepl("meanFreq",features[,2])
n<-seq_along(namesL)[namesL]
DSet<-DSet[,n]
names(DSet)<-features[,2][namesL]
Activity<-rbind(y_test,y_train)%>%left_join(activity_labels)%>%select(Activity=V2)
Subject<-rbind(subject_test,subject_train)
names(Subject)<-"Subject"
DSet<-cbind(Subject,Activity,DSet)
DSet%>%group_by(Subject,Activity)%>%summarise_all(mean)->MeanCleanDSet
