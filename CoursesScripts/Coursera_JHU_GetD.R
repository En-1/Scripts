F<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
F2<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
Download_CSV<-function(file, dest="temp"){
    download.file(file,destfile = paste("./data/",dest,".csv",sep = ""))
    read.csv(paste("./data/",dest,".csv",sep = ""))
}
B<-Download_CSV(F2)

library(XML)
library(RCurl)
F<-"https://www.w3schools.com/XML/simple.xml"
xData <- getURL(F)
doc<-xmlTreeParse(xData,useInternalNodes = TRUE) #htmlTreeParse()
R<-xmlRoot(doc)
xmlName(R)
xmlSApply(R,xmlValue)
xpathSApply(R,"//name",xmlValue)
xpathSApply(R,"//price",xmlValue)
xpathSApply(doc2,"//li[@class='name']",xmlValue)
xpathSApply(doc2,"//li[@class='stat']",xmlValue)

library(jsonlite)
JD<-fromJSON("https://api.github.com/users/jtleek/repos")
names(JD)
O<-JD$owner
MyJ<-toJSON(iris,pretty = TRUE)

library(RMySQL)
ucscdb<-dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
result<-dbGetQuery(ucscdb,"show databases;"); dbDisconnect(ucscdb);
hg19<-dbConnect(MySQL(),user="genome", db="hg19", host="genome-mysql.cse.ucsc.edu")
allTables<-dbListTables(hg19)
dbListFields(hg19,"affyU133Plus2")
dbGetQuery(hg19,"select count(*) from affyU133Plus2")
affyData<-dbReadTable(hg19,"affyU133Plus2")
q<-dbSendQuery(hg19,"select * from affyU133Plus2 where misMatches between 1 and 3")
affySub<-fetch(q,n=10); dbClearResult(q); dbDisconnect(hg19);

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)

library(httr)
myapp <- oauth_app("github", key = "7dbece2ddaf6965cef5d",  secret = "40b4b8e705d82630fcc7557ccddab85328ae0100")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
repo_list<-content(req)
