library(tm)
library(SnowballC)

cor<-Corpus(VectorSource(T$Tweet))
cor<-tm_map(cor,tolower)
cor<-tm_map(cor,removePunctuation)
cor<-tm_map(cor, removeWords, c("apple",stopwords("english")))
cor<-tm_map(cor,stemDocument)
freq<-DocumentTermMatrix(cor)
findFreqTerms(freq, lowfreq = 20)
sparse<-removeSparseTerms(freq, 0.997) #leave words that appear in 0.997*1181=4 or more twets
TM<-as.data.frame(as.matrix(sparse)) #make matrix for model
colnames(TM)<-make.names(colnames(TM)) #make shure that all names are appropriate
colnames(MA) = paste("A", colnames(MA)) #rename columns (add A in the beginnig)
wikiWords = cbind(wordsAdded, wordsRemoved)#combine 2 matrixes