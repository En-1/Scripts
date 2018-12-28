library(rpart)
library(rpart.plot)
M1<-rpart(Y~X, data = St, method = "class", minbucket = 25)
prp(M1)
M2<-randomForest(Y~X, data = St, nodesize = 25, ntree=200)

#CV
library(caret)
library(e1071)
NF<-trainControl(method = "cv", number = 10) 
cpGrid<-expand.grid(.cp=seq(0.01,0.5,0.01))  #cpGrid<-expand.grid(.cp= (0:10)*0.001)
Mod<-train(Y~X, data = St, method = "rpart", trControl = NF, tuneGrid = cpGrid)
BestTree<-Mod$finalMode #cp = 0.2

#CV
library(boot)
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto) #train poly models drom 1 to 5
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1] # estimate error (cv.glm $delta) using lou
  # cv.error[i]=cv.glm(Auto,glm.fit,K=10)$delta[1] # k-fold cv
}
cv.error

#clastering
dist<-dist(M[2:20], method = "euclidean") #learn distanses
clust<-hclust(dist, method = "ward.D") #make cluster tree
clustGr<-cutree(clust, k=10) #devide set into 10 clusters
tapply(M$Action, clustGr, mean) #avg % actiom films in each cluster

KC<-kmeans(HV,centers = 5, iter.max = 1000)
HClas<-KC$cluster

# KNN
library(class)
standardized.X=scale(Caravan[,-86])#standardize the data before using knn
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
knn.pred=knn(train.X,test.X,train.Y,k=1)
table(knn.pred,test.Y)