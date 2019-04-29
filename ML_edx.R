library(caret); library(e1071); library(dslabs); library(tidyverse); library(matrixStats) 
library(rpart); library(rpart.plot); library(randomForest); library(gam)

test_index<-createDataPartition(heights$sex,times = 1,p = 0.5,list = F)
test<-heights[test_index,]; train<-heights[-test_index,]
# Simple cut-off
cutoffs<-seq(61,70)
accuracy<-sapply(cutoffs, function(x){
    y_hat<-ifelse(train$height>x,"Male","Female") %>% factor(levels = levels(test$sex))
    mean(train$sex==y_hat)
})
Best_cut<-cutoffs[which.max(accuracy)]
y_hat3<-ifelse(test$height>Best_cut,"Male","Female") %>% factor(levels = levels(test$sex))
mean(test$sex==y_hat3)
table(pred=y_hat3, act=test$sex)

mutate(test,y_hat=y_hat3) %>%  group_by(sex) %>% summarise(acc=mean(y_hat==sex))
confusionMatrix(y_hat3,test$sex); F_meas(y_hat3,test$sex)
#glm
mnist_27$train %>% ggplot(aes(x_1,x_2, col = y))+geom_point()
fit<-glm(y~x_1+x_2,mnist_27$train, family="binomial")
p_hat<-predict(fit,mnist_27$test)
y_hat<-factor(ifelse(p_hat>0.5,7,2))
confusionMatrix(y_hat,mnist_27$test$y)

#smoothing
fit<-ksmooth(polls_2008$day,polls_2008$margin,x.points = polls_2008$day, kernel = "norm",bandwidth = 7)
fit2<-loess(margin~day,degree = 1,span = 21/diff(range(polls_2008$day)),data = polls_2008)
polls_2008 %>% mutate(smooth=fit$y, smooth2=fit2$fitted) %>% ggplot(aes(day,margin))+geom_point(size=2, alpha = 0.5)+
    geom_line(aes(day,smooth),col="red")+geom_line(aes(day,smooth2),col="black", size=1)+geom_smooth(span=0.3)
mnist_27$train %>% mutate(y = ifelse(y=="7", 1, 0)) %>%ggplot(aes(x_2, as.numeric(y))) + geom_smooth(method = "loess")

grid<-expand.grid(span=seq(0.15,0.65,len=10), degree=1)
train_loess<-train(y~., method = "gamLoess", data = mnist_27$train,tuneGrid = grid)
confusionMatrix(predict(train_loess, mnist_27$test), mnist_27$test$y)

#knn
fit2<-knn3(y~x_1+x_2,mnist_27$train)
y_hat2<-predict(fit2,mnist_27$test, type="class")
confusionMatrix(y_hat2,mnist_27$test$y)

train_knn<-train(y~., method = "knn", data = mnist_27$train,tuneGrid = data.frame(k = seq(1,101,2)))
y_hat4<-predict(train_knn,mnist_27$test)
confusionMatrix(y_hat4,mnist_27$test$y)

#qda
train_qda<-train(y~., method = "qda", data = mnist_27$train)
y_hat3<-predict(train_qda,mnist_27$test)
confusionMatrix(y_hat3,mnist_27$test$y)

#trees
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>% mutate(y_hat = predict(pruned_fit)) %>% 
    ggplot() +  geom_point(aes(day, margin)) + geom_step(aes(day, y_hat), col="red")

train_rpart <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train); confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)
ggplot(train_rpart); prp(train_rpart$finalModel)

#random forest
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)

train_rf_2 <- train(y ~ .,method = "Rborist",tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train); confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)

#boosting
train_ada<-train(y~., method = "ada", data = mnist_27$train) #adaboost
confusionMatrix(predict(train_ada,mnist_27$test),mnist_27$test$y)

train_gmb<-train(y~., method = "gmb", data = mnist_27$train) #gmboost - trees
confusionMatrix(predict(train_gmb,mnist_27$test),mnist_27$test$y)

#RegRegression
#1. method="lasso"; 2.method = "ridge", lambda=5; 3. method="foba", lambda=5, k=4; 4.method="relaxo",lambda=5,phi=0.3

#encemble classofication
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam", "rf", "ranger",  "wsrf", "Rborist", "avNNet", "mlp", "monmlp",
            "adaboost", "gbm","svmRadial", "svmRadialCost", "svmRadialSigma")

fits <- lapply(models, function(model){ 
    print(model)
    train(y ~ ., method = model, data = mnist_27$train)
}) 
pred<-sapply(fits, function(x){
    predict(x,mnist_27$test)
})

#encemble regression

library(ISLR)
inBuild <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage,p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]
# train the data using both glm and random forest models
glm.fit <- train(wage ~.,method="glm",data=training)
rf.fit <- train(wage ~.,method="rf",data=training,
                trControl = trainControl(method="cv"),number=3)
# use the models to predict the results on the testing set
glm.pred.test <- predict(glm.fit,testing)
rf.pred.test <- predict(rf.fit,testing)
# combine the prediction results and the true results into new data frame
combinedTestData <- data.frame(glm.pred=glm.pred.test,
                               rf.pred = rf.pred.test,wage=testing$wage)
# run a Generalized Additive Model (gam) model on the combined test data
comb.fit <- train(wage ~.,method="gam",data=combinedTestData)
# use the resultant model to predict on the test set
comb.pred.test <- predict(comb.fit, combinedTestData)
# use the glm and rf models to predict results on the validation data set
glm.pred.val <- predict(glm.fit,validation)
rf.pred.val <- predict(rf.fit,validation)
# combine the results into data frame for the comb.fit
combinedValData <- data.frame(glm.pred=glm.pred.val,rf.pred=glm.pred.val)
# run the comb.fit on the combined validation data
comb.pred.val <- predict(comb.fit,combinedValData)
# tabulate the results - test data set RMSE Errors
rbind(test = c(glm = sqrt(sum((glm.pred.test-testing$wage)^2)),
               rf = sqrt(sum((rf.pred.test-testing$wage)^2)),
               combined = sqrt(sum((comb.pred.test-testing$wage)^2))),
      # validation data set RMSE Errors
      validation = c(sqrt(sum((glm.pred.val-validation$wage)^2)),
                     sqrt(sum((rf.pred.val-validation$wage)^2)),
                     sqrt(sum((comb.pred.val-validation$wage)^2))))