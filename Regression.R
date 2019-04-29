#sample split for glm
library(caTools)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)

#sample split for lm
spl<-sample(1:nrow(A),size = 0.7*nrow(A))
train<-A[spl,]

#Curves
library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
auc = as.numeric(performance(ROCRpred, "auc")@y.values)

#add prediction and residuals in df
df <-  Housing  %>% modelr::add_predictions(a.lm) %>% modelr::add_residuals(a.lm)

#non-linear LM
lm.fit2=lm(medv~lstat+I(lstat^2),data=Boston)
lm.fit5=lm(medv~poly(lstat,5)) #polynomial lm

#using step
step(lm(score ~ log.savings + log.income + log.address + log.employed, data=creditlog))
plot(fitted(fit), creditlog$score)

#tslm
beer2 <- window(ausbeer,start=1992,end=2006-.1)
fit <- tslm(beer2 ~ trend + season)
plot(beer2)
lines(fitted(fit), col=2)
plot(forecast(fit))
CV(fit) #all measures of predictive accuracy

######################################################################

Lahman::Teams %>% filter(yearID%in%1961:2001) %>% mutate(HRG=HR/G,RG=R/G, SBG=SB/G, BBG=BB/G) %>% 
    ggplot(aes(BBG,RG))+geom_point(alpha=0.5)

Lahman::Teams %>% filter(yearID%in%1961:2001) %>% mutate(HRG=round(HR/G,1),BBG=BB/G,RG=R/G) %>% 
    select(HRG,BBG,RG) %>% filter(HRG<=1.2&HRG>=0.4) ->dat 

get_slope<-function(x){fit=lm(RG~BBG, data = x)
data.frame(name= names(fit$coefficients),slope=fit$coefficients, se = summary(fit)$coefficient[,2])}

dat %>% group_by(HRG) %>% do(get_slope(.))

library(broom); library(Lahman)
fit<-lm(RG~BBG, data = dat); tidy(fit); glance(fit)
dat %>% group_by(HRG) %>% do(tidy(lm(RG~BBG, data = .),conf.int = T)) %>% 
    filter(term=="BBG") %>% select(HRG,estimate,conf.low, conf.high) %>% 
    ggplot(aes(HRG,y = estimate, ymin=conf.low, ymax=conf.high))+geom_errorbar()+geom_point()
