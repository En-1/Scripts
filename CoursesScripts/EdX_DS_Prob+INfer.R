library(gtools)
deck<-expand.grid(number=1:13,suit=c("D","C","H","S"))
deck<-paste(deck$number,deck$suit)
kings<-paste(13, suit=c("D","C","H","S"))
mean(deck %in% kings)
hand<-permutations(52,2,deck)
card1<-hand[,1];card2<-hand[,2]
sum(card1 %in% kings & card2 %in% kings)/sum(card1 %in% kings)

sim_prob<-function(n){
    res<-replicate(10000,{
        bdays<-sample(1:365,n,replace = T)
        any(duplicated(bdays))
    })
    mean(res)
}
A<-sapply(1:60,sim_prob)

ex_prob<-function(n){
 pr<-seq(365,365-n+1)/365
 1-prod(pr)
}
B<-sapply(1:60,ex_prob)

#prob winning 4g
permutations(2,6,v=0:1,repeats.allowed = TRUE);results <- rowSums(possibilities)>=4; mean(results)

#BigShort   
p<-0.04; B=10000; n=10000; x<-0.05*180000; loss<-(-200000)
prof<-replicate(B,{
    new_p<-p+sample(seq(-0.01,0.01,length=100),1)
    draws<-sample(c(x,loss),n,prob=c(1-new_p,new_p),replace = TRUE)
    sum(draws)
})
mean(prof<0);mean(prof<(-10000000));hist(prof)

#MC panini
S<-replicate(10000,{
a<-sample(677,350,replace = TRUE)
ifelse(is.na(as.numeric(table(table(a))[5])),0,as.numeric(table(table(a))[5]))
})

#MC prob of Z
B=10000; N=1000; p<-0.45
inside<-replicate(B,{
    X<-sample(c(0,1),N,prob=c(1-p,p),replace = TRUE)
    X_hat<-mean(X)
    SE_hat<-sqrt(p*(1-p)/N)
    between(p,X_hat-2*SE_hat,X_hat+2*SE_hat)
})
mean(inside)

#prob of null hipotesys (p=0)
N=100; X_bar = 0.52; z<-sqrt(N/(0.5*0.5))*abs(X_bar-0.5); 1-(pnorm(z)-pnorm(-z))

#polls aggragation
d<-0.04; Nc<-c(1300,550,1350,900,800,250,800,300,1300,1050,2200,500); p<-(d+1)/2
ci<-sapply(Nc,function(N){
    X<-sample(c(0,1),N,prob=c(1-p,p),replace = TRUE)
    X_hat<-mean(X)
    SE_hat<-sqrt(p*(1-p)/N)
    2*c(X_hat,X_hat-2*SE_hat,X_hat+2*SE_hat)-1
}); data.frame(seq_along(Nc),t(ci),Nc)->polls
polls%>%summarise(avg=sum(X1*Nc)/sum(Nc))%>%.$avg->d_hat; p_hat<-(1+d_hat)/2
moe<- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$Nc))

#MC Bayes
p=0.00025; N=100000; outcome<-sample(c("D","H"),N,prob=c(p,1-p),replace = TRUE)
Ac=0.99; test<-vector("character",N); 
test[outcome=="D"]<-sample(c("+","-"),sum(outcome=="D"),prob=c(Ac,1-Ac),replace = TRUE)
test[outcome=="H"]<-sample(c("-","+"),sum(outcome=="H"),prob=c(Ac,1-Ac),replace = TRUE)
table(outcome,test)

library(dplyr) ;library(dslabs)
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- polls_us_election_2016 %>%  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100, X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize),
    lower = spread - qnorm(0.975)*se,  upper = spread + qnorm(0.975)*se) %>%select(state, startdate, enddate, pollster, 
    grade, spread, lower, upper)%>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits<-cis %>%mutate(hit=(actual_spread>lower&actual_spread<upper))%>%group_by(pollster)%>%filter(n()>5)%>%
    summarize(proportion_hits=mean(hit),n=n(),grade = grade[1])%>%arrange(desc(proportion_hits))
errors<-cis%>%mutate(error=spread-actual_spread, hit=((spread>0&actual_spread>0)|(spread<0&actual_spread<0)))
p_hits2<-errors%>%group_by(state)%>%filter(n()>5)%>%summarise(proportion_hits=mean(hit),n=n())
errors%>%filter(grade %in% c("A+", "A", "A-", "B+"))%>%group_by(state)%>%filter(n()>=4)%>% ungroup() %>%
    mutate(state=reorder(state,desc(error)))%>%ggplot(aes(x=state,y=error))+geom_boxplot()+geom_point()


research_funding_rates%>%select(-discipline)%>%summarise_all(.funs = sum)%>%
    summarise(Y_M=awards_men,N_M=applications_men-awards_men,Y_W=awards_women,N_W=applications_women-awards_women)
fisher.test(matrix(c(3,1,1,3),2,2),alternative = "g")

