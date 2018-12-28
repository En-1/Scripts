s<-heights%>% filter(sex=="Male") %>% summarise(av=mean(height),sd=sd(height))
heights$height%>%quantile(c(0,0.5,1))

murders %>% mutate(rate=total/population*10^6)%>%summarise(rate= sum(total)/sum(population)*10^6)#%>% .$rate
murders%>% group_by(region)%>%summarise(rate= sum(total)/sum(population)*10^6)
murders%>%arrange(population)%>%head()#murders%>%arrange(desc(rate))%>%head()

CO2%>%select(Plant:conc)%>%filter(conc<200&Treatment=="chilled")%>%arrange(conc,desc(Type))%>%
    rename(Trean=Treatment)%>%mutate(level=factor(1*(conc-mean(conc))>0,labels = c("low","high")))%>%
    group_by(level,Type)%>%summarise(mean(conc))

hflights::hflights%>%group_by(UniqueCarrier)%>%summarise_each(funs(min(.,na.rm = T),max(.,na.rm = T)),matches("Delay"))
hflights::hflights%>%group_by(Month,DayofMonth)%>%tally(sort=T)#summarise(count=n())%>%arrange(desc(count)) / count(Month,DayofMonth)
hflights::hflights%>%group_by(UniqueCarrier)%>%select(Month,DayofMonth,DepDelay)%>%
    top_n(2)%>%arrange(UniqueCarrier,desc(DepDelay))# filter(min_rank(desc(DepDelay))<=2)
hflights::hflights%>%group_by(Month)%>%tally()%>%mutate(lag=n-lag(n))#summarise(count=n())%>%mutate(lag=count-lag(count))

nycflights13::flights%>%select(one_of(c("year","day")))#rename
nycflights13::flights%>%group_by(month,day)%>%top_n(3,dep_delay)#sample_n(3) slice(1:3)
nycflights13::flights%>%count(month,wt = distance)

select(ends_with("2"),starts_with("d"))
qq <- quantile(x, seq(0, 1, 0.2), na.rm = TRUE); mutate(chicago, pm25.quint = cut(pm25, qq)) %>%
    group_by(pm25.quint) %>% summarize(o3 = mean(o3tmean2, na.rm = TRUE),no2 = mean(no2tmean2, na.rm = TRUE))

DF = data.frame(x=1:10, y=11:20, z=rep(1:2, each=5));DF
DF %>% group_by(z) %>% mutate(y = cumsum(y)) ## cum sum
DF %>% group_by(z) %>% mutate_all(funs(sum)) ## mutate all
DF %>% group_by(z) %>% summarise_each(funs(sum, mean,n())) ## double sum
DF %>% group_by(z) %>% summarise(x[1], y[1])
DF %>% group_by(z) %>% do(data.frame(.$x[1:2], .$y[1]))
DF %>% group_by(z) %>% summarise(quantile(x, 0.25))
DF %>% group_by(z) %>% do(data.frame(quantile(.$x, c(0.25, 0.75))))

