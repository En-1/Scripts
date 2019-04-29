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

##############################################################################
library(dslabs); library(tidyverse); library(reshape2)
path<-system.file("extdata",package = "dslabs"); setwd(path)
(read_csv("fertility-two-countries-example.csv")->Ex)

#Gather or melt data
(gather(Ex, year, fertylity,-country, convert = TRUE)->Ex2)
Ex2 %>% ggplot(aes(year,fertylity, color=country))+geom_line()
(head(melt(Ex)))
#Spread or dcast data 
spread(Ex2,year,fertylity)
as.tibble(dcast(Ex2, country~year,sum))
#separate string
(read_csv("life-expectancy-and-fertility-two-countries-example.csv")->Ex3)
(gather(Ex3, key, fertylity,-country)->Ex4)
Ex4 %>% separate(key,c("year","var1"),sep = "_",extra = "merge") %>% spread(var1,fertylity)
Ex4 %>% separate(key,c("year","var1","var2"),fill = "right") %>% unite("var",var1,var2,sep = "_") %>% 
    spread(var,fertylity)
melt(Ex3) %>% separate(variable,c("year","var1"),sep = "_",extra = "merge") %>% 
    dcast(country+year~var1, sum) %>% head()
#Join
library(ggrepel); left_join(murders,results_us_election_2016, by = "state") %>% 
    ggplot(aes(population/10^6,electoral_votes, label=abb))+geom_point()+
    geom_smooth(method = "lm")+geom_text_repel()
left_join(); right_join(); inner_join(); full_join(); semi_join(); anti_join()
#Bind = [cr]bind
bind_cols(); bind_rows()
#intersect / union / setdiff
intersect(murders[1:5,],murders[3:7,]); union(murders[1:5,],murders[3:7,]); setdiff(murders[1:5,],murders[3:7,])
identical(union_all(murders[1:5,],murders[3:7,]),bind_rows(murders[1:5,],murders[3:7,]))

################################################################################
#Web mining
url<-"https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state"
library(rvest); xml2::read_html(url)->h
tab<-h %>% html_nodes("table")
mur<-tab[[2]] %>% html_table()
tab3<-tab[[3]] %>% html_table()

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
ingredients2 <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

################################################################################
#RegExp

library(dslabs)
reported_heights %>% mutate(NewH=as.numeric(height)) %>% filter(is.na(NewH)) %>% head()
str_subset(reported_heights$height,"cm|inches")       # \\s = space; \\d = [0-9]; [a-zA-Z] = all letters
str_detect(reported_heights$height,"\\d") %>% head()  # ^=bigin; $=end; * any N; ? = 0-1 N; '+' = 1 or more N
str_view_all(head(reported_heights$height,10),"\\d")  # {1,2} = N of rep; "^\\d{1,2}$" = 1 or 2 digits number

pattern<-"^[4-7]\\s*'\\s*\\d+\\.?\\d*$"#Start+4-7 +spaces+'+spaces+1 or more dig + .(0 or 1)+digits + end
pat_gr<-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"#Start+4-7 +spaces+[, or . or spase]+spaces+digits numbers +End

not_inch<-function(x, sm=50, tal=84){
    inch<-suppressWarnings(as.numeric(x))
    ind<- !is.na(inch)&( (inch>=sm & inch<=tal) | (inch/2.54>=sm & inch/2.54<=tal))
    !ind }

prob<-reported_heights %>% filter(not_inch(height)) %>% .$height
convert<-prob %>% str_replace("feet|ft|foot","'") %>% str_replace("inches|in|''|\"","") %>% 
    str_replace(pat_gr,"\\1'\\2") %>% str_replace("^([56])'?$", "\\1'0")  
mean(str_detect(convert, pattern))
convert[!str_detect(convert, pattern)]

reported_heights$height %>% str_split("'")->a
