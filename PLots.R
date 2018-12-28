par(mfrow = c(2,1))

library(tidyverse)
library(dslabs)

r=sum(murders$total)/sum(murders$population)*10^6;library(ggrepel); library(ggthemes);library(gridExtra)
murders%>%ggplot(aes(population/10^6,total,label = abb))+geom_text_repel(nudge_x = 0.05)+
  geom_abline(intercept = log10(r),lty=2,col="green")+geom_point(aes(col=region), size = 3)+
  scale_x_log10()+scale_y_log10()+xlab("Populationin, mln")+ylab("N of murders")+ggtitle("Murders")+theme_economist()

heights%>%filter(sex=="Male")%>% ggplot(aes(x=height))+geom_histogram(binwidth = 2, fill="blue", col="black")->p1
heights%>%filter(sex=="Male")%>% ggplot(aes(x=height))+geom_density(fill="blue")->p2
heights%>%filter(sex=="Male") %>% ggplot(aes(sample=scale(height)))+geom_qq()+geom_abline()->p3
grid.arrange(p1,p2,p3, ncol=3)

#facets
gapminder%>%filter(year %in% c (1962,2012, 1980,1990,2000) & continent %in% c("Europe", "Asia"))%>%
  ggplot(aes(life_expectancy,fertility,col=continent))+ geom_point()+facet_wrap(.~year)#facet_grid(continent~year)
#ts
gapminder%>%filter(country %in% c("South Korea","Germany"))%>% ggplot(aes(year,life_expectancy, col= country))+geom_line()
#transform
west<-c("Australia and New Zealand","Western Europe","Northern America", "Northern Europe","Southern Europe")
c1<-gapminder%>%mutate(DPD=gdp/population/365) %>%filter(year==1970&!is.na(DPD))%>%.$country
c2<-gapminder%>%mutate(DPD=gdp/population/365) %>%filter(year==2010&!is.na(DPD))%>%.$country
c<-intersect(c1,c2)
gapminder%>%mutate(DPD=gdp/population/365)%>%filter(year%in%c(1970,2010)&!is.na(DPD)&country%in%c)%>%
    mutate(group=ifelse(region %in% west, "West","East"))%>% ggplot(aes(DPD))+geom_histogram(col="black",binwidth = 1)+
    scale_x_continuous(trans = "log2")+facet_grid(year~group)

gapminder%>%mutate(DPD=gdp/population/365) %>%filter(year%in%c(1970,2010)&!is.na(DPD))%>%
    mutate(region=reorder(region, DPD, FUN = median))%>%ggplot(aes(region,DPD, fill=continent))+
    geom_boxplot(aes(fill=factor(year)))+ theme(axis.text.x = element_text(angle = 90,hjust = 1))+
    scale_y_continuous(trans = "log2")

gapminder%>%mutate(DPD=gdp/population/365)%>%filter(year%in%c(1970,2010)&!is.na(DPD))%>%
  mutate(group=ifelse(region %in% west, "West","East"))%>%ggplot(aes(DPD, y = ..count..,fill=group))+
  geom_density(alpha=0.7, bw=0.75, position = "stack")+scale_x_continuous(trans = "log2")+facet_grid(.~year)
# y = ..count.. for each plot proportional N of counties; group_by(year)%>%mutate(weight=population/sum(population))%>%ungroup()...aes(weight=weight)

gapminder%>%mutate(group=ifelse(region %in% west, "West", as.character(continent) ))%>% 
    filter(year%in%2010&!is.na(gdp)&!is.na(group)&!is.na(infant_mortality))%>%  group_by(group,country)%>% 
    summarise(DPD=sum(gdp)/sum(population)/365,ISR=1-sum(infant_mortality/1000*population)/sum(population))%>%
    ggplot(aes(DPD,ISR,col=group,label=country))+geom_point(size=2)+geom_text_repel(nudge_x = 0.05)+
    scale_x_continuous(trans = "log2")+scale_y_continuous(trans = "logit",breaks = c(0.95,0.99,0.995,0.97,0.98))

#basic ps
par(mfrow=c(1,3))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))#lty ws pch for lines

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

with(airquality, plot(Temp, Ozone))
with(airquality, lines(loess.smooth(Temp, Ozone)))

#lattice
library(lattice)
xyplot(Ozone~Wind | Month ,data = airquality, layout = c(5,1))
xyplot(Ozone~Wind | Month ,data = airquality, layout = c(5,1),
       panel = function(x, y) {
           panel.xyplot(x, y)
           panel.loess(x, y, col = "black")
           panel.lmline(x, y, col = "red")
       })

xyplot(price~carat | color*cut, data = diamonds, strip = FALSE, pch=20, xlab = myxlab, ylab = myylab, main = mymain)

