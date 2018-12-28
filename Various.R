#Converting
W$DateConvert = as.Date(strptime(W$Date, "%m/%d/%y %H:%M")) #string into date
W$Month<-months(W$DateConvert)
W$WD<-weekdays(W$DateConvert)

I$Date = as.Date(I$Date, "%m/%d/%y")

#making lags
ILILag2 = lag(zoo(F$ILI), -2, na.pad=TRUE)
F$ILILag2 = coredata(ILILag2)

#adding missing values
library(mice)
S<-P[3:6] 
I<-complete(mice(S))
vars = setdiff(names(L), "not.fully.paid") #all without last
I = complete(mice(L[vars]))
L[vars] = I

# del duplicates
M<-unique(M)

