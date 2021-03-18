library(readr)
Chicago2001_2017 <- read_csv("/home/utilizador/Documents/Mestrado/VD/proj/Chicago2001_2017.csv")

#Check span of each year
span<-data.frame()
for (i in seq(2001,2017)){
  x1<-data.frame(Date=Chicago2001_2017[Chicago2001_2017$Year==i,"Date"])
  x1$Date<- as.Date(x1$Date,format = "%m/%d/%Y") #p este efeito posso perder horas
  x1<-data.frame(Date=x1[order(x1$Date,na.last = NA),])
  line<-data.frame(start=x1[1,1],end=x1[nrow(x1),1])
  span<-rbind(span,line)
}  
  
  
setwd("/home/utilizador/Documents/Mestrado/VD/proj")
write_csv(span,"span.csv")