
library(dplyr)
library(maptools)
library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(cowplot)
library(readr)
library(biscale)
library(sp)

setwd("/home/utilizador/Documents/Mestrado/VD/proj")
data1<-read_csv("parte2/sample_full.csv")
colnames(data1)<-make.names(colnames(data1))
data2 <- read_csv("Sample_CORRECTED.csv")

na1<- data.frame(year=as.numeric(),count=as.numeric(), count_na=as.numeric())
x1<-data1[is.na(data1$Community.Area),]
for (i in c(2001:2016)){
  y=i
  c<- nrow(data1[data1$Year==i,])
  cna<- nrow(x1[x1$Year==i,])
  v<-data.frame(year=y,count=c,count_na=cna)
  na1<-rbind(na1,v)
}

na2<- data.frame(year=as.numeric(),count=as.numeric(), count_na=as.numeric())
x2<-data2[is.na(data2$Community.Area),]
for (i in c(2001:2016)){
  y=i
  c<- nrow(data2[data2$Year==i,])
  cna<- nrow(x2[x2$Year==i,])
  v<-data.frame(year=y,count=c,count_na=cna)
  na2<-rbind(na2,v)
}

write_csv(na1,"NA_BeforeCorrection.csv")
write_csv(na2,"NA_AfterCorrection.csv")

