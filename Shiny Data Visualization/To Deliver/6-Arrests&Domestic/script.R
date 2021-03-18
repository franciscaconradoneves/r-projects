library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(shiny)
library(scales)
library(ggiraph)

#ggplot com % arrest em domesticos e posso acrescentar outras barras incluindo a geral
#versao condensada de % arrest geral e % arrest domestic

setwd("~/Documents/Mestrado/VD/proj/")
d2<-read_csv("Sample_CORRECTED.csv")
df<-d2 %>%
  count(Domestic, Arrest) %>%
  mutate(prop = prop.table(n))
df<-df[!is.na(df$Arrest),]

df2<-data.frame(Domestic=character(),Percentage=numeric(),n=numeric())
for (i in c(0,1)){
  c<-i
  t0<-df[df$Domestic==i & df$Arrest==TRUE,"n"]
  f0<-df[df$Domestic==i & df$Arrest==FALSE,"n"]
  t<-ifelse(is.na(as.numeric(t0[1,])),0,as.numeric(t0[1,]))
  f<-ifelse(is.na(as.numeric(f0[1,])),0,as.numeric(f0[1,]))
  p<- t/(t+f)
  n<- t+f
  b<- data.frame(Domestic=c,Percentage=p,n=n)
  df2<-rbind(df2,b)
}

df2$Percentage<-as.numeric(df2$Percentage)
df2$n<-as.numeric(df2$n)
df2$Domestic<-factor(df2$Domestic)
levels(df2$Domestic)<-c(levels(df2$Domestic),"ALL")
b2<-c(Domestic="ALL",Percentage=mean(df2$Percentage),n=sum(df2$n))
df2<-rbind(df2,b2)
df2$Percentage<-as.numeric(df2$Percentage)
df2$n<-as.numeric(df2$n)
df2$Percentage<-round(df2$Percentage*100,1)
levels(df2$Domestic)<-c("NO","YES","ALL")

ggplot(df2,aes(x=factor(Domestic),y=Percentage,fill=factor(Domestic))) + geom_bar(stat="identity",width = 0.7) +
  scale_fill_manual(values=c("red","green","blue"),name="Domestic") +
  xlab("") + ylab("Mean % Arrest") + ggtitle("Domestic Crime and Percentage of Arrest") +
  geom_text(aes(label=paste("n= ",n)), vjust=-0.3)


