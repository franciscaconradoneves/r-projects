library(readr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
setwd('/home/utilizador')
sample_1 <- read_csv("Documents/Mestrado/VD/proj/sample_1.csv")
sample_2 <- read_csv("Documents/Mestrado/VD/proj/sample_2.csv")
sample_full <- read_csv("Documents/Mestrado/VD/proj/sample_full.csv")

#Remover coluna X1 que n sei pq voltou
sample_1<-sample_1[-1]
sample_2<- sample_2[-1]

#1. Criminalidade ao longo dos anos
crimes_per_year <- data.frame(table(sample_1$Year))
colnames(crimes_per_year)<-c("Year","Count")
ggplot(crimes_per_year,aes(x=Year,y=Count,group=1)) + geom_line() + geom_point() +
  scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#2. Como se repartem os crimes por tipo?
crimes_per_type<-data.frame(table(sample_2$`Primary Type`))
colnames(crimes_per_type)<-c("Crime","Count")

ggplot(crimes_per_type,aes(x=reorder(Crime,-Count),y=Count)) + geom_bar(stat="Identity") +
         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
         scale_y_continuous(labels = comma)

#3. E como evoluem esses tipos mais comuns ao longo do tempo?
top5<- sample_full[sample_full$`Primary Type`== c('THEFT', 'BATTERY','CRIMINAL DAMAGE','NARCOTICS','OTHER OFFENSE'),]
top5_per_year<- data.frame(table(top5$`Primary Type`,top5$Year))
colnames(top5_per_year)<-c("Crime","Year","Count")
ggplot(top5_per_year,aes(x=factor(Year),y=Count,group=Crime,color=Crime)) + 
  geom_line() +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = comma)

#4. Onde e a que horas?
##Extrair hora
sample_full$hour <- hour(as.POSIXlt(sample_full$Date,format = "%m/%d/%Y %I:%M:%S %p"))

crimes_per_place<- data.frame(table(sample_full$`Community Area`))
colnames(crimes_per_place)<-c("CA","Count")
ggplot(crimes_per_place,aes(x=reorder(factor(CA),-Count),y=Count)) + geom_bar(stat="Identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = comma)

#NOTA: em zoom ve se bem
#Ver qual é o nome dos top 5. Claramente há um q se destaca...
crimes_per_hour<- data.frame(table(sample_full$hour))
colnames(crimes_per_hour)<-c("Hour","Count")
ggplot(crimes_per_hour,aes(x=reorder(factor(Hour),-Count),y=Count)) + geom_bar(stat="Identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = comma)


par(mfrow=c(2,1))

#5.Como se comporta o padrão de detenção? Por ano? e por crime? e por local?
arrest_per_year<- data.frame(table(sample_full$Arrest,sample_full$Year))
colnames(arrest_per_year)<- c("Arrest","Year","Count")
apy<-ggplot(arrest_per_year,aes(x=factor(Year),y=Count,group=Arrest, fill=Arrest)) + 
  geom_bar(stat="identity",position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

arrest_per_crime<- data.frame(table(sample_full$Arrest,sample_full$`Primary Type`))
colnames(arrest_per_crime)<- c("Arrest","Crime","Count")
apc<-ggplot(arrest_per_crime,aes(x=factor(Crime),y=Count,group=Arrest, fill=Arrest)) + 
  geom_bar(stat="identity",position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

arrest_per_place<- data.frame(table(sample_full$Arrest,sample_full$`Community Area`))
colnames(arrest_per_place)<- c("Arrest","Place","Count")
app<-ggplot(arrest_per_place,aes(x=factor(Place),y=Count,group=Arrest, fill=Arrest)) + 
  geom_bar(stat="identity",position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##Provavelmente a variação nos anos e nos bairros deve se a variação em tipo de crimes...
#anos no eixo do x, percentual do top 5 e ao lado um gráfico com % de cada crime 
#OU GRÁFICO 3D
#CA em mapa

#exemplo
plot_grid(apy, apc, labels = "AUTO")


#6. Como se comporta o padrão de crimes domésticos? Por ano por crime e por local
domestic_per_year<- data.frame(table(sample_full$Domestic,sample_full$Year))
colnames(domestic_per_year)<- c("Domestic","Year","Count")
ggplot(domestic_per_year,aes(x=factor(Year),y=Count,group=Domestic, fill=Domestic)) + 
  geom_bar(stat="identity",position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

domestic_per_crime<- data.frame(table(sample_full$Domestic,sample_full$`Primary Type`))
colnames(domestic_per_crime)<- c("Domestic","Crime","Count")
ggplot(domestic_per_crime,aes(x=factor(Crime),y=Count,group=Domestic, fill=Domestic)) + 
  geom_bar(stat="identity",position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

domestic_per_place<- data.frame(table(sample_full$Domestic,sample_full$`Community Area`))
colnames(domestic_per_place)<- c("Domestic","Place","Count")
ggplot(domestic_per_place,aes(x=factor(Place),y=Count,group=Domestic, fill=Domestic)) + 
  geom_bar(stat="identity",position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#7. Is there a relationship between domestic crime and arrests?
"O Domestic é de lado, o Arrest é em cima"
domestic_arrest<- data.frame(table(sample_full$Domestic,sample_full$Arrest))
colnames(domestic_arrest)<- c("Domestic","Arrest","Count")
ggplot(domestic_arrest,aes(x=factor(Domestic),y=Count,group=Arrest, fill=Arrest)) + 
  geom_bar(stat="identity",position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


