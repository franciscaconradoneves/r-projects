##CORREÇÃO COMMUNITY AREA
#Correção pontos da community area 0
#Correção de pontos sem community area. 
##Regra geral anos mais antigos é falha corrigivel e anos mais recentes é pq n ha coords

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

setwd("/home/utilizador/Documents/Mestrado/VD/proj/parte2")
data <- read_csv("sample_full.csv")
census<-read_csv("7-Socioeconomic/Census_Data_-_Selected_socioeconomic_indicators_in_Chicago__2008___2012.csv")
names(census)<-make.names(colnames(census))
census<-census[-78,]
census<-census[,c(1,2,8)]
shp <- readOGR( dsn= "Socioeconomic/geo_export_b999f164-7c24-4834-b8de-3ec909694683.shp")
shp@data[['area_numbe']]<-make.names(shp@data[['area_numbe']])
shp@data[['area_numbe']]<-factor(shp@data[['area_numbe']])
colnames(data)<-make.names(colnames(data))

####
p<-subset(data,Community.Area==0)
teste<-subset(data,is.na(data$Community.Area))
points<-rbind(p,teste)

########### TESTE
t<-points[90,] #ta NA tem coords tem de ir parar à CA 59 roseland

##########

for (i in c(1:nrow(points))){
  for (k in c(1:77)){
    point<-points[i,c("Longitude","Latitude")] #um ponto
    pol<-shp@polygons[[k]]@Polygons #vai a todos os pol
    if(point.in.polygon(point[["Longitude"]],point[["Latitude"]],pol[[1]]@coords[,1],pol[[1]]@coords[,2]) %in% c(1,2,3)){
      points[i,"New_CA"]<-shp@data[k,"area_num_1"]
    }
  }}

points$New_CA<-factor(points$New_CA)

for (i in seq(1:nrow(points))){
  index<-points[i,"X1"]
  data[index[[1]],"Community.Area"]<-as.numeric(as.character(points[i,"New_CA"][[1]]))
}

write.csv(data,file="Sample_CORRECTED.csv")

