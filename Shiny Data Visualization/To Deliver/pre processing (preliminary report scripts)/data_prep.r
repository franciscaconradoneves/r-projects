library(readr)

#NAO USAR read.csv DÁ PARSING ERRORS
#Francisca
# ds1 <- read_csv("Desktop/VD/project/Chicago_Crimes_2001_to_2004.csv")
# ds2 <- read_csv("Desktop/VD/project/Chicago_Crimes_2005_to_2007.csv")
# ds3  <- read_csv("Desktop/VD/project/Chicago_Crimes_2008_to_2011.csv")
# ds4 <- read_csv("Desktop/VD/project/Chicago_Crimes_2012_to_2017.csv")


#Sofia
ds1<-read_csv("/home/utilizador/Documents/Mestrado/VD/proj/Chicago_Crimes_2001_to_2004.csv")
ds2<-read_csv("/home/utilizador/Documents/Mestrado/VD/proj/Chicago_Crimes_2005_to_2007.csv")
ds3<-read_csv("/home/utilizador/Documents/Mestrado/VD/proj/Chicago_Crimes_2008_to_2011.csv")
ds4<-read_csv("/home/utilizador/Documents/Mestrado/VD/proj/Chicago_Crimes_2012_to_2017.csv")
# cmd diz me que df 2 tem 1872346 linhas. Estou a perder 1 linha, n é grave

#Remove 2017 (span is different! vai dar asneira)
ds4=ds4[!ds4$Year == 2017, ]


setwd("/home/utilizador/Documents/Mestrado/VD/proj/")

library(dplyr)

# ds1 = ds1  %>% distinct(`Case.Number`, .keep_all = TRUE)
# 
# ds2 = ds2  %>% distinct(`Case.Number`, .keep_all = TRUE)
# 
# ds3 = ds3  %>% distinct(`Case.Number`, .keep_all = TRUE)
# 
# ds4 = ds4  %>% distinct(`Case.Number`, .keep_all = TRUE)


#remove columns 
ds1 = select(ds1, -c(X1,`Case Number`, IUCR, Beat, Ward, 'FBI Code', 'X Coordinate', 'Y Coordinate', 'Updated On', Location ))

ds2 = select(ds2, -c(X1,`Case Number`, IUCR, Beat, Ward, 'FBI Code', 'X Coordinate', 'Y Coordinate', 'Updated On', Location ))

ds3 = select(ds3, -c(X1,`Case Number`, IUCR, Beat, Ward, 'FBI Code', 'X Coordinate', 'Y Coordinate', 'Updated On', Location ))

ds4 = select(ds4, -c(X1,`Case Number`, IUCR, Beat, Ward, 'FBI Code', 'X Coordinate', 'Y Coordinate', 'Updated On', Location ))


#remove Na on coordinates 

# ds1 = na.omit(ds1, cols = c(Latitude, Longitude))
# 
# ds2 = na.omit(ds2, cols = c(Latitude, Longitude))
# 
# ds3 = na.omit(ds3, cols = c(Latitude, Longitude))
# 
# ds4 = na.omit(ds4, cols = c(Latitude, Longitude))

#grouping var
ds1$group=1
ds2$group=2
ds3$group=3
ds4$group=4


#merge all 4 dataframes
total = Reduce(function(...) merge(..., all=TRUE), list(ds1, ds2, ds3, ds4))
rm(ds1,ds2,ds3,ds4)
write.csv(total, file ="Chicago2001_2017.csv")


#Stratified Sampling -> mantém as proporções reais!
#temos 5 milhoes e tal e com 70% temos 3 milhoes e tal ja e uma boa reducao.. 
library(splitstackshape)
sample<-stratified(total,"group",0.7)
sample<-sample[,-"group"]
write.csv(sample, file ="sample_full.csv")


#Place descriptive columns into another df

sample_descriptive<- select(sample, c("Block","Primary Type", Description, "Location Description"))
write.csv(sample_descriptive, file ="sample_2.csv")

sample_primary<- select(sample, -c("Block","Primary Type", Description, "Location Description"))
write.csv(sample_primary, file ="sample_1.csv")




