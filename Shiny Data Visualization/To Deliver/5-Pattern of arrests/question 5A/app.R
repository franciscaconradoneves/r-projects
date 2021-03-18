#WHY IS THERE MORE ARREST IN CA AUSTIN? (25)


library(readr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(ggiraph)
library(shiny)
library(wordcloud)
library(wordcloud2)
library(maptools)
library(sp)
library(rgdal)
library(dplyr)
library(readr)
library(reshape2)

setwd("~/Documents/Mestrado/VD/proj/")
d<-read_csv("Sample_CORRECTED.csv")
d<-d[!is.na(d$Primary.Type),]

##Austin most common crimes
most_common <- d %>%
  count(Community.Area,Primary.Type) %>%
  mutate(prop = prop.table(n))  %>%
  select(Community.Area,Primary.Type,n)

most_common<-most_common[most_common$Community.Area==25,]
most_common<-most_common[order(most_common$n,decreasing = T),]

##% arrest per crime IN GENERAL CHICAGO
ct2<-d %>%
  count(Primary.Type, Arrest) %>%
  mutate(prop = prop.table(n))
arrests<-data.frame(Primary.Type=numeric(),percentage=numeric())
k=1
for (i in unique(d$Primary.Type)){
  arrests[k,"Primary.Type"]<-i
  t0<-ct2[ct2$Primary.Type==i & ct2$Arrest==TRUE,"n"]
  f0<-ct2[ct2$Primary.Type==i & ct2$Arrest==FALSE,"n"]
  t<-ifelse(is.na(as.numeric(t0[1,])),0,as.numeric(t0[1,]))
  f<-ifelse(is.na(as.numeric(f0[1,])),0,as.numeric(f0[1,]))
  arrests[k,"percentage"]<- t/(t+f)
  k=k+1
}
arrests$percentage<-round(arrests$percentage*100,1)

most_common<-left_join(most_common,arrests,by="Primary.Type")
top5<-most_common[1:5,2:4]
top5$no<- 100-top5$percentage
m<-melt(top5,id.vars = "Primary.Type")

g<-ggplot(top5,aes(x=reorder(factor(Primary.Type),-n),y=n,fill=Primary.Type)) + geom_bar(stat="identity",width = 0.8) +
  geom_text(aes(label=paste("%Arrest:",percentage)), position=position_dodge(width=0.9), 
            vjust=-0.25) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 1,size=8),legend.position = "none") +
  xlab("") + ylab("Number of Crimes") + ggtitle("Most Common Crimes and Their Percentage of Arrest",
                                                subtitle = "Austin, Chicago (IL)")


ui <- fluidPage(
  titlePanel(""),
  mainPanel(width = 12,
            plotOutput("Criminality")
  )
)

server <- function(input, output) {
  output$Criminality <- renderPlot({
    g
  })
}

shinyApp(ui, server)



