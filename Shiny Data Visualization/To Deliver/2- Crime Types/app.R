library(readr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(ggiraph)
library(shiny)
library(dplyr)
library(wordcloud2)
setwd("~/Documents/Mestrado/VD/proj/parte2/1-Crime Over Time")
d<-read_csv("Sample_CORRECTED.csv")
d<-d[!is.na(d$Year),]

#2. Como se repartem os crimes por tipo?
crimes_per_type<-data.frame(table(d$Primary.Type))
colnames(crimes_per_type)<-c("Word","Freq")
crimes_per_type<-crimes_per_type[order(crimes_per_type$Freq,decreasing = T),]


w<-wordcloud2(crimes_per_type,color = "random-dark",size = 2,
          backgroundColor = "white",rotateRatio = 0.5)

# wordcloud(crimes_per_type$Word,crimes_per_type$Freq,min.freq = 1,
#           max.words=35, random.order=FALSE, rot.per=0.35,
#           scale=c(3,0.5),
#           colors=brewer.pal(8, "Dark2"))

ui <- fluidPage(
  titlePanel(title = h2("Most Common Types of Crime",align="center")),
  
  fluidRow(
  sidebarLayout(
  mainPanel(wordcloud2Output("words")),
  sidebarPanel(
    sliderInput("freq",
                "Frequency:",
                min = 0,  max = 1150000, value = c(100000,1147189),step=50),
    sliderInput("max",
                "Maximum Number of Words:",
                min = 1,  max = 35,  value = 10),
    sliderInput("year","Select range:",min = 2001, max = 2016,value = c(2001,2016),sep="")
  ),position="left")))


server <- function(input, output) {
  
  re <- reactive({
    d1<-d[d$Year >= min(input$year) & d$Year <= max(input$year),]
    years<-unique(d1$Year)
    print(years)
    df<-d1 %>%
      count(Primary.Type) %>%
      mutate(prop = prop.table(n)) %>% select(Primary.Type,n)
    colnames(df)<-c("Word","Freq")
    df<-df[order(df$Freq,decreasing = T),]
    df<-head(df,input$max)
    #print(df)
    df})
    
  
  output$words <- renderWordcloud2({
    wordcloud2(re(),color = "random-dark",size = 0.5,
               backgroundColor = "white",rotateRatio = 0.5)
  })
}

shinyApp(ui, server)



#library(rsconnect)
#deployApp()











# ggplot(crimes_per_type,aes(x=reorder(Crime,-Count),y=Count)) + geom_bar(stat="Identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_y_continuous(labels = comma)
