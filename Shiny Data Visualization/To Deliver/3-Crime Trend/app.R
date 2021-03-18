library(readr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(ggiraph)
library(shiny)
library(wordcloud)
library(wordcloud2)
setwd("~/Documents/Mestrado/VD/proj/parte2/1-Crime Over Time")
d<-read_csv("Sample_CORRECTED.csv")

#3. E como evoluem esses tipos mais comuns ao longo do tempo?
## DF para selecionar top k
crimes_per_type<-data.frame(table(d$Primary.Type))
t<-crimes_per_type[order(crimes_per_type$Freq,decreasing = T),]
k=5
topk<- as.character(t[1:k,][[1]])

top<- d[d$Primary.Type %in% topk,]
top_per_year<- data.frame(table(top$Primary.Type, top$Year))
colnames(top_per_year)<-c("Crime","Year","Count")

g<-ggplot(top_per_year,aes(x=factor(Year),y=Count,group=Crime,color=Crime)) + 
  geom_point_interactive(aes(tooltip=paste("Type:",Crime,"\n", "Nr Crimes:",Count),data_id=Count),size=3.5) +
  geom_line() +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = comma) + ylab("Number of Crimes") + xlab("Year") +
  ggtitle("Criminality Throughout Time",subtitle = "Most Frequent Crimes")+
  scale_colour_manual(values = c("BATTERY" = "BLUE", "CRIMINAL DAMAGE" = "GREEN", 
                                 "NARCOTICS"="YELLOW", "OTHER OFFENSE" = "HOTPINK", "THEFT"= "RED"))

                     
x<-girafe(print(g),width_svg = 10)
x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
       opts_sizing(rescale = FALSE),opts_hover(css="stroke:grey;fill:grey;"))
x


l<-list()
for (i in c(1:length(topk))){
  l[[i]]<-topk[i]
}


ui <- fluidPage(
  titlePanel(""),
  mainPanel(
            ggiraphOutput("Criminality")
  ),
  sidebarPanel(
    checkboxGroupInput("crimes",
                       "Select Categories:",
                       choiceNames =
                        l,
                       choiceValues =
                         l
  )
))



server <- function(input, output) {
  
  re <- reactive({
    df <- top_per_year[top_per_year$Crime%in%input$crimes,]
    print(df)
    df})
  
  output$Criminality <- 
  
    renderggiraph({
      
      if (length(input$crimes>0)){
      
        g<-ggplot(re(),aes(x=factor(Year),y=Count,group=Crime,color=Crime)) +
      geom_point_interactive(aes(tooltip=paste("Type:",Crime,"\n", "Nr Crimes:",Count),data_id=Count),size=3.5) +
      geom_line() +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = comma) + ylab("Number of Crimes") + xlab("Year") +
      ggtitle("Criminality Throughout Time",subtitle = "Most Frequent Crimes") +
      scale_colour_manual(values = c("BATTERY" = "BLUE", "CRIMINAL DAMAGE" = "GREEN", 
                 "NARCOTICS"="YELLOW", "OTHER OFFENSE" = "HOTPINK", "THEFT"= "RED")) +
        theme(plot.title = element_text(),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text() 
      )
    
      x<-girafe(print(g),width_svg = 10)
      x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                      opts_sizing(rescale = FALSE),opts_hover(css="stroke:grey;fill:grey;"))
      x}else{
        
        w<-ggplot(top_per_year,aes(x=Year,y=Count)) +  
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                    scale_y_continuous(labels = comma) + ylab("Number of Crimes") + xlab("Year") +
                    ggtitle("Criminality Throughout Time",subtitle = "Most Frequent Crimes") +
          theme(plot.title = element_text(),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text())
                
        girafe(print(w),width_svg = 10)
      }
      
    
    
  })
}

shinyApp(ui,server)

