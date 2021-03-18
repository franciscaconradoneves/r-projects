#################################### TRENDLINE  ###################################
library(readr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(ggiraph)
library(shiny)
#setwd("~/Documents/Mestrado/VD/proj/parte2/1-Crime Over Time")
d<-read_csv("Sample_CORRECTED.csv")

#1. Criminalidade ao longo dos anos
crimes_per_year <- data.frame(table(d$Year))
colnames(crimes_per_year)<-c("Year","Count")

g<-ggplot(crimes_per_year,aes(x=Year,y=Count,group=1)) + geom_line() + geom_point_interactive(aes(tooltip=paste("Nr Crimes:",Count),data_id=Count,size=2)) +
  scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none",
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(size=13)) +
  ylab("Number of Crimes") +
  ggtitle("Criminality Throughout Time (Chicago, IL)")


x<-girafe(print(g))
x<-girafe_options(x=x,opts_tooltip(css="background-color:orange;font-style:italic;font-size=6;"),
                  opts_sizing(rescale = FALSE))

x

ui <- fluidPage(
  titlePanel(""),
  mainPanel(width = 12,
    ggiraphOutput("Criminality")
  )
)

server <- function(input, output) {
  output$Criminality <- renderggiraph({
    x
  })
}

shinyApp(ui, server)



library(rsconnect)
deployApp()



         

