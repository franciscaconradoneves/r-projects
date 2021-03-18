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
setwd("~/Documents/Mestrado/VD/proj/")
d<-read_csv("Sample_CORRECTED.csv")

#onde e a que horas?
d$hour <- hour(as.POSIXlt(d$Date,format = "%m/%d/%Y %I:%M:%S %p"))
d<-d[!is.na(d$hour),]
d[d$hour==0,"hour"]<-24

sh<- readOGR( dsn= "parte2/7-Socioeconomic/geo_export_b999f164-7c24-4834-b8de-3ec909694683.shp")

shp<-sh
shp@data[['area_numbe']]<-make.names(shp@data[['area_numbe']])
shp@data[['area_numbe']]<-factor(shp@data[['area_numbe']])

df<-d %>%
  count(Community.Area) %>%
  mutate(prop = prop.table(n)) %>% select(Community.Area,n)

df<-df[!is.na(df$Community.Area),]
colnames(df)<-c("area_numbe","Count")
df$area_numbe<-make.names(df$area_numbe)
df$area_numbe<-factor(df$area_numbe)

shp@data$id<-rownames(shp@data)
shp@data<-left_join(shp@data,df,by="area_numbe")
shp2<-fortify(shp)
shp2<-left_join(shp2,shp@data,by="id")

ggplot() +
  geom_polygon_interactive(shp2, mapping = aes( x = long, y = lat, group = group,fill=Count,
                                                tooltip=paste("CA:", community, "\n Nr Crimes:",Count), data_id=community), color = "white") +
  labs(
    title = "Criminality, Location and Time of the Day",
    subtitle = "Chicago, IL"
  ) + theme_void() + coord_map("mercator")+ scale_fill_gradient(name="Number of Crimes",
                                             low = "#ffffcc", 
                                             high = "#ff4444",
                                             space = "Lab", 
                                             na.value = "grey50",
                                             guide = "colourbar",
                                             label=comma,
                                             limits=c(0,356061),
                                             breaks= c(0,50000,100000,
                                                       150000,200000,250000,300000,350000)
                                             )


x<-girafe(print(g))
x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                  opts_sizing(rescale = FALSE),opts_hover(css="stroke:lightgrey;fill:lightgrey;"))

x                                                        


ui <- fluidPage(
  mainPanel(
    ggiraphOutput("map")
  ),
  sidebarPanel(h3(strong("Select Hours:")),
    numericInput("start","From:",value=20,min=1,max=24,step=1),
    numericInput("end","To:",value=24,min=1,max=24,step=1)

))

server <- function(input, output) {
  
  re <- reactive({
    if (input$start<input$end){
      y <- d[d$hour>=input$start & d$hour<input$end,]
    }else if (input$start==input$end){
      y<-d[d$hour==input$start,]
    }
      else{
      y <- d[d$hour>=input$start | d$hour<input$end,]
    }
      
    print(unique(y$hour))
    #unique(y$hour)
    
    df<-y %>%
      count(Community.Area) %>%
      mutate(prop = prop.table(n)) %>% select(Community.Area,n)

    colnames(df)<-c("area_numbe","Count")
    df$area_numbe<-make.names(df$area_numbe)
    df$area_numbe<-factor(df$area_numbe)
    df<-df[-78,]
    df$area_numbe<-factor(df$area_numbe)

    print(head(df))
    
    shp<-sh
    shp@data[['area_numbe']]<-make.names(shp@data[['area_numbe']])
    shp@data[['area_numbe']]<-factor(shp@data[['area_numbe']])
    shp@data$id<-rownames(shp@data)
    shp@data<-left_join(shp@data,df,by="area_numbe")
    shp2<-fortify(shp)
    shp2<-left_join(shp2,shp@data,by="id")
    
    })
  
  output$map <- 
    
    renderggiraph({
        
        if (!is.na(input$start) & !is.na(input$end)){
        
        g<-g<-ggplot() +
          geom_polygon_interactive(re(), mapping = aes( x = long, y = lat, group = group,fill=Count,
                                                        tooltip=paste("CA:", community, "\n Nr Crimes:",Count), data_id=Count), color = "white") +
          labs(
            title = "Criminality, Location and Time of the Day",
            subtitle = "Chicago, IL"
          ) + theme_void() + coord_map("mercator")+ scale_fill_gradient(name="Number of Crimes",
                                                                        low = "#ffffcc", 
                                                                        high = "#ff4444",
                                                                        space = "Lab", 
                                                                        na.value = "grey50",
                                                                        guide = "colourbar",
                                                                        label=comma, limits=c(0,356061),
                                                                        breaks= c(0,50000,100000,
                                                                                  150000,200000,250000,300000,350000)
          )
        
        x<-girafe(print(g))
        x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                          opts_sizing(rescale = FALSE),opts_hover(css="stroke:lightgrey;fill:lightgrey;"))
        
        } else{
          
          
          g<-g<-ggplot() +
            labs(
              title = "Criminality, Location and Time of the Day",
              subtitle = "Chicago, IL"
            ) + theme_void() + coord_map("mercator")+ scale_fill_gradient(name="Number of Crimes",
                                                                          low = "#ffffcc", 
                                                                          high = "#ff4444",
                                                                          space = "Lab", 
                                                                          na.value = "grey50",
                                                                          guide = "colourbar",
                                                                          label=comma,
                                                                          limits=c(0,356061),
                                                                          breaks= c(0,50000,100000,
                                                                                    150000,200000,250000,300000,350000)
            )
          
          x<-girafe(print(g))
          x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                            opts_sizing(rescale = FALSE),opts_hover(css="stroke:lightgrey;fill:lightgrey;"))
          
          
        }
        
    })
}

shinyApp(ui,server)
































