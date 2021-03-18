###########################       MAPAS    ################################
##### Um zoom in entre 2008 e 2011

library(readr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(ggiraph)
library(shiny)
library(rgdal)
library(dplyr)
setwd("~/Documents/Mestrado/VD/proj/parte2")
d<-read_csv("1-Crime Over Time/Sample_CORRECTED.csv")

#1. Criminalidade ao longo dos anos
crimes_per_year <- d %>%
  count(Community.Area,Year) %>%
  mutate(prop = prop.table(n)) %>% select(Community.Area,Year,n)

df<-crimes_per_year[!is.na(crimes_per_year$Community.Area),]
colnames(df)<-c("area_numbe","Year","Count")
df$area_numbe<-make.names(df$area_numbe)
df$area_numbe<-factor(df$area_numbe)


##Most common crime per year per CA
most_common <- d %>%
  count(Community.Area,Year,Primary.Type) %>%
  mutate(prop = prop.table(n))  %>%
  group_by(Community.Area,Year) %>%
  slice(which.max(n)) %>% select(Community.Area,Year,Primary.Type)


df2<-most_common[!is.na(most_common$Community.Area),]
colnames(df2)<-c("area_numbe","Year","Type")
df2$area_numbe<-make.names(df$area_numbe)
df2$area_numbe<-factor(df$area_numbe)


sh<- readOGR( dsn= "7-Socioeconomic/geo_export_b999f164-7c24-4834-b8de-3ec909694683.shp")
shp<-sh
shp@data[['area_numbe']]<-make.names(shp@data[['area_numbe']])
shp@data[['area_numbe']]<-factor(shp@data[['area_numbe']])
shp@data$id<-rownames(shp@data)

shp@data<-left_join(shp@data,df)
shp@data<-left_join(shp@data,df2,by=c("area_numbe","Year"))
shp2<-fortify(shp)
shp2<-left_join(shp2,shp@data,by="id")

y=2008
shp3<-shp2[shp2$Year==y,]

r<-rainbow(6)
cols <- c("NARCOTICS" = r[1], "BATTERY" = r[2], "THEFT" = r[3], 
          "CRIMINAL DAMAGE" = r[4], "CRIMINAL TRESPASS"= r[5], 
          "OTHER OFFENSE"= r[6])


g0<-ggplot() +
  geom_polygon_interactive(shp3, mapping = aes( x = long, y = lat, group = group,fill=Type,
                                                tooltip=paste("CA:", community), data_id=community), color = "white") +
  labs(
    subtitle = "Chicago, IL"
  ) + theme_void() + coord_map("mercator")+ scale_fill_manual(values=cols)


x0<-girafe(print(g0))
x0<-girafe_options(x=x0,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                  opts_sizing(rescale = FALSE),opts_hover(css="stroke:lightgrey;fill:lightgrey;"))

x0


g<-ggplot() +
  geom_polygon_interactive(shp3, mapping = aes( x = long, y = lat, group = group,fill=Count,
                                                tooltip=paste("CA:", community, "\n Nr Crimes:",Count), data_id=community), color = "white") +
  labs(
    subtitle = "Chicago, IL"
  ) + theme_void() + coord_map("mercator")+ scale_fill_gradient(name="Number of Crimes",
                                                                low = "#ffffcc", 
                                                                high = "#ff4444",
                                                                space = "Lab", 
                                                                na.value = "grey50",
                                                                guide = "colourbar",
                                                                label=comma,
                                                                limits=c(0,40000),
                                                                breaks= c(0,10000,20000,30000,40000))


x<-girafe(print(g))
x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                  opts_sizing(rescale = FALSE),opts_hover(css="stroke:lightgrey;fill:lightgrey;"))

x

girafe( ggobj = plot_grid(g, g0,align="v"))

cpy <- data.frame(table(d$Year))
colnames(cpy)<-c("Year","Count")

g_<-ggplot(cpy,aes(x=Year,y=Count,group=1)) + geom_line() + geom_point_interactive(aes(tooltip=paste("Nr Crimes:",Count),data_id=Count,size=2)) +
  scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none",
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(size=13)) +
  ylab("Number of Crimes") +
  ggtitle("Criminality Throughout Time (Chicago, IL)")


ui <- fluidPage(
  titlePanel(title = h2("Criminality, Location and Time of the Day")),
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("10%", "90%"),
    ggiraphOutput("m0"),
    ggiraphOutput("map"))
    )
    ,width = 10.5),

  
  sidebarPanel(
    radioButtons("boxes","Select Year", choices = sort(unique(d$Year)),inline = T,
                       selected = 2008), width = 1.5
  ))

server <- function(input, output,session) {

  re <- reactive({
    y<-shp2[shp2$Year==input$boxes,]
    print(y)
    })
  
  output$m0<-
    renderggiraph({
      girafe(print(g_))
             
    })
  
  
  output$map <- 
    
    renderggiraph({
        
        g<-ggplot() +
          geom_polygon_interactive(re(), mapping = aes( x = long, y = lat, group = group,fill=Count,
                                                        tooltip=paste("CA:", community, "\n Nr Crimes:",Count), data_id=community), color = "white") +
          labs(
          
            subtitle = "Chicago, IL") + 
          theme_void() + 
          coord_map("mercator")+ scale_fill_gradient(name="Number of Crimes",
                                                                        low = "#ffffcc", 
                                                                        high = "#ff4444",
                                                                        space = "Lab", 
                                                                        na.value = "grey50",
                                                                        guide = "colourbar",
                                                                        limits=c(0,40000),
                                                                        breaks= c(0,10000,20000,30000,40000))
        
        g0<-ggplot() +
          geom_polygon_interactive(re(), mapping = aes( x = long, y = lat, group = group,fill=Type,
                                                        tooltip=paste("CA:", community), data_id=community), color = "white") +
          labs(
            subtitle = "Chicago, IL"
          ) + theme_void() + coord_map("mercator")+ scale_fill_manual(values=cols)
        
          
        
        x<-girafe( ggobj = plot_grid(g, g0,align="v"),width_svg = 12, height_svg = 6)
        x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                          opts_sizing(rescale = FALSE),opts_hover(css="stroke:lightgrey;fill:lightgrey;"))
    
    })
    }


shinyApp(ui,server)



