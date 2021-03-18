#mapa com % na escala. boxes p escolher tipos de crime. botao select all
# e deselect all
#NOTA: AQUI EM VEZ DE TABLE USO EQUIVALENTE DO DPLYR PRA SER MAIS RAPIDO


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
d<-d[!is.na(d$Primary.Type),]


#% arrests
v<-c("HOMICIDE")
d2<-d[d$Primary.Type %in% v,]
ct2<-d2 %>%
  count(Community.Area, Arrest) %>%
  mutate(prop = prop.table(n))
arrests<-data.frame(ca=numeric(),percentage=numeric(),n=numeric())
for (i in c(1:77)){
  arrests[i,"ca"]<-i
  t0<-ct2[ct2$Community.Area==i & ct2$Arrest==TRUE,"n"]
  f0<-ct2[ct2$Community.Area==i & ct2$Arrest==FALSE,"n"]
  t<-ifelse(is.na(as.numeric(t0[1,])),0,as.numeric(t0[1,]))
  f<-ifelse(is.na(as.numeric(f0[1,])),0,as.numeric(f0[1,]))
  arrests[i,"percentage"]<- t/(t+f)
  arrests[i,"n"]<- t+f
}
arrests$percentage<-round(arrests$percentage*100,1)







sh<- readOGR( dsn= "parte2/7-Socioeconomic/geo_export_b999f164-7c24-4834-b8de-3ec909694683.shp")

shp<-sh
shp@data[['area_numbe']]<-make.names(shp@data[['area_numbe']])
shp@data[['area_numbe']]<-factor(shp@data[['area_numbe']])

colnames(arrests)<-c("area_numbe","Percentage","n")
arrests$area_numbe<-make.names(arrests$area_numbe)
arrests$area_numbe<-factor(arrests$area_numbe)

shp@data$id<-rownames(shp@data)
shp@data<-left_join(shp@data,arrests,by="area_numbe")
shp2<-fortify(shp)
shp2<-left_join(shp2,shp@data,by="id")

ggplot() +
  geom_polygon_interactive(shp2, mapping = aes( x = long, y = lat, group = group,fill=Percentage,
                                                tooltip=paste("CA:", community, "\n %Arrest: ",Percentage), data_id=community), color = "white") +
  labs(
    title = "Types of Crimes, Arrests and Location",
    subtitle = "Chicago, IL"
  ) + theme_void() + coord_map("mercator")+ scale_fill_gradient(name="Percentage of Arrest",
                                                                low = "#ffffcc", 
                                                                high = "#ff4444",
                                                                space = "Lab", 
                                                                na.value = "grey50",
                                                                guide = "colourbar",
                                                                label=comma,
                                                                limits=c(0,70),
                                                                breaks= c(0,10,20,30,40,50,60,70)
  )


x<-girafe(print(g))
x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                  opts_sizing(rescale = FALSE))

x                                                        





ui <- fluidPage(
  mainPanel(
    ggiraphOutput("map")
  ),
  sidebarPanel(
    checkboxGroupInput("boxes","Select Crime Type:", choices = sort(unique(d$Primary.Type)),inline = T,
                       selected =unique(d$Primary.Type) )
  ),
  actionButton("selectall", label="Select/Deselect all")
  )

server <- function(input, output,session) {
  
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="boxes",
                                 choices = sort(unique(d$Primary.Type)),
                                 inline = T,
                                 selected=sort(unique(d$Primary.Type)))
        
        
      } else{
        updateCheckboxGroupInput(session=session, 
                                 inputId="boxes",
                                 choices = sort(unique(d$Primary.Type)),
                                 inline = T,
                                 selected=c())
      }}
  })

  
  re <- reactive({
      y <- d[d$Primary.Type %in% input$boxes,]
      print(unique(y$Primary.Type))
      ct2<-y %>%
        count(Community.Area, Arrest) %>%
        mutate(prop = prop.table(n))
      arrests<-data.frame(ca=numeric(),percentage=numeric())
      for (i in c(1:77)){
        arrests[i,"ca"]<-i
        t0<-ct2[ct2$Community.Area==i & ct2$Arrest==TRUE,"n"]
        f0<-ct2[ct2$Community.Area==i & ct2$Arrest==FALSE,"n"]
        t<-ifelse(is.na(as.numeric(t0[1,])),0,as.numeric(t0[1,]))
        f<-ifelse(is.na(as.numeric(f0[1,])),0,as.numeric(f0[1,]))
        arrests[i,"percentage"]<- t/(t+f)
        arrests[i,"n"]<- t+f
      }
      arrests$percentage<-round(arrests$percentage*100,1)
      print(arrests)
      
      shp<-sh
      shp@data[['area_numbe']]<-make.names(shp@data[['area_numbe']])
      shp@data[['area_numbe']]<-factor(shp@data[['area_numbe']])
      
      colnames(arrests)<-c("area_numbe","Percentage","n")
      arrests$area_numbe<-make.names(arrests$area_numbe)
      arrests$area_numbe<-factor(arrests$area_numbe)
      
      shp@data$id<-rownames(shp@data)
      shp@data<-left_join(shp@data,arrests,by="area_numbe")
      shp2<-fortify(shp)
      shp2<-left_join(shp2,shp@data,by="id")
      
      
  })
  
        
  
  
  output$map <- 
    
    renderggiraph({
      
      if (length(input$boxes)!=0){
        
        g<-ggplot() +
          geom_polygon_interactive(re(), mapping = aes( x = long, y = lat, group = group,fill=Percentage,
                                                        tooltip=paste("CA:", community, "\n Arrest: ",Percentage,"%", "\n Nr Crimes:", n), 
                                                        data_id=community), color = "white") +
          labs(
            title = "Types of Crimes, Arrests and Location",
            subtitle = "Chicago, IL"
          ) + theme_void() + coord_map("mercator")+ scale_fill_gradient(name="Percentage of Arrest",
                                                                        low = "#ffffcc", 
                                                                        high = "#ff4444",
                                                                        space = "Lab", 
                                                                        na.value = "grey50",
                                                                        guide = "colourbar",
                                                                        label=comma,
                                                                        limits=c(0,100),
                                                                        breaks= c(0,10,20,30,40,50,60,70,80,90,100)
          )
        
        x<-girafe(print(g))
        x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                          opts_sizing(rescale = FALSE),opts_hover(css="stroke:lightgrey;fill:lightgrey;"))
        
      } else{
        
        
        g<-ggplot() +
          labs(
            title = "Types of Crimes, Arrests and Location",
            subtitle = "Chicago, IL"
          ) + theme_void() + coord_map("mercator")+ scale_fill_gradient(name="Percentage of Arrest",
                                                                        low = "#ffffcc", 
                                                                        high = "#ff4444",
                                                                        space = "Lab", 
                                                                        na.value = "grey50",
                                                                        guide = "colourbar",
                                                                        label=comma,
                                                                        limits=c(0,70),
                                                                        breaks= c(0,10,20,30,40,50,60,70)
          )
        
        x<-girafe(print(g))
        x<-girafe_options(x=x,opts_tooltip(css="background-color:grey;font-style:italic;font-size=6;"),
                          opts_sizing(rescale = FALSE),opts_hover(css="stroke:lightgrey;fill:lightgrey;"))
        
        
      }
      
    })
}

shinyApp(ui,server)
