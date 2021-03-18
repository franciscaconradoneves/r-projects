##PROBLEMA COM CA 0 -> pedir ajuda ao prof
#VISUALIZAÇÃO FINAL  -> Bivariate choropleth maps?
#PROBLEMA DA C AREA 0

#NESTA QUESTÃO
  #Dados só de 2008 a 2012 p match com census
  #Remover NAs 
  #Não ha dados socioeconomicos nas medidas da policia, so nas governamentais
  #ward tem menos NAs mas nao encontramos dados

#DADOS TÊ PROBLEMA
#1 Community Area a mais 
#porque va de 0 a 77 e não de 1 a 77
#os 0 que têm coordenadas conseguem.se encaixar no sítio. later on.
## alguns caem fora de qq CA, outros têm coords NA mas outros deu
##NOTA: NAs crescem mas isso é pq os que eram 0 passam a NA


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
library(shiny)
library(plotly)

setwd("/home/utilizador/Documents/Mestrado/VD/proj/parte2/Socioeconomic")
data <- read_csv("Sample_CORRECTED.csv")
census<-read_csv("Census_Data_-_Selected_socioeconomic_indicators_in_Chicago__2008___2012.csv")
names(census)<-make.names(colnames(census))
census<-census[-78,]
census<-census[,c(1,2,8)]
d<- data[,-1]
d<-data.frame(d)

#Dados só de 2008 a 2012 p match com census
y<-c(2008:2012)
d<-d[data$Year %in% y,]
d$Community.Area<-as.numeric(d$Community.Area)


shp <- readOGR( dsn= "Socioeconomic/geo_export_b999f164-7c24-4834-b8de-3ec909694683.shp")
shp@data[['area_numbe']]<-make.names(shp@data[['area_numbe']])
shp@data[['area_numbe']]<-factor(shp@data[['area_numbe']])


#Dataframe com CA, count crimes (2008-2012) e medida socioec(2008-2012) 
d2<-data.frame(table(d$Community.Area)) 
names(d2)<-c("area_numbe","Crimes")
d2$Name<-census$COMMUNITY.AREA.NAME
d2$PCI<-census$PER.CAPITA.INCOME
d2$area_numbe<-make.names(d2$area_numbe)
d2$area_numbe<-factor(d2$area_numbe)
order(levels(shp@data[['area_numbe']]))
order(levels(d2$area_numbe))

shp@data$id<-rownames(shp@data)
shp@data<-left_join(shp@data,d2,by="area_numbe")
shp2<-fortify(shp)
shp2<-left_join(shp2,shp@data,by="id")
text<- data.frame(getSpPPolygonsLabptSlots(shp))
names(text)<-c("long","lat")
text$crimes<-d2$Crimes

####################################################################################
############################# ALL CRIMES ###########################################
####################################################################################

################################
#https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html
bi<-bi_class(shp2,x=Crimes,y=PCI,style = "quantile", dim = 3)


map <- ggplot() +
  geom_polygon(data = bi, mapping = aes( x = long, y = lat, group = group,fill = bi_class,label=community), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Criminality and Per Capita Income",
    subtitle = "Chicago, IL"
  ) + coord_map("mercator") + theme_void()


legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Criminality ",
                    ylab = "Per Capita Income ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) + draw_plot(legend, 0.08, .65, 0.2, 0.2)

m<-ggplot() +
  geom_polygon_interactive(data = bi, mapping = aes( x = long, y = lat, group = group,fill = bi_class,
                                                     tooltip=paste("Community:",community,"\n","PCI:",PCI,"\n","Nr Crimes:",Crimes),
                                                     data_id=community), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Criminality and Per Capita Income",
    subtitle = "Chicago, IL"
  ) + coord_map("mercator") + theme_void()

ggiraph(code = print(m), hover_css = "cursor:pointer;fill:red;stroke:red;")


legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Criminality ",
                    ylab = "Per Capita Income ",
                    size = 8)

g<-girafe( ggobj = plot_grid(m, legend,rel_widths = c(5, 1),rel_heights = c(5, 1)), width_svg = 10, height_svg = 10)
g




ui <- fluidPage(
  titlePanel(""),
  mainPanel(
    ggiraphOutput("Criminality")
  )
)
  
  server <- function(input, output) {
    output$Criminality <- renderggiraph({
     g
    })
  }
  
shinyApp(ui, server)



library(rsconnect)
deployApp()

