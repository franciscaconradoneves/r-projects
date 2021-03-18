library(shiny)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(lubridate)
library(plyr)


Sun2010 = as.data.frame(Sun2010)
for (i in 1:nrow(Sun2010)){
  x = Sun2010[i, "Date"]
  xx = strsplit(x, " ")
  if (length(xx[[1]])>3) {
    xxx = xx[[1]]
    mes = xxx[2]
    dia = xxx[4]
  } else {
    xxx = xx[[1]]
    mes = xxx[2]
    dia= xxx[3] }
  final = paste(mes,dia,as.character("2010"), collapse = NULL)
  final = gsub(" ", "-", final)
  data2010 =parse_date_time(x = final,
                            orders = c("m d y", "m/d/y"))
  fd2010 = format(data2010, format = "%m/%d/%Y")
  Sun2010[i, "Date"] = fd2010
}


Sun2009 = as.data.frame(Sun2009)
for (i in 1:nrow(Sun2009)){
  x = Sun2009[i, "Date"]
  xx = strsplit(x, " ")
  if (length(xx[[1]])>3) {
    xxx = xx[[1]]
    mes = xxx[2]
    dia = xxx[4]
  } else {
    xxx = xx[[1]]
    mes = xxx[2]
    dia= xxx[3] }
  final = paste(mes,dia,as.character("2009"), collapse = NULL)
  final = gsub(" ", "-", final)
  data2009 =parse_date_time(x = final,
                            orders = c("m d y", "m/d/y"))
  fd2009 = format(data2009, format = "%m/%d/%Y")
  Sun2009[i, "Date"] = fd2009
}


Sun2008 = as.data.frame(Sun2008)
for (i in 1:nrow(Sun2008)){
  x = Sun2008[i, "Date"]
  xx = strsplit(x, " ")
  if (length(xx[[1]])>3) {
    xxx = xx[[1]]
    mes = xxx[2]
    dia = xxx[4]
  } else {
    xxx = xx[[1]]
    mes = xxx[2]
    dia= xxx[3] }
  final = paste(mes,dia,as.character("2008"), collapse = NULL)
  final = gsub(" ", "-", final)
  data2008 =parse_date_time(x = final,
                            orders = c("m d y", "m/d/y"))
  fd2008 = format(data2008, format = "%m/%d/%Y")
  Sun2008[i, "Date"] = fd2008
}




Sun2011 = as.data.frame(Sun2011)
for (i in 1:nrow(Sun2011)){
  x = Sun2011[i, "Date"]
  xx = strsplit(x, " ")
  if (length(xx[[1]])>3) {
    xxx = xx[[1]]
    mes = xxx[2]
    dia = xxx[4]
  } else {
    xxx = xx[[1]]
    mes = xxx[2]
    dia= xxx[3] }
  final = paste(mes,dia,as.character("2011"), collapse = NULL)
  final = gsub(" ", "-", final)
  data2011 =parse_date_time(x = final,
                            orders = c("m d y", "m/d/y"))
  fd2011 = format(data2011, format = "%m/%d/%Y")
  Sun2011[i, "Date"] = fd2011
}



Sun2012 = as.data.frame(Sun2012)
for (i in 1:nrow(Sun2012)){
  x = Sun2012[i, "Date"]
  xx = strsplit(x, " ")
  if (length(xx[[1]])>3) {
    xxx = xx[[1]]
    mes = xxx[2]
    dia = xxx[4]
  } else {
    xxx = xx[[1]]
    mes = xxx[2]
    dia= xxx[3] }
  final = paste(mes,dia,as.character("2012"), collapse = NULL)
  final = gsub(" ", "-", final)
  data2012 =parse_date_time(x = final,
                            orders = c("m d y", "m/d/y"))
  fd2012 = format(data2012, format = "%m/%d/%Y")
  Sun2012[i, "Date"] = fd2012
}



Sun2008$group=1
Sun2009$group=2
Sun2010$group=3
Sun2011$group=4
Sun2012$group =5
total = Reduce(function(...) merge(..., all=TRUE), list(Sun2008,Sun2009,Sun2010, Sun2011, Sun2012))
rm(Sun2008,Sun2009,Sun2010,Sun2011, Sun2012)
write.csv(total[1:3], file ="totalSun.csv")
#quando abrimos vem o x1 
totalSun = total[1:3]



Sample_CORRECTED = Sample_CORRECTED[-c(1,2)]
Hours <- format(as.POSIXct(strptime(Sample_CORRECTED$Date,"%m/%d/%Y %I:%M:%S")) ,format = "%I:%M:%S")
Dates <- format(as.POSIXct(strptime(Sample_CORRECTED$Date,"%m/%d/%Y %H:%M")) ,format = "%m/%d/%Y")
Sample_CORRECTED$Date = Dates
Sample_CORRECTED$Hour = Hours


library(dplyr)
Sample_CORRECTED = as.data.frame(Sample_CORRECTED)
totalSun = as.data.frame(totalSun)
dplr <- left_join(Sample_CORRECTED, totalSun, by=c("Date"))
dplr$Sunrise = as.character(dplr$Sunrise)
dplr$Sunset = as.character(dplr$Sunset)
dplr$Light = if_else((dplr$Hour>dplr$Sunrise & dplr$Hour < dplr$Sunset), 1, 0)

a = dplr[which(!(is.na(dplr$Light))),]
write.csv(a,"dados.csv")


#Shiny and stacked barplot 
dados = dados[-1]
a = table(dados$Primary.Type, dados$Light)
aa = as.data.frame(a)
colnames(aa) = c("CrimeType", "Light", "Freq")

#d <- ggplot(aa, aes(x = CrimeType, y = Freq , fill = factor(Light))) + geom_bar(stat = "identity")
ce <- ddply(aa, c("CrimeType"), transform, percent_freq = Freq / sum(Freq) * 100)
ce$percent_freq = round(ce$percent_freq, 2)



Crimes = sort(unique(ce$CrimeType))
ui <- fluidPage(
  titlePanel("Light influence in Chicago Crimes between 2008 and 2012"),
  sidebarPanel(
    checkboxGroupInput("crime","Select Crime Type:", choices = Crimes ,inline = T,
                       selected = "ARSON" )
  ),
  actionButton("selectall", label="Select/Deselect all"),
  mainPanel( 
    ggiraphOutput("plot")
    ))

server <- function(input, output, session) {
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="crime",
                                 choices = Crimes,
                                 inline = T,
                                 selected=Crimes)
        
      } else{
        updateCheckboxGroupInput(session=session, 
                                 inputId="crime",
                                 choices = Crimes,
                                 inline = T,
                                 selected=c())
      }}
  })
  subsetData <- reactive({
      ce[which(ce$CrimeType %in% input$crime ),]
  })
  output$plot <- renderGirafe({
    if (length(input$crime !=0)){
    gg <- ggplot(subsetData(), aes(x = CrimeType , y = percent_freq, fill = Light, tooltip = paste(" ",CrimeType, "\n","%Crime:", factor(percent_freq), "\n", "Nr Crimes:", factor(Freq)))) + 
      geom_bar_interactive(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8), axis.title.y = element_text(face = "bold", vjust = 1, size = 13), axis.title.x = element_text(face= "bold",size = 12)) + 
      scale_fill_manual_interactive("Presence of Light", values =c( "0" = "#003366", "1" = "#ffd500"), labels = c("No", "Yes")) + 
      labs(x = "Crime Type", y = "% Crimes" ) 
    girafe(ggobj = gg)
    } else {
      gg <- ggplot() + 
        geom_bar_interactive(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12), axis.title.y = element_text(face = "bold", vjust = 1, size = 13), axis.title.x = element_text(face= "bold",size = 13)) + 
        scale_fill_manual_interactive("Presence of Light", values =c( "0" = "#003366", "1" = "#ffd500"), labels = c("No", "Yes")) + 
        labs(x = "Crime Type", y = "% Crimes" )
      girafe(ggobj = gg)
    } })
}

shinyApp(ui = ui, server = server)


