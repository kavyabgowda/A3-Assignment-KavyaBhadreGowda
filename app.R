#########################
#load libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library("plotly")
library('data.table')
library(ggplot2)
library(hrbrthemes)

#######################
#add css code
css <- 
  "
  #expr-container .selectize-dropdown-content .option {
    border-bottom: 2px dotted #ccc;
  }
  .nav-tabs>li.active>a, .nav-tabs>li.active>a:hover, .nav-tabs>li.active>a:focus
  {
  color:red!important;
  }
  
  .container-fluid #title{
  text-align: center;
  } 
   .g-gtitle{
  text-align: center;
  }
  
 
"

######################
#import data
air_data <- read.csv("data/Measurment-Per-Day.csv")

######################
#UI code
ui<-fluidPage(
  theme = shinythemes::shinytheme("slate"),
  
  h1(id="title", "SEOUL Air Pollution Data Visualization", style="font-family: algerian;"),
  tags$style(HTML("#title{color: #3ddb42}")),
  
  sidebarPanel(
    width = 2,
    br(),
    div(style="font-family: times-new-roman;font-size: medium;color: #3ddb42;",
    sliderInput("year", label = "Select Year", 
                min = 2017,
                max = 2019,
                value = 2017)),#sliderinput ends here
    br(),
    div(style="font-family: times-new-roman;font-size: medium;color: #3ddb42;",
    selectInput("month", label="Select Month", 
                c("January", "February", "March", "April", 
                  "May", "June", "July", "August", "September", 
                  "October", "November", "December"), width = "100%")),
    br(),
    div(id = "expr-container", tags$style(css), style="font-family: times-new-roman;font-size: medium;color: #3ddb42;",
        selectInput("address", label="Select Station Code", choices = 
                      c("101", "102", "103", "104", "105",
                        "106", "107", "108", "109", "110",
                        "111", "112", "113", "114", "115",
                        "116", "117", "118", "119", "120", 
                        "121", "122", "123", "124", "125"
                      ), width = "100%")),
    br(),
        div("Address of Selected Station Code:",style="font-family: times-new-roman;font-weight: bolder;font-size: medium;color: #3ddb42;"),
    div("-------------------------------",style="font-weight: bolder;"),
        div(style="font-family: times-new-roman;font-size: medium;font-weight: bolder;color: #c2bf2d;",textOutput("selected_var")),
    br(),
    br(),
    br()
        
    
  ),   #sliderbar layout ends here     
  
  mainPanel(
    width=10,
    div(style="font-family: times-new-roman;font-size: 18px;font-weight: bold;",
    tabsetPanel
    (
      tabPanel("All Pollutants",
               fluidRow(
                 div(
                   span("Pollutants Total Concentration Across Seoul Districts"
                   ,style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;"),span("Pollutants Variations Across Months"
                                ,style="margin-top:5px;margin-left:140px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;")),
                 column(7, leafletOutput(outputId = "mymap",height = "230px")),
                 column(5, plotlyOutput(outputId = "pie1", height = "230px"))
               )
               ,
               fluidRow(
                 div(
                   span("Most and Least polluted districts in Seoul (2017 - 2019)"
                        ,style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;"),span("Total AQI Variation (2017 - 2019)"
                                ,style="margin-top:5px;margin-left:125px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;")),
                 column(7, plotlyOutput(outputId = "bar1", height = "270px")),
                 column(5, plotlyOutput(outputId = "multi1", height = "270px"))
            )),
      tabPanel("PM2.5",
               fluidRow(
                 div(
                   span("PM2.5 pollutant Concentration Across Seoul Districts"
                        ,style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;"),span("Evolution of PM2.5 level over a month"
                                ,style="margin-top:5px;margin-left:50px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;")),
                 column(6, leafletOutput(outputId = "PM2.5map", height = "230px")),
                 column(6, plotlyOutput(outputId = "line1", height = "230px"))
               ),
               br(),
               fluidRow(
                 div("PM2.5 Air Quality Index Health per day for selected month",style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;text-align:center"),
                 column(1.1,offset = 1),
                 column(10.1, plotlyOutput(outputId = "circle1", height = "250px")
                 ))
      ),
      
      tabPanel("PM10",
               fluidRow(
                 div(
                   span("PM10 pollutant Concentration Across Seoul Districts"
                        ,style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;"),span("Evolution of PM10 level over a month"
                                ,style="margin-top:5px;margin-left:50px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;")),
                 column(6, leafletOutput(outputId = "PM10map", height = "230px")),
                 column(6, plotlyOutput(outputId = "line2", height = "230px"))
               ),
               br(),
               fluidRow(
                 div("PM10 Air Quality Index Health per day for selected month",style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;text-align:center"),
                 column(1.1,offset = 1),
                 column(10.1, plotlyOutput(outputId = "circle2", height = "250px")))
      ),
      tabPanel("CO",
               fluidRow(
                 div(
                   span("CO pollutant Concentration Across Seoul Districts"
                        ,style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;"),span("Evolution of CO level over a month"
                                ,style="margin-top:5px;margin-left:75px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;")),
                 column(6, leafletOutput(outputId = "COmap", height = "230px")),
                 column(6, plotlyOutput(outputId = "line3", height = "230px"))
               ),
               br(),
               fluidRow(
                 div("CO Air Quality Index Health per day for selected month",style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;text-align:center"),
                 column(1.1,offset = 1),
                 column(10.1, plotlyOutput(outputId = "circle3", height = "250px")))
      ),
      tabPanel("O3",
               fluidRow(
                 div(
                   span("O3 pollutant Concentration Across Seoul Districts"
                        ,style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;"),span("Evolution of O3 level over a month"
                                ,style="margin-top:5px;margin-left:80px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;")),
                 column(6, leafletOutput(outputId = "O3map", height = "230px")),
                 column(6, plotlyOutput(outputId = "line4", height = "230px"))
               ),
               br(),
               fluidRow(
                 div("O3 Air Quality Index Health per day for selected month",style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;text-align:center"),
                 column(1.1,offset = 1),
                 column(10.1, plotlyOutput(outputId = "circle4", height = "250px")))
      ),
      tabPanel("NO2",
               fluidRow(
                 div(
                   span("NO2 pollutant Concentration Across Seoul Districts"
                        ,style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;"),span("Evolution of NO2 level over a month"
                                ,style="margin-top:5px;margin-left:65px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;")),
                 column(6, leafletOutput(outputId = "NO2map", height = "230px")),
                 column(6, plotlyOutput(outputId = "line5", height = "230px"))
               ),
               br(),
               fluidRow(
                 div("NO2 Air Quality Index Health per day for selected month",style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;text-align:center"),
                 column(1.1,offset = 1),
                 column(10.1, plotlyOutput(outputId = "circle5", height = "250px")))
      ),
      tabPanel("SO2",
               fluidRow(
                 div(
                   span("SO2 pollutant Concentration Across Seoul Districts"
                        ,style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;"),span("Evolution of SO2 level over a month"
                                ,style="margin-top:5px;margin-left:65px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;")),
                 column(6, leafletOutput(outputId = "SO2map", height = "230px")),
                 column(6, plotlyOutput(outputId = "line6", height = "230px"))
               ),
               br(),
               fluidRow(
                 div("SO2 Air Quality Index Health per day for selected month",style="margin-top:5px;margin-left:20px;font-family: georgia;font-size: large;font-weight: italic;color: #3ddb42;
    font-style: oblique;text-align:center"),
                 column(1.1,offset = 1),
                 column(10.1, plotlyOutput(outputId = "circle6", height = "250px")))
      )
    ))
  )#Main panel ends here
  
)#fluid page ends here


#Server code
server<-function(input, output, session) 
{
  
  #assign data
  output$selected_var <- renderText({ 
    data_pie_all <- air_data[air_data$Station.code == input$address,]
    data_sd2<- unique(data_pie_all$Trunc_Address)
    paste(input$address,":",data_sd2)
  })
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Toner)%>%
      setView(lng = 127, lat = 37.55, zoom = 10.3)#  %>% #setting the view over ~ center of Seoul
      })
  
  #observe all pollutants datapoints
  observe({
    year_month <- air_data[air_data$Year == input$year & air_data$Month == input$month,]
    year_data <- aggregate(year_month$AQI_Daily, by=list(station_code=year_month$Station.code,latitude=year_month$Latitude
                                                         ,longitude=year_month$Longitude,address=year_month$Trunc_Address), mean)
    map <- leafletProxy("mymap")
    map %>% 
      clearMarkers() %>%
      addCircleMarkers(data = year_data,
                       lng = year_data$longitude,
                       lat = year_data$latitude,
                       stroke = TRUE,
                       weight = 1,
                       color = "#405394",
                       fill = TRUE,
                       fillColor = "#405394",
                       fillOpacity = 0.9,
                       popup = paste(sep="\n", year_data$station_code,":", year_data$address),
                       radius = year_data$x*12/max(year_data$x))
    
  })#observe add pollutants data points ends
  
  ########################
  output$PM2.5map <- renderLeaflet({
    leaflet() %>%
      
      addTiles()%>%
      setView(lng = 127, lat = 37.55, zoom = 10.3) %>% #setting the view over ~ center of Seoul
      addCircleMarkers(data = air_data, lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = 6, fillOpacity = 0.1, color = "#4026a6", popup= ~ Longitude)
  })
  
  
  #observe pm2.5 datapoints
  observe({
    year_month <- air_data[air_data$Year == input$year & air_data$Month == input$month,]
    year_data <- aggregate(year_month$PM2.5, by=list(station_code=year_month$Station.code,latitude=year_month$Latitude
                                                     ,longitude=year_month$Longitude,address=year_month$Trunc_Address), mean)
    
    
    map <- leafletProxy("PM2.5map")
    map %>% 
      clearMarkers() %>%
      addCircleMarkers(data = year_data,
                       lng = year_data$longitude,
                       lat = year_data$latitude,
                       stroke = TRUE,
                       weight = 1,
                       color = "#4026a6",
                       fill = TRUE,
                       fillColor = "#4026a6",
                       fillOpacity = 0.7,
                       popup = paste(sep="\n", year_data$station_code,":", year_data$address),
                       radius = year_data$x*12/max(year_data$x))
    
  })#observe data points ends
  
  
  
  ########################
  #create the map
  output$PM10map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$MtbMap) %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addProviderTiles(providers$Stamen.TonerLabels)%>%
      setView(lng = 127, lat = 37.55, zoom = 10.3)  %>% #setting the view over ~ center of Seoul
      addCircleMarkers(data = air_data, lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = 6, fillOpacity = 0.1, color = "#405394", popup= ~ Longitude)
  })
  
  
  #observe pm10 datapoints
  observe({
    year_month <- air_data[air_data$Year == input$year & air_data$Month == input$month,]
    year_data <- aggregate(year_month$PM10, by=list(station_code=year_month$Station.code,latitude=year_month$Latitude
                                                    ,longitude=year_month$Longitude,address=year_month$Trunc_Address), mean)
    
    
    map <- leafletProxy("PM10map")
    map %>% 
      clearMarkers() %>%
      addCircleMarkers(data = year_data,
                       lng = year_data$longitude,
                       lat = year_data$latitude,
                       stroke = TRUE,
                       weight = 1,
                       color = "#405394",
                       fill = TRUE,
                       fillColor = "#405394",
                       fillOpacity = 0.9,
                       popup = paste(sep="\n", year_data$station_code,":", year_data$address),
                       radius = year_data$x*12/max(year_data$x))
    
  })#observe data points ends
  
  
  ########################
  #create the map
  output$COmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
      setView(lng = 127, lat = 37.55, zoom = 10.3)  %>% #setting the view over ~ center of Seoul
      addCircleMarkers(data = air_data, lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = 6, fillOpacity = 0.1, color = "#c91c25", popup= ~ Longitude)
  })
  
  
  #observe CO datapoints
  observe({
    year_month <- air_data[air_data$Year == input$year & air_data$Month == input$month,]
    year_data <- aggregate(year_month$CO_mg_per_mm3, by=list(station_code=year_month$Station.code,latitude=year_month$Latitude
                                                             ,longitude=year_month$Longitude,address=year_month$Trunc_Address), mean)
    
    
    map <- leafletProxy("COmap")
    map %>% 
      clearMarkers() %>%
      addCircleMarkers(data = year_data,
                       lng = year_data$longitude,
                       lat = year_data$latitude,
                       stroke = TRUE,
                       weight = 1,
                       color = "#c91c25",
                       fill = TRUE,
                       fillColor = "#c91c25",
                       fillOpacity = 0.8,
                       popup = paste(sep="\n", year_data$station_code,":", year_data$address),
                       radius = year_data$x*12/max(year_data$x))
    
  })#observe data points ends
  
  ########################
  #create the map
  output$O3map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Toner)%>%
      setView(lng = 127, lat = 37.55, zoom = 10.3)  %>% #setting the view over ~ center of Seoul
      addCircleMarkers(data = air_data, lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = 6, fillOpacity = 0.1, color = "#405394", popup= ~ Longitude)
  })
  
  
  #observe O3 datapoints
  observe({
    year_month <- air_data[air_data$Year == input$year & air_data$Month == input$month,]
    year_data <- aggregate(year_month$O3_mg_per_mm3, by=list(station_code=year_month$Station.code,latitude=year_month$Latitude
                                                             ,longitude=year_month$Longitude,address=year_month$Trunc_Address), mean)
    
    
    map <- leafletProxy("O3map")
    map %>% 
      clearMarkers() %>%
      addCircleMarkers(data = year_data,
                       lng = year_data$longitude,
                       lat = year_data$latitude,
                       stroke = TRUE,
                       weight = 1,
                       color = "#405394",
                       fill = TRUE,
                       fillColor = "#405394",
                       fillOpacity = 0.9,
                       popup = paste(sep="\n", year_data$station_code,":", year_data$address),
                       radius = year_data$x*12/max(year_data$x))
    
  })#observe data points ends
  
  ########################
  #create the map
  output$NO2map <- renderLeaflet({
    leaflet() %>%
      
      addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
      setView(lng = 127, lat = 37.55, zoom = 10.3)  %>% #setting the view over ~ center of Seoul
      addCircleMarkers(data = air_data, lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = 6, fillOpacity = 0.1, color = "#c91c25", popup= ~ Longitude)
  })
  
  
  #observe NO2 datapoints
  observe({
    year_month <- air_data[air_data$Year == input$year & air_data$Month == input$month,]
    year_data <- aggregate(year_month$NO2_mg_per_mm3, by=list(station_code=year_month$Station.code,latitude=year_month$Latitude
                                                              ,longitude=year_month$Longitude,address=year_month$Trunc_Address), mean)
    
    
    map <- leafletProxy("NO2map")
    map %>% 
      clearMarkers() %>%
      addCircleMarkers(data = year_data,
                       lng = year_data$longitude,
                       lat = year_data$latitude,
                       stroke = TRUE,
                       weight = 1,
                       color = "#c91c25",
                       fill = TRUE,
                       fillColor = "#c91c25",
                       fillOpacity = 0.8,
                       popup = paste(sep="\n", year_data$station_code,":", year_data$address),
                       radius = year_data$x*12/max(year_data$x))
    
  })#observe data points ends
  
  
  ########################
  #create the map
  output$SO2map <- renderLeaflet({
    leaflet() %>%addTiles()%>%
      addProviderTiles(providers$MtbMap) %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addProviderTiles(providers$Stamen.TonerLabels)%>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
      setView(lng = 127, lat = 37.55, zoom = 10.3)  %>% #setting the view over ~ center of Seoul
      addCircleMarkers(data = air_data, lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = 6, fillOpacity = 0.1, color = "#405394", popup= ~ Longitude)
  })
  
  
  #observe SO2 datapoints
  observe({
    year_month <- air_data[air_data$Year == input$year & air_data$Month == input$month,]
    year_data <- aggregate(year_month$SO2_mg_per_mm3, by=list(station_code=year_month$Station.code,latitude=year_month$Latitude
                                                              ,longitude=year_month$Longitude,address=year_month$Trunc_Address), mean)
    
    
    map <- leafletProxy("SO2map")
    map %>% 
      clearMarkers() %>%
      addCircleMarkers(data = year_data,
                       lng = year_data$longitude,
                       lat = year_data$latitude,
                       stroke = TRUE,
                       weight = 1,
                       color = "#405394",
                       fill = TRUE,
                       fillColor = "#405394",
                       fillOpacity = 0.9,
                       popup = paste(sep="\n", year_data$station_code,":", year_data$address),
                       radius = year_data$x*12/max(year_data$x))
    
  })#observe data points ends
  ###############################Maps ends############################### 
  
  ## to display pie charts
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    pie_all<- data_pie_all%>%
      summarise(SO2=mean(SO2_mg_per_mm3),
                NO2=mean(NO2_mg_per_mm3),
                SO2=mean(SO2_mg_per_mm3),
                O3=mean(O3_mg_per_mm3),
                CO=mean(CO_mg_per_mm3),
                PM10=mean(PM10),
                PM2.5=mean(PM2.5),)
    data <- melt(as.data.table(pie_all, keep.rownames = "Vars"), id.vars = "Vars")  
    
    output$pie1 = renderPlotly(
      data %>%
        plot_ly(title = 'Pollutants Variations across selected months',textposition = 'inside',insidetextfont = list(color = '#FFFFFF')) %>%
        add_pie(
          labels = ~variable,
          values = ~value)
    )
    
  })#observe data points ends
  
  
  ## to display line charts
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$line1 = renderPlotly(
        plot_ly(data_pie_all,x = ~Date,y = ~PM2.5, name="PM2.5", type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)'))%>%
        layout(title = 'Evolution of PM2.5 pollution level over a month',
               textposition = 'inside',
              xaxis = list(title = "Date"),
               yaxis = list (title = "PM2.5 (μg/m3)"))
    )
  })#observe data points ends
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$line2 = renderPlotly(
      plot_ly(data_pie_all,x = ~Date,y = ~PM10, name="PM10", type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)'))%>%
        layout(title = "Evolution of PM10 pollution level over a month",
               textposition = 'inside',
               xaxis = list(title = "Date"),
               yaxis = list (title = "PM10 (μg/m3)"))
    )
  })#observe data points ends
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$line3 = renderPlotly(
        plot_ly(data_pie_all,x = ~Date,y = ~CO_mg_per_mm3, name="CO", type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)'))%>%
        layout(title = "Evolution of CO pollution level over a month",
               textposition = 'inside',
               xaxis = list(title = "Date"),
               yaxis = list (title = "CO (microgram/m3)"))
    )
  })#observe data points ends
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$line4 = renderPlotly(
        plot_ly(data_pie_all,x = ~Date,y = ~O3_mg_per_mm3, name="O3", type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)'))%>%
        layout(title = "Evolution of O3 pollution level over a month",
               textposition = 'inside',
               xaxis = list(title = "Date"),
               yaxis = list (title = "O3 (microgram/m3)"))
    )
  })#observe data points ends
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$line5 = renderPlotly(
        plot_ly(data_pie_all,x = ~Date,y = ~NO2_mg_per_mm3, name="NO2", type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)'))%>%
        layout(title = "Evolution of NO2 pollution level over a month",
               textposition = 'inside',
               xaxis = list(title = "Date"),
               yaxis = list (title = "NO2 (microgram/m3)"))
    )
  })#observe data points ends
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$line6 = renderPlotly(
      plot_ly(data_pie_all,x = ~Date,y = ~SO2_mg_per_mm3, name="SO2", type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)'))%>%
      layout(title = "Evolution of SO2 pollution level over a month",
               textposition = 'inside',
               xaxis = list(title = "Date"),
               yaxis = list (title = "SO2 (microgram/m3)"))
    )
  })#observe data points ends
  
  
  ############Bubble chart
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$circle1 = renderPlotly(
      data_pie_all %>%
        ggplot(aes(x=Date, y=PM2.5, size=PM2.5, fill=AQIH_PM2.5)) +
        geom_point(alpha=0.5, shape=21, color="black") +
        scale_size(range = c(.1, 24), name="PM2.5 AQIH") +
        theme(legend.position="bottom") +
        ylab("PM2.5 Average AQIH") +
        xlab("Monthly Date") +
        theme(legend.position = "right")+
        theme(axis.text.x=element_text(angle=45, colour="#38222a", size=8), 
              axis.text.y=element_text(colour="#38222a", hjust=1, vjust=0.8, size=8),
              axis.title.x=element_text(colour="black", face="bold", size=10),
              axis.title.y=element_text(colour="black", face="bold", size=10))
    )
  })#observe data points ends
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$circle2 = renderPlotly(
      data_pie_all %>%
        ggplot(aes(x=Date, y=PM10, size=PM10, fill=AQIH_PM10)) +
        geom_point(alpha=0.5, shape=21, color="black") +
        scale_size(range = c(.1, 24), name="PM10 AQIH") +
        theme(legend.position="bottom") +
        ylab("PM10 Average AQIH") +
        xlab("Monthly Date") +
        theme(legend.position = "right")+
        theme(axis.text.x=element_text(angle=45, colour="#38222a", size=8), 
              axis.text.y=element_text(colour="#38222a", hjust=1, vjust=0.8, size=8),
              axis.title.x=element_text(colour="black", face="bold", size=10),
              axis.title.y=element_text(colour="black", face="bold", size=10))
    )
  })#observe data points ends
  
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$circle3 = renderPlotly(
      data_pie_all %>%
        ggplot(aes(x=Date, y=CO_mg_per_mm3, size=CO_mg_per_mm3, fill=AQIH_CO)) +
        geom_point(alpha=0.5, shape=21, color="black") +
        scale_size(range = c(.1, 24), name="CO AQIH") +
        theme(legend.position="bottom") +
        ylab("CO Average AQIH") +
        xlab("Monthly Date") +
        theme(legend.position = "right")+
        theme(axis.text.x=element_text(angle=45, colour="#38222a", size=8), 
              axis.text.y=element_text(colour="#38222a", hjust=1, vjust=0.8, size=8),
              axis.title.x=element_text(colour="black", face="bold", size=10),
              axis.title.y=element_text(colour="black", face="bold", size=10))
    )
  })#observe data points ends
  
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$circle4 = renderPlotly(
      data_pie_all %>%
        ggplot(aes(x=Date, y=O3_mg_per_mm3, size=O3_mg_per_mm3, fill=AQIH_O3)) +
        geom_point(alpha=0.5, shape=21, color="black") +
        scale_size(range = c(.1, 24), name="O3 AQIH") +
        theme(legend.position="bottom") +
        ylab("O3 Average AQIH") +
        xlab("Monthly Date") +
        theme(legend.position = "right")+
        theme(axis.text.x=element_text(angle=45, colour="#38222a", size=8), 
              axis.text.y=element_text(colour="#38222a", hjust=1, vjust=0.8, size=8),
              axis.title.x=element_text(colour="black", face="bold", size=10),
              axis.title.y=element_text(colour="black", face="bold", size=10))
    )
  })#observe data points ends
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$circle5 = renderPlotly(
      data_pie_all %>%
        ggplot(aes(x=Date, y=NO2_mg_per_mm3, size=NO2_mg_per_mm3, fill=AQIH_NO2)) +
        geom_point(alpha=0.5, shape=21, color="black") +
        scale_size(range = c(.1, 24), name="NO2 AQIH") +
        theme(legend.position="bottom") +
        ylab("NO2 Average AQIH") +
        xlab("Monthly Date") +
        theme(legend.position = "right")+
        theme(axis.text.x=element_text(angle=45, colour="#38222a", size=8), 
              axis.text.y=element_text(colour="#38222a", hjust=1, vjust=0.8, size=8),
              axis.title.x=element_text(colour="black", face="bold", size=10),
              axis.title.y=element_text(colour="black", face="bold", size=10))
    )
  })#observe data points ends
  
  observe({
    data_pie_all <- air_data[air_data$Year == input$year & air_data$Month == input$month & air_data$Station.code == input$address,]
    output$circle6 = renderPlotly(
      data_pie_all %>%
        ggplot(aes(x=Date, y=SO2_mg_per_mm3, size=SO2_mg_per_mm3, fill=AQIH_SO2)) +
        geom_point(alpha=0.5, shape=21, color="black") +
        scale_size(range = c(.1, 24), name="SO2 AQIH") +
        theme(legend.position="bottom") +
        ylab("SO2 Average AQIH") +
        xlab("Monthly Date") +
        theme(legend.position = "right")+
        theme(axis.text.x=element_text(angle=45, colour="#38222a", size=8), 
              axis.text.y=element_text(colour="#38222a", hjust=1, vjust=0.8, size=8),
              axis.title.x=element_text(colour="black", face="bold", size=10),
              axis.title.y=element_text(colour="black", face="bold", size=10))
    )
  })#observe data points ends
  
  
  ##############bar chart
  observe({
    pie_all<- air_data%>% group_by(Address_Code=air_data$Station.code)%>%
      summarise(SO2=sum(SO2_mg_per_mm3),
                NO2=sum(NO2_mg_per_mm3),
                SO2=sum(SO2_mg_per_mm3),
                O3=sum(O3_mg_per_mm3),
                CO=sum(CO_mg_per_mm3),
                PM10=sum(PM10),
                PM2.5=sum(PM2.5),)
    year_data <- pie_all%>%
      mutate(Total = select(., SO2:PM2.5) %>% rowSums(na.rm = TRUE))
    theme_set(theme_classic())
    
    # Plot
    output$bar1 = renderPlotly(
      year_data%>%
        ggplot(aes(Address_Code, Total))
      + geom_bar(stat="identity", width = 0.5, fill="#d11d5f") + 
        # labs(title="Most and Least polluted Area", 
        #      subtitle="Most and Least polluted Area", 
        #      caption="Most and Least polluted Area") +
        theme(legend.position = "right")+
        theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme(axis.text.x=element_text(angle=45, colour="#38222a", size=8), 
            axis.text.y=element_text(colour="#38222a", hjust=1, vjust=0.8, size=8),
            axis.title.x=element_text(colour="black", face="bold", size=10),
            axis.title.y=element_text(colour="black", face="bold", size=10))+
        ylab("Total Pollutants") +
        xlab("Address Code") 
    )
  })

  
  ############multiple line chart
  observe({
        all_yearly<- air_data%>% group_by(Year=air_data$Year)%>%
          summarise(SO2=mean(SO2_mg_per_mm3),
                    NO2=mean(NO2_mg_per_mm3),
                    SO2=mean(SO2_mg_per_mm3),
                    O3=mean(O3_mg_per_mm3),
                    CO=mean(CO_mg_per_mm3),
                    PM10=mean(PM10),
                    PM2.5=mean(PM2.5),)
 
  
   output$multi1 = renderPlotly(
      ggplot(all_yearly, aes(Year, Pollutants))+
      geom_line(aes(x=Year,y=SO2)) +
      geom_line(aes(x=Year,y=NO2),color="red")+
      geom_line(aes(x=Year,y=CO),color="green")+
      geom_line(aes(x=Year,y=O3),color="orange")+
      geom_line(aes(x=Year,y=PM10),color="blue")+
      geom_line(aes(x=Year,y=PM2.5),color="yellow")+#+theme_ipsum() +
      ggtitle("Air Quality Index variation from 2017 till 2019")+
        theme(legend.position = "right")+
        theme(plot.title = element_text(lineheight=3, face="bold", color="#38222a",size=11)) +
        theme(axis.text.x=element_text(angle=45, colour="#38222a", size=8), 
              axis.text.y=element_text(colour="#38222a", hjust=1, vjust=0.8, size=8),
              axis.title.x=element_text(colour="black", face="bold", size=10),
              axis.title.y=element_text(colour="black", face="bold", size=10))
      
    
     #scale_color_discrete(name = "Pollutants", labels = c("co", "o3"))
   )
   
  
  })
  
}#server function ends


#Run shiny app

shinyApp(ui,server)
