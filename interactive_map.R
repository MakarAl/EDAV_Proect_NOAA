library(shiny)
library(leaflet)
library(RColorBrewer)
library(raster)
library(lubridate)
library(dplyr)
library(maptools)
library(ggplot2)
library(ggthemes)
library(foreach)

setwd("~/Documents/Columbia/Exploratory Data Analysis and Visualization/HW2/EDAV_Proect_NOAA/")

coastline = readShapeLines("ne_110m_coastline.shp")
countries = readShapeLines("ne_110m_admin_0_boundary_lines_land.shp")

load("floods.RData")
load("pressure.RData")

floods = floods %>% mutate(severity = as.numeric(as.character(severity)),
                           began = ymd(began),
                           ended = ymd(ended),
                           date_interval = interval(began, ended),
                           began_year = year(began),
                           began_month = month(began))


orig_date = ymd("1985-01-01")
day = 1
date = orig_date + days(day)


get_date = function(day) orig_date + days(day)
########
date_map = data.frame(index= 1:dim(phi)[3], date = get_date(1:dim(phi)[3])) %>%
  mutate(year = year(date))

indexes = unlist(date_map %>%  filter(year=="2005") %>% dplyr::select(index))


phi_df= foreach(i=indexes, .combine='rbind') %do% 
  as.vector(phi[,,i])

pca_1 = prcomp(phi_df,center = TRUE,scale. = TRUE)$rotation[,1]



########
long_fix= ifelse(long>180,long-360,long)


pal_severity <- colorNumeric("YlOrRd", floods$severity,
                    na.color = "transparent")

ui <- bootstrapPage(
  tags$head(
    includeCSS("styles.css")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE,
                dateInput("date", "Pick a date", value= get_date(1),min = get_date(1), max = get_date(dim(phi)[3])),
                plotOutput("floodsMonth", height = 200),
                plotOutput("magnitudeMonth", height = 200)
  ),
  absolutePanel(top = 10, left = 25,headerPanel("Pressure"))
)

server <- function(input, output, session) {
  # Reactive expression to subset data
  filteredData <- reactive({
    selected_date = ymd(input$date)
    print(selected_date)
    slice = as.numeric(selected_date - orig_date) 
    new = data.frame(phi = as.vector(phi[,,slice]), 
                     x = rep(long_fix, 15), 
                     y = rep(lat, times=rep(144,15))) 
    coordinates(new) = ~x+y
    r2 = rasterFromXYZ(new)
    proj4string(r2) <- CRS("+proj=longlat +ellps=WGS84")
    
    p = rasterToPolygons(r2)
    filtered_floods = floods[selected_date %within% floods$date_interval,] %>% filter(!is.na(severity))
    print(dim(filtered_floods))
    list('poly' = p, 'floods' = filtered_floods)
    
  })
  
  output$floodsMonth <- renderPlot({
    ggplot(data = floods %>% filter(!is.na(began_month))) + geom_bar(aes(x=factor(began_month))) + 
      theme_fivethirtyeight()
    
  })
  output$magnitudeMonth <- renderPlot({
    magnitude = floods %>% group_by(began_month) %>% summarize(magnitude = mean(magnitude))
    ggplot(data = magnitude %>% filter(!is.na(began_month))) + 
      geom_line(aes(x=began_month, y = magnitude)) + 
      scale_x_continuous(breaks=c(1:12))+
      theme_fivethirtyeight()
    
  })
  output$map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    leaflet() %>%
      addTiles(options=tileOptions(continuousWorld= TRUE))%>%
      setView(0,42, 2)
      # fitBounds(-177,-35, 180, 35)
      
  })
  
  # Incremental changes to the map
  observe({
    pal <- colorNumeric(rev(brewer.pal(10, "RdYlBu")), filteredData()$poly$layer,
                        na.color = "transparent")

    
    leafletProxy("map", data= filteredData()$poly) %>%
      # addRasterImage(filteredData(), colors = pal, opacity = 0.8) 
      clearControls() %>%
      clearShapes() %>%
      addPolygons(color = ~pal(layer),stroke = FALSE, fillOpacity = 0.6) %>%
      addLegend(pal = pal, 
                values = filteredData()$poly$layer,
                position = "bottomright",
                title = "Pressure") %>%
      addCircles(lng =~centroid.x, lat = ~centroid.y, stroke = FALSE,radius = ~2000*magnitude^2, 
                 fillColor= ~pal_severity(severity),fillOpacity = .7 ,data = filteredData()$floods) %>%
      addPolylines(data = coastline, color = "black", weight = .5) %>%
      addPolylines(data = countries, color = "black", weight = .5)

  })
}

shinyApp(ui, server)
