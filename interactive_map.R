library(shiny)
library(leaflet)
library(RColorBrewer)
library(RNetCDF)
library(raster)
library(xlsx)
library(lubridate)
library(dplyr)
library(maptools)

setwd("~/Documents/Columbia/Exploratory Data Analysis and Visualization/HW2/EDAV_Proect_NOAA/")

coastline = readShapeLines("ne_110m_coastline.shp")
countries = readShapeLines("ne_110m_admin_0_boundary_lines_land.shp")

load("floods.RData")
floods = floods %>% mutate(severity = as.numeric(as.character(severity)),
                           date_interval = interval(ymd(began),ymd(ended)))
nc = open.nc("NOAA_Daily_phi_500mb.nc")

phi = var.get.nc(nc,"phi")

## Remove days before 1985-01-01
# phi = phi[,,13515:dim(phi)[3]]

lat =  var.get.nc(nc,"Y")
long =  var.get.nc(nc,"X")
time =  var.get.nc(nc,"T")
orig_date = ymd("1948-01-01")
day = 13515
date = orig_date + days(day)


get_date = function(day) orig_date + days(day)

long_fix= ifelse(long>180,long-360,long)


pal_severity <- colorNumeric("YlOrRd", floods$severity,
                    na.color = "transparent")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                dateInput("date", "Pick a date", value= get_date(13515),min = get_date(13515), max = get_date(dim(phi)[3]))
  ),
  absolutePanel(top = 10, left = 25,headerPanel("Pressure"))
)

server <- function(input, output, session) {
  # Reactive expression to subset data
  filteredData <- reactive({
    selected_date = ymd(input$date)
    slice = as.numeric(selected_date - orig_date) 
    new = data.frame(phi = as.vector(phi[,,slice]), 
                     x = rep(long_fix, 15), 
                     y = rep(lat, times=rep(144,15))) 
    coordinates(new) = ~x+y
    r2 = rasterFromXYZ(new)
    proj4string(r2) <- CRS("+proj=longlat +ellps=WGS84")
    
    p = rasterToPolygons(r2)
    
    filtered_floods = floods[selected_date %within% floods$date_interval,]
    list('poly' = p, 'floods' = filtered_floods)
    
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
