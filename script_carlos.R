setwd("~/Documents/Columbia/Exploratory Data Analysis and Visualization/HW2/EDAV_Proect_NOAA/")

library(RNetCDF)
library(leaflet)
library(raster)
library(xlsx)
library(lubridate)
library(dplyr)

load("floods.RData")
master_table = read.xlsx("GlobalFloodsRecord.xls", 1 )
analyses = read.xlsx("GlobalFloodsRecord.xls", 2)
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
date

      
long_fix= ifelse(long>180,long-360,long)

new = data.frame(phi = as.vector(phi[,,day]), 
                 x = rep(long_fix, 15), 
                 y = rep(lat, times=rep(144,15))) 
coordinates(new) = ~x+y
r2 = rasterFromXYZ(new)
proj4string(r2) <- CRS("+proj=longlat +ellps=WGS84")
spplot(r2)

pal <- colorNumeric("RdYlBu", phi,
                    na.color = "transparent")

leaflet(data = a) %>% addTiles(options=tileOptions(continuousWorld= TRUE)) %>%
  addRasterImage(r2, colors = pal, opacity = 0.5) %>%
  addLegend(pal = pal, values = c(4522, 6054),
            title = paste("Pressure ", date ,sep=""))
%>%
  addCircles(color = "black",lng =~Centroid.X, lat = ~Centroid.Y, weight = ~Affected.sq.km)
  

