library(lubridate)
library(readxl)
llibrary(dplyr)
library(plotly)
flood <- read_excel("GlobalFloodsRecord.xls", "MasterTable")
flood1 <- flood[1:4319,]
nalist = which(is.na(flood1$Began))
flood <- flood1[-nalist,]
flood$Began <- as.numeric(as.character(flood$Began)) + as.Date('1899-12-30')
flood$Ended <- as.numeric(as.character(flood$Ended)) + as.Date('1899-12-30')
flood1 <- data.frame(Began = flood$Began, Ended = flood$Ended, Year = year(flood$Began),
                     M6 = flood$`M>6`, M4 = flood$`M>4`)
df6 <- aggregate(flood1$M6, by=list(flood1$Year), FUN="sum")
df4 <- aggregate(flood1$M4, by=list(flood1$Year), FUN="sum")
df0 <- count(flood1, Year)
df_count <- data.frame(Year=df6$Group.1, `M>6`=df6$x, `M>4`=df4$x, Total=df0$n)
p <- plot_ly(df_count, x=Year, y=Total, name="Total") %>%
  add_trace(y=`M.6`,name="M>6",yaxis="") %>%
  add_trace(y=`M.4`,name="M>4",yaxis="") %>%
  layout(title='France Flood Events (1990 - 2015)')
p