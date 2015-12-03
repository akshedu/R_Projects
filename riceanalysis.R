library(data.table)
library(ggplot2)

data <- fread("formattedrice.csv")
data <- data[,V1 := NULL]
data <- data[,Commodity_Name := NULL]
data <- data[,Date := as.Date(Date, format = "%Y-%m-%d")]

avg_monthly <- data[Centre_Name %in% c("MUMBAI", "DELHI"), .(Monthly_Average = mean(Price)),by = .(month(Date), year(Date),Centre_Name)]

avg_monthly <- avg_monthly[,month:=sprintf("%02d",month)]
avg_monthly <- avg_monthly[,year:=as.character(year)]

avg_monthly <- avg_monthly[,Date := as.Date(paste(year,month,1,sep="-"),format = "%Y-%m-%d")]

p <- ggplot(avg_monthly, aes(Date, Monthly_Average, color = Centre_Name)) + geom_point()
