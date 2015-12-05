library(data.table)
library(ggplot2)
library(reshape2)
library(gridExtra)

data <- fread("formattedrice.csv")
data <- data[,V1 := NULL]
data <- data[,Commodity_Name := NULL]
data <- data[,Date := as.Date(Date, format = "%Y-%m-%d")]

avg_monthly <- data[Centre_Name %in% c("MUMBAI", "DELHI"), .(Monthly_Average = mean(Price)),by = .(month(Date), year(Date),Centre_Name)]

avg_monthly <- avg_monthly[,month:=sprintf("%02d",month)]
avg_monthly <- avg_monthly[,year:=as.character(year)]

avg_monthly <- avg_monthly[,Date := as.Date(paste(year,month,1,sep="-"),format = "%Y-%m-%d")]

p <- ggplot(avg_monthly, aes(Date, Monthly_Average, color = Centre_Name)) + geom_point()

avg_data_centre  <- data[Date %between% c("2012-1-1","2013-1-1"), .(Centre_Average = mean(Price,na.rm=TRUE)), by = .(Date)]

q <- ggplot(avg_data_centre, aes(Date, Centre_Average)) + geom_line()

data_rainfall <- fread("rainfall.csv")
data_rainfall <- data.table(data_rainfall,check.names=TRUE)
data_rainfall <- data_rainfall[,Percentage.change.w.r.t.Normal.Rainfall.in.2010 := NULL]
data_rainfall <- data_rainfall[,Percentage.change.w.r.t.Normal.Rainfall.in.2011 := NULL]
data_rainfall <- data_rainfall[,Normal.Rainfall.2010..in.millimetre. := NULL]
data_rainfall <- data_rainfall[,Normal.Rainfall.2011..in.millimetre. := NULL]
data_rainfall <- data_rainfall[,Sub.division := NULL]
data_rainfall <- data_rainfall[,lapply(.SD,as.numeric)]
data_rainfall <- data_rainfall[,lapply(.SD,sum,na.rm=TRUE)]

rainfall <- data.frame(2002:2011,as.numeric(data_rainfall))
colnames(rainfall) <- c("Year","Annual_Total_Rainfall")
rainfall_ts <- ts(rainfall[,2], start=2002)
rainfall_change <- rainfall_ts/lag(rainfall_ts,-1) - 1
rainfall_change <- as.data.frame(as.numeric(rainfall_change))
colnames(rainfall_change) <- "Value"
rainfall_change["Year"] <- 2003:2011
rainfall_change["Type"] <- rep("rainfall_change",9)

avg_yearly <- data[,.(Yearly_Average_Price = mean(Price, na.rm=TRUE)),by = .(year(Date))]
avg_yearly_2002_11 <- avg_yearly[year %between% c(2002,2011)]
setnames(avg_yearly_2002_11,"year","Year")
avg_yearly_ts <- ts(as.numeric(avg_yearly_2002_11[,Yearly_Average_Price]),start=2002)
avg_yearly_change <- avg_yearly_ts/lag(avg_yearly_ts, -1) - 1
avg_yearly_change <- as.data.frame(as.numeric(avg_yearly_change))
colnames(avg_yearly_change) <- "Value"
avg_yearly_change["Year"] <- 2003:2011
avg_yearly_change["Type"] <- rep("avg_yearly_change",9)

change_data <- rbind(rainfall_change,avg_yearly_change)

r <- ggplot(change_data,aes(Year, Value, color = Type)) + geom_line() + geom_point()

rainfall <- as.data.table(rainfall)
rainfall_norm <- rainfall[,Annual_Total_Rainfall := scale(Annual_Total_Rainfall,center = min(Annual_Total_Rainfall),scale = max(Annual_Total_Rainfall)-min(Annual_Total_Rainfall))]

avg_yearly_2002_11 <- as.data.table(avg_yearly_2002_11)
avg_yearly_norm <- avg_yearly_2002_11[, Yearly_Average_Price := scale(Yearly_Average_Price,center = min(Yearly_Average_Price),scale = max(Yearly_Average_Price)-min(Yearly_Average_Price))]

norm_change_data <- merge(rainfall_norm,avg_yearly_norm,by = "Year")

norm_change_data_m <- melt(norm_change_data,id.vars="Year")

s <- ggplot(norm_change_data_m,aes(Year,value,color = variable)) + geom_line() + geom_point()
