library(data.table)
library(reshape2)
library(ggplot2)

data <- fread("Data_Extract_From_World_Development_Indicators_Data.csv")
colnames(data) <- make.names(colnames(data))
india_gdp <- data[Country.Name == "India" & X.Series.Name %like% "GDP"]
india_gdp <- india_gdp[,Series.Code := NULL]
india_gdp <- india_gdp[,Country.Code := NULL]
india_gdp <- india_gdp[,Country.Name := NULL]

india_gdp <- india_gdp[,lapply(.SD,as.numeric),by = .(X.Series.Name)]
colnames(india_gdp) <- c("Parameter",1990,2000,2006:2015)
india_gdp_m <- melt(india_gdp,"Parameter")

r <- ggplot(india_gdp_m,aes(variable,value,color = Parameter)) + geom_line(aes(group = Parameter)) + geom_point()