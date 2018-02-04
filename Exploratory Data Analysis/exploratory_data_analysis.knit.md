---
title: "Exploratory Anamytics"
output:
  pdf_document: default
  html_document: default
---


```r
library(ggplot2)
library(plyr)
setwd("H:/Data Science Johns Hopkins/exploratory-data-analysis/Week4/data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
```

## Including Plots

You can also embed plots, for example:


```r
totEmissions <- aggregate(Emissions ~ year,NEI, sum)
barplot(
  totEmissions$Emissions,
  names.arg=totEmissions$year,
  xlab="Year",
  ylab="PM2.5 Emissions",
  col= "red",
  main="Total PM2.5 Emissions in U.S"
)
```

![](exploratory_data_analysis_files/figure-latex/unnamed-chunk-759-1.pdf)<!-- --> 


```r
totEmBaltimore <- aggregate(Emissions ~ year,NEI[NEI$fips=="24510",], sum)
barplot(
  totEmBaltimore$Emissions,
  names.arg=totEmBaltimore$year,
  xlab="Year",
  ylab="PM2.5 Emissions",
  col= "red",
  main="Total PM2.5 Emissions in Baltimore"
)
```

![](exploratory_data_analysis_files/figure-latex/unnamed-chunk-760-1.pdf)<!-- --> 


```r
totEmBaltimore <- aggregate(Emissions ~ year + type,NEI[NEI$fips=="24510",], sum)
ggplot(data=totEmBaltimore,
       aes(x=year, y=Emissions, colour=type)) +
  geom_line(size=1) + 
   geom_point(size=3) +
  ggtitle("Total PM2.5 Emissions by Year and Type in Baltimore") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

![](exploratory_data_analysis_files/figure-latex/unnamed-chunk-761-1.pdf)<!-- --> 

```r
ggplot(totEmBaltimore,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity") +
  facet_grid(.~type) + 
  labs(x="Year", y="Total Emission") + 
  labs(title=expression("Total PM2.5 Emissions by Year and Type in Baltimore"))+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

![](exploratory_data_analysis_files/figure-latex/unnamed-chunk-762-1.pdf)<!-- --> 

#### Find all coal combustion-related sources

```r
coalRow <- grep("coal", SCC$EI.Sector, ignore.case=TRUE)
coalRelated <-SCC[coalRow,"SCC"]
emFromCoal<-NEI[NEI$SCC %in% coalRelated,]
totEmCoal <- aggregate(Emissions ~ year ,emFromCoal, sum)

ggplot(data=totEmCoal,
       aes(x=year, y=Emissions)) +
  geom_line(size=1) + 
  geom_point(size=3) +
  ggtitle("Total Coal Emissions by Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

![](exploratory_data_analysis_files/figure-latex/unnamed-chunk-763-1.pdf)<!-- --> 


```r
vehicleRow<- grep("Vehicle", SCC$EI.Sector, ignore.case = TRUE)
vehicleRelated<-SCC[vehicleRow,"SCC"]
emFromBaltVehicles<-NEI[NEI$SCC %in% vehicleRelated & NEI$fips=="24510",]
totEmBaltVehicles <- aggregate(Emissions ~ year ,emFromBaltVehicles, sum)

ggplot(data=totEmBaltVehicles,
       aes(x=year, y=Emissions)) +
  geom_line(size=1) + 
  geom_point(size=3) +
  ggtitle("Total Vehicles Emissions by Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

![](exploratory_data_analysis_files/figure-latex/unnamed-chunk-764-1.pdf)<!-- --> 



```r
emFromBaltLAVehicles<- NEI[NEI$SCC %in% vehicleRelated & NEI$fips %in% c("24510","06037"),]
totEmBaltLAVehicles <- aggregate(Emissions ~ year + fips ,emFromBaltLAVehicles, sum)
totEmBaltLAVehicles <- rename(totEmBaltLAVehicles, c("fips"="City"))
totEmBaltLAVehicles$City <- factor(totEmBaltLAVehicles$City, levels=c( "24510","06037"), labels=c( "Baltimore","Los Angeles"))
ggplot(data=totEmBaltLAVehicles,
       aes(x=year, y=Emissions,col=City)) +
  geom_line(size=1) + 
  geom_point(size=3) +
  ggtitle("Total Vehicles Emissions by Year in Baltiimore & LA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

![](exploratory_data_analysis_files/figure-latex/unnamed-chunk-765-1.pdf)<!-- --> 

