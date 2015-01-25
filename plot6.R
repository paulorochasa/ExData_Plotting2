#Imported a function to distribute multiple ggplots in a same graphics device

library(dplyr)
library(ggplot2)
source("multiplot.R")

#Load datasets
if (!"NEI" %in% ls()) {
  NEI <- readRDS("summarySCC_PM25.rds")
}
if (!"SCC" %in% ls()) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

#Filter in SCC only motor vehicle sources
MotorVehicle<-
  SCC %>% 
  filter(grepl("[Vv]ehicle",SCC.Level.Two) | grepl("[Vv]ehicle",SCC.Level.Three) |grepl("[Vv]ehicle",SCC.Level.Four)) %>%
  select(SCC)

#Filter in NEI by Baltimore emissions related to motor vehicle sources
#New column with the U.S. County 
#New column with the sum of emissions to perform the evoltion calculation
Baltimore.NEI <- 
  NEI %>% 
  filter(fips == "24510" & SCC %in% MotorVehicle$SCC ) %>% 
  mutate(USCounty = "Baltimore City") %>%
  mutate(Total_Emission = sum(Emissions))

#Filter in NEI by Los Angeles emissions related to motor vehicle sources
#New column with the sum of emissions to perform the evoltion calculation
LA.NEI <- 
  NEI %>% 
  filter (fips == "06037" & SCC %in% MotorVehicle$SCC) %>% 
  mutate(USCounty = "Los Angeles County") %>%
  mutate(Total_Emission = sum(Emissions))
  
#Merge Baltimore and LA dataset to summarize by year and US County
#New column with the percentage of evolution of total emissions
Comparison.MotorVehicle.Emissions <- 
  rbind(Baltimore.NEI,LA.NEI)%>% 
  group_by(year,USCounty) %>% 
  summarize(
      Total_PM25 = sum(Emissions),
      Total_Emission = unique(Total_Emission)
  ) %>% 
  mutate(Perc_PM25_change = round(Total_PM25 *100 / Total_Emission ,2))

#Two plots with diferent perpectives
# 1. ggplot with the evolution of the growth of total emissions in percent per US County (in percentage)
# 2. ggplot with the total emission in tons per fips name

png("plot6.png",width = 1000, height = 350, units = "px")
p1<-ggplot(data=Comparison.MotorVehicle.Emissions, aes(x=year, y=Perc_PM25_change, colour=USCounty))+ 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = paste0(round(Perc_PM25_change),"%")), size = 4, vjust= -0.5, hjust=0.3)+
  ylab("Total emission evolution (%)") + 
  xlab("Year")+
  ggtitle("Evolution PM2.5 emission by motor vehicles") +
  scale_colour_hue(name="U.S. Conty") +
  scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
  scale_y_continuous(breaks=c(10,20,30,40,50)) +
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())

p2<-ggplot(data=Comparison.MotorVehicle.Emissions, aes(x=year, y=Total_PM25 , colour=USCounty))+ 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(Total_PM25 )), size = 4, vjust= -0.3, hjust=0.4)+
  ylab("Total emission (Tons)") + 
  xlab("Year")+
  ggtitle("Total PM2.5 emission by motor vehicles") +
  scale_colour_hue(name="U.S. Conty") +
  scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())

multiplot(p1, p2, cols=2)
dev.off()