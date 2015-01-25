library(dplyr)
library(ggplot2)

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

#Filter in NEI by Baltimore related with SCC motor vehicle sources and summarize per year
Baltimore.MotorVehicle.Emissions <- 
  NEI %>%
  filter(fips == "24510" & SCC %in% MotorVehicle$SCC ) %>% 
  group_by(year) %>% 
  summarise(Total_PM25 = sum(Emissions))

#qplot with a line and point values
png("plot5.png")
p <- qplot(year, Total_PM25,data = Baltimore.MotorVehicle.Emissions, geom="line") +
  ggtitle("Total PM2.5 emission by motor vehicles in Baltimore city") +
  ylab("Total emission (Tons)") +
  xlab("Year") +
  scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()) + 
  geom_point(size = 3)+
  geom_text(aes(label = round(Total_PM25)), size = 4, vjust= -0.5, hjust=-0.1)
print(p)                                       
dev.off()