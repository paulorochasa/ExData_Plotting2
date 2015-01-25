library(dplyr)
library(ggplot2)

#Load datasets
if (!"NEI" %in% ls()) {
  NEI <- readRDS("summarySCC_PM25.rds")
}
if (!"SCC" %in% ls()) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

#Filter in SCC only coal combustion-related sources
Coal<-
  SCC %>% 
  filter(grepl("Coal",Short.Name)| grepl("Coal",EI.Sector)) %>%
  select(SCC)
  
#Filter in NEI by SCC coal sources and summarize per year
Coal.Emissions <- 
  NEI %>% 
  filter(SCC %in% Coal$SCC) %>% 
  group_by(year) %>% 
  summarise( Total_PM25 = round(sum(Emissions)/1000,2) )

#qplot with points and a regresion line 
png("plot4.png")
p <- qplot(year, Total_PM25,data = Coal.Emissions , geom=c("point", "smooth"),method="lm") +
  ggtitle("Total PM2.5 emission from coal combustion-related sources") +
  ylab("Total emission (Kilotons)") +
  xlab("Year") +
  scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())
print(p)                                       
dev.off()