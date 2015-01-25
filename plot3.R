library(dplyr)
library(ggplot2)

#Load datasets
if (!"NEI" %in% ls()) {
  NEI <- readRDS("summarySCC_PM25.rds")
}
if (!"SCC" %in% ls()) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

#Filter dataset by Baltimore and summarize per year and type
baltimore.data <- 
  NEI %>% 
  filter(fips == "24510") %>% 
  group_by(year, type) %>% 
  summarise( Total_PM25 = sum(Emissions))


#qplot with facets per type
#points with regresion line 
png("plot3.png")
p <- qplot(year, Total_PM25,data = baltimore.data,facets = .~type) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE) +
  ggtitle("Total PM2.5 emission in Baltimore city by source type") +
  ylab("Total emission (Tons)") +
  xlab("Year") +
  ylim(0,2500) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())
print(p)                                       
dev.off()