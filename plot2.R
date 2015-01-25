library(dplyr)

#Load datasets
if (!"NEI" %in% ls()) {
  NEI <- readRDS("summarySCC_PM25.rds")
}
if (!"SCC" %in% ls()) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

#Filter dataset by Baltimore and summarize per year
baltimore.data <- 
  NEI %>% 
  filter(fips == "24510")%>% 
  group_by(year) %>% 
  summarise( Total_PM25 = sum(Emissions) )

#Basic plot
png("plot2.png")
with(baltimore.data, {
  plot(year, Total_PM25, main = "Total PM2.5 emission in Baltimore city",xaxt="n",
       xlab="Year", ylab = "Total emission (Tons)")
  lines(year, Total_PM25)
  axis(1,at=c(1999,2002,2005,2008) )
})                                                     
dev.off()