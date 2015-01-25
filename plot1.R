library(dplyr)

#Load datasets
if (!"NEI" %in% ls()) {
  NEI <- readRDS("summarySCC_PM25.rds")
}
if (!"SCC" %in% ls()) {
  SCC <- readRDS("Source_Classification_Code.rds")
}

#Summarize dataset per year
yearly.data <- 
  NEI %>% 
  group_by(year) %>% 
  summarise( Total_PM25 = round(sum(Emissions)/1000,2) )

#Basic plot
png("plot1.png")
with(yearly.data, {
     plot(year, Total_PM25, main = "Total US Annual PM 2.5 Emissions",xaxt="n",
          xlab="Year", ylab = "Total emission (Kilotons)")
     lines(year, Total_PM25)
     axis(1,at=c(1999,2002,2005,2008) )
})                                                     
dev.off()