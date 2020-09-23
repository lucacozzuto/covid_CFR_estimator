library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(raster)
library(tidyverse)

spain <- getData('GADM', country='Spain', level=1)
spain$name = spain$NAME_1
ccaa <- map_data(spain)

ccaa_covid<-read.table("ccaa.txt", sep="\t", header=TRUE)
ccaa_data <- left_join(ccaa,ccaa_covid, by.y="CCAA", by.x="region")

png("espana_uci_increase.png", width=1000)
ggplot(data = ccaa_data) + 
  ggtitle("Increase of ICU beds since 20/08/20") +
  geom_polygon(aes(x = long, y = lat, fill = fold_increase, group = group), color = "white") + 
  coord_fixed(1.3) +
  scale_fill_continuous(low="#eaecf8",high="#4884b8") +
  theme_classic()
dev.off()
  
png("espana_uci_mln.png", width=1000)
ggplot(data = ccaa_data) + 
  ggtitle("ICU beds per million of inhabitants with Covid19 patients (15/09/20)") +
  geom_polygon(aes(x = long, y = lat, fill = UCI_mln, group = group), color = "white") + 
  coord_fixed(1.3) +
  scale_fill_continuous(limits=c(0, max(ccaa_covid$UCI_mln)), low="white",high="red") +
  theme_void()
dev.off()




