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

png("espana_uci.png")
ggplot(data = ccaa_data) + 
  ggtitle("Increase of ICUs since 20/08/20") +
  geom_polygon(aes(x = long, y = lat, fill = fold_increase, group = group), color = "white") + 
  coord_fixed(1.3) +
  scale_fill_continuous(low="#eaecf8",high="#4884b8") +
  theme_classic()
dev.off()
  

