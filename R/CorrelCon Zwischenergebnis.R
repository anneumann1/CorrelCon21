library(cowplot)
library(sf)
library(ggplot2)
library(dplyr)
library(ggforce)
library(scales)
library(glue)
library(stringr)

##load data set

bikeshare_station <- read.csv("https://raw.githubusercontent.com/anneumann1/correlaid-hackathon-spatialdata/main/data/bikeshare_stations_hh.csv")
options( "encoding" = "UTF-8" )
bikeshare_trips_hh <- read.csv("https://raw.githubusercontent.com/anneumann1/correlaid-hackathon-spatialdata/main/data/bikeshare_trips_hh.csv", encoding="UTF-8")
paths <- read.csv("https://raw.githubusercontent.com/anneumann1/correlaid-hackathon-spatialdata/main/data/bikeshare_stations_hh.csv")


land_use<- readRDS("/home/jaezzy/Documents/Projects/CorrelCon21/data/land_use.Rds") # wie einlesen?
land_use<- st_set_crs(land_use, 4326)

##new categories
land_use$cat<-case_when(
  str_detect(land_use$fclass, "allotments|forest|grass|heath|meadow|nature_reserve|park|recreation_ground|scrub|park|orchard|cemetery") ~ "green space",
  str_detect(land_use$fclass, "retail|commercial|industrial|quarry") ~ "business",
  str_detect(land_use$fclass, "residential|military") ~ "residential",
  str_detect(land_use$fclass, "farmland|farmyard|vineyard") ~ "agriculture")  

library(extrafont)
font_import()
loadfonts(device="win")

##Create map --------------------------------
hh<-
  ggplot()+
  #  geom_sf(subset(land_use, !is.na(cat)),mapping=aes(col=cat,fill=cat))+
  geom_sf(land_use,mapping=aes(fill=cat,col=cat))+
  #geom_point(bikeshare_station,mapping=aes(x=lon,y=lat),col="#FF0000",shape = 4,size=2)+
  geom_sf(lines_between_stations_sampled,
          mapping=aes(fill=start_name),col="#ffffff", size=0.1) +
  labs(title = "HAMBURGs",subtitle = "bike-station locations")+
  scale_fill_manual(name="Category:",values=c("green space"="seagreen4","residential" ="seashell4", "agriculture"="red4","pedestrian"="yellow","business"="deeppink3"))+
  scale_color_manual(values=c("green space"="seagreen4","residential" ="seashell4", "agriculture"="red4","business"="deeppink3"), guide = "none")+
  theme(plot.background = element_rect("#00101f"),
        legend.background = element_rect("#00101f"),
        axis.title = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("#00101f"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        panel.border = element_rect(colour = "grey85", fill=NA, size=8),
        legend.position = "bottom",
        text=element_text(family="Times New Roman", face="bold", size=21),
        plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="white"),
        plot.title = element_text(color="grey85",hjust=.5, face="italic"))
hh
##Create buffers around Polygons using metric system
buffers<-st_transform(land_use, CRS("+init=epsg:3068 +datum=WGS84 +units=m"))
bikeshare_station<-bikeshare_station%>%
  st_as_sf(coords = c("lon", "lat"))

##Add 12meter buffer
buffers<- st_buffer(buffers, 12)

buffers<- buffers%>% st_transform(4326) 


##Find out which bike-station is located where
intersectionsii<-st_join(bikeshare_station,buffers)


stationen<-intersectionsii%>%
  group_by(cat)%>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))

##rename NAs
stationen$cat[is.na(stationen$cat)] <- "pedestrian"


