library(cowplot)
library(sf)
library(ggplot2)
library(dplyr)
library(ggforce)
library(scales)
library(glue)

##load data set

bikeshare_station <- read_csv("https://raw.githubusercontent.com/anneumann1/correlaid-hackathon-spatialdata/main/data/bikeshare_stations_hh.csv")
options( "encoding" = "UTF-8" )
#bikeshare_trips_hh <- read.csv("https://raw.githubusercontent.com/anneumann1/correlaid-hackathon-spatialdata/main/data/bikeshare_trips_hh.csv", encoding="UTF-8")


land_use<-read_rds("land_use.rds")
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

##Create map

hh<-
  ggplot()+
  #  geom_sf(subset(land_use, !is.na(cat)),mapping=aes(col=cat,fill=cat))+
  geom_sf(land_use,mapping=aes(fill=cat,col=cat))+
  geom_point(bikeshare_station,mapping=aes(x=lon,y=lat),col="#ffffff",shape = 4,size=2)+
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
        plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="white",size=15),
        plot.title = element_text(color="grey85",hjust=.5, face="italic",size=20))

##Projections
bikeshare_station<-bikeshare_station%>%
  st_as_sf(coords = c("lon", "lat"))

bikeshare_station<-bikeshare_station  %>% st_set_crs(4326) %>% st_transform(4326)

##Create buffers around Polygons using metric system
buffers<-st_transform(land_use, CRS("+init=epsg:3068 +datum=WGS84 +units=m"))

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

# Add mini-chart
hg<-
  ggplot(stationen)+
  geom_bar(mapping=aes(reorder(cat,-freq),freq,fill=cat),stat="identity",width=.25)+
  scale_fill_manual(name="Category:",values=c("green space"="seagreen4","residential" ="seashell4", "pedestrian"="orange","business"="deeppink3", "agriculture"="red4"),guide="none")+
 # geom_text(mapping=aes(reorder(cat,-freq),freq,label = cat),colour = "#00101f", size = 4,hjust = 2.5)+
  geom_text(mapping=aes(reorder(cat,-freq),freq,label = scales::percent(freq)),colour = "white", size = 4,hjust = -.5)+
  labs(subtitle = "and where they are located")+
  theme(plot.background = element_rect("#00101f"),
        legend.background = element_rect("#00101f"),
        axis.title = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect("#00101f"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.position = "none",
        text=element_text(family="Times New Roman", face="bold", size=21),
        plot.subtitle=element_text(size=15, hjust=0.5, face="italic", color="white"))+
  coord_flip()

##Put everything together
library("patchwork")
hh+inset_element(hg, left = .3, bottom = .7, right = .1, top =.98)
