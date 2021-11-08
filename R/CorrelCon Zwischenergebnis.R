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
  geom_sf(lines_between_stations_sampled %>% select(-geometry),
          mapping=aes(fill=start),col="#ffffff", size=0.1) +
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


# LINE CODES  ------------
library(stplanr)
library(sp)

# remove loop trips (not needed for bike infrastructure)
bike_inter <- bikeshare_trips_hh %>%
  filter(start_rental_zone_hal_id != end_rental_zone_hal_id) %>%
  dplyr::group_by(start_rental_zone_hal_id, end_rental_zone_hal_id) %>%
  dplyr::summarise(trip_count = n())
glimpse(bike_inter)
# stations as spatial object -------------------------------------------------------------------------
stations_coord <- bikeshare_station
coordinates(stations_coord) <- ~lon+lat
glimpse(stations_coord)
class(stations_coord)
# stations_xy <- SpatialPointsDataFrame(coords = stations_coord[,c("lon", "lat")], data = stations,
#                                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# create straight connection
lines_between_stations = od2line(bike_inter, stations_coord)

# convert to sf (newer format)
lines_between_stations <- st_as_sf(lines_between_stations)

lines_between_stations <- lines_between_stations %>% 
  left_join(bikeshare_station %>% dplyr::select(station_id, start_name=name),
            by=c("start_rental_zone_hal_id"="station_id")) %>% 
  left_join(bikeshare_station %>% dplyr::select(station_id, end_name=name),
            by=c("end_rental_zone_hal_id"="station_id"))

lines_between_stations <- st_set_crs(lines_between_stations, 4326)

# cyclestreets api ----
library(cyclestreets)
api_cycle <- Sys.getenv("CYCLESTREETS")

# use cyclestreets.net API to get shortest bicycle route for each station-pair
# source: https://geocompr.robinlovelace.net/transport.html
routes <- line2route(
  lines_between_stations,
  route_fun = stplanr::route_cyclestreets,
  pat = api_cycle
)
lines_between_stations$geom_bike <- st_geometry(routes)
saveRDS(lines_between_stations, "/home/jaezzy/Documents/Projects/CorrelCon21/data/shortest_cycle_paths.rds")

lines_between_stations_sampled <- routes %>% 
  filter(length > 3000)
  


# sample ------
lines_between_stations_sampled <- lines_between_stations %>% 
  slice_sample(prop=0.2)

glimpse(lines_between_stations)
class(lines_between_stations)
saveRDS(lines_between_stations, "/home/jaezzy/Documents/Projects/CorrelCon21/data/lines_between_stations.rds")
