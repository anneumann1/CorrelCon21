library(tidyverse)
library(cowplot)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(ggforce)
library(scales)
library(glue)
library("patchwork")
library(extrafont)
font_import()
loadfonts(device="win")


# LOAD DATA --------------------------------------------------------------------
options( "encoding" = "UTF-8" )
bikeshare_station <- read_csv("datasets/bikeshare_stations_hh.csv")
land_use <- read_rds("datasets/land_use.Rds")
shortest_cycle_paths <- readRDS("datasets/shortest_cycle_paths.rds")

# PREPARE FOR PLOTTING ----------------------------------------------------
# set right projection
land_use <- st_set_crs(land_use, 4326)

# clean up landuse categories
land_use$cat <- case_when(
  str_detect(land_use$fclass, "allotments|forest|grass|heath|meadow|nature_reserve|park|recreation_ground|scrub|park|orchard|cemetery") ~ "green space",
  str_detect(land_use$fclass, "retail|commercial|industrial|quarry") ~ "business",
  str_detect(land_use$fclass, "residential|military") ~ "residential",
  str_detect(land_use$fclass, "farmland|farmyard|vineyard") ~ "agriculture")  

# sample path data because otherwise it will crash your computer when plotting
set.seed(123)
shortest_cycle_paths_sampled <- shortest_cycle_paths %>% 
  slice_sample(prop=0.08) # we only take 10%


# CREATE MAP ------------------------------------------------------------------

hamburg_map <-
  ggplot()+
  geom_sf(land_use,
          mapping=aes(fill=cat,col=cat),
          alpha = .4)+
  geom_point(bikeshare_station,
             mapping=aes(x=lon,y=lat),
             col="white",
             shape = 4,size=2,
             alpha=5)+
  geom_sf(shortest_cycle_paths_sampled,
          mapping=aes(geometry=geom_bike),
          col=alpha("white",5))+
  labs(title = "HAMBURG´s",
       subtitle = "most prominent routes\n for shared bike users")+
  scale_fill_manual(name="Category:",values=c("green space"="seagreen4",
                                              "residential" ="seashell4", 
                                              "agriculture"="darkred",
                                              "pedestrian"="orange",
                                              "business"="darkorchid4"))+
  scale_color_manual(values=c("green space"="seagreen4",
                              "residential" ="seashell4", 
                              "agriculture"="darkred",
                              "business"="darkorchid4"), guide = "none")+
theme(
plot.background = element_rect("#00101f"),
panel.grid.major = element_blank(),
panel.background = element_rect("#00101f"),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank(),
axis.title.y = element_blank(),
text=element_text(family="Times New Roman", face="bold", size=21),
plot.subtitle=element_text(size=12, hjust=0.5, face="italic", color="white"),
plot.title = element_text(color="grey85",hjust=.5, face="italic"),
legend.background = element_rect("#00101f"),
legend.text = element_text(color = "white",face="italic"),
legend.title = element_text(color = "white",face="italic"),
legend.position = "bottom")

# CREATE BAR CHART -------------------------------------------------------------
# prepare stations data for matching to landuse --------------------------------
# Convert stations dataframe to sf-object (spatial object)
bikeshare_station <- bikeshare_station %>%
  st_as_sf(coords = c("lon", "lat")) %>% # specify coordinate columns
  st_set_crs(4326) %>%  # set correct projection
  st_transform(4326) # transform to sf using correct projection

# Create buffers around Polygons using metric system
## makes it more likely for a station to intersect with a landuse polygon,
## slightly hacky soluation
buffers <- st_transform(land_use, CRS("+init=epsg:3068 +datum=WGS84 +units=m"))

# Add 12meter buffer
buffers <- st_buffer(buffers, 12)

# Again, set right CRS
buffers <- buffers %>% st_transform(4326) 

# Find out which bike-station is located in which landuse polygon
intersections <- st_join(bikeshare_station,buffers)

# Count the number of stations per landuse type
station_landuse_count <- intersections %>%
  group_by(cat)%>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# rename NAs
# we are missing quite a few polygons in Hamburg city center, make them
# pedestrian
station_landuse_count$cat[is.na(station_landuse_count$cat)] <- "pedestrian"

# CREATE MAP WITH BARCHAPRT ---------------------------------------------------
station_count_barchart <-
  ggplot(station_landuse_count) +
  geom_bar(mapping=aes(reorder(cat,-freq),freq,fill=cat),
           stat="identity",
           width=.25,
           alpha=.4)+
  scale_fill_manual(name="Category:",
                    values=c("green space"="seagreen4",
                             "residential" ="seashell4", 
                             "pedestrian"="orange",
                             "business"="darkorchid4", 
                             "agriculture"="darkred"),
                    guide="none")+
  geom_text(mapping=aes(reorder(cat,-freq),freq,label = scales::percent(freq)),
            colour = "white", 
            size = 4,
            hjust = -.5)+
  labs(subtitle = "where the bike-stations are located")+
  theme(plot.background = element_rect("#00101f"),
        plot.subtitle=element_text(size=12, hjust=0.5, face="italic", color="white"),
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
        text=element_text(family="Times New Roman", face="bold", size=21))+
coord_flip()

# COMBINE BOTH MAPS -----------------------------------------------

# set the box where the barplot should be located
hamburg_map+inset_element(station_count_barchart, 
                          left = .3, 
                          bottom = .7, 
                          right = .1, 
                          top =.98)
