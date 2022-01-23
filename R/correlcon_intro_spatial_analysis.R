library("tidyverse")
library("cowplot")
library("sf")
library("sp")
library("ggplot2")
library("dplyr")
library("ggforce")
library("scales")
library("glue")
library("patchwork")
library("extrafont")
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
  str_detect(land_use$fclass, "allotments|forest|grass|heath|meadow|nature_reserve|park|recreation_ground|scrub|park|orchard|cemetery|farmland|farmyard|vineyard") ~ "green space",
  str_detect(land_use$fclass, "retail|commercial|industrial|quarry") ~ "business",
  str_detect(land_use$fclass, "residential|military") ~ "residential")  

# sample path data because otherwise it will crash your computer when plotting
set.seed(123)
shortest_cycle_paths_sampled <- shortest_cycle_paths %>% 
  slice_sample(prop=0.08) # we only take 10%


# CREATE MAP ------------------------------------------------------------------

hamburg_map <-
ggplot()+
geom_sf(land_use,mapping=aes(fill=cat,col=cat),alpha = .4)+
geom_point(bikeshare_station,mapping=aes(x=lon,y=lat),col="white",shape = 4,size=2,alpha=5)+
geom_sf(shortest_cycle_paths_sampled ,mapping=aes(geometry=geom_bike),col=alpha("white",5))+
labs(title = "\nHAMBURG´s",subtitle = "most prominent routes for shared bike users",caption=glue("Andreas Neumann & Jasmin Classen"))+
scale_fill_manual(name="Category:",values=c("green space"="seagreen4","residential" ="seashell4","business"="darkorchid4"),guide="none")+
scale_color_manual(values=c("green space"="seagreen4","residential" = "seashell4","business"="darkorchid4"), guide = "none")+
theme(
plot.background = element_rect("#00101f"),
panel.grid.major = element_blank(),
panel.background = element_rect("#00101f"),
axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank(),
axis.title.y = element_blank(),
text=element_text(family="Times New Roman", face="bold", size=21, color="white"),
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
station_landuse_count$cat[is.na(station_landuse_count$cat)] <- "unclassified"

# CREATE MAP WITH BARCHAPRT ---------------------------------------------------
station_count_barchart <-
  ggplot(station_landuse_count) +
geom_bar(mapping=aes(reorder(cat,-freq),freq,fill=cat),stat="identity",width=.25,alpha=.4,col="white")+
labs(subtitle = "where the bike-stations are located")+
scale_fill_manual(name="Category:",values=c("green space"="seagreen4","residential" ="seashell4", "unclassified"="#00101f","business"="darkorchid4"),guide="none")+
geom_text(mapping=aes(reorder(cat,-freq),freq,label = scales::percent(freq)),colour = "white",size = 4,vjust = -.8)+
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="white"),
        axis.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=17, face="italic",angle=-30,hjust = .48,vjust = -.8,color="white"),
        axis.ticks = element_blank(),
        plot.background = element_rect("#00101f"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect("#00101f"),
        text=element_text(family="Times New Roman", face="bold", size=21))

# COMBINE BOTH MAPS -----------------------------------------------

# set the box where the barplot should be located
hamburg_map+inset_element(station_count_barchart, 
                          left = .3, 
                          bottom = .7, 
                          right = .1, 
                          top =.98)
