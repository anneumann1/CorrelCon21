# script to get shortest cycle paths with cycle street API
## NICE TO HAVE CODE, probably too much for the talk

library(dplyr)
library(sf)
library(sp)
library(stplanr)
library(cyclestreets) # queries cyclestreets API and gets shortest paths between coordinates

# READ IN DATA -----------------------------------------------------
stations <- read.csv("https://raw.githubusercontent.com/anneumann1/correlaid-hackathon-spatialdata/main/data/bikeshare_stations_hh.csv")
options( "encoding" = "UTF-8" )
trips <- read.csv("https://raw.githubusercontent.com/anneumann1/correlaid-hackathon-spatialdata/main/data/bikeshare_trips_hh.csv", encoding="UTF-8")

# PREPARE DATA -----------------------------------------------------
# remove loop trips (not needed for paths)
trips_no_loops <- trips %>%
  filter(start_rental_zone_hal_id != end_rental_zone_hal_id) %>%
  dplyr::group_by(start_rental_zone_hal_id, end_rental_zone_hal_id) %>%
  dplyr::summarise(trip_count = n())
glimpse(trips_no_loops)

# stations as spatial object (SP library is needed unfortunately) --------------
stations_coord <- stations
coordinates(stations_coord) <- ~lon+lat

# CREATE STRAIGHT CONNECTION ---------------------------------------------------
lines_between_stations = od2line(trips_no_loops, stations_coord)

# convert back to sf 
lines_between_stations <- st_as_sf(lines_between_stations)

# join stations data to get the right information
lines_between_stations <- lines_between_stations %>% 
  left_join(stations %>% dplyr::select(station_id, start_name=name),
            by=c("start_rental_zone_hal_id"="station_id")) %>% 
  left_join(stations %>% dplyr::select(station_id, end_name=name),
            by=c("end_rental_zone_hal_id"="station_id"))

# needed for plotting later
lines_between_stations <- st_set_crs(lines_between_stations, 4326)

# too much data on the map otherwise
lines_between_stations_sampled <- lines_between_stations %>% 
  slice_sample(prop=0.01) 

glimpse(lines_between_stations_sampled)
# SHORTEST PATHS WITH CYCLESTREETS API -----------------------------------------
token <- "372a7ee582db962a" # you need to apply for an API token here: https://www.cyclestreets.net/api/apply/

# use cyclestreets.net API to get shortest bicycle route for each station-pair
# source: https://geocompr.robinlovelace.net/transport.html
shortest_cycle_paths <- line2route(
  lines_between_stations, # dataset with coordinate pairs
  route_fun = stplanr::route_cyclestreets,
  pat = token
)

# add shortest paths to existing dataframe
lines_between_stations$geom_bike <- st_geometry(shortest_cycle_paths)
# SAVE -----------------------------------------------------------------------
saveRDS(lines_between_stations, "/home/jaezzy/Documents/Projects/CorrelCon21/data/paths_street_direct.rds")
