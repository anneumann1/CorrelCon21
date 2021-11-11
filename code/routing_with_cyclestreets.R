#install.packages("cyclestreets")
library(cyclestreets) # https://cran.r-project.org/web/packages/cyclestreets/cyclestreets.pdf
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(stplanr)

# load data ---------------------------------------------------------------------------------------------------
stations <- read_csv("datasets/bikeshare_stations_hh.csv")

# stations as spatial object (sp package) -------------------------------------------------------------------------
stations_coord <- stations
coordinates(stations_coord) <- ~lon+lat
glimpse(stations_coord)

# load trips
# this is the original dataframe we cannot upload due to datasize
#trip_df <- read.csv("datasets/bikeshare_trips_hh.csv")

# we sample it to demonstrate how the code works
# also the api is really slow with a lot of data
trips_sampled <- trip_df %>% 
  slice_sample(prop=0.08) # we only take a small portion of the original data

#saveRDS(trips_sampled, "datasets/sampled_db_trips.rds")

# remove loop trips (not needed for bike infrastructure)
bike_trips <- trips_sampled %>%
  filter(start_rental_zone_hal_id != end_rental_zone_hal_id) %>%
  dplyr::group_by(start_rental_zone_hal_id, end_rental_zone_hal_id) %>%
  dplyr::summarise(trip_count = n())

# create straight connection between stations using the stplanr library
desire_lines = od2line(bike_trips, stations_coord)

# convert to sf format (easier to use & good for plotting)
desire_lines_sf <- st_as_sf(desire_lines)

# add station information to OD-pairs
desire_lines_sf <- desire_lines_sf %>% 
  left_join(stations %>% dplyr::select(station_id, start_name=name),
            by=c("start_rental_zone_hal_id"="station_id")) %>% 
  left_join(stations %>% dplyr::select(station_id, end_name=name),
            by=c("end_rental_zone_hal_id"="station_id"))

# get routes from Cyclestreets API -----------------------------------
# you need to sign up for an API-key for cyclestreets.net
# link: https://www.cyclestreets.net/api/apply/
token <- "" # the token you got after applying

# use cyclestreets.net API to get shortest bicycle route for each station-pair
# source/tutorial: https://geocompr.robinlovelace.net/transport.html
routes <- line2route(
  desire_lines_sf,
  route_fun = stplanr::route_cyclestreets,
  pat = token
)

# assign roads as additional geometry column
desire_lines_sf$geom_bike <- st_geometry(routes)

# rds files can store spatial formats
saveRDS(routes, "data/shortest_cycle_paths.rds")
