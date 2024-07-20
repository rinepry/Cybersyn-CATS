# load library
library(flexdashboard)
library(tidyverse)
library(tidytransit)
library(tidycensus)
library(gtfsway)
library(sf)
library(leaflet)
library(plotly)

# load scripts
source("metro.R")

# get feed
feed <- c(
  "https://gtfsrealtime.ridetransit.org/GTFSStatic/api/GTFSDownload/GTFS.zip",
  "https://gtfsrealtime.ridetransit.org/GTFSRealTime/TripUpdate/Tripupdates.pb",
  "https://gtfsrealtime.ridetransit.org/GTFSRealTime/Vehicle/VehiclePositions.pb",
  "https://gtfsrealtime.ridetransit.org/GTFSRealTime/Alerts/Alerts.pb"
)

gtfs <- read_gtfs(feed[1])

response <- httr::GET(feed[2])
gtfs_rt <- gtfs_realtime(response, content = "FeedMessage")
gtfs_tu <- gtfs_tripUpdates(gtfs_rt)

response <- httr::GET(feed[3])
gtfs_rt <- gtfs_realtime(response, content = "FeedMessage")
gtfs_vp <- gtfs_vehiclePosition(gtfs_rt)

validation <- validate_gtfs(gtfs)

route_freq <- get_route_frequency(gtfs)
stop_freq <- get_stop_frequency(gtfs)

# get shapes
gtfs_sp <- set_servicepattern(gtfs)
gtfs_sf <- gtfs_as_sf(gtfs)
shapes_sf <- shapes_as_sf(gtfs$shapes)
stops_sf <- stops_as_sf(gtfs$stops)
trips_sf <- get_trip_geometry(gtfs_sf, gtfs_sf$trips$trip_id)
routes_sf <- get_route_geometry(gtfs_sf, gtfs_sf$routes$route_id)

metro_map <- metro %>%
  ggplot() +
  geom_sf(data = metro, aes(fill = county)) +
  ggtitle("Charlotte Metro") +
  theme_void()

population_map <- ggplot() +
  geom_sf(data = metro_transit, aes(fill = populationE)) +
  geom_sf(data = shapes_sf) +
  theme_void()

medincome_map <- ggplot() +
  geom_sf(data = metro_transit, aes(fill = medincomeE)) +
  geom_sf(data = stops_sf) +
  theme_void()

# map routes and stops
routes_sf <- routes_sf %>%
  left_join(gtfs$routes, by = "route_id") %>%
  select(c(1, 2, 4)) %>%
  relocate(3, .after = 1)

map <- leaflet() %>%
  addTiles() %>%
  setView(
    lng = -80.8431,
    lat = 35.2271,
    zoom = 12
  ) %>%
  addPolylines(
    data = route_table,
    color = "blue",
    stroke = 0.5,
    opacity = 0.5
  ) %>%
  addCircleMarkers(
    data = stop_table,
    color = "red",
    stroke = 0.5,
    opacity = 1
  )
