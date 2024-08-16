# load library
library(flexdashboard)
library(tidyverse)
library(tidytransit)
library(tidycensus)
library(gtfsway)
library(sf)
library(leaflet)
library(plotly)
library(RSQLite)

# load Census scripts
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

## Load data into SQLite DB ## 
conn <- dbConnect(RSQLite::SQLite(), "cats.db")
# Write datasets to the DB as tables - initial load & check for updates
dbWriteTable(conn, "routes", gtfs$routes)
dbWriteTable(conn, "stops", stops_sf)
dbWriteTable(conn, "stop_times", gtfs$stop_times) #NOTE: Convert <time> columns into hours/minutes, values in DB as seconds, 24-hour time
dbWriteTable(conn, "trips", trips_sf)
# TODO: Figure out best way to flatten and use data in gtfs_tu, CATS-provided delay and arrival updates

# Constant load / load this data as close to real-time as possible
# Write a table by appending the data frames inside the list
for(k in 1:length(gtfs_vp)){
  dbWriteTable(conn,"vehicle_positions", gtfs_vp[[k]], append = TRUE)
}
# NOTE: gtfs_vp[[8]][["timestamp"]] = Epoch/UNIX timestamp, need to convert to EST

# List all the tables available in the database
dbListTables(conn)

# Close DB connection
dbDisconnect(conn)

## Load maps ##
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
    data = routes_sf, #route_table
    color = "blue",
    stroke = 0.5,
    opacity = 0.5
  ) %>%
  addCircleMarkers(
    data = stops_sf, #stop_table
    color = "red",
    stroke = 0.5,
    opacity = 1
  )
