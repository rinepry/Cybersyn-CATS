# load library
library(flexdashboard)
#library(thematic)
library(tidyverse)
library(tidytransit)
library(sf)
library(leaflet)
library(plotly)
library(DT)

# get feed
feed <- "https://gtfsrealtime.ridetransit.org/GTFSStatic/api/GTFSDownload/GTFS.zip"
gtfs <- read_gtfs(feed)

# get shapes
gtfs_sp <- set_servicepattern(gtfs)
gtfs_sf <- gtfs_as_sf(gtfs)
shapes_sf <- shapes_as_sf(gtfs$shapes)
stops_sf <- stops_as_sf(gtfs$stops)
trips_sf <- get_trip_geometry(gtfs_sf, gtfs_sf$trips$trip_id)
routes_sf <- get_route_geometry(gtfs_sf, gtfs_sf$routes$route_id)

# map routes
routes_sf <- routes_sf %>%
  left_join(gtfs$routes, by = "route_id") %>%
  select(c(1, 2, 4)) %>%
  relocate(3, .after = 1)

map <-leaflet() %>%
  addTiles() %>%
  setView(lng = -80.8431, lat = 35.2271, zoom = 12) %>%
  addPolylines(data = routes_sf, color = "red", stroke = 0.5, opacity = 0.5)

# service schedule
calendar <- tibble(date = unique(gtfs_sp$.$dates_services$date)) %>% 
  mutate(
    weekday = (function(date) {
      c("Sunday", "Monday", "Tuesday", 
        "Wednesday", "Thursday", "Friday", 
        "Saturday")[as.POSIXlt(date)$wday + 1]
    })(date)
  )

servicepattern_table <- gtfs_sp$.$dates_servicepatterns %>%
  left_join(calendar, by = "date")

patterns <- ggplot(servicepattern_table) + theme_bw() + 
  geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) + 
  scale_x_date(breaks = scales::date_breaks("1 month")) + theme(legend.position = "bottom")

# graph and table
route_freq <- get_route_frequency(gtfs) %>%
  left_join(gtfs$routes) %>%
  select(-1, -7,-(9:13)) %>%
  rename("total departures" = total_departures,
         "median headways" = median_headways,
         "st dev headways" = st_dev_headways,
         "stop count" = stop_count) %>%
  relocate(6) %>%
  select(1, 4)

stop_freq <- get_stop_frequency(gtfs) %>%
  left_join(gtfs$stops) %>%
  select(-3, -7, -(9:14)) %>%
  left_join(gtfs$routes) %>%
  select(-(1:2),-7,-(9:13)) %>%
  rename("service id" = service_id,
         "n departues" = n_departures,
         "mean headway" = mean_headway,
         "stop name" = stop_name) %>%
  relocate(4) %>%
  relocate(5) %>%
  relocate(3, .after = last_col())

route_freq_plot <- route_freq %>%
  ggplot(aes(route_long_name, mean_headways)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust=1))

stop_freq_table <- datatable(stop_freq, fillContainer = TRUE)