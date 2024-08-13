library(tidyverse)
library(tidytransit)

feed <- c(
  "https://gtfsrealtime.ridetransit.org/GTFSStatic/api/GTFSDownload/GTFS.zip",
  "https://gtfsrealtime.ridetransit.org/GTFSRealTime/TripUpdate/Tripupdates.pb",
  "https://gtfsrealtime.ridetransit.org/GTFSRealTime/Vehicle/VehiclePositions.pb",
  "https://gtfsrealtime.ridetransit.org/GTFSRealTime/Alerts/Alerts.pb"
)

## service patterns
gtfs <- read_gtfs(feed[1]) %>%
  set_servicepattern()

calendar <- tibble(date = unique(gtfs$.$dates_services$date)) %>%
  mutate(
    weekday = (function(date) {
      c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )[as.POSIXlt(date)$wday + 1]
    })(date)
  )

date_servicepattern_table <- gtfs$.$dates_servicepatterns %>%
  left_join(calendar, by = "date")

date_servicepattern_plot <- date_servicepattern_table %>%
  ggplot() +
  geom_point(aes(x = date, y = servicepattern_id, color = weekday)) +
  ylab("service pattern id") +
  theme(legend.position = "bottom")

## route headways
gtfs <- read_gtfs(feed[1]) %>%
  gtfs_as_sf()

route_table <- get_route_geometry(gtfs,
  service_ids = NULL
) %>%
  inner_join(get_route_frequency(gtfs,
    start_time = "06:00:00",
    end_time = "18:00:00",
    service_id = NULL
  ), by = "route_id") %>%
  mutate(mean_headways =  str_split(dseconds(mean_headways), "[:symbol:]{5}"))

route_headways_plot <- route_table %>%
  ggplot() +
  geom_sf(aes(color = as.factor(mean_headways))) +
  geom_sf_text(aes(label = route_id)) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")

## stop departures
gtfs <- read_gtfs(feed[1]) %>%
  gtfs_as_sf()

stop_table <- gtfs$stops %>%
  inner_join(get_stop_frequency(gtfs,
    start_time = "06:00:00",
    end_time = "18:00:00",
    service_id = NULL,
    by_route = TRUE
  ), by = "stop_id") %>%  
  group_by(stop_name) %>%
  mutate(total_departures = sum(n_departures, na.rm=TRUE)) %>%
  filter(total_departures > 50)

stop_departures_plot <- ggplot() +
  geom_sf(route_table, color = "blue", mapping = aes(alpha = total_departures)) +
  geom_sf(stop_table, color = "red", mapping = aes(size = total_departures), shape = 1) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "bottom")