options(scipen = 999)
# Run manually only when ServicePlanningFunctions needs updating:
devtools::install_github("KC-Metro-Transit/ServicePlanningFunctions")
library(ServicePlanningFunctions)
library(DBI)
library(tidyverse)
library(viridis)
library(plotly)
library(tidytransit)
library(tidycensus)
library(sf)
library(leaflet)
library(dplyr)

source(here::here("analyses", "kcm_style.R"))
source(here::here("analyses", "ND_functions.R"))

con <- ServicePlanningFunctions::connect_to_tbird()

# Use consistent service-period colors throughout this report
service_period_colors <- c(
  "Fall 2024" = "#FDB515",
  "Spring 2025"   = "#F97316",
  "Fall 2025" = "#348DA5",
  "Spring 2026"   = "#4B146A"
)

# Rides per Hour Productivity M and M plots - Weekday
# getting an error message on dplyr::mutate() and cannot get ggplot2 to appear
plot_productivity_distribution(
  service_change = 261,
  tbird_connection = con,
  svc_family = NULL,
  sched_day_type_coded_num = 0,
  period_type = "day_part_cd",
  period = "DAY",
  route = c( 203, 221, 222, 223, 225, 226, 240, 
             241, 245, 246, 249, 250, 256, 257, 
             269, 271, 311),
  # MZ: how to have it color only ELC routes? the list of routes is ELC fixed routes
  route_gain = NULL,
  route_maintain = NULL,
  route_lose = NULL,
  activity_type = "rides_per_platform_hour",
  binwidth = 1,
  point_size = 20,
  label_size = 4,
  style_size = 'large'
)
ggplot2::ggsave("ELC Rides per Platform Hour 261.png", width = 12.5, height = 6.9, units = "in")

# Get Stop Ridership for a Route - All Stops - And write to a csv file

rte240_245_261_table <- get_stop_ridership(
  service_change_num = 261,
  route = c(240, 245),
  stop_id = "All",
  tbird_connection = con
)
write_csv(rte240_245_261_table, "rte240-245_261.csv")


# To see a map of LOCUS areas in the Viewer window
show_areas()

# Get Stop Ridership for a LOCUS district/area - All stops in area  
# works for LOCUS areas, yay! issue with legend
# doesn't seem to work with "King County Council Districts" if I change area to "6" or 6
get_stop_ridership_by_area(
  area = "Ballard",
  gtfs_date = '2025-09-30',
  service_change_num = 253,
  route = "All",
  tbird_connection = con,
  return_type = "interactive_map",
  time_period <- c("AM","PM"),
  # legend says "AM Peak, Midday, PM Peak, Night" even though filtering to AM and PM Peak
  activity_type = "ons",
  data_source = "LOCUS"
)

# Get stops by area - list of all stops in a LOCUS area or King County Council District
# Getting error when trying to run with King County Council District
get_stops_by_area(
  area = "Ballard",
  gtfs_date = '2025-09-30',
  tbird_connection = con,
  return_type = "table",
  data_source = "LOCUS"
)

# Get Stop Frequency - I don't know if this is complete or what the gtfs_obj would be?
# runs but I don't understand resulting table - what units is mean_headway in (seconds?) - csv works too
stop_freq_table <- get_stop_frequency(
  # what is gtfs_duke?
  gtfs_obj = gtfs_duke,
  start_time = "05:00:00",
  end_time = "08:59:59",
  by_route = FALSE
)
write_csv(stop_freq_table, "stop_freq_AM.csv")

# creates a table of productivity metrics for all routes or selected routes for a service change
# getting an error message
route_productivity_table <- get_route_productivity(
  service_change = 261,
  tbird_connection = con,
  period_type = "service_guidelines",
  sched_day_type_coded_num = c(0,1,2),
  filter_routes = FALSE,
  route <- c(203, 221, 222, 223, 225, 226, 240, 
             241, 245, 246, 249, 250, 256, 257, 
             269, 271, 311)
)

write_csv(rte_productivity_table, "rte_productivity_261.csv")