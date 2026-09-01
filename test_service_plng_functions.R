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

# PRODUCTIVITY ---------------------------------------------------------------

# Rides per Hour Productivity M and M plots - Weekday
# ERROR message on dplyr::mutate() and cannot get ggplot2 to appear
plot_productivity_distribution(
  service_change = 261,
  tbird_connection = con,
  svc_family = NULL,
  sched_day_type_coded_num = 0,
  period_type = "day_part_cd",
  period = "DAY",
  route <- c( 203, 221, 222, 223, 225, 226, 240, 
             241, 245, 246, 249, 250, 256, 257, 
             269, 271, 311),
  route_gain = NULL,
  route_maintain = NULL,
  route_lose = NULL,
  activity_type = "rides_per_platform_hour",
  binwidth = 1,
  point_size = 20,
  label_size = 4,
  style_size = 'large'
)
ggplot2::ggsave("ELC Rides per Platform Hour 261.png", width = 6.5, height = 4, units = "in")

# creates a table of productivity metrics for all routes or selected routes for a service change
# WORKS for 253
# ERROR message for service_change = 261
route_productivity_table <- get_route_productivity(
  service_change = 253,
  tbird_connection = con,
  period_type = "service_guidelines",
  sched_day_type_coded_num = c(0,1,2),
  filter_routes = TRUE,
  route <- c(203, 221, 222, 223, 225, 226, 240, 
             241, 245, 246, 249, 250, 256, 257, 
             269, 271, 311)
)
write_csv(route_productivity_table, "rte_productivity_253.csv")

# creates table of productivity thresholds by service family for all routes for a service change 
# does not provide route-level productivity data
# WORKS for 253
# ERROR for 261
productivity_thresholds_table <- get_productivity_thresholds(
  service_change = 253,
  tbird_connection = con,
  period_type = "service_guidelines",
  sched_day_type_coded_num = 0
)
write_csv(productivity_thresholds_table, "productivity_thresholds_table_253.csv")


# STOP RIDERSHIP ------------------------------------------------------------

# Get Stop Ridership for a Route - All Stops - And write to a csv file
# WORKS
ELC_stop_ridership_table <- get_stop_ridership(
  service_change_num <- c(253, 261),
  route <- c( 203, 221, 222, 223, 225, 226, 240, 
              241, 245, 246, 249, 250, 256, 257, 
              269, 271, 311),
  stop_id = "All",
  tbird_connection = con
)
write_csv(ELC_stop_ridership_table, "ELC ridership.csv")

# plot of stop-level ridership for one or more stops for one or more routes by period for one or more service change
# WORKS
ELC_plot_stop_crosstab <- plot_stop_crosstab(
  dataframe = ELC_stop_ridership_table,
  #dataframe must be for a get_stop_ridership result you've already run for service_change_num, routes, and stop_id used here
  service_change_num <- c(253, 261),
  route <- c(203, 226, 240),
  stop_id = "84264",
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  x_axis = "period",
  activity_type = "ons"
)
ggplot2::ggsave("ELC stop ridership plot_84264_253.png", width = 6.5, height = 4, units = "in")

# To see a map of LOCUS areas in the Viewer window
# WORKS
show_areas()

# Get Stop Ridership for a LOCUS district/area - All stops in area  
# WORKS for LOCUS areas, yay! issue with legend
# ERROR for data_source = "King County Council Districts" and area = "6" or 6
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
# WORKS for LOCUS as data_source and area
# ERROR when trying to run with King County Council District

get_stops_by_area(
  area = "Ballard",
  gtfs_date = '2025-09-30',
  tbird_connection = con,
  return_type = "table",
  data_source = "LOCUS"
)

# Get ridership for a segment of a route defined by two stops (start and end)
# WORKS
# can include multiple routes
# need to get stop ridership for routes first
stop_ridership_table_230_231 <- get_stop_ridership(
  service_change_num = 261,
  route <- c( 230, 231),
  stop_id = "All",
  tbird_connection = con
)
# then run plot function
# would be cool if you could produce two plots split by direction using split_by
segment_ridership_230_231 <- plot_segment_ridership(
  dataframe = ELC_stop_ridership_table,
  service_change_num = 261,
  route <- c(230, 231),
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  direction = c("I", "O"),
  x_axis = "period",
  activity_type = "ons",
  start_stop = 74580,
  end_stop = 70220
)
ggplot2::ggsave("Segment Ridership_Rte 230-231.png", width = 6.5, height = 4, units = "in")


# Get Stop Frequency - I don't know if this is complete or what the gtfs_obj would be?
# WORKS but I don't understand resulting table - what units is mean_headway in (seconds?) - csv works too
# unsure how to get right GTFS file - using test data?
stop_freq_table <- get_stop_frequency(
  # what is gtfs_duke?
  gtfs_obj = gtfs_duke,
  start_time = "05:00:00",
  end_time = "08:59:59",
  by_route = FALSE
)
write_csv(stop_freq_table, "stop_freq_AM.csv")


# TRIP RIDERSHIP -------------------------------------------------------------

# creates table of trip-level ridership (ons, offs, departure load) by period and direction
# can filter by route and day type or view all
# would be nice to pick period or have it aggregate to All
# WORKS
elc_trip_ridership_table <- get_trip_ridership(
  service_change_num <- c(243, 251, 253, 261),
  route <- c(203, 221, 222, 223, 225, 226, 240, 
             241, 245, 246, 249, 250, 256, 257, 
             269, 271, 311),
  sched_day_type_coded_num <- c(0,1,2),
  tbird_connection= con
)
write_csv(elc_trip_ridership_table, "trip_ridership_ELCfixedroutes_261.csv")

# creates plot of trip ridership
# WORKS
# can create separate plots by day type and route (use in split_by)
# can use multiple service_change_num
ELC_trip_ridership <- plot_trip_crosstab(
  dataframe = elc_trip_ridership_table,
  service_change_num <- c(243,251, 253, 261),
  route <- c(203, 221, 222, 223, 225, 226, 240, 
             241, 245, 246, 249, 250, 256, 257, 
             269, 271, 311),
  day = c("Weekday", "Saturday", "Sunday"),
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  x_axis = "period",
  activity_type = "ons",
  split_by = "day",
  color_palette = "viridis",
  color_palette_direction = 1
)
ggplot2::ggsave("ELC trip ridership_243-261_route.png", width = 6.5, height = 4, units = "in")


# creates a plot of route-level ridership for top 15 routes, if list all times then gives for all day
# WORKS
# is there a way to customize title, number of routes shown, or color of bars?
plot_route_ridership <- plot_route_by_service_change(
  dataframe = trip_ridership_table,
  #dataframe is output from get_trip_ridership
  service_change_num = 261,
  route <- c(203, 221, 222, 223, 225, 226, 240, 
             241, 245, 246, 249, 250, 256, 257, 
             269, 271, 311),
  day = c("Weekday","Saturday","Sunday"),
  time_period = c("AM","PM"),
  activity_type = "ons"
)
ggplot2::ggsave("ELC route ridership_AM_PM.png", width = 6.5, height = 4, units = "in")


#EQUITY DATA ------------------------------------------------------------------

# creates table of opportunity index scores by route and lists service family, along with some other data
# does this include equity priority score as well? (what is mean_stop_opportunity_score?)
# WORKS
ELC_equity_scores <- get_route_equity_scores(
  tbird_connection = con,
  route <- c(203, 221, 222, 223, 225, 226, 240, 
             241, 245, 246, 249, 250, 256, 257, 
             269, 271, 311),
  service_change = 261
)
write_csv(ELC_equity_scores, "equity_scores_ELCfixedroutes_261.csv")

# SERVICE LEVEL DATA -------------------------------------------------------------------

# create summary table of trips and headways by day type for list of routes or all routes
# not sure how to get GTFS for baseline or comparison/proposed
# CANNOT TEST
ELC_trips_table <- create_trips_table(
  gtfs = #not sure what goes here,
  netplan_gtfs = TRUE,
  reference_date = NULL,
  day_type = 'wkd',
  network = 'baseline_gtfs'
  by_direction = TRUE,
  by_period = TRUE,
  routes <- c(203, 221, 222, 223, 225, 226, 240, 
              241, 245, 246, 249, 250, 256, 257, 
              269, 271, 311)
)
