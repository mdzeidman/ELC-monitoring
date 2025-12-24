# code

options(scipen = 999)
# install.packages("devtools")
devtools::install_github("KC-Metro-Transit/ServicePlanningFunctions")
library(DBI)
library(tidyverse)
library(viridis)
library(plotly)
library(tidytransit)
library(tidycensus)
library(sf)
library(leaflet)

## run functions
#source(here::here('analyses', 'kcm_style.R'))
source(here::here('analyses', 'renton-restructure', '0_ec_functions.R'))


#route_list <- c(240, 241, 246, 203, 246, 221, 222, 223, 256, 257, 311)
# MZ excluded route 8 and 111 since Seattle routes with no recent changes.
route_list <- c(
  # 8,
  # 111,
  114,
  167,
  200,
  203,
  204,
  208,
  212,
  214,
  215,
  216,
  217,
  218,
  219,
  220,
  221,
  222,
  223,
  224,
  225,
  226,
  232,
  237,
  240,
  241,
  245,
  246,
  249,
  250,
  251,
  252,
  256,
  257,
  268,
  269,
  270,
  271,
  311,
  342,
  630,
  672,
  930,
  931
)
# route list without dart
## query parameters if you know you study area routes
service_changes <- c(243, 251, 253)
# if no routes to exclude put in dummy?
# these are all dart routes
dart_routes <- c(204, 224, 249, 630, 930, 931)
exclude_routes <- c(204, 224, 249, 630, 930, 931)
#exclude_routes <- c(118, 119, 981)

## load dart data -------------------------------------
dart_stop_raw <- read.csv(here::here(
  'analyses',
  'dart',
  'dart_stop_243_251.csv'
)) %>%
  mutate(Route = as.character(Route), dart = 'DART') %>%
  select(-Service, -avg_trip_dart, -stop_name_dart) %>%
  filter(route %in% dart_routes)

## elink stat
count_all_routes <- length(route_list)

## custom colors:
source(here::here('analyses', 'kcm_style.R'))

productivity_palette <- function() {
  kcm_custom_colors <- c(
    # Productivity by service change
    # MZ changed colors
    'Spring 2019' = '#FDB71A',
    'Fall 2024' = '#FDB71A',
    'Spring 2025' = '#FF7B21',
    'Fall 2025' = '#31859F'
  )

  list(
    ggplot2::scale_color_manual(values = kcm_custom_colors),
    ggplot2::scale_fill_manual(values = kcm_custom_colors)
  )
}

ridership_palette <- function() {
  kcm_custom_colors <- c(
    # Ridership by service change
    # MZ changed colors
    'Fall 2024' = '#FDB71A',
    'Spring 2025' = '#FF7B21',
    'Fall 2025' = '#31859F'
    # 'Spring 2019' = '#FDB71A',
    # 'Fall 2019' = '#FDB71A',
    # 'Spring 2024' = '#BC7D2D',
    # 'Fall 2024' = '#7A4241',
    # 'Spring 2025' = '#390854',
    # 'Fall 2025' = '#390854'
  )

  list(
    ggplot2::scale_color_manual(values = kcm_custom_colors),
    ggplot2::scale_fill_manual(values = kcm_custom_colors)
  )
}

# load tbird data ------------------------------

server <- "kcitazrsqlprp01.database.windows.net"
database = "tbird_dw"

# Establish connection to VPN before this step, which connects to Metro's SQL servers
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = server,
  Database = database,
  Authentication = "ActiveDirectoryIntegrated"
)


### Get stop ridership --------------------------------------

stop_ridership <- ServicePlanningFunctions::get_stop_ridership(
  service_change_num = service_changes,
  route = route_list,
  stop_id = 'All',
  tbird_connection = con
) %>%
  # append dart data
  bind_rows(dart_stop_raw) %>%
  mutate(dart = case_when(is.na(dart) ~ 'Fixed-Route', TRUE ~ dart)) %>%
  # ServicePlanningFunctions::clean_service_rte_num()
  mutate(
    Route = as.character(route),
    Route = case_when(
      Route == '671' ~ 'A Line',
      Route == '672' ~ 'B Line',
      Route == '673' ~ 'C Line',
      Route == '674' ~ 'D Line',
      Route == '675' ~ 'E Line',
      Route == '676' ~ 'F Line',
      Route == '677' ~ 'G Line',
      TRUE ~ Route
    ),
    Service = case_when(
      service_change_num == 191 ~ 'Spring 2019',
      service_change_num == 193 ~ 'Fall 2019',
      service_change_num == 241 ~ 'Spring 2024',
      service_change_num == 243 ~ 'Fall 2024',
      service_change_num == 251 ~ 'Spring 2025',
      service_change_num == 253 ~ 'Fall 2025'
    )
  )


## get scope stop list
stop_list <- unlist(list(unique(sort(stop_ridership$stop_id))))
route_list <- unlist(list(unique(sort(stop_ridership$route))))

## elink route counts by service change
route_243 <- unlist(list(unique(sort(stop_ridership$route[
  stop_ridership$service_change_num == 243
]))))
route_251 <- unlist(list(unique(sort(stop_ridership$route[
  stop_ridership$service_change_num == 251
]))))
route_253 <- unlist(list(unique(sort(stop_ridership$route[
  stop_ridership$service_change_num == 253
]))))

count_route_243 <- length(route_243)
count_route_251 <- length(route_251)
count_route_253 <- length(route_253)

## elink stop counts by service change
stop_243 <- unlist(list(unique(stop_ridership$stop_id[
  stop_ridership$service_change_num == 243
])))
stop_251 <- unlist(list(unique(stop_ridership$stop_id[
  stop_ridership$service_change_num == 251
])))
stop_253 <- unlist(list(unique(stop_ridership$stop_id[
  stop_ridership$service_change_num == 253
])))

count_stop_243 <- length(stop_243)
count_stop_251 <- length(stop_251)
count_stop_253 <- length(stop_253)


## write csv
#write_csv(stop_ridership, "stop_ridership_2025.12.15.csv")
# table_stop_ridership <- summ_table(
#   source = 'stop_ridership',
#   group_var = c('stop_id', 'host_street_nm', 'cross_street_nm', 'route'),
#   select_service_change = service_changes,
#   var1 = c('rider', 'ons', 'offs')
# )
# write_csv(table_stop_ridership, "stop_ridership_2025.12.15.csv")

## create route neighborhood xwalk
# xwalk_route_neighborhood <- st_join(shape_neighborhoods, shape_stops) %>%
#   inner_join(stop_ridership, by = join_by(stop_id)) %>%
#   filter(neighborhood %in% select_neighborhood) %>%
#   select(route, neighborhood) %>%
#   st_drop_geometry() %>%
#   group_by(route, neighborhood) %>%
#   slice(1)

## get trip ridership ---------------------------------------------------------------------

# dart ridership from dart dashboard Boarding Details page on 7/8/25
# add 2024 and 2019 annual boardings averaged to weekday

# dart_data <- data.frame(
#   service_change_num = service_changes,
#   route = 907,
#   Day = 'Weekday',
#   ons = c(20861/255, 20585/255, 20585/255, 20585/255)
# )

trip_ridership <- DBI::dbGetQuery(
  con,
  glue::glue_sql(
    "
SELECT [SERVICE_CHANGE_NUM]
      ,[SERVICE_RTE_NUM] as route
      ,[INBD_OUTBD_CD] as direction
      ,[SCHED_DAY_TYPE_CODED_NUM] as day_code
      ,[TRIP_ID]
      ,[DAY_PART_CD]
      ,[SCHED_START_TIME_MNTS_AFTER_MIDNT] as trip_time
      ,[SCHED_START_TIME]
      ,[OBSERVED_TRIPS]
      ,[AVG_PSNGR_BOARDINGS] as ons
      ,[AVG_PSNGR_ALIGHTINGS] as offs
      ,[AVG_OF_MAX_PSNGR_LOAD] as avg_load
      ,[BUS_TYPE_NUM]
      ,[SEAT_CNT]
      ,[LOAD_FACTOR]
      ,[CROWDING_THRESHOLD_NBR]
      ,[AVG_PSNGR_MILES]
      ,[PLATFORM_HOURS]
      ,[REVENUE_HOURS]
  FROM [DP].[VW_TRIP_SUMMARY]
  WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
  --AND [SCHED_DAY_TYPE_CODED_NUM] = 0
  AND SERVICE_RTE_NUM IN ({vals2*})
  ",
    vals1 = service_changes,
    vals2 = route_list,
    .con = con
  )
) %>%
  janitor::clean_names() %>%
  mutate(
    period = case_when(
      trip_time >= 300 & trip_time < 540 ~ 'AM Peak',
      trip_time >= 540 & trip_time < 900 ~ 'Midday',
      trip_time >= 900 & trip_time < 1140 ~ 'PM Peak',
      trip_time >= 1140 & trip_time < 1320 ~ 'Evening',
      TRUE ~ 'Night'
    ),
    hour = as.character(trip_time / 60),
    Day = case_when(
      day_code == 0 ~ 'Weekday',
      day_code == 1 ~ 'Saturday',
      day_code == 2 ~ 'Sunday'
    )
  ) %>%
  separate_wider_delim(
    hour,
    delim = ".",
    names = c("hour", "min"),
    too_few = "align_start"
  ) %>%
  mutate(hour = as.integer(hour)) %>%
  select(-min) %>%
  # add dart data
  filter(route != 907) %>%
  #bind_rows(dart_data) %>%
  mutate(
    Route = as.character(route),
    Route = case_when(
      Route == '671' ~ 'A Line',
      Route == '672' ~ 'B Line',
      Route == '673' ~ 'C Line',
      Route == '674' ~ 'D Line',
      Route == '675' ~ 'E Line',
      Route == '676' ~ 'F Line',
      Route == '677' ~ 'G Line',
      TRUE ~ Route
    ),
    Service = case_when(
      service_change_num == 191 ~ 'Spring 2019',
      service_change_num == 193 ~ 'Fall 2019',
      service_change_num == 241 ~ 'Spring 2024',
      service_change_num == 243 ~ 'Fall 2024',
      service_change_num == 251 ~ 'Spring 2025',
      service_change_num == 253 ~ 'Fall 2025'
    ),
    Day = factor(Day, levels = c("Weekday", "Saturday", "Sunday")),
    Service = reorder(Service, service_change_num)
  )

#rm(dart_data)

## get trip productivity ---------------------------------------------------------------------

trip_productivity_period <- trip_ridership %>%
  group_by(
    service_change_num,
    route,
    day_code,
    day_part_cd,
    period,
    Route,
    Service
  ) %>%
  summarise_at(
    c('ons', 'platform_hours', 'revenue_hours'),
    sum,
    na.rm = TRUE
  ) %>%
  # TODO: platform hours or revenue hours?
  mutate(rides_per_hour = ons / platform_hours)

trip_productivity_hour <- trip_ridership %>%
  group_by(service_change_num, route, day_code, hour, Route, Service) %>%
  summarise_at(
    c('ons', 'platform_hours', 'revenue_hours'),
    sum,
    na.rm = TRUE
  ) %>%
  # TODO: platform hours or revenue hours?
  mutate(rides_per_hour = ons / platform_hours)

# test <- route_ridership_map(day_of_week = 'Weekday',
#                                select_service_change = 251,
#                                select_route = route_list,
#                                var1 = 'ons')
# test

## get stop and route geography --------------------------------------------------

# get latest regular and reg/lay stops from the EDW:

shape_all_stops <- DBI::dbGetQuery(
  con,
  glue::glue_sql(
    "
SELECT * 
FROM (
SELECT [STOP_ID]
      ,[ON_STREET_NM]
      ,[CROSS_STREET_NM]
      ,[GPS_LATITUDE]
      ,[GPS_LONGITUDE]
      ,[STOP_LEN]
      ,[STOP_STATUS_SHORT_DESC]
      ,[STOP_TYPE_SHORT_DESC]
      ,[EFF_START_DATE]
      ,[EFF_END_DATE]
      --,[IS_ACTIVE_FLAG]
      --,[TRANSIT_STOP_KEY]
	  ,ROW_NUMBER() OVER(PARTITION BY STOP_ID ORDER BY [EFF_END_DATE] DESC) AS rn
  FROM [EDW].[DIM_TRANSIT_STOP]) A
  WHERE rn = 1
  and [STOP_TYPE_SHORT_DESC] IN ('Regular', 'Reg/Lay') --'Layover'
  AND [STOP_ID] IN ({vals1*})
  ",
    vals1 = stop_list,
    .con = con
  )
) %>%
  janitor::clean_names() %>%
  select(-rn) %>%
  unite(
    'stop_name',
    on_street_nm:cross_street_nm,
    sep = " & ",
    remove = TRUE,
    na.rm = FALSE
  )

shape_stops <- st_as_sf(
  shape_all_stops,
  coords = c("gps_longitude", "gps_latitude"),
  crs = 4326
)
