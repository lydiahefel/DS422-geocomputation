#https://geoportal.hawaii.gov/datasets/437b77f132ed416294b45bde6aef23f4_11/explore?location=20.546870%2C-157.491600%2C7.83

library(tidyverse)
library(sf)
library(here)
library(mapgl)
library(tidycensus)
#library(rsocrata)

tsunami_zones <- st_read(here("data/Tsunami_Evacuation_All_Zones.geojson"))

mapboxgl(bounds = tsunami_zones) |>
  add_fill_layer(
    id = "tsunami_evac_all",
    source = tsunami_zones,
    fill_color = match_expr(
      column = "zone_type",
      values = c("Tsunami Safe Zone", "Extreme Tsunami Evacuation Zone", "Tsunami Evacuation Zone"),
      stops = c("#1a9850", "#fee08b", "#d73027")
    ),
    fill_opacity = 0.55,
    tooltip = "zone_type"
  ) |>
  add_legend(
    legend_title = "Tsunami Evacuation Zones",
    values = c("Tsunami Safe Zone", "Extreme Tsunami Evacuation Zone", "Tsunami Evacuation Zone"),
    colors = map_colors,
    type = "categorical"
  )

bg <- get_acs(
  geography = "block group",
  variables = "B01003_001",   # total population
  state = "HI",
  year = 2023, survey = "acs5",
  geometry = TRUE, cache_table = TRUE
) |>
  st_transform(4326) |>
  st_make_valid() |>
  rename(pop = estimate) |>
  select(GEOID, pop)

block_groups <- bg |>
  mutate(blockgroup_area = st_area(geometry))

#Safe Zone - 1124864

safe <- tsunami_zones[tsunami_zones$zone_type == "Tsunami Safe Zone",]

safe <- safe |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())

safe <- safe |>
  select(objectid, island, zone_type, zone_desc)

intersections <- st_intersection(block_groups, safe)

intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))

intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))

intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)

sum(intersections$pop_in_zone)

#Extreme Evacuation Zone - 174554.6

extreme <- tsunami_zones[tsunami_zones$zone_type == "Extreme Tsunami Evacuation Zone",]

extreme <- extreme |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())

extreme <- extreme |>
  select(objectid, island, zone_type, zone_desc)

intersections <- st_intersection(block_groups, extreme)

intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))

intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))

intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)

sum(intersections$pop_in_zone)

#Evacuation Zone - 130617.1

evac <- tsunami_zones[tsunami_zones$zone_type == "Tsunami Evacuation Zone",]

evac <- evac |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())

evac <- evac |>
  select(objectid, island, zone_type, zone_desc)

intersections <- st_intersection(block_groups, evac)

intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))

intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))

intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)

sum(intersections$pop_in_zone)

#This assumes that the population in the census are evenly distributed.