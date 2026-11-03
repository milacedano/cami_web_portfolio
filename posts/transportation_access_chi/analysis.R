library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

# Load Chicago community areas
community_areas <- st_read(
  "posts/transportation_access_chi/data/community_areas.geojson"
)

# Load CTA station data
cta_stations <- read.csv(
  "posts/transportation_access_chi/data/cta_stations.csv"
)

# Load Divvy station data
divvy_stations <- read.csv(
  "posts/transportation_access_chi/data/divvy_stations.csv"
)

# Inspect the datasets
View(community_areas)
names(community_areas)

View(cta_stations)
names(cta_stations)

View(divvy_stations)
names(divvy_stations)

head(community_areas$community, 10)
head(cta_stations$Location)

# Basic map of Chicago community areas
ggplot(community_areas) +
  geom_sf() +
  theme_minimal() +
  labs(
    title = "Chicago Community Areas"
  )

# Split the CTA Location column into latitude and longitude
cta_stations <- cta_stations %>%
  separate(
    Location,
    into = c("latitude", "longitude"),
    sep = ","
  ) %>%
  mutate(
    latitude = as.numeric(gsub("\\(", "", latitude)),
    longitude = as.numeric(gsub("\\)", "", longitude))
  )

# Check cleaned coordinates
head(cta_stations[, c("STATION_NAME", "latitude", "longitude")])

# Keep one row per physical CTA station
cta_stations_unique <- cta_stations %>%
  distinct(MAP_ID, .keep_all = TRUE)

# Check number of unique physical stations
nrow(cta_stations_unique)

# Convert CTA stations into spatial points
cta_stations_sf <- st_as_sf(
  cta_stations_unique,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Match CTA stations to Chicago community areas
cta_with_community <- st_join(
  cta_stations_sf,
  community_areas["community"],
  join = st_intersects
)

# Find stations that do not fall inside Chicago community areas
outside_chicago <- cta_with_community %>%
  filter(is.na(community)) %>%
  st_drop_geometry() %>%
  select(MAP_ID, STATION_NAME)

View(outside_chicago)

# Keep only CTA stations located within Chicago
cta_chicago <- cta_with_community %>%
  filter(!is.na(community))

# Count CTA stations by community area
cta_station_counts <- cta_chicago %>%
  st_drop_geometry() %>%
  count(community, name = "cta_stations") %>%
  arrange(desc(cta_stations))

# View community areas with the most CTA stations
head(cta_station_counts, 10)

# Count how many CTA lines serve each station
cta_chicago <- cta_chicago %>%
  mutate(
    line_count = rowSums(
      across(
        c(RED, BLUE, G, BRN, P, Pnk, O, Y),
        ~ toupper(as.character(.)) == "TRUE"
      )
    )
  )

# Identify CTA transfer stations
transfer_stations <- cta_chicago %>%
  filter(line_count > 1)

# View transfer stations
transfer_stations %>%
  st_drop_geometry() %>%
  select(
    STATION_NAME,
    MAP_ID,
    line_count
  )

# Convert Divvy stations into spatial points
divvy_stations_sf <- st_as_sf(
  divvy_stations,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# Match Divvy stations to Chicago community areas
divvy_with_community <- st_join(
  divvy_stations_sf,
  community_areas["community"],
  join = st_intersects
)

# Check for Divvy stations outside Chicago community areas
divvy_outside_chicago <- divvy_with_community %>%
  filter(is.na(community)) %>%
  st_drop_geometry() %>%
  select(ID, Station.Name)

View(divvy_outside_chicago)

# Keep only Divvy stations within Chicago
divvy_chicago <- divvy_with_community %>%
  filter(!is.na(community))

# Count Divvy stations by community area
divvy_station_counts <- divvy_chicago %>%
  st_drop_geometry() %>%
  count(community, name = "divvy_stations") %>%
  arrange(desc(divvy_stations))

# View community areas with the most Divvy stations
head(divvy_station_counts, 10)

# Combine CTA and Divvy counts with community area boundaries
access_map <- community_areas %>%
  left_join(
    cta_station_counts,
    by = "community"
  ) %>%
  left_join(
    divvy_station_counts,
    by = "community"
  ) %>%
  mutate(
    cta_stations = replace_na(cta_stations, 0),
    divvy_stations = replace_na(divvy_stations, 0)
  )

# Calculate relative station availability
access_map <- access_map %>%
  mutate(
    cta_access = cta_stations / max(cta_stations),
    divvy_access = divvy_stations / max(divvy_stations)
  )

# Reshape access data for side-by-side maps
access_map_long <- access_map %>%
  select(
    community,
    cta_access,
    divvy_access,
    geometry
  ) %>%
  pivot_longer(
    cols = c(cta_access, divvy_access),
    names_to = "transportation_type",
    values_to = "access"
  ) %>%
  mutate(
    transportation_type = recode(
      transportation_type,
      cta_access = "CTA Rail",
      divvy_access = "Divvy"
    )
  )

# Create CTA + Divvy station map
station_map <- ggplot() +
  geom_sf(
    data = community_areas,
    fill = "white",
    color = "gray80",
    linewidth = 0.3
  ) +
  
  # Divvy stations
  geom_sf(
    data = divvy_chicago,
    aes(shape = "Divvy Station"),
    color = "#00AEEF",
    size = 1.1,
    alpha = 0.65
  ) +
  
  # CTA stations
  geom_sf(
    data = cta_chicago,
    aes(shape = "CTA Station"),
    color = "#C60C30",
    size = 2.8
  ) +
  
  # CTA transfer stations
  geom_sf(
    data = transfer_stations,
    aes(shape = "CTA Transfer Station"),
    color = "#333333",
    size = 3.8,
    stroke = 1
  ) +
  
  scale_shape_manual(
    name = "Transportation Type",
    values = c(
      "CTA Station" = 16,
      "CTA Transfer Station" = 1,
      "Divvy Station" = 16
    )
  ) +
  
  labs(
    title = "Transportation Access in Chicago",
    subtitle = "CTA rail and Divvy station locations across Chicago community areas"
  ) +
  
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display station map
station_map

# Save station map
ggsave(
  "posts/transportation_access_chi/graphs/station_map.png",
  width = 9,
  height = 9,
  dpi = 300
)

# Create relative station availability map
relative_access_map <- ggplot(access_map_long) +
  geom_sf(
    aes(fill = access),
    color = "black",
    linewidth = 0.2
  ) +
  facet_wrap(~ transportation_type) +
  scale_fill_gradient(
    low = "white",
    high = "darkgreen"
  ) +
  labs(
    title = "Transportation Access Across Chicago",
    subtitle = "Relative station availability by community area",
    fill = "Relative Station\nAvailability"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display relative access map
relative_access_map

# Save relative access map
ggsave(
  "posts/transportation_access_chi/graphs/relative_access_map.png",
  plot = relative_access_map,
  width = 10,
  height = 6,
  dpi = 300
)
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
ggsave(
  "posts/transportation_access_chi/graphs/station_map.png",
  width = 9,
  height = 9,
  dpi = 300
)
