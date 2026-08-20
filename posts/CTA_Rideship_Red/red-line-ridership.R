library(ggplot2)
library(dplyr)

# North Side stations
north_stations <- c(
  "Howard",
  "Jarvis",
  "Morse",
  "Loyola",
  "Granville",
  "Thorndale",
  "Bryn Mawr",
  "Berwyn",
  "Argyle",
  "Lawrence",
  "Wilson",
  "Sheridan",
  "Addison-North Main",
  "Belmont-North Main",
  "Fullerton"
)
redline %>%
  filter(stationame %in% north_stations) %>%
  
  # Make station order match north_stations vector
  mutate(
    stationame = factor(
      stationame,
      levels = north_stations
    )
  ) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#C60C30",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Red Line Ridership - North Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(
  "images/red-line-north.png",
  width = 10,
  height = 7,
  dpi = 300
)
# Downtown / Near North stations
downtown_stations <- c(
  "North/Clybourn",
  "Clark/Division",
  "Chicago/State",
  "Grand/State",
  "Lake/State",
  "Monroe/State",
  "Jackson/State",
  "Harrison",
  "Roosevelt"
)

redline %>%
  filter(stationame %in% downtown_stations) %>%
  
  # Make station order match downtown_stations vector
  mutate(
    stationame = factor(
      stationame,
      levels = downtown_stations
    )
  ) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#C60C30",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Red Line Ridership - Near North/Downtown",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(
  "images/red-line-downtown.png",
  width = 10,
  height = 7,
  dpi = 300
)
# South Side stations
south_stations <- c(
  "Cermak-Chinatown",
  "Sox-35th-Dan Ryan",
  "47th-Dan Ryan",
  "Garfield-Dan Ryan",
  "63rd-Dan Ryan",
  "69th",
  "79th",
  "87th",
  "95th/Dan Ryan"
)
redline %>%
  filter(stationame %in% south_stations) %>%
  
  # Make station order match north_stations vector
  mutate(
    stationame = factor(
      stationame,
      levels = south_stations
    )
  ) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#C60C30",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Red Line Ridership - South Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(
  "images/red-line-south.png",
  width = 10,
  height = 7,
  dpi = 300
)
