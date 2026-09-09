library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

# Create Blue Line image folder
dir.create(
  "posts/CTA_Rideship_Blue/images-bl",
  recursive = TRUE,
  showWarnings = FALSE
)

# O'Hare station

ohare_station <- c(
  "O'Hare Airport"
)

blueline %>%
  filter(stationame %in% ohare_station) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#00A1DE",
    linewidth = 1
  ) +
  
  scale_y_continuous(
    limits = c(0, 350000),
    breaks = seq(0, 350000, by = 100000),
    labels = scales::comma
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Blue Line Ridership - O'Hare",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Blue/images-bl/blue-line-ohare.png",
  width = 10,
  height = 5,
  dpi = 300
)

# Far Northwest Side stations
far_northwest_stations <- c(
  "Rosemont",
  "Cumberland",
  "Harlem-O'Hare",
  "Jefferson Park",
  "Montrose-O'Hare",
  "Irving Park-O'Hare"
)

blueline %>%
  filter(stationame %in% far_northwest_stations) %>%
  
  # Make station order match far_northwest_stations vector
  mutate(
    stationame = factor(
      stationame,
      levels = far_northwest_stations
    )
  ) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#00A1DE",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  scale_y_continuous(
    limits = c(0, 150000),
    breaks = seq(0, 150000, by = 50000),
    labels = scales::comma
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Blue Line Ridership - Far Northwest Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Blue/images-bl/blue-line-far-nw.png",
  width = 10,
  height = 5,
  dpi = 300
)


# Northwest Side stations
northwest_stations <- c(
  "Addison-O'Hare",
  "Belmont-O'Hare",
  "Logan Square",
  "California/Milwaukee",
  "Western/Milwaukee",
  "Damen/Milwaukee"
)

blueline %>%
  filter(stationame %in% northwest_stations) %>%
  
  # Make station order match northwest_stations vector
  mutate(
    stationame = factor(
      stationame,
      levels = northwest_stations
    )
  ) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#00A1DE",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_y_continuous(
    limits = c(0, 150000),
    breaks = seq(0, 150000, by = 50000),
    labels = scales::comma
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Blue Line Ridership - Northwest Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Blue/images-bl/blue-line-nw.png",
  width = 10,
  height = 5,
  dpi = 300
)


# Near Northwest Side stations
near_northwest_stations <- c(
  "Division/Milwaukee",
  "Chicago/Milwaukee",
  "Grand/Milwaukee"
)

blueline %>%
  filter(stationame %in% near_northwest_stations) %>%
  
  # Make station order match near_northwest_stations vector
  mutate(
    stationame = factor(
      stationame,
      levels = near_northwest_stations
    )
  ) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#00A1DE",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_y_continuous(
    limits = c(0, 150000),
    breaks = seq(0, 150000, by = 50000),
    labels = scales::comma
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Blue Line Ridership - Near Northwest Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Blue/images-bl/blue-line-near-nw.png",
  width = 10,
  height = 5,
  dpi = 300
)


# Downtown stations
downtown_stations <- c(
  "Clark/Lake",
  "Washington/Dearborn",
  "Monroe/Dearborn",
  "Jackson/Dearborn",
  "LaSalle"
)

blueline %>%
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
    color = "#00A1DE",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_y_continuous(
    limits = c(0, 400000),
    breaks = seq(0, 400000, by = 100000),
    labels = scales::comma
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Blue Line Ridership - Downtown",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Blue/images-bl/blue-line-downtown.png",
  width = 10,
  height = 5,
  dpi = 300
)


# Near West Side stations
near_west_stations <- c(
  "Clinton-Forest Park",
  "UIC-Halsted",
  "Racine",
  "Medical Center",
  "Western-Forest Park",
  "Kedzie-Homan-Forest Park"
)

blueline %>%
  filter(stationame %in% near_west_stations) %>%
  
  # Make station order match near_west_stations vector
  mutate(
    stationame = factor(
      stationame,
      levels = near_west_stations
    )
  ) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#00A1DE",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_y_continuous(
    limits = c(0, 200000),
    breaks = seq(0, 200000, by = 50000),
    labels = scales::comma
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Blue Line Ridership - Near West Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Blue/images-bl/blue-line-near-w.png",
  width = 10,
  height = 5,
  dpi = 300
)


# West Side stations
west_stations <- c(
  "Pulaski-Forest Park",
  "Cicero-Forest Park",
  "Austin-Forest Park",
  "Oak Park-Forest Park",
  "Harlem-Forest Park",
  "Forest Park"
)

blueline %>%
  filter(stationame %in% west_stations) %>%
  
  # Make station order match west_stations vector
  mutate(
    stationame = factor(
      stationame,
      levels = west_stations
    )
  ) %>%
  
  ggplot(aes(
    x = month_beginning,
    y = monthtotal
  )) +
  
  geom_line(
    color = "#00A1DE",
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ stationame,
    scales = "free_y",
    ncol = 3
  ) +
  
  scale_y_continuous(
    limits = c(0, 100000),
    breaks = seq(0, 100000, by = 20000),
    labels = scales::comma
  ) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  
  labs(
    title = "Blue Line Ridership - West Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Blue/images-bl/blue-line-west.png",
  width = 10,
  height = 5,
  dpi = 300
)
