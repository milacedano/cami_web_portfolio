library(ggplot2)
library(dplyr)

# Load Red Line ridership data
ctastations <- read.csv(
  "posts/CTA_Rideship_Red/CTA-ridership-routes.csv"
)

# Prepare data
ctastations$month_beginning <- as.Date(
  ctastations$month_beginning,
  format = "%m/%d/%Y"
)

ctastations$monthtotal <- as.numeric(
  gsub(",", "", ctastations$monthtotal)
)

# Filter Red Line stations
redline <- ctastations %>%
  filter(grepl("Red", route))
# -------------------------------------------------------
# FAR NORTH SIDE
# -------------------------------------------------------

far_north_stations <- c(
  "Howard",
  "Jarvis",
  "Morse",
  "Loyola",
  "Granville",
  "Thorndale",
  "Bryn Mawr",
  "Berwyn"
)

redline %>%
  filter(stationame %in% far_north_stations) %>%
  mutate(
    stationame = factor(
      stationame,
      levels = far_north_stations
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
    ncol = 2,
    labeller = labeller(
      stationame = c(
        "Howard" = "Howard",
        "Jarvis" = "Jarvis",
        "Morse" = "Morse",
        "Loyola" = "Loyola",
        "Granville" = "Granville",
        "Thorndale" = "Thorndale",
        "Bryn Mawr" = "Bryn Mawr",
        "Berwyn" = "Berwyn"
      )
    )
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
    title = "Red Line Ridership - Far North Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Red/images/red-line-super-north.png",
  width = 10,
  height = 7,
  dpi = 300
)


# -------------------------------------------------------
# NORTH SIDE
# -------------------------------------------------------

north_stations <- c(
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
    ncol = 2,
    labeller = labeller(
      stationame = c(
        "Argyle" = "Argyle",
        "Lawrence" = "Lawrence",
        "Wilson" = "Wilson",
        "Sheridan" = "Sheridan",
        "Addison-North Main" = "Addison",
        "Belmont-North Main" = "Belmont",
        "Fullerton" = "Fullerton"
      )
    )
  ) +
  scale_y_continuous(
    limits = c(0, 300000),
    breaks = seq(0, 300000, by = 50000),
    labels = scales::comma
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
  "posts/CTA_Rideship_Red/images/red-line-north.png",
  width = 10,
  height = 7,
  dpi = 300
)


# -------------------------------------------------------
# NEAR NORTH / DOWNTOWN
# -------------------------------------------------------

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
    ncol = 3,
    labeller = labeller(
      stationame = c(
        "North/Clybourn" = "North/Clybourn",
        "Clark/Division" = "Clark/Division",
        "Chicago/State" = "Chicago",
        "Grand/State" = "Grand",
        "Lake/State" = "Lake",
        "Monroe/State" = "Monroe",
        "Jackson/State" = "Jackson",
        "Harrison" = "Harrison",
        "Roosevelt" = "Roosevelt"
      )
    )
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
  "posts/CTA_Rideship_Red/images/red-line-downtown.png",
  width = 10,
  height = 7,
  dpi = 300
)


# -------------------------------------------------------
# SOUTH SIDE
# -------------------------------------------------------

south_stations <- c(
  "Cermak-Chinatown",
  "Sox-35th-Dan Ryan",
  "47th-Dan Ryan",
  "Garfield-Dan Ryan"
)

redline %>%
  filter(stationame %in% south_stations) %>%
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
    ncol = 2,
    labeller = labeller(
      stationame = c(
        "Cermak-Chinatown" = "Cermak-Chinatown",
        "Sox-35th-Dan Ryan" = "Sox-35th",
        "47th-Dan Ryan" = "47th",
        "Garfield-Dan Ryan" = "Garfield"
      )
    )
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
  "posts/CTA_Rideship_Red/images/red-line-south.png",
  width = 10,
  height = 7,
  dpi = 300
)


# -------------------------------------------------------
# FAR SOUTH SIDE
# -------------------------------------------------------

far_south_stations <- c(
  "63rd-Dan Ryan",
  "69th",
  "79th",
  "87th",
  "95th/Dan Ryan"
)

redline %>%
  filter(stationame %in% far_south_stations) %>%
  mutate(
    stationame = factor(
      stationame,
      levels = far_south_stations
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
    ncol = 2,
    labeller = labeller(
      stationame = c(
        "63rd-Dan Ryan" = "63rd",
        "69th" = "69th",
        "79th" = "79th",
        "87th" = "87th",
        "95th/Dan Ryan" = "95th/Dan Ryan"
      )
    )
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
    title = "Red Line Ridership - Far South Side",
    x = NULL,
    y = "Monthly Ridership"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "posts/CTA_Rideship_Red/images/red-line-super-south.png",
  width = 10,
  height = 7,
  dpi = 300
)
