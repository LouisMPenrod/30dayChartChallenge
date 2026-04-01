# Day 1 - Part to whole

# Makea pie chart of the fisheries landings per region.
# Make pretty by making the pie chart look like a cast net 
# If have time and space, fill the empty space to with images of the most prevelent species

library(tidyverse)
library(png)
library(grid)
library(ggforce)
library(magick)
library(geomtextpath)
library(ggtext)

# from https://s3.amazonaws.com/media.fisheries.noaa.gov/2025-01/FUS-2022-final3.pdf

df <- tibble::tribble(
  ~Region, ~Percent,
  "North Pacific", 60.7, 
  "Pacific", 8, 
  "Western Pacific", 0.4, 
  "Gulf of Mexico", 17.4, 
  "South Atlantic", 1.2, 
  "Mid-Atlantic", 6.8, 
  "New England", 5.6, 
)
ord <- unique(df$Region)

df$Region <- factor(df$Region, levels = ord)

# popular <- read.csv("data/raw/FOSS/FOSS_landings.csv", header = TRUE)

regions <- tibble::tribble(
  ~State, ~Region,
  "ALABAMA", "Gulf of Mexico",
  "ALASKA", "North Pacific",
  "CALIFORNIA", "Pacific",
  "CONNECTICUT", "New England",
  "DELAWARE", "Mid-Atlantic",
  "FLORIDA-EAST", "South Atlantic",
    "FLORIDA-WEST", "Gulf of Mexico",
    "GEORGIA", "South Atlantic",
    "HAWAII", "Western Pacific",
    "LOUISIANA",  "Gulf of Mexico",
    "MAINE", "New England",
    "MARYLAND","Mid-Atlantic",
    "MASSACHUSETTS", "New England",
    "MISSISSIPPI", "Gulf of Mexico",
    "NEW HAMPSHIRE", "New England",
    "NEW JERSEY", "Mid-Atlantic",
    "NEW YORK","Mid-Atlantic",
    "NORTH CAROLINA","South Atlantic",
     "OREGON", "Pacific",
     "PROCESS AT SEA", NA,
     "RHODE ISLAND","New England",
    "SOUTH CAROLINA", "South Atlantic",
    "TEXAS", "Gulf of Mexico",
    "VIRGINIA", "Mid-Atlantic",
    "WASHINGTON", "Pacific",
)

# popular <- popular |> 
#   left_join(regions, by = "State") |> 
#   # fix values
#   mutate(Pounds = as.numeric(stringr::str_replace_all(Pounds, ",",""))) |> 
#   # fix sci names
#   mutate(Scientific.Name = stringr::str_squish(Scientific.Name))

# popular_sum <- popular |> 
#   group_by(Region, Scientific.Name) |> 
#   summarise(sum = sum(Pounds, na.rm = TRUE)) |> 
#   ungroup() |> 
#   arrange(Region, desc(sum))

# popular_sum |> group_by(Region) |> slice(1:10)

ggplot(df, aes(x="", y=Percent, fill=Region)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

# final version

img <- readPNG("../media/cast_net_midgray.png")

net_grob <- rasterGrob(img, interpolate = TRUE, width = 1, height = 1)

rotation <- pi / 4 # to get more in line with geographic locations

df <- df |>
  arrange(desc(Region)) |> 
  mutate(
    end_angle   = 2 * pi * cumsum(Percent / sum(Percent)) + rotation,
    start_angle = lag(end_angle, default = rotation),
    mid_angle   = (start_angle + end_angle) / 2
  )


arc_points <- df |>
  rowwise() |>
  reframe({
    span  <- (end_angle - start_angle) * 0.6  # percent curve
    theta <- seq(mid_angle - span/2, mid_angle + span/2, length.out = 50)
    tibble(
      Region = Region,
      x      = 1.1 * sin(theta), # distance cruves above pie
      y      = 1.1 * cos(theta)
    )
  })


p <- ggplot(df) +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0, r0 = 0, r = 1,
      start = start_angle, end = end_angle,
      fill = Region
    ),
    alpha = 0.6,
    colour = "white", linewidth = 0.5,
    show.legend = FALSE
  ) +  
  # labels
  geom_textpath(
    data = arc_points,
    aes(x = x, y = y, label = Region, group = Region),
    size     = 4,
    hjust    = 0.5,
    vjust    = -0.15,  # space between text and underlines
    upright  = TRUE
  ) +
  annotation_custom(
    net_grob,
    xmin = -1, xmax = 1,
    ymin = -1, ymax = 1
  ) +
  labs(
    title   = expression(bold(underline("U.S. Commercial Fisheries Landings, 2022"))),
    caption = "Data: National Marine Fisheries Service\nViz: Louis M Penrod | #30DayChartChallenge"
  ) +
  coord_fixed() +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(legend.position  = "none",
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold", margin = margin(b = 0)),
    plot.caption = element_text(size = 10, color = "grey30",
                                margin = margin(r=2, b = 0)),
    plot.caption.position = "plot")
p

ggsave("../www/outputs/day01_part_to_whole_landings.png",
       p,
       width = 8, height = 9, dpi = 300,
       bg = "white"
)
