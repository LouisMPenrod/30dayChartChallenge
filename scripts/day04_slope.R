# Day 4 - Slope

# Make a survival curve with alone data.
# Split by Season to show difference
# Differences could be caused by more difficult location or less experienced contestants.

library(alone)
library(tidyverse)
library(rphylopic)

df <- alone::survivalists

# only US version
# Drop season 4 since pairs


df <- df |>
  filter(version == "US",
         !season == 4) %>% 
  mutate(season = factor(season))

df |> count(version)
df |> count(season)


season_locations <- alone::seasons |>
  filter(version == "US",
         !season == 4) %>% 
  select(season, location) |> 
  # switch out some with broader locations
  mutate(location = case_when(
    season == 5 ~ "Mongolia",
    season == 8 ~ paste0(location, ", BC"),
    season == 9 ~ "Labrador",
    season == 10 ~ paste0(location, ", SK"),
    season == 11 ~ "Arctic Circle",
    season == 12 ~ "Africa",
    TRUE ~ str_replace(location, "Island", "Is.")
  )) |> 
    mutate(season = factor(season))

season_locations

# check dists
# ggplot(df, aes(x = days_lasted, fill = season))+
#   geom_density(alpha = 0.25)
  

df |> 
  group_by(season) |> 
  summarise(min = min(days_lasted,na.rm = TRUE),
            max = max(days_lasted, na.rm = TRUE))




# expand
times <- expand.grid(
  day = 0:100
)

nrow(df)
df <- df |> 
  group_by(season) |> 
  mutate(max_days = max(days_lasted)) |> 
  ungroup() |> 
  cross_join(times)
nrow(df)


# filter down to max day and fill
df <- df |> 
  filter(day <= days_lasted) |> 
  select(id, name, season, days_lasted, day)

df_raw <- df

#check
df |> 
  filter(id == "US007") |> 
  select(days_lasted, day) |> 
  print(n = Inf)



df <- df |> 
  group_by(season, day) |> 
  summarise(n_remain = n_distinct(id)) |> 
  ungroup()

df <- df |>
  group_by(season) |>
  arrange(season, day) |>
  mutate(n_remain_next = lead(n_remain)) |>
  # add a duplicate row at same day with the next count value
  bind_rows(
    df |>
      group_by(season) |>
      arrange(season, day) |>
      mutate(n_remain_next = lead(n_remain)) |>
      filter(!is.na(n_remain_next) & n_remain_next != n_remain) |>
      mutate(n_remain = n_remain_next)
  ) |>
  select(-n_remain_next) |>
  arrange(season, day, desc(n_remain)) |>
  ungroup()


# get labels for season and annotations
label_df <- df |>
  group_by(season) |>
  slice_max(day, n = 1, with_ties = FALSE) |>
  ungroup() |>
  left_join(season_locations, by = "season") |>
  mutate(
    label     = paste0("S", season),
    sublabel  = location,
    n_remain = 0.8,
    day = case_when(season %in% c(9) ~ day + 2,
                    season %in% c(10) ~ day + 1.8,
                    season == 2 ~ day - 3,
                    season %in% c(8) ~ day - 2,
                    season %in% c(6) ~ day - 1,
                    season %in% c(1,5) ~ day - 2,
                    season %in% c(11) ~ day + 1,
                    season %in% c(3) ~ day + 2,
                    TRUE ~ day)
  )

roland <- df |>
  filter(season == 7, day == 100)

# plot

## colors
season_colors <- c(
  "1"  = "#7F77DD",
  "2"  = "#1D9E75",
  "3"  = "#D85A30",
  "5"  = "#D4537E",
  "6"  = "#BA7517",
  "7"  = "#378ADD",
  "8"  = "#639922",
  "9"  = "#993C1D",
  "10" = "#888780",
  "11" = "#B8A9F0",
  "12" = "#E24B4A"
)

season_rank <- df |>
  distinct(season) |>
  arrange(season) |>
  mutate(rank = row_number()) 

offset_amount <- 0.04
n_seasons <- nrow(season_rank)

df_plot <- df |>
  left_join(season_rank, by = "season") |>
  mutate(
    n_remain = if_else(
      n_remain == 1,
      n_remain,
      n_remain + (rank - (n_seasons + 1) / 2) * offset_amount
    )
  )

theme_alone <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "#2B3A42", color = NA),
      panel.background = element_rect(fill = "#2B3A42", color = NA),
      panel.grid.major = element_line(color = "#3D5260", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title    = element_text(
        color    = "white",
        face     = "bold",
        size     = 16,
        family   = "sans",
        hjust    = 0,
        margin   = margin(b = 4)
      ),
      plot.subtitle = element_text(
        color  = "#8BAAB8", 
        size   = 13,
        hjust  = 0,
        margin = margin(b = 12)
      ),
      plot.caption = element_text(color = "#8BAAB8", size = 9, hjust = 0.5),
      axis.title   = element_text(color = "white", size = 12, face = "bold"), 
      axis.text    = element_text(color = "#8BAAB8", size = 10),
      axis.ticks   = element_line(color = "#3D5260"),
      plot.margin  = margin(16, 16, 12, 12)
    )
}

# get wolf glyph
uuid <- "9c311a13-a032-400d-b7dc-5e18f392e286"
get_attribution(uuid = uuid, text = TRUE)

p <- ggplot(df_plot, aes(x = day, y = n_remain, color = season)) +
 geom_phylopic(
            uuid  = uuid,
            x     = 80,
            y     = 8,
            size = 2.5,
            color = "white",
            horizontal = TRUE
          )+
  # cover gridlines
  annotate("rect",
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = 1,
    fill = "#2B3A42"   
  ) +
  geom_path(
    data  = filter(df_plot),
    linewidth = 1
  ) +

  # season labels
  geom_text(
    data       = label_df,
    aes(label  = label),
    hjust      = 1,
    vjust      = 0.4,
    fontface   = "bold",
    size       = 3.5,
    angle = 45,
    show.legend = FALSE
  ) +
  geom_text(
    data       = label_df,
    aes(label  = sublabel),
    hjust      = 1,
    vjust      = 1.6,
    size       = 3.5,
    angle = 45,
    show.legend = FALSE
  ) +

  # Roland annotation
  annotate(
    "segment",
    x = 95, xend = 95, y = 1.05, yend = 4.5,
    color = "#378ADD", linewidth = 0.75, linetype = "dotted"
  ) +
  annotate(
    "label",
    x = 95, y = 4.5,
    label    = "Roland Welker,\n'The 100 Day King',\nsurvived -40°F and\nknifed a musk ox",
    size     = 3.5, 
    fill       = "#2B3A42",     
    color      = "#378ADD",
    fontface = "bold"
  ) +

  # Africa / S12 annotation
  annotate(
    "segment",
    x = 33, xend = 15, y = 1.5, yend = 1,
    color = "#E24B4A", linewidth = 0.75, linetype = "dotted"
  ) +
  annotate(
    "label",
    x = 15, y = 1,
    label = "S12: Africa lasted\njust 34 days.\nHeat, unfamiliar\nterrain, or under-\nvetted contestants?",
    size = 3.5, 
    fill = "#2B3A42",
    color = "#E24B4A",
    fontface = "bold"
  ) +

  scale_color_manual(values = season_colors) +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0.01, 0.05)) 
  ) +
  scale_y_continuous(
    breaks = 1:10,
    expand = expansion(mult = c(0.1, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 10))+
  labs(
  title    = "ALONE — CONTESTANT TAP-OUTS BY SEASON",
  subtitle = "US version  |  Season 4 (pairs) excluded",
  x        = "D A Y",
  y        = "C O N T E S T A N T S   R E M A I N I N G",
  caption  = paste0("Data: {alone} R package | Viz: Louis M Penrod | #30DayChartChallenge",
  "\nGlyphs: PhyloPic via rphylopic (Gearty & Jones 2023) by Margot Michaud, 2018 (CC0 1.0)")
  ) +
  theme_alone() +
  theme(
    legend.position  = "none"
  )

# p

ggsave("../www/outputs/day04_slope_alone.png", 
      p, 
      width = 8, 
      height = 8, 
      dpi = 300,
      bg = "#2B3A42")

