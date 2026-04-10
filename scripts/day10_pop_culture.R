# Day 10 - Pop culture

# Scatterplot of wingspan vs weight of avian pokemon and real animals

library(pokemon)
library(tidyverse)
library(ggimage)
library(grid)
library(patchwork)

# pokemon

df <- pokemon::pokemon


# see what has flying
df_sub <- df |> filter(type_1 == "normal" & type_2 == "flying")

# filter to bird (Normal-flying)
df <- df |> 
  filter((type_1 == "flying" | type_2 == "flying") & 
          !type_1 %in% c("dragon", "poison", "ghost") &
          !type_2 %in% c("dragon") &
          !egg_group_1 %in% c("bug", "dragon", "fairy","humanshape","mineral") &
          !egg_group_2 %in% c("bug","dragon","fairy", "plant") & 
          !is.na(generation_id)) |> 
  select(id, pokemon, type_1, type_2, egg_group_1, egg_group_2, height, weight, url_image)

df |> count(type_1, type_2)

df |> pull(pokemon)

# drop non-winged and special
df <- df |> 
  filter(!pokemon %in% c("doduo", "dodrio", "mantine", "mantyke", "woobat", "swoobat", "sigilyph",
                          "emolga", "celesteela") &
    !stringr::str_detect(pokemon, "incarnate"))

# x <- df |> pull(url_image)

# for(i in x){
#   browseURL(i)
# }

df_sub$pokemon[!df_sub$pokemon %in% df$pokemon]


df <- df |> 
  mutate(wingspan = 2.5*height) |> 
  rename(Species = pokemon) |> 
  select(Species, weight, wingspan) |> 
  mutate(source = "Pokemon")
# pokemon weight is in kg, wingspan (derived from height) in meters


# birds

df_bird <- read.csv("../data/raw/BirdWingData/BirdWingData_tidy_ver2.1.csv")

df_bird <- df_bird |> 
  mutate(weight = body.mass_g / 1000) |> 
  select(Species = Species_IOC13.1, wingspan = wingspan_m, weight) |> 
  drop_na(wingspan, weight)

nrow(df_bird) == length(unique(df_bird$Species))

df_bird |> count(Species) |> head()

# pokemon only gives one measurement so assume max
df_bird <- df_bird |> 
  group_by(Species) |> 
  summarise(weight = mean(weight, na.rm = TRUE),
            wingspan = mean(wingspan, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(source = "bird")

# combine
df <- bind_rows(df, df_bird)

# Add path to image
df <- df |>
  mutate(img = if_else(source == "Pokemon", "../media/pokeball.png", NA_character_))


# theme
theme_pokedex <- function() {
  theme_minimal() %+replace%
    theme(
      plot.background = element_rect(fill = "#cc1414", color = "#cc1414"),
      panel.background = element_rect(fill = "#64b1bb", color = NA),
      panel.grid.major = element_line(color = "#2255aa", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#d2d3d5", fill = NA, linewidth = 3),
      axis.text = element_text(color = "#ffffff", family = "mono", size = 12),
      axis.title = element_text(color = "#ffffff", face = "bold", family = "mono", size = 14),
      axis.title.x = element_text(margin = margin(t=3)),
      plot.title = element_text(color = "#ffffff", face = "bold", family = "mono",
                                        size = 20 , margin = margin(b = 6), hjust = 0.5),
      plot.subtitle = element_text(color = "#ffffff", family = "mono", 
                                        size = 12, margin = margin(b = 20), hjust = 0.5),
      plot.caption = element_text(color = "#ffffff", size = 9, hjust = 1,
                                  margin = margin(t=20, b = 0)),
      legend.background  = element_rect(fill = "#51ad60", color = "#1a2a3a"),
      legend.text = element_text(color = "#1a2a3a", family = "mono", size = 10),
      legend.title = element_text(color = "#1a2a3a", face = "bold", family = "mono", size = 12),
      plot.margin = margin(20, 12, 5, 12),
      legend.position = "bottom",
      legend.justification = c(0,0)
    )
}

main_plot <- ggplot(df, aes(x = weight, y = wingspan)) +
  geom_point(
    data  = filter(df, source != "Pokemon"),
    color = "black",
    size  = 1
  ) +
  geom_image(
    data = filter(df, source == "Pokemon"),
    aes(image = img),
    size = 0.06
  ) +
  scale_x_log10(breaks = c(10^(-2:2)),
                labels = scales::label_number()) +
  scale_y_log10(expand = expansion(mult = c(0.2, 0.2))) +
  labs(x = "Weight (kg)",
       y = "Wingspan (m)",
       title = "Pokémon follow similar wingspan-weight\nscaling as real birds, just bigger!",
       subtitle = "Pokémon wingspan estimated as 2.5 × height.",
       caption  = paste0("Data: {pokemon} R package and BirdWingData (Shiomi et al. [2024])", 
       "\nViz: Louis M Penrod | #30DayChartChallenge")
  )+
  theme_pokedex() +
  theme(legend.position = "none")



legend_df <- data.frame(
  x     = c(0.75, 3.35),
  x_lbl = c(1, 3.65),
  y     = c(1.3, 1.3),
  label = c("Real Bird", "Pokémon"),
  img   = c(NA, "../media/pokeball.png")
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y)) +
  geom_point(
    data  = filter(legend_df, label == "Real Bird"),
    color = "black",
    size  = 5
  ) +
  geom_image(
    data = filter(legend_df, label == "Pokémon"),
    aes(image = img),
    size = 0.4
  ) +
  geom_text(
    aes(x = x_lbl, label = label),
    hjust = 0,
    size  = 4,
    color = "#1a2a3a",
    family = "mono"
  ) +
  xlim(0.5, 5) +
  ylim(0, 3) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#51ad60", color = "#1a2a3a", linewidth = 1),
    panel.background = element_rect(fill = "#51ad60", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),

  )


circle_grob <- grobTree(
  circleGrob(
    x  = unit(0.5, "npc"),
    y  = unit(0.5, "npc"),
    r  = unit(18, "points"),
    gp = gpar(fill = "#3a6fd8", col = "#d2d3d5", lwd = 4)
  ),
  # outer glare ring
  circleGrob(
    x  = unit(0.46, "npc"),
    y  = unit(0.61, "npc"),
    r  = unit(8, "points"),
    gp = gpar(fill = "#6a9fe8", col = NA)
  ),
  # inner glare spot
  circleGrob(
    x  = unit(0.46, "npc"),
    y  = unit(0.61, "npc"),
    r  = unit(4, "points"),
    gp = gpar(fill = "#aaccf5", col = NA)
  )
)

final_plot <- (main_plot + inset_element(
  legend_plot,
  left = -0.05,
  bottom = -0.35,  
  right = 0.3,   
  top = -0.10,      
  align_to = "panel"
) +
  inset_element(
    wrap_elements(full = circle_grob) & 
      theme(plot.background = element_rect(fill = "transparent", color = NA)),,
    left     = -0.13,   
    bottom   =  1.06,   
    right    =  0.06,   
    top      =  1.40,  
    align_to = "panel"
  )) & theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave("../www/outputs/day10_pop_culture_pokemonvbird.png", 
      final_plot, 
      width = 8, 
      height = 5, 
      dpi = 300,
      bg = "#cc1414")
