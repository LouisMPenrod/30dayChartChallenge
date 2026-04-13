# Day 13 - ecosystems

# Concept: Ecosystem of the Stardew Valley mine system
# Radial bar plots showing frequency of monsters at each level group

library(tidyverse)
library(openxlsx)
library(patchwork)
library(ggimage)
library(png)
library(grid)

# Read data ----

df <- read.xlsx("../data/raw/stardew/monsters_in_mines.xlsx")

df

# Process data ----

# Assign floor groups
floors <- bind_rows(
  data.frame(Floor = 1:29,
             zone = "Brown"),
  data.frame(Floor = 31:39,
             zone = "Gray"),  
  data.frame(Floor = 41:79,
             zone = "Frozen"),
  data.frame(Floor = 81:119,
             zone = "Lava")    
)

df <- df |> 
  left_join(floors, by = "Floor")

# pivot runs down
df <- df |> 
  mutate(Run2_Number = as.character(Run2_Number))
df <- df |> 
  pivot_longer(
    cols = starts_with("Run"),
    names_to = "Run",
    names_pattern = "Run(\\d+)_Number",
    names_transform = list(Run = as.integer),
    values_to = "Number"
  )

# clean
df <- df |> 
  mutate(Number = if_else(is.na(Number), "0", Number)) |> 
  filter(!Number == "INFESTED")

df |> count(Number)

df <- df |> 
  mutate(Number = as.numeric(Number))

# get relative abundance per zone
df <- df |>
  group_by(zone, Monster, Run) |> 
  summarise(run_total = sum(Number)) |>  # total per monster per run
  ungroup() |> 
  group_by(zone, Monster) |>
  summarise(mean_count = mean(run_total)) |>  # average across runs
  ungroup() |> 
  group_by(zone) |>
  mutate(rel_abund = mean_count / sum(mean_count)) |> 
  ungroup()

df <- df |> 
  filter(rel_abund > 0) |> 
  mutate(Monster = factor(Monster))

# ggplot(df |> filter(zone == "Brown"), aes(x = Monster, y = rel_abund)) +
#   geom_col(width = 1, linewidth = 0.3) +
#   coord_polar() +
#   scale_y_continuous(expand = c(0, 0))+
#   scale_x_discrete(drop = TRUE)

# ggplot(df |> filter(zone == "Gray"), aes(x = Monster, y = rel_abund)) +
#   geom_col(width = 1, linewidth = 0.3) +
#   coord_polar() +
#   scale_y_continuous(expand = c(0, 0))+
#   scale_x_discrete(drop = TRUE)

# ggplot(df |> filter(zone == "Frozen"), aes(x = Monster, y = rel_abund)) +
#   geom_col(width = 1, linewidth = 0.3) +
#   coord_polar() +
#   scale_y_continuous(expand = c(0, 0))+
#   scale_x_discrete(drop = TRUE)

# ggplot(df |> filter(zone == "Lava"), aes(x = Monster, y = rel_abund)) +
#   geom_col(width = 1, linewidth = 0.3) +
#   coord_polar() +
#   scale_y_continuous(expand = c(0, 0))+
#   scale_x_discrete(drop = TRUE)


# theme ----
bg_col     <- "#010102"
bar_col    <- "#FFFFFF"
label_col  <- "#C8C8C8"
axis_col   <- "#3A3A3A"
title_col  <- "#E8E8E8"

zone_colors <- c(
  Brown  = "#C4915A",
  Gray   = "#9BA8B0",
  Frozen = "#9DD4E8",
  Lava   = "#E8896A"
)

# radial function
make_radial <- function(zone_name) {
  
  d <- df |>
    filter(zone == zone_name) |>
    arrange(desc(rel_abund)) |>
    mutate(
      Monster = fct_reorder(Monster, rel_abund),
      pct_label = paste0(round(rel_abund * 100, 1), "%")
    )
  
  accent <- zone_colors[[zone_name]]
  
  # floor range label for subtitle
  floor_label <- switch(zone_name,
    Brown  = "Floors 1-29",
    Gray   = "Floors 31-39",
    Frozen = "Floors 41-79",
    Lava   = "Floors 81-119"
  )
  
  ggplot(d, aes(x = Monster, y = rel_abund)) +
    
    # Faint circular gridlines
    # geom_hline(
    #   yintercept = c(0.1, 0.2, 0.3, 0.4, 0.5),
    #   color = axis_col, linewidth = 0.3, linetype = "dotted"
    # ) +
    
    # Bars — filled with zone accent, outlined in near-white
    geom_col(
      fill      = accent,
      color     = colorspace::lighten(accent, 0.4),
      width     = 1,
      linewidth = 0.25
    ) +
    
    # Monster name labels at bar tips
    # geom_text(
    #   aes(
    #     y     = rel_abund + 0.03,
    #     label = Monster
    #   ),
    #   color    = label_col,
    #   size     = 2.8,
    #   family   = "mono",       
    #   hjust    = 0,
    #   angle    = 0           
    # ) +
    
    # Percentage labels inside bars
    # geom_text(
    #   aes(
    #     y     = rel_abund / 2,
    #     label = pct_label
    #   ),
    #   color  = bg_col,
    #   size   = 2.2,
    #   family = "mono",
    #   fontface = "bold"
    # ) +
    
    coord_polar(start = 0, clip = "off") +
    
    scale_y_continuous(
      limits = c(0, max(d$rel_abund) * 1.45), 
      expand = c(0, 0)
    ) +
    scale_x_discrete(drop = TRUE, expand = c(0,0)) +
    
    labs(
      #title    = toupper(zone_name),
      #subtitle = floor_label,
      x = NULL, y = NULL
    ) +
    
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.title = element_text(
        color    = title_col,
        size     = 11,
        face     = "bold",
        family   = "mono",
        hjust    = 0.5,
        margin   = margin(b = 2)
      ),
      plot.subtitle = element_text(
        color  = label_col,
        size   = 7.5,
        family = "mono",
        hjust  = 0.5,
        margin = margin(b = 0)
      ),
      plot.margin = margin(-40, 0, -40, 0),
      panel.grid = element_blank()
    )
}

load_png_plot <- function(path) {
  img <- png::readPNG(path)
  ggplot() +
    annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent", color = NA))
}


# make radial ----
p_brown  <- make_radial("Brown")
p_gray   <- make_radial("Gray")
p_frozen <- make_radial("Frozen")
p_lava   <- make_radial("Lava")

# get images ----
floor_brown  <- load_png_plot("../media/sd_brown_floor.png")
floor_gray   <- load_png_plot("../media/sd_gray_floor.png")
floor_frozen <- load_png_plot("../media/sd_frozen_floor.png")
floor_lava   <- load_png_plot("../media/sd_lava_floor.png")
ui_toolbar  <- load_png_plot("../media/sd_toolbar.png")
ui_energy   <- load_png_plot("../media/sd_health_bar.png")
ui_day_info <- load_png_plot("../media/sd_day_info.png")

grn_slime <- load_png_plot("../media/sd_green_slime.png")
bug <- load_png_plot("../media/sd_bug.png")
cave_fly <- load_png_plot("../media/sd_cave_fly.png")
duggy <- load_png_plot("../media/sd_duggy.png")
grub <- load_png_plot("../media/sd_grub.png")
rock_crab <- load_png_plot("../media/sd_rock_crab.png")
bat <- load_png_plot("../media/sd_bat_bright.png")
golem <- load_png_plot("../media/sd_stone_golem.png")
dust_sprite <- load_png_plot("../media/sd_dust_sprite.png")
frost_bat <- load_png_plot("../media/sd_frost_bat.png")
blue_slime <- load_png_plot("../media/sd_frost_jelly.png")
ghost <- load_png_plot("../media/sd_ghost.png")
skeleton <- load_png_plot("../media/sd_skeleton.png")
lava_bat <- load_png_plot("../media/sd_lava_bat.png")
lava_crab <- load_png_plot("../media/sd_lava_crab.png")
metal_head <- load_png_plot("../media/sd_metal_head.png")
red_slime <- load_png_plot("../media/sd_red_sludge.png")
brute <- load_png_plot("../media/sd_shadow_brute.png")
shaman <- load_png_plot("../media/sd_shadow_shaman.png")
squidkid <- load_png_plot("../media/sd_squid_kid.png")

brown_floor_num <- load_png_plot("../media/sd_brown_floor_nums.png")
gray_floor_num <- load_png_plot("../media/sd_gray_floor_nums.png")
frozen_floor_num <- load_png_plot("../media/sd_frozen_floor_nums.png")
lava_floor_num <- load_png_plot("../media/sd_lava_floor_nums.png")

# combine ----
blank <- ggplot() +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_col, color = NA),
        plot.margin = margin(0, 0, 0, 0))


main_plot <- (wrap_elements(p_brown) / wrap_elements(p_gray) / 
  wrap_elements(p_frozen) / wrap_elements(p_lava)) +
  plot_layout(heights = c(1, 1, 1, 1))&
  theme(
    plot.background = element_rect(fill = bg_col, color = NA)
  )

main_plot <- blank + wrap_elements(main_plot) + 
  plot_layout(widths = c(0.5, 0.5)) &
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin     = margin(-20, 0, -20, 0)
  )


# title plot
p_title <- ggplot() +
  labs(
    title    = "STARDEW VALLEY MINE ECOSYSTEM",
    subtitle = "Monster frequency by zone"
  ) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = bg_col, color = NA),
    plot.title = element_text(
      color    = title_col,
      size     = 22,
      face     = "bold",
      family   = "mono",
      hjust    = 0
    ),
    plot.subtitle = element_text(
      color    = label_col,
      size     = 12,
      family   = "mono",
      hjust    = 0,
      margin   = margin(t = 2)
    ),
    plot.margin = margin(4, 0, -2, 0)
  )


p_caption <- ggplot() +
  annotate(
    "text",
    x     = 0.5,
    y     = 0.5,
    label = "Data: Stardew Valley gameplay | Viz: Louis M Penrod | #30DayChartChallenge",
    color  = label_col,
    family = "mono",
    size   = 9/ggplot2:::.pt,
    angle  = 90,
    hjust  = 0.5,
    vjust  = 1
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin     = margin(0, 0, 0, 0)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)


main_plot <- (p_title / wrap_elements(main_plot)) +
  plot_layout(heights = c(0.05, 0.95)) & 
  theme(plot.background = element_rect(fill = bg_col, color = NA))

main_plot <- patchwork::wrap_elements(main_plot)



img_left <- 0.1
img_right <- img_left + 0.27
lava_start <- 0.05
frozen_start <- 0.27
gray_start <- 0.5
brown_start <- 0.72
img_hgt <- 0.17

num_hgt <- 0.03
num_width <- 0.11
lava_num <- 0.115
frozen_num <- 0.33
gray_num <- 0.56
brown_num <- 0.79


final_plot <- main_plot +
  # do floors reverse incase overlap
  # lava floor
  inset_element(
    floor_lava,
    left     = img_left,
    bottom   = lava_start,
    right    = img_right,
    top      = lava_start + img_hgt,
    align_to = "full"
  ) +
  # frozen floor
  inset_element(
    floor_frozen,
    left     = img_left,
    bottom   = frozen_start,
    right    = img_right,
    top      = frozen_start + img_hgt,
    align_to = "full"
  ) +
  # gray floor
  inset_element(
    floor_gray,
    left     = img_left,
    bottom   = gray_start,
    right    = img_right,
    top      = gray_start + img_hgt,
    align_to = "full"
  ) +
  # brown floor
  inset_element(
    floor_brown,
    left     = img_left,
    bottom   = brown_start,
    right    = img_right,
    top      = brown_start + img_hgt,
    align_to = "full"
  ) +
  # footnote (on right side)
  inset_element(
  p_caption,
  left     = 0.97,
  bottom   = 0.1,
  right    = 1,
  top      = 0.9,
  align_to = "full"
)+
  # day info
  inset_element(
    ui_day_info,
    left     = 0.82,
    bottom   = 0.84,
    right    = 1.00,
    top      = 1.00,
    align_to = "full"
  ) +
  # tool bar
  inset_element(
    ui_toolbar,
    left     = 0.25,
    bottom   = 0.00,
    right    = 0.75,
    top      = 0.05,
    align_to = "full"
  ) +
  # energy bar
  inset_element(
    ui_energy,
    left     = 0.90,
    bottom   = 0.00,
    right    = 1.00,
    top      = 0.17,
    align_to = "full"
  )+inset_element(
    lava_floor_num,
    left     = 0.425,
    bottom   = lava_num,
    right    = 0.425+num_width,
    top      = lava_num + num_hgt,
    align_to = "full"
  )+inset_element(
    frozen_floor_num,
    left     = 0.43,
    bottom   = frozen_num,
    right    = 0.43+num_width,
    top      = frozen_num + num_hgt,
    align_to = "full"
  )+inset_element(
    gray_floor_num,
    left     = 0.43,
    bottom   = gray_num,
    right    = 0.43+num_width,
    top      = gray_num + num_hgt,
    align_to = "full"
  )+inset_element(
    brown_floor_num,
    left     = 0.43,
    bottom   = brown_num,
    right    = 0.43+num_width,
    top      = brown_num + num_hgt,
    align_to = "full"
  ) +inset_element(
    rock_crab,
    left     = 0.75,
    bottom   = 0.82,
    right    = 0.79,
    top      = 0.85,
    align_to = "full"
  )+inset_element(
    duggy,
    left     = 0.78,
    bottom   = 0.78,
    right    = 0.82,
    top      = 0.81,
    align_to = "full"
  )+inset_element(
    cave_fly,
    left     = 0.75,
    bottom   = 0.74,
    right    = 0.79,
    top      = 0.77,
    align_to = "full"
  )+inset_element(
    bug,
    left     = 0.66,
    bottom   = 0.715,
    right    = 0.70,
    top      = 0.745,
    align_to = "full"
  )+inset_element(
    grub,
    left     = 0.58,
    bottom   = 0.78,
    right    = 0.62,
    top      = 0.81,
    align_to = "full"
  )+inset_element(
    grn_slime,
    left     = 0.64,
    bottom   = 0.87,
    right    = 0.68,
    top      = 0.90,
    align_to = "full"
  )+inset_element(
    bug,
    left     = 0.79,
    bottom   = 0.59,
    right    = 0.83,
    top      = 0.62,
    align_to = "full"
  )+inset_element(
    bat,
    left     = 0.70,
    bottom   = 0.48,
    right    = 0.76,
    top      = 0.52,
    align_to = "full"
  )+inset_element(
    golem,
    left     = 0.57,
    bottom   = 0.59,
    right    = 0.61,
    top      = 0.63,
    align_to = "full"
  )+inset_element(
    ghost,
    left     = 0.75,
    bottom   = 0.36,
    right    = 0.79,
    top      = 0.40,
    align_to = "full"
  )+inset_element(
    skeleton,
    left     = 0.78,
    bottom   = 0.30,
    right    = 0.82,
    top      = 0.35,
    align_to = "full"
  )+inset_element(
    frost_bat,
    left     = 0.715,
    bottom   = 0.27,
    right    = 0.75,
    top      = 0.30,
    align_to = "full"
  )+inset_element(
    blue_slime,
    left     = 0.61,
    bottom   = 0.31,
    right    = 0.65,
    top      = 0.34,
    align_to = "full"
  )+inset_element(
    dust_sprite,
    left     = 0.62,
    bottom   = 0.41,
    right    = 0.66,
    top      = 0.44,
    align_to = "full"
  )+inset_element(
    squidkid,
    left     = 0.75,
    bottom   = 0.14,
    right    = 0.79,
    top      = 0.17,
    align_to = "full"
  )+inset_element(
    lava_crab,
    left     = 0.78,
    bottom   = 0.11,
    right    = 0.82,
    top      = 0.14,
    align_to = "full"
  )+inset_element(
    shaman,
    left     = 0.78,
    bottom   = 0.06,
    right    = 0.82,
    top      = 0.10,
    align_to = "full"
  )+inset_element(
    metal_head,
    left     = 0.72,
    bottom   = 0.05,
    right    = 0.76,
    top      = 0.08,
    align_to = "full"
  )+inset_element(
    lava_bat,
    left     = 0.64,
    bottom   = 0.07,
    right    = 0.69,
    top      = 0.10,
    align_to = "full"
  )+inset_element(
    brute,
    left     = 0.62,
    bottom   = 0.10,
    right    = 0.66,
    top      = 0.15,
    align_to = "full"
  )+inset_element(
    red_slime,
    left     = 0.65,
    bottom   = 0.19,
    right    = 0.69,
    top      = 0.22,
    align_to = "full"
  ) & theme(plot.background = element_rect(fill = bg_col, color = NA))





# final_plot

ggsave(
  "../www/outputs/day13_ecosystems_startdew_mines.png",
  plot   = final_plot,
  width  = 8,
  height = 12,
  dpi    = 300,
  bg     = bg_col
)