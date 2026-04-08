# Day 8 - Circular

# Set of 3 circular plots showing each of the PADI dive tables.
# Table 1 shows nitrogen loading
# Table 2 shows offloading
# Table 3 shows residual load added to new load of subsequent dive

library(openxlsx)
library(tidyverse)
library(geomtextpath)
library(patchwork)

# Data ----

df1 <- read.xlsx("../data/raw/padi/table1.xlsx")
df2 <- read.xlsx("../data/raw/padi/table2.xlsx")
df3 <- read.xlsx("../data/raw/padi/table3.xlsx")

# Table 1 ----
## Data prep ----
df1 <- df1 |> 
  arrange(Depth, Bottom.Time) |>
  group_by(Depth) |>
  mutate(
    seg  = Bottom.Time - lag(Bottom.Time, default = 0),
    xmax = cumsum(seg),
    xmin = xmax - seg
  ) |>
  ungroup()

depths <- distinct(df1, Depth) |>
        arrange(Depth) |>
        mutate(ymin = lag(Depth, default = 0), ymax = Depth)

df1 <- df1 |> 
  left_join(
    depths,
    by = "Depth"
  ) |>
  mutate(
    Group = factor(Group, levels = LETTERS),
    Depth = factor(Depth, levels = sort(unique(Depth), decreasing = TRUE))
  )

spiral_df <- df1 |>
  group_by(Group, Depth) |>
  summarise(x = max(xmax), ymin = first(ymin), ymax = first(ymax), .groups = "drop") |>
  arrange(Group, Depth) |>
  group_by(Group) |>
  mutate(x_next = lead(x), ymax_next = lead(ymin)) |>
  filter(!is.na(x_next)) |>
  ungroup() |> 
  group_by(Group) |>
  group_modify(~{
    approx(
      x = c(.x$ymax, .x$ymax_next),
      y = c(.x$x,    .x$x_next),
      n = 5
    ) |>
      as_tibble() |>
      rename(y = x, x = y)
  }) |>
  ungroup()

label_segments <- spiral_df |>
  group_by(Group) |>
  slice_min(y) |>
  ungroup() |>
  mutate(flipped = Group %in% c("X", "Y", "Z"))

# Time breaks you want to label
time_breaks <- seq(0, 200, 50)

# Get y position on spiral for each time break using approx
spiral_for_ticks <- tibble(
  x = seq(0, 205, length.out = 205),
  y = seq(180, 100, length.out = 205)
)

tick_df <- tibble(
  x     = time_breaks,
  y     = approx(spiral_for_ticks$x, spiral_for_ticks$y, xout = time_breaks)$y,
  label = time_breaks
) |>
  mutate(y_tick = y - 8)


## plot ----
pt1 <- ggplot(df1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Group)) +
  geom_rect(show.legend = FALSE) +
  geom_path(
    data = spiral_df,
    aes(x = x, y = y, group = Group),
    color = "gray40",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = label_segments,
    aes(x = x, xend = x, y = y, yend = -10),
    color = "gray40",
    linewidth = 0.4,
    inherit.aes = FALSE)+
  geom_text(
    data = label_segments,
    aes(x = x, y = -18, label = Group,
        angle = ifelse(flipped, 45, 0)),
    vjust = 0.5,
    color = "gray40",
    size = 3,
    inherit.aes = FALSE)+
  # fix axis placement in polar
  geom_text(
    data = bind_rows(depths, tibble(Depth = 0, ymin = 0, ymax = 0)) |> 
      mutate(ymax = if_else(Depth == 35, 30, Depth)),
    aes(x = 0, y = ymax, label = Depth),
    hjust = 1,
    size = 2.5,
    color = "gray40",
    inherit.aes = FALSE)+
  # Circle
geom_path(
  data = tibble(x = seq(0, 205, length.out = 205), y = seq(180, 100, length.out = 205)),
  aes(x = x, y = y),
  inherit.aes = FALSE,
  color = "gray40",
  linewidth = 0.4) +
# Tick marks for bottom time axis
geom_segment(
  data = tick_df,
  aes(x = x, xend = x, y = y, yend = y_tick),
  inherit.aes = FALSE,
  color = "gray40",
  linewidth = 0.4
) +
# Labels for bottom time axis
geom_text(
  data = tick_df,
  aes(x = x, y = y_tick - 8, label = label),
  size = 2.5,
  color = "gray40",
  inherit.aes = FALSE
) +
  annotate("text", x = 180, y = 155, label = "Bottom Time\n(min)", size = 3, color = "gray40")+
  annotate("text", x = 285, y = 75, label = "Depth (ft)", size = 3, color = "gray40", angle = 90)+
  scale_y_reverse(limits = c(180, -20)) +
  scale_x_continuous(limits = c(0, 300))+
  # scale_fill_manual(values = colorRampPalette(c("#FFE5E5", "red"))(35)) +
scale_fill_manual(values = setNames(
    colorRampPalette(c("#FFE5E5", "red"))(35)[1:26],
    LETTERS
  ))+
  coord_polar(theta = "x", clip = "off")+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA))


# Table 2 ----
## Data prep ----

df2 <- df2 |>
  mutate(
    starting_group = factor(starting_group, levels = LETTERS),
    ending_group   = factor(ending_group,   levels = LETTERS),
    starting_time  = period_to_seconds(hm(starting_time)) / 60,
    ending_time    = period_to_seconds(hm(ending_time))   / 60
  ) |>
  arrange(starting_group, starting_time) |>
  group_by(starting_group) |>
  mutate(
    ymin = as.numeric(starting_group) - 0.5,
    ymax = as.numeric(starting_group) + 0.5
  ) |>
  ungroup()

# check only one minute off
df2 |>
  arrange(starting_group, starting_time) |>
  group_by(starting_group) |>
  mutate(
    expected_start = lag(ending_time) + 1,
    gap_ok         = is.na(expected_start) | starting_time == expected_start
  ) |>
  filter(!gap_ok) |>
  select(starting_group, starting_time, ending_time, ending_group, expected_start)
 
# close 1 minute gap
df2 <- df2 |> 
  group_by(starting_group) |> 
  mutate(ending_time = if_else(row_number() == n(), ending_time, ending_time + 1)) |> 
  ungroup()

# add spiral
spiral_df2 <- df2 |>
  group_by(ending_group, starting_group) |>
  summarise(x = max(ending_time), ymin = first(ymin), ymax = first(ymax), .groups = "drop") |>
  arrange(ending_group, starting_group) |>
  group_by(ending_group) |>
  filter(n() > 1) |>
  group_modify(~{
    approx(
      x = c(.x$ymin, .x$ymax),
      y = c(.x$x,    .x$x),
      n = 20
    ) |>
      as_tibble() |>
      rename(y = x, x = y)
  }) |>
  ungroup()

# grab z with is dropped
spiral_single <- df2 |>
  group_by(ending_group, starting_group) |>
  summarise(x = max(ending_time), ymin = first(ymin), ymax = first(ymax), .groups = "drop") |>
  group_by(ending_group) |>
  filter(n() == 1) |>
  ungroup() |>
  rowwise() |>
  mutate(path = list(tibble(x = c(x, x), y = c(ymin, ymax)))) |>
  select(ending_group, path) |>
  unnest(path) |>
  ungroup()
 
spiral_df2 <- bind_rows(spiral_df2, spiral_single)

# ending group labels
label_segments2 <- spiral_df2 |>
  group_by(ending_group) |>
  slice_max(y) |>
  ungroup() |>
  mutate(flipped = FALSE) |> 
  mutate(ending_group = if_else(ending_group %in% c("Q","S","U","W","Y"), "", ending_group))

# starting group labels
group_labels <- df2 |>
  distinct(starting_group, ymin, ymax)  |>
  mutate(
    x = ifelse(as.numeric(starting_group) %% 2 == 0, -6, 0)
  )

# time breaks
time_breaks2 <- seq(0, 360, 60)

# time spiral
outer_edge <- df2 |>
  group_by(starting_group) |>
  summarise(x = max(ending_time) + 20, y = first(ymin) - 3, .groups = "drop") |>
  arrange(starting_group) |>
  filter(x > 180 + 20) 
 
spiral_for_ticks2 <- bind_rows(
  tibble(x = c(0, 180), y = c(-4, -4)),   
  outer_edge  
) |>
  arrange(x)
 
tick_df2 <- tibble(
  x     = time_breaks2,
  y     = approx(spiral_for_ticks2$x, spiral_for_ticks2$y, xout = time_breaks2)$y,
  label = time_breaks2
) |>
  mutate(y_tick = y + 0.4)  

# additional lines form base to spiral start
h_lines2 <- spiral_df2 |>
  group_by(ending_group) |>
  slice_min(y) |>
  slice(1) |>
  ungroup()

## Plot ----
pt2 <- ggplot(df2, aes(xmin = starting_time, xmax = ending_time, ymin = ymin, ymax = ymax, fill = ending_group)) +
  geom_rect(show.legend = FALSE) +
  # spiral and lines
  geom_segment(
    data = h_lines2,
    aes(x = 0, xend = x, y = y, yend = y),
    color     = "gray40",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  geom_path(
    data = spiral_df2,
    aes(x = x, y = y, group = ending_group),
    color     = "gray40",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  # spiral ticks and labels for ending group
  geom_segment(
    data = label_segments2,
    aes(x = x, xend = x, y = y, yend = 28),
    color     = "gray40",
    linewidth = 0.4,
    inherit.aes = FALSE
  )+
  geom_text(
    data = label_segments2,
    aes(x = x, y = 30, label = ending_group,
        angle = ifelse(flipped, 45, 0)),
    vjust = 0.7,
    color = "gray40",
    size  = 3,
    inherit.aes = FALSE
  ) +
  # starting group labels
  geom_text(
    data = group_labels,
    aes(x = x, y = ymax, label = starting_group),
    hjust = -0.2,
    vjust = 1,
    color = "gray40",
    size = 2.5,
    inherit.aes = FALSE
  ) +
  # time spiral
  geom_path(
    data = spiral_for_ticks2,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    color = "gray40",
    linewidth   = 0.4
  ) +
  geom_segment(
    data = tick_df2,
    aes(x = x, xend = x, y = y, yend = y_tick),
    inherit.aes = FALSE,
    color = "gray40",
    linewidth   = 0.4
  ) +
  geom_text(
    data = tick_df2,
    aes(x = x, y = y+2, label = label),
    size = 2.5,
    color = "gray40",
    inherit.aes = FALSE
  ) +
  annotate("text", x = 360, y = 3, label = "Surface Interval\n(min)", size = 3.5, color = "gray40") +
  annotate("text", x = 485, y = 15, label = "Plot 1 Group", size = 3, color = "gray40", angle = -90)+
  scale_y_continuous(limits = c(-8, 32)) +
  scale_x_continuous(limits = c(-2, 500)) + # will clip half the starting groups
  scale_fill_manual(values = setNames(
    colorRampPalette(c("#FFE5E5", "red"))(35)[1:26],
    LETTERS
  )) +
  coord_polar(theta = "x", clip = "off", direction = -1) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

## Tabls 3

start_depth <- 30

df3 <- df3 |>
  mutate(
    StartingGroup = factor(StartingGroup, levels = LETTERS),
    # Bin RNT to nearest 10 min
    RNT_bin = ceiling(RNT / 10) * 10
  ) |>
  arrange(StartingGroup, Depth) |>
  group_by(StartingGroup) |>
  mutate(
    ymin = as.numeric(StartingGroup) - 0.5,
    ymax = as.numeric(StartingGroup) + 0.5
  ) |>
  ungroup() |> 
  mutate(
  xmin = lag(Depth, default = start_depth),
  xmax = Depth,
  .by = StartingGroup
)

# spiral
spiral_df3 <- df3 |>
  group_by(RNT_bin, StartingGroup) |>
  summarise(
    x    = min(xmax),
    y    = first(ymax),
    .groups = "drop"
  ) |>
  arrange(RNT_bin, StartingGroup)

# if bin below has a higher y, use that y and interp x from bin below and above
vals <- unique(spiral_df3$RNT_bin)
for(dn in seq_along(vals)){

  # check if lower has higher y
  if(dn > 1){
  sub_y <- spiral_df3 |> filter(RNT_bin == vals[dn]) |> pull(y) |> max()
  sub_x <- spiral_df3 |> filter(RNT_bin == vals[dn]) |> pull(x) |> max()
  sub_lower_y <- spiral_df3 |> filter(RNT_bin == vals[dn-1]) |> pull(y) |> max()
  sub_lower_x <- spiral_df3 |> filter(RNT_bin == vals[dn-1]) |> pull(x) |> max()
  if(sub_y < sub_lower_y){

  sub_upper_x <- spiral_df3 |> filter(RNT_bin == vals[dn+1]) |> pull(x) |> max()
    ((sub_upper_x-sub_lower_x)/2)+sub_lower_x

    df <- data.frame(RNT_bin = vals[dn],
               y = sub_lower_y,
               x = ((sub_upper_x-sub_lower_x)/2)+sub_lower_x)
    
    spiral_df3 <- bind_rows(spiral_df3,
                            df) 
  }
  }   
}

spiral_df3 <- spiral_df3 |> arrange(RNT_bin, StartingGroup)

spiral_multi3 <- spiral_df3 |>
  group_by(RNT_bin) |>
  filter(n() > 1) |>
  group_modify(~{
    approx(
      x = .x$y,
      y = .x$x,
      n = 20
    ) |>
      as_tibble() |>
      rename(y = x, x = y)
  }) |>
  ungroup()

spiral_single3 <- spiral_df3 |>
  group_by(RNT_bin) |>
  filter(n() == 1) |>
  ungroup() |>
  rowwise() |>
  mutate(path = list(tibble(x = c(x, x), y = c(y - 0.5, y)))) |>
  select(RNT_bin, path) |>
  unnest(path) |>
  ungroup()

spiral_df3 <- bind_rows(spiral_multi3, spiral_single3)

# Labels at spiral tip per StartingGroup
label_segments3 <- spiral_df3 |>
  group_by(RNT_bin) |>
  slice_max(y) |>
  ungroup() |> 
  group_by(x) |> 
  summarise(y = max(y),
RNT_bin = paste0(RNT_bin, collapse = "/")) |> 
  ungroup() |> 
  mutate(x_lbl = case_when(#RNT_bin == "50/60" ~ x + 1.5,
RNT_bin == "210" ~ x - 1,
RNT_bin == "170" ~ x + 0.5,
RNT_bin == "160" ~ x + 1,
RNT_bin %in% c("90","100") ~ x + 0.7,
RNT_bin == "120/130/140" ~ x + 4,
TRUE~x),
y_lbl = case_when(RNT_bin == "120/130/140" ~ y + 4,
  RNT_bin == "50/60" ~ y + 2.5,
  RNT_bin == "190" ~ y + 2.5,
  RNT_bin == "170" ~ y + 1,
  RNT_bin %in% c("160","210") ~ y,
TRUE ~ y+0.5))

group_labels3 <- df3 |>
  group_by(StartingGroup) |>
  summarise(
    x    = min(xmin),
    ymin = first(ymin),
    ymax = first(ymax)-0.5,
    .groups = "drop"
  )  |>
  mutate(
    x = ifelse(as.numeric(StartingGroup) %% 2 == 0, -6, start_depth)
  )

tick_df3 <- tibble(
  x     = c(unique(df3$Depth)),
  y     = -4,
  label = c(unique(df3$Depth))
) |>
  mutate(y_tick = y + 0.4)


pt3 <- ggplot(df3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = StartingGroup)) +
  geom_rect(show.legend = FALSE) +
  geom_path(
    data = spiral_df3,
    aes(x = x, y = y, group = RNT_bin),
    color = "gray40",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = label_segments3,
    aes(x = x, xend = x, y = y, yend = y+0.5),
    color = "gray40",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = label_segments3,
    aes(x = x_lbl, y = y_lbl+1.5, label = RNT_bin),
    size = 2.5,
    color = "gray40",
    inherit.aes = FALSE
  ) +
  # starting group
  geom_text(
  data = group_labels3,
  aes(x = x, y = ymax, label = StartingGroup),
  hjust = 1,
  size = 2.5,
  color = "gray40",
  inherit.aes = FALSE
)+
  # depth line
  geom_segment(y = -4, yend = -4, x = start_depth, xend = 130,
    inherit.aes = FALSE,
    color = "gray40",
    linewidth   = 0.3)+
  geom_segment(
    data = tick_df3,
    aes(x = x, xend = x, y = y, yend = y_tick),
    inherit.aes = FALSE,
    color = "gray40",
    linewidth   = 0.4
  ) +
  geom_text(
    data = tick_df3,
    aes(x = x, y = y+2.5, label = label),
    size = 2.5,
    color = "gray40",
    inherit.aes = FALSE
  ) +
  geom_segment(y = 38, yend = 38, x = start_depth, xend = 75,
arrow = arrow(type = "closed", unit(15, "cm")),
    inherit.aes = FALSE,
    color = "gray40",
    linewidth   = 0.4)+
geom_textpath(
  data = tibble(x = seq(start_depth+25, start_depth+35, length.out = 100), y = 34),
  aes(x = x, y = y, label = "Residual Nitrogen Time"),
  size = 4,
  color = "gray40",
  inherit.aes = FALSE
)+
  annotate("text", x = 65, y = -9.5, label = "Planned     \nDepth\n(ft)", size = 2.5, color = "gray40") +
  annotate("text", x = 163, y = 15, label = "Plot 2 Group", size = 3.5, color = "gray40", angle = 90)+
  scale_y_continuous(limits = c(-10, 38)) +
  scale_x_continuous(limits = c(30, max(df3$xmax) * 1.3)) +
  scale_fill_manual(values = setNames(
    colorRampPalette(c("#FFE5E5", "red"))(35)[1:26],
    LETTERS
  )) +
  coord_polar(theta = "x", clip = "off") +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

pt1 <- pt1 + theme(plot.margin = margin(0, 0, 0, 0))
pt2 <- pt2 + theme(plot.margin = margin(0, 0, 0, 0))
pt3 <- pt3 + theme(plot.margin = margin(0, 0, 0, 0))

layout <- ggplot() +
  theme_void() +
  
  theme(plot.margin = margin(0, 0, 0, 0)) +
  inset_element(pt1, left = -0.2, right = 0.65, top = 0.9, bottom = 0.3) +
  inset_element(pt2, left = 0.3,  right = 1.3, top = 0.9, bottom = 0.3) +
  inset_element(pt3, left = 0.2, right = 0.8, top = 0.55, bottom = -0.15)+
  inset_element(
    ggplot() + 
      theme_void() +
      labs(title = "PADI Dive Tables",
            subtitle = paste(
      c("The following is a graphical representation of each of the three PADI Dive Tables. How to read:",
    "In plot 1 (upper left) select your planned depth and bottom time to find your pressure group.",
    "Take this group to plot 2 (upper right) and your planned surface interval between dives to find your new pressure group.",
    "Take this group to plot 3 (lower middle) and your planned depth of your second dive to find the Residual Nitrogen Time (RNT).",
    "Take this RNT back to plot 1 and add it to your planned bottom time. Repeat the process for subsequent dives."),
  collapse = "\n")) +
      theme(
        plot.title    = element_text(size = 14, color = "grey30"),
        plot.subtitle = element_text(size = 8.5, color = "grey30", lineheight = 1.3)
      ),
    left = 0, right = 1, top = 1, bottom = 0.85,
    clip = FALSE
  ) +
  inset_element(
    ggplot() +
      theme_void() +
      labs(caption = "Only for educational purposes. Do not use for dive planning.\nData: PADI Dive Tables | Viz: Louis M Penrod | #30DayChartChallenge") +
      theme(
        plot.caption = element_text(size = 8.5, color = "grey30", lineheight = 1.3,
                                    margin = margin(t = 0), hjust = 1)
      ),
    left = 0, right = 1, top = 0.04, bottom = 0,
    clip = FALSE
  )

layout

ggsave("../www/outputs/day08_circular_padi.png", 
      width = 8, 
      height = 7, 
      dpi = 300,
      bg = "white"
)

