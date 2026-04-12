
# Day 12 - Theme Day: FlowingDate

library(icesSAG)
library(tidyverse)
library(ggtext)
library(showtext)
font_add_google("IBM Plex Mono", "ibm")
showtext_auto()
showtext_opts(dpi = 300)

all_2024 <- getListStocks(year = 2024)

groundfish_stocks <- all_2024 |>
  filter(str_detect(StockKeyLabel, "^(cod|had|whg|ple|sol|hke|pol|tur|wit|dab|fle)\\.")) |>
  mutate(stock_class = "Groundfish")

pel_stocks <- all_2024 |>
  filter(str_detect(StockKeyLabel, "^(mac|her|bli|alf|swo|bft|alb)\\.")) |>
  mutate(stock_class = "Pelagics")

forage_stocks <- all_2024 |>
  filter(str_detect(StockKeyLabel, "^(san|spr|cap|pil)\\.")) |>
  mutate(stock_class = "Forage Fish")

shellfish_stocks <- all_2024 |>
  filter(str_detect(StockKeyLabel, "^(nep|csk|scal|lob|cra|oys|mus)\\.")) |>
  mutate(stock_class = "Shellfish")

all_stock_meta <- bind_rows(groundfish_stocks, pel_stocks, forage_stocks, shellfish_stocks)

# get SAG data

get_sag_keys <- function(stocks) {
  getSAG(stock = stocks |> pull(StockKeyLabel) |> unique(), year = 2024) |>
    select(StockKeyLabel = fishstock, AssessmentKey) |>
    distinct()
}

all_keys <- map_dfr(
  list(groundfish_stocks, pel_stocks, forage_stocks, shellfish_stocks),
  get_sag_keys
) |>
  pull(AssessmentKey) |>
  unique() |>
  na.omit()

dl_all <- map_dfr(all_keys, ~{
  getStockDownloadData(.x) |>
    select(AssessmentKey, StockKeyLabel, Year,
           FishingPressure, FishingPressureDescription, FishingPressureUnits,
           FMSY) |>
    mutate(across(everything(), as.character))
})

dl_all |> count(FishingPressureDescription, FishingPressureUnits)
# Keep only F or Fproxy, and those not in %
dl_all <- dl_all |> 
  filter(
  FishingPressureDescription %in% c("F", "Fproxy"),
  !FishingPressureUnits %in% c("%")
)
dl_all |> count(FishingPressureDescription, FishingPressureUnits)


df_plot <- dl_all |>
  mutate(
    Year            = as.integer(Year),
    FishingPressure = as.numeric(FishingPressure),
    FMSY            = as.numeric(FMSY)
  ) |>
  filter(Year == 2023, !is.na(FishingPressure), !is.na(FMSY)) |>
  left_join(
    all_stock_meta |> select(StockKeyLabel, StockDescription, stock_class, SpeciesName) |> distinct(),
    by = "StockKeyLabel"
  ) |>
  filter(!is.na(stock_class), stock_class != "Shellfish") |>
  mutate(
    common_name = str_extract(StockDescription, "^[^(]+") |> str_trim(),
    F_diff      = FishingPressure - FMSY,
    over        = FishingPressure > FMSY,
    bar_color   = if_else(over, "red", "steelblue")
  ) |>
  mutate(stock_class = factor(stock_class, levels = c("Forage Fish","Pelagics", "Groundfish"))) |> 
  arrange(stock_class, common_name, F_diff) |>
  mutate(
    y_int      = row_number(),
    show_label = !duplicated(common_name, fromLast = TRUE)
  )

band_df <- df_plot |>
  group_by(stock_class) |>
  summarise(ymin = min(y_int) - 0.5, ymax = max(y_int) + 0.5) |>
  mutate(fill = rep(c("grey92", "grey97"), length.out = n()))

y_labels <- df_plot |>
  mutate(label_text = if_else(show_label, common_name, "")) |>
  select(y_int, label_text) |>
  deframe()

arrow_ann <- df_plot |>
  group_by(common_name) |>
  filter(n() > 3) |>
  summarise(
    y_label = y_int[show_label],
    y_min   = min(y_int),
    y_max   = max(y_int)
  ) |>
  filter((y_max - y_min) > 0) 


# theme based on flowing data outputs
theme_flowing <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text                = element_text(family = "ibm"),

      # Titles
      plot.title          = element_text(family = "ibm", face = "bold",
                                         size = base_size + 4,
                                         color = "#1a1a1a",
                                         margin = margin(b = 4)),
      plot.subtitle = element_markdown(
            size      = base_size,
            color     = "#444444",
            face      = "italic",
            lineheight = 1.2,
            margin    = margin(b = 2)
),
      plot.caption        = element_text(family = "ibm",
                                         size = base_size - 2,
                                         color = "#888888",
                                         hjust = 1,
                                         margin = margin(t = 8, b = 2)),

      # Axes
      axis.title          = element_text(family = "ibm", size = base_size,
                                         color = "#333333"),
      axis.text           = element_text(family = "ibm", size = base_size - 1,
                                         color = "#555555"),
      axis.ticks.y        = element_blank(),

      # Grid — vertical (x-axis) only
      panel.grid.major.x  = element_line(color = "#e8e8e8", linewidth = 0.35),
      panel.grid.major.y  = element_blank(),
      panel.grid.minor    = element_blank(),

      # Margins
      plot.margin         = margin(t = 12, r = 16, b = 6, l = 30)
    )
}

p <- ggplot(df_plot) +
  geom_rect(data = band_df,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
            alpha = 1) +
  scale_fill_identity() +
  geom_segment(aes(x = FMSY, xend = FishingPressure,
                   y = y_int, yend = y_int,
                   color = bar_color), linewidth = 0.8) +
  geom_point(aes(x = FMSY, y = y_int),
             shape = 21, size = 2.5, fill = "white", color = "grey40") +
  geom_point(aes(x = FishingPressure, y = y_int, color = bar_color),
             size = 2.5) +
  geom_text(data = band_df,
            aes(x = Inf, y = (ymin + ymax) / 2, label = stock_class),
            hjust = 1.1, fontface = "bold", size = 4, color = "grey40",
          family = "ibm") +
  # dumbbell annotations
  annotate("text",
           x = 1, y = 40,
           label = "F[MSY]", parse = TRUE,
           size = 3.5, colour = "grey40", hjust = 0.5,
          family = "ibm") +
  annotate("text",
           x = 0.46, y = 40,
           label = "F",
           size = 3.5, colour = "gray40", hjust = 0.5,
          family = "ibm") +
  # color meaning annotations
  annotate("text",
           x = 0.7, y = 31.5,
           label = "Within safe limits",
           size = 3.7, colour = "steelblue", hjust = 0,
          fontface = "bold",
          family = "ibm") +
  annotate("curve",
           x = 0.69, xend = 0.62,
           y = 31.5, yend = 38,
           colour = "steelblue", linewidth = 0.35,
           curvature = -0.25,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text",
           x = 0.7, y = 29.5,
           label = "Overfished",
           size = 3.7, colour = "red", hjust = 0,
          fontface = "bold",
          family = "ibm") +
  annotate("curve",
           x = 0.69, xend = 0.62,
           y = 29.5, yend = 23,
           colour = "red", linewidth = 0.4,
           curvature = 0.25,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  # stock arrows
  geom_segment(data = arrow_ann,
               aes(x = -0.045, xend = -0.045,
                   y = y_label - 0.7, yend = y_min),
               colour = "grey60", linewidth = 0.4,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"))+
  scale_color_identity() +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_continuous(
    breaks = as.integer(names(y_labels)),
    labels = y_labels,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  coord_cartesian(xlim = c(0, NA), clip = "off")+
  labs(
     title   = "FISHING MORTALITY VS. SUSTAINABLE LIMIT, 2023",
    subtitle = "Comparison of Fishing Mortality (F) vs Maximum Sustainable<br>Yield (F<sub>MSY</sub>) — by stock (species & location)",
    x        = "Fishing Mortality (F)",
    y        = NULL,
    caption  = "Data: ICES Stock Assessment Graphs via {icesSAG} R package\nViz: Louis M Penrod | #30DayChartChallenge"
  ) +
theme_flowing()

ggsave("../www/outputs/day12_flowing_fmsy.png",
       p,
       width = 8, height = 10,
       dpi = 300,
       bg = "white")
