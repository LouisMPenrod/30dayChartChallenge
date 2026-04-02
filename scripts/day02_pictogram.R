# Day 2 - Pictogram

# Concept: Pictogram showing number of acres of oyster restoration
# by region.


library(httr)
library(jsonlite)
library(tidyverse)
library(rphylopic)
library(patchwork)
library(ggh4x)
library(ggtext)

# identify range from https://www.habitat.noaa.gov/apps/restoration-atlas/index.html
# Get path from site page:
# F12 -> Network -> Reload -> top option which is the id number
# Headers -> RequestURL.

# fetch_project <- function(id) {
#   url <- paste0("https://restorationdata.noaa.gov/rcdb_services/rcdb/project/", id)
  
#   response <- GET(url, user_agent("Mozilla/5.0"))
  
#   if (status_code(response) != 200) return(NULL)
  
#   content(response, "text", encoding = "UTF-8") |>
#     fromJSON(flatten = TRUE) |>
#     list()
# }

# max_id <- 8000  # adjust if needed

# raw <- map(1:max_id, possibly(fetch_project, NULL))

# # Filter out NULLs
# raw <- compact(raw)

# # Extract flat fields and oyster-relevant nested fields
# parse_project <- function(proj) {
#   p <- proj[[1]]

#   # Check if oyster strategy
#   is_oyster <- if (!is.null(p$strategies)) {
#     any(grepl("oyster", p$strategies$name, ignore.case = TRUE))
#   } else FALSE
  
#   if (!is_oyster) return(NULL)
  
#   # Remove DARRP
#   is_darrp <- if (!is.null(p$program)) {
#     any(grepl("DARRP", p$program, ignore.case = TRUE))
#   } else FALSE

#   if (is_darrp) return(NULL)
  
#   # Acres from habitats
#   acres <- if (!is.null(p$habitats)) {
#     sum(as.numeric(p$habitats$actAcresCreateCnt), na.rm = TRUE)
#   } else NA_real_

#   # Year
#   year <- if (!is.null(p$habitats)) {
#     val <- max(as.numeric(p$habitats$fiscalYear, na.rm = TRUE), na.rm = TRUE)
#     if(is.infinite(val)) NA_real_ else val
#   } else NA_real_
  
#   # Total funding
#   noaa_contrib <- if (!is.null(p$funding)) {
#     sum(as.numeric(gsub(",", "", p$funding$noaaContribution)), na.rm = TRUE)
#   } else NA_real_
  
#   tibble(
#     id             = p$id,
#     name           = p$name,
#     state          = p$state %||% NA,
#     region         = p$region %||% NA,
#     status         = p$status %||% NA,
#     fed_leverage   = as.numeric(p$fedLeverage),
#     nonfed_leverage= as.numeric(p$nonFedLeverage),
#     noaa_contrib   = noaa_contrib,
#     acres          = acres,
#     fiscalYear     = year
#   )
# }

# oyster_projects <- map_dfr(raw, possibly(parse_project, NULL))

# glimpse(oyster_projects)


# oyster_projects %>%
#   summarise(
#     n = n(),
#     n_acres    = sum(!is.na(acres) & !acres == 0)
#   )

# # counts of available data
# oyster_projects %>%
#   group_by(region) |> 
#   summarise(
#     n = n(),
#     n_acres    = sum(!is.na(acres) & !acres == 0),
#     years = paste0(range(fiscalYear, na.rm = TRUE),collapse = ",")
#   )


# df <- oyster_projects |> 
#   filter(!is.na(acres) & !acres == 0)

# df <- df |> 
#   group_by(region) |> 
#   summarise(
#     n = n(),
#     sum_acres    = sum(acres),
#     years = paste0(range(fiscalYear, na.rm = TRUE),collapse = ",")
#   )

# write.csv(df, "../data/processed/day02_oysterrest_byregion.csv", row.names = FALSE)

df <- read.csv("../data/processed/day02_oysterrest_byregion.csv")

df

# Update region labels
df <- df |> 
  mutate(region = str_replace(region, "(.*) - (\\w)", "\\2"))|>
  mutate(region = factor(region, levels = c("Northwest", "Northeast",
                                            "Southwest", "Southeast")))

df

# plot

## get glyph
uuid <- "d6d7b28f-9d6c-4720-a71b-827caeaae2af"
img  <- get_phylopic(uuid = uuid)
img  <- rotate_phylopic(img = img, angle = 180) 
get_attribution(uuid = uuid, text = TRUE)

## calc position
acres_per_glyph <- 10
glyphs_per_row  <- 10

glyph_df <- df |>
  mutate(
    n_glyphs = ceiling(sum_acres / acres_per_glyph)
  ) |>
  rowwise() |>
  reframe(
    region      = region,
    sum_acres   = sum_acres,
    glyph_index = seq_len(n_glyphs)
  ) |>
  mutate(
    col_idx = (glyph_index - 1) %% glyphs_per_row,
    row_idx = (glyph_index - 1) %/% glyphs_per_row,
    x       = col_idx,
    y       = -row_idx
  )

## Need masks for partial
df

buffer <- 0.06 # fraction of image that is white space
# Image not symetric so will have to adjust for higher percents
mask_df <- tribble(
  ~region,      ~xmin,  ~xmax,  ~ymin,  ~ymax,
  "Northeast",   6.5+0.7,    7.5,   -6.5,   -5.5,   # should show 70% of the 68th glyph
  "Northwest",   1.5,    2.5,   -0.6,    0.5,   # completly cover the 3rd from the round up to 10
  "Southeast",   -0.5+0.75,    0.5,   -2.5,   -1.6,   # should show 80% of the 21st glyph
  "Southwest",   0.5+0.35+buffer,    1.5,   -0.7,    0.5    # should show 35% of the 
) |>
  mutate(region = factor(region, levels = levels(df$region)))

## theme

bg_color     <- "#ffffff"
glyph_color  <- "#0a0a0a"
mask_color   <- "white" 
title_color  <- "#0a0a0a"
caption_color <- "#555555"

theme_pictogram <- theme_void() +
  theme(
    plot.background  = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    plot.title = element_text(
      color  = title_color, family = "serif",
      size   = 20, face = "bold",
      hjust  = 0.5, margin = margin(b = 0, t = 0)
    ),
    plot.caption = element_text(
      color  = caption_color, family = "serif",
      size   = 9, hjust = 0.5,
      margin = margin(t = 3)
    ),
    plot.margin      = margin(20, 20, 16, 20),
    legend.position  = "none",
    panel.spacing    = unit(8, "pt"),
    aspect.ratio     = NULL
  )

## layout

panel_heights <- df |>
  mutate(n_glyphs = ceiling(sum_acres / acres_per_glyph),
         n_rows   = ceiling(n_glyphs / glyphs_per_row)) |>
  arrange(region) |> 
  pull(n_rows)

top_height    <- max(panel_heights[1], panel_heights[2])  
bottom_height <- max(panel_heights[3], panel_heights[4]) 

x_lim      <- c(-0.5, glyphs_per_row - 0.5)   
# y_lim_full <- c(-(top_height - 0.5), 0.5)  

region_rows <- df |>
  mutate(n_glyphs = ceiling(sum_acres / acres_per_glyph),
         n_rows   = ceiling(n_glyphs / glyphs_per_row)) |>
  select(region, n_rows)

## plot
make_panel <- function(rgn) {

  glyph_data <- glyph_df |> filter(region == rgn)
  mask_data  <- mask_df  |> filter(region == rgn)
  max_rows   <- region_rows |> 
    filter(str_starts(region, str_sub(rgn, 1, 5))) |> 
    pull(n_rows) |> 
    max()
  y_clip     <- c(-(max_rows - 0.5), 0.5)

  ggplot() +
    geom_phylopic(
      data   = glyph_data,
      aes(x  = x, y = y),
      img    = img,
      height = 1,
      fill   = glyph_color,
      alpha  = 1
    ) +
    geom_rect(
      data  = mask_data,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill  = mask_color,
    ) +
    # scale_x_continuous(limits = x_lim) +
    # scale_y_continuous(limits = y_lim_full) +
    coord_fixed(ratio = 1, xlim = x_lim, ylim = y_clip) +
    labs(title = rgn) +
    theme_pictogram +
    theme(
      plot.title = element_textbox_simple(
        family = "serif",
        face       = "bold", size = 20, hjust = 0.5, halign = 0.5,
        padding = margin(5, 4, 5, 4),
        margin     = margin(0, 0, 4, 0), box.colour = NA,
        width      = unit(1, "npc")
      )
    )
}

panels <- map(levels(df$region), make_panel)
names(panels) <- levels(df$region)

center <- 2
legend_p <- ggplot() +
  geom_phylopic(
    data   = tibble(x = center, y = 0),
    aes(x  = x, y = y),
    img    = img,
    height = 1,
    fill   = glyph_color,
    color  = NA,
    alpha  = 1
  ) +
  geom_text(
    data = tibble(x = center+0.15, y = 0),
    aes(x = x, y = y, label = "= 10 Acres Restored"),
    hjust  = 0, vjust = 0.5,
    family = "serif",
    color  = title_color, size = 4.5
  ) +
  xlim(-0.5, 5) + ylim(-0.5, 0.5) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_color, color = NA))

# combine
grid <- (panels$Northwest | panels$Northeast) /
        (panels$Southwest | panels$Southeast) +
  plot_layout(widths = c(1, 1), heights = c(top_height, bottom_height))

# Add legend
final <- legend_p / grid +
  plot_layout(heights = c(1, top_height + bottom_height)) +
  # footnote and title
  plot_annotation(
    title   = "NOAA OYSTER REEF RESTORATION BY REGION",
    caption = paste0(
      "Data: NOAA Restoration Atlas | Viz: Louis M Penrod | #30DayChartChallenge",
      "\nGlyphs: PhyloPic via {rphylopic} (Gearty & Jones 2023) by Katie S. Collins, 2018 (CC0 1.0)"
    ),
    theme = theme(
      plot.title   = element_text(color = title_color, family = "serif",
                                  size = 23, face = "bold", hjust = 0.5,
                                  margin = margin(b = 4, t = 0)),
      plot.caption = element_text(color = caption_color, family = "serif",
                                  size = 12, hjust = 0.5,
                                  margin = margin(t = 12)),
      plot.background = element_rect(fill = bg_color, color = NA)
    )
  )

final

## export

ggsave(
  filename = "../www/outputs/day02_pictogram_oysters.png",
  plot     = final,
  width    = 8,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)
