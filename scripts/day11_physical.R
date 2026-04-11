library(httr)
library(jsonlite)
library(readr)
library(tidyverse)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(glue)
library(ggtext)
library(png)
library(grid)

# install.packages("hurricaneexposuredata", 
#                  repos = "https://geanders.github.io/drat")
# install.packages("hurricaneexposure")
library(hurricaneexposuredata)
library(hurricaneexposure)

# arcgis_base <- "https://permitting.sjrwmd.com/arcgis/rest/services/SVC/wsd/MapServer"
 
# # IDs to query
# layers <- list(
#   surface_water = 13,
#   groundwater   = 16
# )
 

# arcgis_fields <- paste(
#   "HYDRON_NUMBER", "SHORT_NAME", "LONG_NAME",
#   "COUNTY", "STATION_TYPE", "DATA_TYPE", "GROUND_WATER_TYPE",
#   "LATITUDE", "LONGITUDE", "LAT_NO", "LONG_NO",
#   "MAJOR_SW_BASIN", "MINOR_SW_BASIN", "STTS_CD",
#   sep = ","
# )
 
# # download URL
# hds_url <- "http://webapub.sjrwmd.com/agws10/hdsnew/data.aspx"
 
# datum  <- "NAVD1988"
# period <- "POR"  # Period of Record
 
# output_dir <- "../data/raw/sjrwmd"
# # dir.create(output_dir, showWarnings = FALSE)
 
# arcgis_headers <- c(
#   "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
# )
 
# hds_headers <- c(
#   "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
#   "Referer"    = "http://webapub.sjrwmd.com/agws10/hdsnew/"
# )
 
# query_layer <- function(layer_id, layer_name) {
#   message(sprintf("Querying layer %d (%s)...", layer_id, layer_name))
 
#   url <- sprintf("%s/%d/query", arcgis_base, layer_id)
 
#   resp <- GET(
#     url,
#     query = list(
#       where             = "1=1",
#       outFields         = arcgis_fields,
#       returnGeometry    = "false",
#       resultRecordCount = "2000",
#       f                 = "json"
#     ),
#     add_headers(.headers = arcgis_headers),
#     timeout(30)
#   )
 
#   stop_for_status(resp)
 
#   data <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON()
#   df   <- data$features$attributes
 
#   if (nrow(df) == 2000) {
#     warning(sprintf("Layer %d hit the 2000-record limit — may need pagination.", layer_id))
#   }
 
#   df <- df |> mutate(LAYER = layer_name, LAYER_ID = layer_id)
#   message(sprintf("  -> %d stations found", nrow(df)))
#   df
# }

# all_stations <- bind_rows(
#   query_layer(layers$surface_water, "Surface Water"),
#   query_layer(layers$groundwater,   "Groundwater")
# )

# # check
# all_stations |> count(STATION_TYPE)
# all_stations |> filter(STATION_TYPE == "Well") |> count(GROUND_WATER_TYPE)
# all_stations |> count(STATION_TYPE, MINOR_SW_BASIN)

# # Filter to MINOR_SW_BASIN contains "St. Johns"
# # Filter Well to GROUND_WATER_TYPE = "FAS - Upper Floridan aquifer"

# all_stations <- all_stations |> 
#   filter(str_detect(MINOR_SW_BASIN, "St\\. Johns"))
# all_stations |> count(STATION_TYPE, MINOR_SW_BASIN)

# all_stations <- all_stations |> 
#   filter(STATION_TYPE == "Surface Water" | 
#     (STATION_TYPE == "Well" & GROUND_WATER_TYPE == "FAS - Upper Floridan aquifer"))
# all_stations |> filter(STATION_TYPE == "Well") |> count(GROUND_WATER_TYPE)

# all_stations <- all_stations |>
#   filter(!is.na(HYDRON_NUMBER), HYDRON_NUMBER != "") |>
#   distinct(HYDRON_NUMBER, .keep_all = TRUE)

# station_list_path <- file.path(output_dir, "station_list.csv")
# write_csv(all_stations, station_list_path)


# station_ids <- all_stations$HYDRON_NUMBER

# # download data


# download_station <- function(station_id, des_string) {
#   stn_param <- paste(station_id, datum, period, sep = "_")
 
#   resp <- tryCatch(
#     GET(
#       hds_url,
#       query = list(tp = "1", stn = stn_param, des = des_string),
#       add_headers(.headers = hds_headers),
#       timeout(60)
#     ),
#     error = function(e) {
#       message("  [ERROR] ", station_id, ": ", conditionMessage(e))
#       return(NULL)
#     }
#   )
 
#   if (is.null(resp) || http_error(resp)) return(NULL)
 
#   raw_bytes <- content(resp, as = "raw")
#   if (length(raw_bytes) == 0) return(NULL)
 
#   # Strip null byte separator between metadata and data sections
#   clean_text <- rawToChar(raw_bytes[raw_bytes != as.raw(0)])
#   lines      <- strsplit(clean_text, "\n")[[1]]
#   data_start <- which(grepl("^STATION NUMBER,TYPE", lines, ignore.case = TRUE))
 
#   if (length(data_start) == 0) {
#     message("  [SKIP] ", station_id, " — no data rows found")
#     return(NULL)
#   }
 
#   df <- tryCatch(
#     read_csv(I(paste(lines[data_start:length(lines)], collapse = "\n")),
#              show_col_types = FALSE,
#              col_types = cols(.default = "c")),
#     error = function(e) {
#       message("  [WARN] ", station_id, ": couldn't parse CSV")
#       return(NULL)
#     }
#   )
 
#   if (is.null(df) || nrow(df) == 0) return(NULL)
 
#   mutate(df, HYDRON_NUMBER = station_id, .before = 1)
# }

# all_data <- list()
# total    <- nrow(all_stations)
 
# for (i in seq_len(total)) {
#   sid <- all_stations$HYDRON_NUMBER[i]
#   message(sprintf("[%d/%d] Station %s ...", i, total, sid))
 
#   result <- download_station(sid, all_stations$des_param[i])
 
#   if (!is.null(result)) {
#     all_data[[as.character(sid)]] <- result
#     message("  -> ", nrow(result), " rows")
#   }
 
#   Sys.sleep(0.5)
# }


# irma_start <- as.Date("2017-08-27")  # 2 weeks before
# irma_end   <- as.Date("2017-09-24")  # 2 weeks after
 
# combined <- bind_rows(all_data) |>
#   left_join(
#     all_stations |> select(HYDRON_NUMBER, SHORT_NAME, LONG_NAME, COUNTY,
#                             STATION_TYPE, DATA_TYPE, GROUND_WATER_TYPE,
#                             LATITUDE, LONGITUDE, LAYER),
#     by = "HYDRON_NUMBER"
#   ) |>
#   mutate(DATE = as.Date(DATE, format = "%m/%d/%Y")) |>
#   filter(DATE >= irma_start & DATE <= irma_end)
 
# combined_path <- file.path(output_dir, "IRMA_WATER_LEVEL.csv")
# write_csv(combined, combined_path)

# summary(combined)


# combine <- read.csv("../data/raw/sjrwmd/IRMA_WATER_LEVEL.csv")

# combine |> count(QUALITY.CODE, QUALITY.DESCRIPTION)

# combine <- combine |> 
#   filter(QUALITY.CODE < 130)
# combine |> count(QUALITY.CODE, QUALITY.DESCRIPTION)

# summarise_columns <- function(df, n = 15) {
#   for (col in names(df)) {
#     vals <- df[[col]]
#     n_unique <- n_distinct(vals, na.rm = TRUE)
#     n_missing <- sum(is.na(vals))
    
#     cat(sprintf("\n── %s [%s] | %d unique | %d missing ──\n",
#                 col, class(vals)[1], n_unique, n_missing))
    
#     if (n_unique <= n) {
#       counts <- sort(table(vals, useNA = "no"), decreasing = TRUE)
#       for (nm in names(counts)) {
#         cat(sprintf("  %-30s  n = %d\n", nm, counts[[nm]]))
#       }
#     } else {
#       if (is.numeric(vals)) {
#         cat(sprintf("  min = %s, max = %s\n",
#                     format(min(vals, na.rm = TRUE)),
#                     format(max(vals, na.rm = TRUE))))
#       } else {
#         cat(sprintf("  sample: %s\n",
#                     paste(head(unique(na.omit(vals)), 5), collapse = ", ")))
#       }
#     }
#   }
# }


# summarise_columns(combine)

# combine |> count(GROUND_WATER_TYPE, LAYER)

# # only keep GROUND_WATER_TYPE is FAS - Upper Floridan aquifer
# combine <- combine |> 
#   filter(GROUND_WATER_TYPE == "FAS - Upper Floridan aquifer" | is.na(GROUND_WATER_TYPE))
# combine |> count(GROUND_WATER_TYPE)

# colnames(combine)

# glimpse(combine)

# combine <- combine |>
#   mutate(DATE = as.Date(DATE)) |> 
#   rename(NAVD88 = WATER.LEVEL..ft.NAVD88.)

# combine <- combine |> 
#   mutate(LAYER = factor(LAYER, levels = c("Surface Water", "Groundwater")))


# # get distance to coast.

# # Get Florida coastline
# florida <- ne_states(country = "united states of america", returnclass = "sf") |>
#   filter(name == "Florida")

# # Convert to just the boundary (coastline)
# fl_coast <- st_boundary(florida)

# # Convert stations to sf points (WGS84)
# stations_sf <- combine |>
#   filter(!is.na(LATITUDE), !is.na(LONGITUDE)) |>
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# # Project both to a planar CRS for accurate distance in meters
# # EPSG 3087 = Florida-specific Albers Equal Area
# stations_proj <- st_transform(stations_sf, crs = 3087)
# coast_proj    <- st_transform(fl_coast,    crs = 3087)

# # Calculate distance from each station to the coastline (in meters)
# dist_m <- st_distance(stations_proj, coast_proj)

# # Add to station data
# combine <- combine |>
#   filter(!is.na(LATITUDE), !is.na(LONGITUDE)) |>
#   mutate(
#     dist_to_coast_m  = as.numeric(dist_m),
#     dist_to_coast_km = round(dist_to_coast_m / 1000, 2)
#   )

# range(combine$dist_to_coast_km)

# # get hurricane track and calc min dist to eye
# data("hurr_tracks")
# hurr_tracks |> filter(str_detect(storm_id, "Irma")) |> count(storm_id)
# irma_track <- hurr_tracks |>
#   filter(storm_id == "Irma-2017") |>
#   mutate(date = as.Date(as.character(date), format = "%Y%m%d%H%M"))

# irma_sf <- irma_track |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
#   st_transform(3087)

# stations_unique <- combine |>
#   distinct(HYDRON_NUMBER, LATITUDE, LONGITUDE) |>
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |>
#   st_transform(3087)

# irma_daily <- irma_track |>
#   group_by(date) |>
#   slice(1) |>  # one per day
#   ungroup()

# combine <- combine |>
#   left_join(irma_daily |> select(date, longitude, latitude),
#             by = c("DATE" = "date")) |>
#   rename(irma_lon = longitude, irma_lat = latitude)

# # Calculate distance row-wise
# compute_dist_to_irma <- function(sta_lon, sta_lat, irma_lon, irma_lat) {
#   mapply(function(slon, slat, ilon, ilat) {
#     if (any(is.na(c(slon, slat, ilon, ilat)))) return(NA_real_)
#     p1 <- st_sfc(st_point(c(slon, slat)), crs = 4326) |> st_transform(3087)
#     p2 <- st_sfc(st_point(c(ilon, ilat)), crs = 4326) |> st_transform(3087)
#     as.numeric(st_distance(p1, p2)) / 1000  # km
#   }, slon = sta_lon, slat = sta_lat, ilon = irma_lon, ilat = irma_lat)
# }

# combine <- combine |>
#   mutate(dist_to_irma_km = compute_dist_to_irma(LONGITUDE, LATITUDE, irma_lon, irma_lat))

# # get min dist
# all_dates <- combine |> distinct(DATE)
# all_hydrons <- combine |> distinct(HYDRON_NUMBER, LATITUDE, LONGITUDE)

# full_grid <- tidyr::crossing(all_hydrons, all_dates) |>
#   left_join(irma_track |> select(date, longitude, latitude),
#             by = c("DATE" = "date")) |>
#   rename(irma_lon = longitude, irma_lat = latitude) |>
#   mutate(dist_to_irma_km = compute_dist_to_irma(LONGITUDE, LATITUDE, irma_lon, irma_lat))

# min_dist_irma <- full_grid |>
#   group_by(HYDRON_NUMBER) |>
#   summarise(min_dist_to_irma_km = min(dist_to_irma_km, na.rm = TRUE)) |>
#   ungroup()

# # Join back to combine
# combine <- combine |>
#   left_join(min_dist_irma, by = "HYDRON_NUMBER")

# range(combine$min_dist_to_irma_km, na.rm = TRUE)

# combine |> select(HYDRON_NUMBER, LONGITUDE, LATITUDE, irma_lon, irma_lat, dist_to_irma_km, min_dist_to_irma_km)

# combine |> distinct(HYDRON_NUMBER, min_dist_to_irma_km) |> arrange(desc(min_dist_to_irma_km))

# # identify those with good change
# combine <- combine |> 
#   group_by(HYDRON_NUMBER) |> 
#   mutate(change = max(NAVD88) - min(NAVD88)) |> 
#   ungroup()

# keep <- combine |> 
#   distinct(HYDRON_NUMBER, change, LAYER) |> 
#   arrange(desc(change)) |> 
#   group_by(LAYER) |> 
#   slice(1:20) |> 
#   pull(HYDRON_NUMBER)

# combine <- combine |> 
#   filter(HYDRON_NUMBER %in% c(keep))

# # Calc CFB
# combine <- combine |>
#   arrange(HYDRON_NUMBER, DATE) |> 
#   group_by(HYDRON_NUMBER) |>
#   mutate(
#     baseline     = first(NAVD88),
#     NAVD88CFB = NAVD88 - baseline,
#     NAVD88PCFB = NAVD88CFB/baseline * 100
#   ) |>
#   ungroup()


# combined_path <- file.path("../data/raw/sjrwmd/", "FINAL_IRMA_WATER_LEVEL.csv")
# write_csv(combine, combined_path)

# read data ----
combine <- read.csv("../data/raw/sjrwmd/FINAL_IRMA_WATER_LEVEL.csv")
# stations <- read.csv("../data/raw/sjrwmd/station_list.csv")

combine |> count(LAYER)

combine <- combine |> 
  mutate(LAYER = factor(LAYER, levels = c("Surface Water","Groundwater")),
         DATE = as.Date(DATE))

# plot ----

# colors
col_sw  <- "#2166ac"   # Surface Water 
col_gw  <- "#8c510a"   # Groundwater  
col_irma <- "#636363"  # Irma fill

# unofficial SJRWMD theme
theme_sjrwmd <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text
      plot.title    = element_text(face = "bold", size = base_size + 3,
                                   color = "#1a1a1a", margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = base_size + 0.5,   # ← element_markdown!
                                       color = "#444444",
                                       margin = margin(b = 10)),
      plot.caption  = element_text(size = base_size - 2, color = "#888888",
                                   hjust = 0.5, margin = margin(t = 8, b = 2)),
 
      # Axes
      axis.title    = element_text(size = base_size, color = "#333333", face = "bold"),
      axis.title.x  = element_blank(),
      axis.text     = element_text(size = base_size - 1, color = "#555555"),
      axis.text.x   = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.ticks    = element_line(color = "#cccccc", linewidth = 0.3),
 
      # Grid
      panel.grid.major   = element_line(color = "#e8e8e8", linewidth = 0.35),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(color = "#cccccc", fill = NA,
                                        linewidth = 0.5),
      panel.background   = element_rect(fill = "#fafafa", color = NA),
      plot.background    = element_rect(fill = "white",   color = NA),
 
      # Legend
      legend.position    = "bottom",
      legend.title       = element_text(size = base_size - 1, face = "bold"),
      legend.text        = element_text(size = base_size - 1),
      legend.key.width   = unit(1.8, "cm"),
      legend.key.height  = unit(0.4, "cm"),
      legend.background  = element_blank(),
      legend.margin      = margin(t = 4),
 
      # Margins
      plot.margin = margin(t = 12, r = 16, b = 6, l = 10)
    )
}

# labels
subtitle_md <- glue::glue(
  "<span style='color:{col_sw}'>**Surface Water**</span>",
  " and ",
  "<span style='color:{col_gw}'>**Groundwater**</span>",
  " two weeks pre/post landfall"
)

hurricane_img <- readPNG("../media/hurricane_icon.png")
hurricane_grob <- rasterGrob(hurricane_img, interpolate = TRUE)

p <- ggplot(
    combine,
    aes(x = DATE, y = NAVD88CFB, color = LAYER,
        alpha = min_dist_to_irma_km)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  # Irma region
  annotate(
    "rect",
    xmin  = as.Date("2017-09-10"), xmax = as.Date("2017-09-12"),
    ymin  = -Inf, ymax = Inf,
    fill  = col_irma,
    alpha = 0.4,
    color = NA
  )+
  annotate(
    "text",
    x = as.Date("2017-09-11"),
    y = 18,
    label = "Irma FL\nLandfall",
    size  = 4, color = col_irma, fontface = "bold.italic",
    hjust = 0.5, vjust = 1
  ) +
  # lines
  geom_path(aes(group = HYDRON_NUMBER), 
            linewidth = 0.45,
            show.legend = FALSE) +
  # Flood control annotation
  annotate(
    "curve",
    x = as.Date("2017-09-03"),
    y = 9,
    xend  = as.Date("2017-08-31"), 
    yend  = 5, 
    curvature = -0.25,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "#444444", linewidth = 0.5
  ) +
  annotate(
    "label",
    x = as.Date("2017-09-03"),
    y = 8.7,
    label = "Downstream of a\nFlood Control Release",
    size = 3.5, color = "#333333",
    fill = "white", label.padding = unit(0.3, "lines"),
    hjust = 0.5, vjust = 0
  ) +
  # Alpha annotation
  annotate(
    "curve",
    x = as.Date("2017-09-20"),
    y = 15,
    xend  = as.Date("2017-09-18"), 
    yend  = 11, 
    curvature = -0.25,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "#444444", linewidth = 0.5
  ) +
  annotate(
    "label",
    x = as.Date("2017-09-20"),
    y = 15,
    label = "Fainter lines are stations\nfarther from Irma's path",
    size = 3.5, color = "#333333",
    fill = "white", label.padding = unit(0.3, "lines"),
    hjust = 0.5, vjust = 0
  ) +
  #icon
  annotation_custom(
    grob   = hurricane_grob,
    xmin   = as.Date("2017-08-28"),  # ← left edge of plot
    xmax   = as.Date("2017-09-01"),  # ← controls width, adjust to taste
    ymin   = 12,                     # ← controls vertical position
    ymax   = 16.5                      # ← controls height, match your ylim top
  )+
  scale_color_manual(
    values = c("Surface Water" = col_sw, "Groundwater" = col_gw),
    name = "Layer"
  ) +
  scale_alpha_continuous(
    trans  = "reverse",
    breaks = c(5, 10, 25, 50, 75),
    range  = c(0.25, 1),
    name   = "Distance to Irma track (km)"
  ) +
  scale_y_continuous(breaks = seq(-2, 16, by = 2), 
                     expand = expansion(mult = c(0.05, 0.15))) +
  scale_x_date(
    breaks = seq(as.Date("2017-08-27"), by = "2 days", length.out = 15),
    date_labels = "%b %d",
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(ylim = c(-2, 16)) +
  labs(
    title = "Water Level Response — Hurricane Irma, 2017 — Florida",
    subtitle = subtitle_md,
    y = "Stage Height (ft NAVD88) Change from Baseline",
    caption  = "Data: St. Johns River Water Management District & {hurricaneexposuredata} R package | Viz: Louis M Penrod | #30DayChartChallenge"
  ) +
  theme_sjrwmd() +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1.2), order = 1),
    alpha = guide_legend(order = 2)
  )


ggsave("../www/outputs/day11_physical_irma.png", p, 
       width = 10, height = 6, 
       dpi = 300,
      bg = "white")
