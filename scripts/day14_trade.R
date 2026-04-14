# Day 14 - Trade


library(tidyverse)
library(circlize)
library(countrycode)
library(cowplot)
library(grid)

# Reach out if you need this initial data. It is large so not wanting to put on github
# data_dir   <- "../data/raw/CITES/Trade_database_download_v2025.1/"
# output_file <- "../data/raw/CITES/cites_live_summary.csv"
# output_file_full <- "../data/raw/CITES/cites_live_summary_full.csv"
# target_years <- 2018:2022
# target_term  <- "live"

# csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
# cat(sprintf("  Found %d CSV files\n", length(csv_files)))
 
# files_with_years <- map_dfr(csv_files, function(f) {
#   cat(sprintf("  Checking: %s\n", basename(f)))
  
#   # Read only Year and Term columns to keep memory low
#   tryCatch({
#     d <- read_csv(f, col_select = c(Year, Term), show_col_types = FALSE,
#                   name_repair = "minimal")
    
#     has_target <- any(d$Year %in% target_years, na.rm = TRUE) &
#                   any(tolower(d$Term) == target_term, na.rm = TRUE)
    
#     tibble(
#       file       = f,
#       keep       = has_target,
#       year_range = paste(range(d$Year, na.rm = TRUE), collapse = "–"),
#       n_rows     = nrow(d)
#     )
#   }, error = function(e) {
#     tibble(file = f, keep = FALSE, year_range = NA, n_rows = NA)
#   })
# })

# kept_files <- files_with_years %>% filter(keep)
# cat(sprintf("\n  %d of %d files contain target years + live term\n",
#             nrow(kept_files), nrow(files_with_years)))
# print(kept_files %>% select(file, year_range, n_rows))

# cat("\nStep 2: Processing qualifying files...\n")
 
# summary_list <- map(kept_files$file, function(f) {
#   cat(sprintf("  Processing: %s\n", basename(f)))
  
#   d <- read_csv(f, show_col_types = FALSE, name_repair = "minimal") %>%
#     filter(
#       Year %in% target_years,
#       tolower(Term) == target_term
#     ) %>%
#     # Coerce Quantity to numeric (some files have NA or text)
#     mutate(Quantity = suppressWarnings(as.numeric(Quantity))) %>%
#     group_by(Year, Class, Order, Family, Taxon, Importer, Exporter, Purpose, Source) %>%
#     summarise(
#       Total_Quantity = sum(Quantity, na.rm = TRUE),
#       N_Records      = n(),
#       .groups = "drop"
#     )
  
#   cat(sprintf("    → %d summary rows\n", nrow(d)))
#   d
# })

# combined_full <- bind_rows(summary_list) %>%
#   group_by(Year, Class, Order, Family, Taxon, Importer, Exporter, Purpose, Source) %>%
#   summarise(
#     Total_Quantity = sum(Total_Quantity, na.rm = TRUE),
#     N_Records      = sum(N_Records),
#     .groups = "drop"
#   ) %>%
#   arrange(Year, Class, Taxon, Exporter, Importer)

# combined <- bind_rows(summary_list) %>%
#   group_by(Class, Importer, Exporter) %>%
#   summarise(
#     Total_Quantity = sum(Total_Quantity, na.rm = TRUE),
#     N_Records      = sum(N_Records),
#     .groups = "drop"
#   ) %>%
#   arrange(Class, Exporter, Importer)
 
# write_csv(combined, output_file)
# write_csv(combined_full, output_file_full)

df <- read.csv("../data/raw/CITES/cites_live_summary.csv")

df |> count(Class)

# Drop Dipneusti, Gastropoda, Hirudinoidea, Holothuroidea, Hydrozoa, and missing
# since less interesting to most people. Shame.
# Drop Cephalopoda and Insecta due to low record

df <- df |> 
  filter(!Class %in% c("Dipneusti", "Gastropoda", "Hirudinoidea", 
         "Holothuroidea", "Hydrozoa", "Cephalopoda", "Insecta") & !is.na(Class))

df |> count(Class)

# Check import/export
df |> count(Importer)
df |> count(Exporter)

# drop NA
df <- df |> 
  drop_na(Importer, Exporter)

df |> count(Exporter, Importer)

# get country names
df <- df |>
  mutate(
    Imp = countrycode(Importer, origin = "iso2c", destination = "country.name"),
    Exp = countrycode(Exporter, origin = "iso2c", destination = "country.name")
  )

df |> filter(is.na(Imp)) |>
  count(Importer, Imp)

df |> filter(is.na(Exp)) |>
  count(Exporter, Exp)

# Fix those that did not work
df <- df |>
  mutate(
    Imp = case_when(
      Importer == "AN" ~ "Netherlands Antilles",
      Importer == "CS" ~ "Serbia & Montenegro",
      Importer == "KV" ~ "Kosovo",
      Importer == "XV" ~ NA_character_,   # drop — not meaningful
      Importer == "XX" ~ NA_character_,   # drop — unknown origin
      TRUE ~ Imp
    ),
    Exp = case_when(
      Exporter == "HS" ~ NA_character_,   # drop — unclear
      Exporter == "XV" ~ NA_character_,
      Exporter == "XX" ~ NA_character_,
      TRUE ~ Exp
    )
  ) |>
  filter(!is.na(Imp), !is.na(Exp))

df |> summarise(
  imp_na = sum(is.na(Imp)),
  exp_na = sum(is.na(Exp))
)
nrow(df)
df |> count(Class)


# labels too long so switch back to 2 letter
df <- df |> 
  mutate(Imp = Importer, Exp = Exporter)

png("../www/outputs/day14_trade_cites.png",
    width = 10, height = 10, units = "in", res = 300, bg = "white")

par(mfrow = c(3, 3), mar = c(0.5, 0, 0.5, 0), bg = "white", cex = 1.2,
oma = c(1, 0, 3, 0))

purrr::walk(unique(df$Class), function(cls) {
  
  # Subset to class
  df_cls <- df |> filter(Class == cls)
  
  # Rank countries by total involvement (imports + exports combined)
  top_countries <- df_cls |>
    pivot_longer(c(Imp, Exp), values_to = "Country") |>
    group_by(Country) |>
    summarise(vol = sum(Total_Quantity, na.rm = TRUE)) |>
    slice_max(vol, n = 12, with_ties = FALSE) |>
    pull(Country)
  

  top_countries <- unique(c("US", top_countries))
  
  # Keep only flows where both sides are in the top set
  df_cls <- df_cls |>
    filter(Imp %in% top_countries, Exp %in% top_countries)
  
  # set up chord diagram 
  mat <- matrix(
    0,
    nrow = length(top_countries),
    ncol = length(top_countries),
    dimnames = list(sort(top_countries), sort(top_countries))
  )
  
  # Fill matrix with Total_Quantity
  for (i in seq_len(nrow(df_cls))) {
    mat[df_cls$Exp[i], df_cls$Imp[i]] <- df_cls$Total_Quantity[i]
  }

  # color to highlight US
  grid_col <- setNames(
    rep("#D0CEC8", length(top_countries)),
    sort(top_countries)
  )
  if ("US" %in% top_countries) {
    grid_col["US"] <- "#E85D24"
  }

  link_col <- matrix(
    "#D0CEC825",
    nrow = length(top_countries),
    ncol = length(top_countries),
    dimnames = list(sort(top_countries), sort(top_countries))
  )
  link_col["US", ] <- "#1D9E7599"  # US exports (row = exporter)
  link_col[, "US"] <- "#E85D2499"  # US imports (col = importer)


  # draw chord
  circos.clear()
  circos.par(
    start.degree = 90,
    clock.wise   = TRUE,
    gap.after    = 2
  )
  
  chordDiagram(
    mat,
    grid.col          = grid_col,
    col               = link_col,
    transparency      = 0,
    annotationTrack   = "grid",
    annotationTrackHeight = 0.05,
    preAllocateTracks = list(track.height = 0.15)
  )
  
  # Country labels
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    s    <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    ylim <- get.cell.meta.data("ylim")
    circos.text(
      mean(xlim), ylim[1] + 0.3, s,
      facing     = "bending.inside",
      niceFacing = TRUE,
      adj        = c(0.5, 0),
      cex        = if (s == "US") 1 else 0.5,
      col        = if (s == "US") "#E85D24" else "grey45",
      font       = if (s == "US") 2 else 1
    )
  }, bg.border = NA)
  
  title(cls, cex.main = 1.2, font.main = 2, line = -0.5)
  circos.clear()
   
})



fontsize <- 17


pieces <- tibble::tribble(
  ~txt, ~col, ~x, ~y,
  "Live Wildlife Trade: US", "black", 0.15, 0.985,
  "Imports", "#E85D24", 0.415, 0.985,
  "&", "black", 0.5095, 0.985,
  "Exports", "#1D9E75", 0.535, 0.985,
  "by Taxonomic Class", "black", 0.63, 0.985,
)

for (p in 1:nrow(pieces)) {
  grid.text(
    pieces$txt[p],
    x    = pieces$x[p],
    y    = pieces$y[p],
    just = c("left", "top"),
    gp   = gpar(fontsize = fontsize, fontface = "bold", col = pieces$col[p])
  )
}

#subtitle
grid.text(
  "Trade flows among top inter-trading countries (+US), 2018-2022",
  x  = 0.5,
  y  = 0.955,
  just = c("center", "top"),
  gp = gpar(fontsize = 16, fontface = "plain", col = "grey40")
)

# Footnote — bottom left
grid.text(
  "Data: CITES\nViz: Louis M Penrod | #30DayChartChallenge",
  x    = 0.01,
  y    = 0.01,
  just = c("left", "bottom"),
  gp   = gpar(fontsize = 11, fontface = "plain", col = "grey60")
)

dev.off()
