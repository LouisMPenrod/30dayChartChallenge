# Day 9 - Wealth

# Show Nike gross margins and SG&A through time. 
# Shade between two lines.
library(openxlsx)
library(tidyverse)
library(ggtext)
library(showtext)

df <- openxlsx::read.xlsx("../data/raw/macrotrends/nike_data.xlsx",
                              sheet = 1)

df <- df |> 
  mutate(date = lubridate::mdy(Quarter),
         year = year(date),
         month = month(date),
         quarter = case_when(month == 5 ~ 1,
                            month == 8 ~ 2,
                            month == 11 ~ 3,
                            month == 2 ~ 4),
         FYQ = paste0("FY",year,"Q",quarter))

ord <- df |> count(year,quarter,FYQ) |> pull(FYQ)

df$FYQ <- factor(df$FYQ, levels = ord)

# calc percents
df <- df |> 
  arrange(FYQ) |> 
  mutate(GMP = Gross_Profit_Margin_mil/Revenue_mil * 100,
         SGAP = SG_A_mil/Revenue_mil * 100,
        REVP = 100) 

# df for ribbon
df_ribbon <- df |>
  select(FYQ, year, quarter, GMP, SGAP, REVP) |>
  mutate(x_num = as.numeric(FYQ)) 

dfp <- df |> 
  pivot_longer(c(GMP, SGAP), names_to = "metric", values_to = "perc") |>  # REVP
   mutate(
    metric = case_when(
      metric == "GMP"  ~ "Gross Profit Margin",
      metric == "SGAP" ~ "SG&A",
      # metric == "REVP" ~ "Revenue (100%)"
    ),
    metric = factor(metric, levels = c("Gross Profit Margin", "SG&A"))) # "Revenue (100%)", 



# fonts
font_add_google("Share Tech Mono", "mono_main")   # terminal monospace
font_add_google("Oswald", "display")               # bold display headers
showtext_auto()
showtext_opts(dpi = 300)


# labels
x_labels <- df |>
  filter(quarter == 1) |>
  mutate(label = paste0("FY'", year-2000)) |>
  select(FYQ, label)

all_levels  <- levels(df$FYQ)
tick_labels <- setNames(rep("", length(all_levels)), all_levels)
tick_labels[as.character(x_labels$FYQ)] <- x_labels$label

# theme following bloomberg
col_bg       <- "#0A0E14"   
col_panel    <- "#0D1117"   
col_grid     <- "#1C2333"   
col_amber    <- "#FFB300"   
col_green    <- "#00E676"   
col_cyan     <- "#00BCD4"   
col_text     <- "#C9D1D9"   
col_subtext  <- "#6E7681"   
 
# Region fill colours
fill_cogs    <- "#FF5252"   
fill_profit  <- "#69F0AE"   
fill_sga     <- "#FFD740"   
fill_cross   <- "#8B0000"  

cross_x <- 36
cross_y <- 78

narrow_x <- 50
narrow_y <- 68

p <- ggplot() +
  
  # regions and lines
   geom_ribbon(data = df_ribbon, aes(x = x_num, ymin = SGAP, ymax = GMP),
    fill  = fill_profit,
    alpha = 0.18) +
   geom_ribbon(data = df_ribbon, aes(x = x_num, ymin = 0, ymax = SGAP),
    fill  = fill_sga,
    alpha = 0.22) +
  geom_ribbon(data = df_ribbon, aes(x = x_num, ymin = GMP, ymax = REVP),
    fill  = fill_cogs,
    alpha = 0.18) +
  geom_line(data = dfp, aes(x = as.numeric(FYQ), y = perc, color = metric),
    linewidth = 0.9, show.legend = FALSE) +
 
  # line annotations
  annotate("text",
           x = 2, y = 47.5,
          label = "Gross Profit Margin",
          color = fill_profit,
          family = "mono_main", size = 4, fontface = "bold",
          hjust = 0
      )+
  annotate("text",
           x = 2, y = 27,
          label = "SG&A",
          color = fill_sga,
          family = "mono_main", size = 4, fontface = "bold",
          hjust = 0
      )+

  # region annotations
  annotate("text",
          x = 8, y = 75, label = "COST OF GOODS SOLD",
          color = col_text,
          family = "mono_main", size = 5, fontface = "bold",
          hjust = 0
      )+
  annotate(
    "text",
    x     = 8, y = 38, label = "OPERATING PROFIT & BUSINESS EXPENSES",
    color = col_text, 
    family = "mono_main", size = 5, fontface = "bold",
    hjust = 0) +
  annotate(
    "text",
    x     = 8, y = 15, label = "Selling (INCL. MARKETING), GENERAL, & ADMINISTRATION",
    color = col_text,
    family = "mono_main", size = 5, fontface = "bold",
    hjust = 0
  ) +
  # annotate features
  annotate(
    "segment",
    x = cross_x, xend = cross_x,
    y = 50.2, yend = cross_y,
    color = "#FF5252", linewidth = 0.5,
    arrow = arrow(length = unit(5, "pt"), type = "closed")
  ) +
  annotate(
    "text",
    x = cross_x, y = cross_y + 3,
    label = "[!] SG&A > GPM",
    color = "#FF5252", family = "mono_main",
    size = 4, fontface = "bold", hjust = 0.5
  ) +
  annotate(
    "text",
    x = cross_x, y = cross_y + 8.5,
    label = "Margin collapses: COVID store closures\nwhile DTC investment accelerates",
    color = "#C9D1D9", family = "mono_main",
    size = 3.2, hjust = 0.5, lineheight = 1.2
  ) +

  annotate(
    "segment",
    x = narrow_x+5, xend = narrow_x,
    y = 45, yend = narrow_y,
    color = "#FFD740", linewidth = 0.5,
    arrow = arrow(length = unit(5, "pt"), type = "closed")
  ) +
  annotate(
    "text",
    x = narrow_x, y =narrow_y + 3,
    label = "[!] SPREAD NARROWS",
    color = "#FFD740", family = "mono_main",
    size = 4, fontface = "bold", hjust = 0.5
  ) +
  annotate(
    "text",
    x = narrow_x, y = narrow_y + 8.5,
    label = "Post-pandemic inventory discounting;\ninflation & rising competition",
    color = "#C9D1D9", family = "mono_main",
    size = 3.2, hjust = 0.5, lineheight = 1.2
  )+
 
  scale_color_manual(
    values = c("Gross Profit Margin" = col_green, "SG&A" = col_amber)
  ) +
  scale_x_continuous(
    breaks = which(all_levels %in% names(tick_labels[tick_labels != ""])),
    labels = x_labels$label,
    expand = expansion(mult = c(0,0))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
   labs(
    title    = "NIKE, INC. — REVENUE BREAKDOWN",
    subtitle = "Gross Profit Margin & SG&A as % of Revenue  |  Quarterly",
    caption  = "Data: Macrotrends | Viz: Louis M Penrod | #30DayChartChallenge",
    x        = NULL,
    y        = "% of Revenue",
    color    = NULL
  ) +
 
  theme_minimal(base_family = "mono_main") +
  theme(
    plot.background  = element_rect(fill = col_bg,    color = NA),
    panel.background = element_rect(fill = col_panel, color = NA),
    panel.grid.major = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor = element_line(color = col_grid, linewidth = 0.2),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.25, linetype = "dotted"),
    axis.text = element_text(color = col_text, size = 9,  family = "mono_main"),
    axis.text.x = element_text(color = col_text, size = 9.5,angle = 0, hjust = 0.5),
    axis.title.y = element_text(color = col_text, size = 12,  margin = margin(r = 8)),
    axis.ticks = element_line(color = col_grid),
    axis.ticks.length = unit(3, "pt"),
    plot.title       = element_text(
      family = "display", color = col_amber,
      size = 18, face = "bold", margin = margin(b = 4)
    ),
    plot.subtitle    = element_text(
      color = col_text, size = 10,
      family = "mono_main", margin = margin(b = 12)
    ),
    plot.caption     = element_text(
      color = col_subtext, size = 9,
      family = "mono_main", hjust = 1, margin = margin(t = 10)
    ),
    plot.margin      = margin(16, 20, 12, 16),
    legend.position     = "top",
    legend.justification = "left",
    legend.text         = element_text(color = col_text, size = 9, family = "mono_main"),
    legend.key          = element_rect(fill = NA, color = NA),
    legend.spacing.x    = unit(12, "pt"),
    legend.margin       = margin(b = 6),
    panel.border = element_rect(color = "#2A3A50", fill = NA, linewidth = 0.6)
  )


ggsave(
  "../www/outputs/day09_wealth_nike.png",
  plot   = p,
  width  = 10,
  height = 7,
  dpi    = 300,
  bg     = col_bg
)
