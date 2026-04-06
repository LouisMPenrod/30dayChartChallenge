# Day 6 - Date Day: RSF

# Concept: Pair RSF data with EPI 
# See if there is an association between Free press and environmental sustainability

library(tidyverse)
library(countrycode)
# install.packages("ggflags", repos = c(
#   "https://jimjam-slam.r-universe.dev",
#   "https://cloud.r-project.org"))
library(ggflags)

# RSF data
rsf <- read.csv("../data/raw/RSF/RWB_PFI.csv")

range(rsf$TIME_PERIOD)
rsf |> count(INDICATOR, INDICATOR_LABEL)

rsf <- rsf |> 
  filter(TIME_PERIOD == 2024,
         INDICATOR == "RWB_PFI_SCORE")

# confirm one record per country
rsf |> count(REF_AREA)

# reduce
rsf <- rsf |> select(iso = REF_AREA, RSF = OBS_VALUE)

epi <- read.csv("../data/raw/epi/epi2024results.csv")

epi <- epi |> 
  select(iso, EPI = EPI.new)

# check oneper iso
epi |> count(iso)

# join
df <- inner_join(rsf, epi, by = c("iso"))

# convert iso to iso2 for flags
df <- df |>
  mutate(
    iso2 = tolower(
      countrycode(iso, origin = "iso3c", destination = "iso2c")
    )
  )

# get medians for quadrants and set positions, get correlation coeff
med_rsf <- median(df$RSF, na.rm = TRUE)
med_epi <- median(df$EPI, na.rm = TRUE)
r_val   <- cor(x = df$EPI, y = df$RSF)



quad_labels <- tribble(
  ~y, ~x, ~label,
  95,25,"High press freedom\nLow EPI",
  95,75,"High press freedom\nHigh EPI",
  10,25,"Low press freedom\nLow EPI",
  10,75,"Low press freedom\nHigh EPI"   
)

quad_fill <- c(
  "#E8D5C4", # good press, bad env # old : "#B5D4F4"
  "#C0DD97",   # both good
  "#F4C4B3",   # both bad
  "#E8D5C4"   # bad press, good env
)

range(df$RSF)
range(df$EPI)

# plot
p <- ggplot(df, aes(x = EPI, y = RSF)) +
 # quadrants
  annotate("rect",
    xmin = -Inf,    xmax = med_epi,
    ymin = med_rsf, ymax = Inf,
    fill = quad_fill[1], alpha = 0.55
  ) +
  annotate("rect",
    xmin = med_epi, xmax = Inf,
    ymin = med_rsf, ymax = Inf,
    fill = quad_fill[2], alpha = 0.55
  ) +
  annotate("rect",
    xmin = -Inf,    xmax = med_epi,
    ymin = -Inf,    ymax = med_rsf,
    fill = quad_fill[3], alpha = 0.55
  ) +
  annotate("rect",
    xmin = med_epi, xmax = Inf,
    ymin = -Inf,    ymax = med_rsf,
    fill = quad_fill[4], alpha = 0.55
  ) +
 
  # med ref lines
  geom_vline(xintercept = med_epi, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_hline(yintercept = med_rsf, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
 
  # quad labels
  geom_text(
    data      = quad_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size      = 3,
    colour    = "grey45",
    lineheight = 1.2,
    fontface  = "italic"
  ) +
 
  # flags
  geom_flag(aes(country = iso2), size = 6) +
 
  # regression —
  # geom_smooth(
  #   method  = "lm",
  #   colour  = "#444",
  #   # fill    = "#AAAAAA",
  #   linewidth = 0.6,
  #   alpha   = 0.18,
  #   se      = FALSE
  # ) +
  
  scale_y_continuous(
    breaks = seq(10, 100, by = 10),
    limits = c(10, 100)
  ) +
  scale_x_continuous(
    breaks = seq(20, 80, by = 10),
    limits = c(20, 80)
  ) +
 
  # labels and titles
  labs(
    title    = "Countries with stronger press freedom tend to\nscore higher on environmental performance",
    subtitle = paste0(
      "Medians: EPI ", round(med_epi, 1),
      ", RSF  ", round(med_rsf, 1),
       "  | r = ", round(r_val, 2)
    ),
    y        = "RSF Press Freedom Index",
    x        = "Environmental Performance Index (EPI)",
    caption  = "Data: Reporters Without Borders (RSF) & Yale EPI (2024) \nViz: Louis M Penrod | #30DayChartChallenge"
  ) +
 
  # theming
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 17, hjust = 0),
    plot.subtitle      = element_text(colour = "grey40", size = 14, hjust = 0,
                                      margin = margin(b = 8)),
    plot.caption       = element_text(colour = "grey55", size = 10, hjust = 1),
    panel.grid.major   = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    axis.title         = element_text(size = 12, colour = "grey30", face = "bold"),
    axis.title.x         = element_text(margin = margin(t=8)),
    axis.text          = element_text(size = 10,  colour = "grey40"),
    plot.margin        = margin(16, 16, 12, 16)
  )

# p

ggsave("../www/outputs/day06_data_rsf_v_epi.png",
       p,
       width = 8, height = 8, dpi = 300,
       bg = "white"
)
