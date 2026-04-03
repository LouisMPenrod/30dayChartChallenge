# Day 3 - Mosaic

# Marimekko plot of Aquaculture volume by US region and type.
# Using NASS data from just 1 year (2023)

library(tidyverse)
# devtools::install_github("gogonzo/marimekko")
library(marimekko)

df <- read.csv("../data/raw/NASS/nass_aq_2023.csv")

# reduce
df <- df |> 
  select(State, State.ANSI, Data.Item, Value)

df |> count(Data.Item)

df <- df |> 
  filter(str_detect(Data.Item, "SALES, MEASURED IN \\$"))

df |>count(Data.Item)



# grab type rollups

targets <- paste0(c("FOOD FISH", "SPORT FISH", "BAITFISH", 
             "CRUSTACEANS", "MOLLUSKS", "ORNAMENTAL FISH", "AQUACULTURE, OTHER"),
             " - SALES, MEASURED IN $")

df <- df |> 
  filter(Data.Item %in% targets)

df |>count(Data.Item)
df <- df |> 
  mutate(Data.Item = str_to_title(str_remove_all(Data.Item, " - SALES, MEASURED IN \\$")),
         Data.Item = if_else(Data.Item == "Aquaculture, Other", "Other", Data.Item))
df |>count(Data.Item)

df$Data.Item <- factor(df$Data.Item, levels = c("Baitfish", "Food Fish", "Sport Fish", 
                                                "Ornamental Fish", "Crustaceans", "Mollusks", 
                                                "Other"))

# fix values
df |> count(Value)
df |> count(str_detect(Value, "D"))

df <- df |> 
  mutate(Value = case_when(str_detect(Value, "D") ~ NA_real_,
                           TRUE ~ as.numeric(str_remove_all(Value, ","))))

df |> count(is.na(Value))

# Roll up into region by US census region
df |> count(State, State.ANSI)

NE <- data.frame(State.ANSI = c(9, 23, 25, 33, 44, 50, 34, 36, 42),
                 Region = "Northeast")

MW <- data.frame(State.ANSI = c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46),
                 Region = "Midwest")

South <- data.frame(State.ANSI = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48),
                 Region = "South")

West <- data.frame(State.ANSI = c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53),
                 Region = "West")

reg <- bind_rows(NE, MW, South, West)

df <- df |> 
  left_join(reg, by = "State.ANSI") |> 
  mutate(Region = factor(Region, levels = c("West","Midwest","South","Northeast")))

df |> count(Region, State)

# sum within region
df_sum <- df |> 
  group_by(Region, Data.Item) |>
  summarise(Value = sum(Value, na.rm = TRUE)) |> 
  ungroup()

# plot

my_palette <- marimekko_pal[-7]

p <- ggplot(df_sum)+
  geom_marimekko(aes(fill = Data.Item, weight = Value),
                 formula = ~Region | Data.Item) +
  theme_marimekko()+
  scale_fill_manual(values = my_palette) +
  labs(fill = NULL,
       title = "How US Aquaculture Sales Vary by Region and Type",
      caption = "Data: USDA NASS 2023 Census of Aquaculture | Viz: Louis M Penrod | #30DayChartChallenge")+
  guides(fill = guide_legend(byrow = TRUE,ncol = 7))+
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 10),
        legend.position = "top",
        legend.margin = margin(b = 0, t=5),
        legend.box.spacing = unit(2, "pt"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(hjust = 0.5, size = 17, face = "bold", 
                              margin = margin(b = 7, t = 1)),
        plot.caption = element_text(size = 12, color = "grey30",
                                margin = margin(t = 7),
                              hjust = 0.5),
        plot.margin = margin(t=1, b = 2,l=5,r=5))

p

ggsave("../www/outputs/day03_mosaic_aq_prod.png", 
      p, 
      width = 8, 
      height = 8, 
      dpi = 300,
      bg = "white")
