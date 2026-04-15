
library(tidyverse)
library(scales)
library(rotl)
library(ape)

tax_key <- read.csv("../data/raw/personal/tax_key.csv")

csLongBM <- read.csv("../data/raw/personal/cs_expanded_sub.csv")

csLongBM <- csLongBM |> 
  left_join(tax_key, by = "species")

csLongBM |> pull(spine) |> unique() |> dput()
spine_ord <- c("Rostral", "Nasal", "PreocularRegion", "Preocular",
               "Supraocular", "Postocular", "Neurocranial",  "Neurocrest",
              "Supraoral","Oral","Suboral","Ventral",  "Subocular",
            "SubocularRegion", "Temporal","Postemporal","Supraopercular",
          "PreopercularCover", "PreopercularEdge","OpercularCover", "OpercularEdge", "Postopercular")

csLongBM <- csLongBM |>
  mutate(spine = factor(spine, levels = spine_ord))
  
  
csLongBM <- csLongBM %>% 
  group_by(order,species,spine) %>% 
  mutate(id = str_pad(row_number(),2,side="left","0")) %>% 
  mutate(newid=paste0(spine,"_",id)) %>% 
  ungroup()

newid_ord <- csLongBM %>% 
  arrange(spine, newid) |> 
  select(spine, newid) |> 
  pull(newid) |> unique()

csLongBM <- csLongBM |> 
  mutate(newid = factor(newid, levels = newid_ord)) |> 
  dplyr::select(-spine,-id)


# get family stats
csLongBM_fam <- csLongBM |> 
  group_by(order, family) |> 
  mutate(num_spp_ord = n_distinct(species)) |> 
  ungroup() |> 
  group_by(order, family, num_spp_ord, newid) |> 
  summarise(num_spp_spine = n_distinct(species)) |> 
  ungroup() |> 
  mutate(pct = round((num_spp_spine/num_spp_ord)*100, 2))

fams <- unique(csLongBM$family)
ords <- unique(csLongBM$order)
spp <- unique(csLongBM$species)

taxa <- rotl::tnrs_match_names(c(fams, ords, spp), context_name = "Vertebrates")

keep <- taxa |> filter(!is.na(ott_id), !is_synonym, flags == "")


tmp <- tempfile(fileext = ".nwk")

tree <- tol_induced_subtree(ott_ids = na.omit(keep$ott_id), 
                            label_format = "name",
                            file = tmp)

tree <- read.tree(tmp)

clean_tips <- tree$tip.label[!str_detect(tree$tip.label, "^mrcaott|^ott\\d+")]

df <- data.frame(sp=clean_tips)

df <- df |> 
  mutate(genus = stringr::str_replace(sp, "(.*)_.*", "\\1")) |> 
  left_join(tax_key |> distinct(family, genus), by = "genus")

family_ord <- unique(df$family)

check <- setdiff(fams, df$family)
check
family_ord <- c(family_ord, check)

csLongBM_fam <- csLongBM_fam |> 
  mutate(family = factor(family, levels = family_ord))


spine_levels <- csLongBM_fam |>
  distinct(newid) |>
  mutate(spine_group = str_remove(newid, "_\\d+$")) |>
  arrange(desc(newid)) |>   # match limits=rev
  mutate(row = row_number())

group_breaks <- spine_levels |>
  group_by(spine_group) |>
  summarise(first_row = min(row), last_row = max(row), .groups = "drop") |> 
  ungroup()

hline_positions <- unique(c(group_breaks$first_row - 0.5, group_breaks$last_row + 0.5))

red_col <- "#ff9f9f9e"

ggplot(csLongBM_fam, aes(x = factor(family, levels = family_ord), y = newid, fill = pct)) +
  geom_tile(color = "grey15") +  # subtle cell border so tiles don't bleed together
  geom_hline(yintercept = hline_positions, color = red_col, linetype = "dashed", linewidth = 0.5) +
  scale_fill_gradient(
    low  = "grey15",
    high = "white",
    labels = label_percent(accuracy = 1.0, scale = 1)
  ) +
  scale_y_discrete(
    breaks = csLongBM_fam$newid[str_detect(csLongBM_fam$newid, "_01")],
    limits = rev
  ) +
  annotate("text", x = 5, y = 280, 
         label = "Dashed lines separate\nspine groups", 
         color = red_col, size = 4.5, hjust = 0, lineheight = 0.9) +
  annotate("segment", x = 3.5, xend = 3.5, y = 272, yend = 288, 
  arrow = arrow(ends = "both", type = "closed", length = unit(0.2, "cm")), color = red_col
)+
  annotate("text", x = 58, y = 280, 
         label = "Each row within a group\nrepresents a unique expression", 
         color = red_col, size = 4.5, hjust = 0, lineheight = 0.9)+
  annotate("segment", x = 74.5, xend = 79.5, y = 282.5, yend = 282.5, 
  arrow = arrow(ends = "last", type = "closed", length = unit(0.2, "cm")), color = red_col
)+
  annotate("text", x = 50, y = -20, 
         label = "← Families (arranged by phylogenetic relatedness) →", 
         color = "white", size = 30/ggplot2:::.pt, hjust = 0.5, fontface = "bold") +
  coord_cartesian(clip = "off")+
  labs(
    title = "Cranial Spine Expression in Fishes",
    subtitle = "Heatmap showing the percent of species per family expressing each cranial spine (spine and count)",
    x    = " ",#"Family",
    y    = "Spine",
    fill = NULL,
    caption = "Data: My dissertation\nViz: Louis M Penrod | #30DayChartChallenge"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.text.x = element_blank(),
    # axis.title.x = element_blank(),
    axis.title.x = element_text(color = "white", margin = margin(t=5), face = "bold", size = 10),
    axis.title.y = element_text(color = "white", margin = margin(r=10),face = "bold", size = 30),
    legend.background = element_rect(fill = "black", color = NA),
    legend.title = element_text(color = "white", size = 25, margin = margin(b = 10, r = 5)),
    legend.text = element_text(color = "white", size = 16),
    plot.title = element_text(color = "white", face = "bold", size = 36, margin = margin(t=5, b = 5)),
    plot.subtitle = element_text(color = "grey70", size = 20, margin = margin(b = 20)),
    plot.caption = element_text(color = "grey50", size = 12, hjust = 1, margin = margin(t = 0))
  )

ggsave(filename="../www/outputs/day15_correlation.png", plot=last_plot(),
       dpi=600,height = 12,width=14,units="in")
