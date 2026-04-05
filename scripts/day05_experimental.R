# Day 5 - Experimental

# Show diet overlap of common fish genera in FL as a matrix plot of eulerr plots
# Eulerr in ggplot: https://gist.github.com/danlooo/d23d8bcf8856c7dd8e86266097404ded

library(tidyverse)
library(rfishbase)
library(openxlsx)
library(eulerr)
library(ggforce)
library(ggtext)
library(patchwork)
library(rphylopic)

df <- read.xlsx("../data/raw/diet/diet_extract.xlsx") |> 
  mutate(Prey = tolower(word(Prey, 1))) |> 
  distinct() |> 
  mutate(source = "Literature")


species <- df$Pred_species
species_u <- unique(species)

all_sp <- rfishbase::species_names()

species_df <- all_sp |> 
  filter(Species %in% species)

species_df |> count(Species)

diet <- rfishbase::diet(species_df$Species) |> 
  select(Species, DietCode)
diet_codes <-  rfishbase::diet_items() |> 
  select(DietCode,  Prey = ItemName) # ItemName, FoodI, FoodII, FoodIII,


# clean diet_code

diet_codes <- diet_codes |>
    mutate(Prey = str_remove_all(Prey, "[\r\n]")) |>
    mutate(Prey = str_to_lower(str_squish(Prey))) |>
    mutate(Prey = 
    # Remove entries that are non-taxonomic
    ifelse(
      str_detect(Prey, paste0(
        "^(unidentified|unspecified|unknown|other|others|",
        "bony fish|finfish|fish|fishes|pelagic finfish|",
        "invertebrates|invertebrate|benth\\.|plank\\.|pelagic|",
        "detritus|debris|carcasses|remains|fragments|material|",
        "algae|macroalgae|seagrass|spermatophytes|phytoplankton|",
        "zooplankton|plant|benthic algae|",
        "animal remains|fish remains|fish eggs|fish larvae|",
        "crustacean fragments|gastropod larvae|crab larvae|",
        "annelid|worm|polyp|",  # too broad
        "sargos|doncellas|loros|lenguados|meros|langostas|",  # Spanish common names
        "silver-biddy|threadfin bream|round scad|squid$|shrimp$|",
        "perch|rays|octopi|octopus$|lobsters$|",
        "sea stars|brittle stars|cymothoid)"
      )),
      NA_character_,
      Prey
    ),
   Prey = dplyr::case_when(
    is.na(Prey) ~ NA_character_,
    str_detect(Prey, "idae$|inae$") ~ Prey,
    str_detect(Prey, "^[a-z][a-z]+") ~ word(Prey, 1),
    TRUE ~ NA_character_
  ),
Prey = ifelse(
      !is.na(Prey) &
        nchar(Prey) >= 4 &
        str_detect(Prey, "^[a-z]+$"),
      Prey,
      NA_character_
    ))


diet <- diet |> 
  left_join(diet_codes, by = "DietCode") |> 
  distinct() |> 
  drop_na(Prey) |> 
  select(-DietCode) |> 
  mutate(source = "fishbase") |> 
  rename(Pred_species = Species) |> 
  bind_rows(df)

diet |> group_by(Pred_species) |> summarise(nd = n_distinct(Prey))
diet |> group_by(Prey) |> summarise(nd = n_distinct(Pred_species))

head(diet)

# check distance for QC of plot

presence_matrix <- diet |>
  select(-contains("source")) |>         
  distinct() |>
  mutate(present = 1L) |>
  pivot_wider(
    names_from  = Pred_species,
    values_from = present,
    values_fill = 0L
  ) |>
  arrange(Prey)

pred_cols <- species_u[species_u %in% names(presence_matrix)]
mat        <- as.matrix(presence_matrix[, pred_cols])
rownames(mat) <- presence_matrix$Prey

overlap_summary <- expand.grid(A = pred_cols, B = pred_cols,
                               stringsAsFactors = FALSE) |>
  mutate(unique_flag = A < B) |> 
  rowwise() |>
  mutate(
    n_A       = sum(mat[, A]),
    n_B       = sum(mat[, B]),
    n_shared  = sum(mat[, A] == 1 & mat[, B] == 1),
    jaccard   = n_shared / (n_A + n_B - n_shared),
    sorensen  = 2 * n_shared / (n_A + n_B)
  ) |>
  ungroup() |>
  arrange(desc(jaccard))

colnames(overlap_summary)

overlap_summary |> filter(unique_flag)

global_max <- max(overlap_summary$n_A, 
                  overlap_summary$n_B)

okabe_ito_no_yellow <- c(
  "#E69F00", "#56B4E9", "#009E73",
  "#0072B2", "#D55E00", "#CC79A7", "#000000"
)

pred_colors <- setNames(
  okabe_ito_no_yellow[1:length(pred_cols)],
  pred_cols
)

# build control table for matrix
xvar <- yvar <- pred_cols
p_control <- tidyr::expand_grid(xvar, yvar)
colnames(p_control) <- c("xvar", "yvar")
key <- p_control |> 
          dplyr::select(xvar, yvar) |> 
          dplyr::distinct() |> 
          dplyr::mutate(colnum = rep(1:length(unique(xvar)), each = length(unique(xvar))),
                        rownum = rep(1:length(unique(xvar)), times = length(unique(xvar)))) |> 
          dplyr::arrange(rownum, colnum) |> 
          dplyr::mutate(mat_pos = case_when(rownum == colnum ~ "diag",
                                            rownum < colnum ~ "upper",
                                            rownum > colnum ~ "lower")) |> 
          dplyr::mutate(edge = case_when(colnum == 1 & rownum == max(rownum, na.rm = TRUE) ~ "xy",
                                         colnum == 1 ~ "y",
                                         rownum == max(rownum, na.rm = TRUE) ~ "x",
                                         TRUE ~ NA_character_))
p_control <- p_control |> 
          dplyr::left_join(key) |> 
  arrange(rownum,colnum)



get_euler_gg <- function(n_A, n_B, n_shared, label_A, label_B, global_max, pred_colors) {
  
  scale_f <- global_max
  n_A_s      <- n_A / scale_f
  n_B_s      <- n_B / scale_f
  n_shared_s <- n_shared / scale_f
  
  fit <- euler(c(
    A     = n_A_s - n_shared_s,
    B     = n_B_s - n_shared_s,
    "A&B" = n_shared_s
  ))
  
  edata <- plot(fit, quantities = TRUE) |> pluck("data")
  
  ellipses <- edata$ellipses |>
    as_tibble(rownames = "Set") |>
    mutate(Set = recode(Set, A = label_A, B = label_B))
  
  fill_vals <- pred_colors[c(label_A, label_B)]
  
  ggplot() +
    ggforce::geom_ellipse(
      data = ellipses,
      aes(x0 = h, y0 = k, a = a, b = b, angle = 0, fill = Set),
      alpha = 0.4, color = "white", linewidth = 0.3,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = fill_vals) +
    fixed_coord +
    theme_void() +
    labs(fill = NULL)
}

base_size <- 9
max_fit <- euler(c(A = 1, B = 0, "A&B" = 0))
max_edata <- plot(max_fit, quantities = TRUE) |> pluck("data")
max_r <- max_edata$ellipses |> pull(a) |> max()
buffer_x <- 1.7
buffer_y <- 1.05  

axis_xlim <- c(-max_r * buffer_x, max_r * buffer_x)
axis_ylim <- c(-max_r * buffer_y, max_r * buffer_y)

fixed_coord <- coord_fixed(
  xlim = axis_xlim,
  ylim = axis_ylim
)

pics <- tribble(
  ~pred, ~uuid, ~size, ~flip,
  "Sciaenops ocellatus", "2f984c47-7b30-4b33-ae6f-d411d2b96003",0.75, TRUE,
  "Lutjanus griseus", "107de00e-8571-4caf-b7dc-16476796dfee",0.65, TRUE,
  "Lutjanus synagris", "c1bb2fb0-08dc-4841-ae91-6c09146c4289",0.8, FALSE, # not specific species
  "Seriola dumerili", "e952cd97-6db2-4833-8af2-881c8cf55184",0.75, FALSE,
  "Caranx hippos", "4972110b-5e1f-4308-b315-449a2a400dbc",0.7, FALSE,
  "Centropomus undecimalis", "a113b966-b8a0-4b3c-8cdf-7a178f9da2e1",0.6, FALSE,
  "Pterois volitans", "ce8b864d-36cc-4fa3-a6f5-1d07a75c9a10",0.8, FALSE,
)

get_attribution(uuid = pics$uuid, text = TRUE)


plotit <- function(rownum){

if(p_control$mat_pos[[rownum]] %in% c("diag","lower")) {
  p <- ggplot(data = data.frame(x = 0, y = 0),
                  aes(x = x, y = y))
  
  if(p_control$mat_pos[[rownum]] == "diag"){

    sp_name  <- p_control$xvar[[rownum]]
    sp_color <- pred_colors[[sp_name]]
    pic_row  <- pics |> filter(pred == sp_name)
    nudge_y_pic   <-  max_r * 0.45
    nudge_y_label <- -max_r * 0.6
    
        p <- p +
          geom_phylopic(
            uuid  = pic_row$uuid,
            x     = 0,
            y     = nudge_y_pic,
            height = pic_row$size,
            color = sp_color,
            alpha = 0.85,
            horizontal = pic_row$flip
          )+
          geom_richtext(
            label = paste0("<span style='color:", sp_color, "'><i>",
                     stringr::str_replace_all(str_wrap(sp_name, width = 8), "\\n", "<br>"),
                     "</i></span>"),
            size = base_size / ggplot2:::.pt,
            fontface = "bold",
            fill = NA,
            label.color = NA,
            nudge_y = nudge_y_label,
            label.padding = grid::unit(rep(0, 4), "pt")
          )+
          fixed_coord
  } else if(p_control$mat_pos[[rownum]] == "lower") {
        # keep empty for now
        p <- p +
          fixed_coord
      }
  
  p <- p + 
    theme_void()+
    theme(plot.margin = margin(0, 0, 0, 0))
   
}
  
  if(p_control$mat_pos[[rownum]] == "upper") {
            
    vals <- overlap_summary |> 
      filter(A == p_control$xvar[[rownum]],
             B == p_control$yvar[[rownum]])

      p <- get_euler_gg(
        n_A      = vals$n_A,
        n_B      = vals$n_B,
        n_shared = vals$n_shared,
        label_A  = p_control$xvar[[rownum]],
        label_B  = p_control$yvar[[rownum]],
        global_max  = global_max,  
        pred_colors = pred_colors)+
        theme(legend.position = "none",
              plot.margin = margin(0, 0, 0, 0))
        
    
    } # end if upper
  
  p
  
}

p_control$plotout <- purrr::map(seq_along(1:nrow(p_control)), plotit) # apply plotit to all rows in p_control

# arrange
n_plots <- nrow(p_control)
ncol_use <- nrow_use <- sqrt(n_plots)

plotlist <- p_control$plotout

plots <- patchwork::wrap_plots(plotlist, nrow = nrow_use, ncol = ncol_use)

plots


ellipse_demo <- tibble(
  label = c("Species A", "Species B"),
  x0    = c(-0.18,  0.18),
  y0    = c(0,      0),
  a     = c(0.28,   0.18),
  b     = c(0.28,   0.18),
  fill  = c("#0072B2", "#D55E00")
)

explanation_plot <- ggplot() +
  ggforce::geom_ellipse(
    data = ellipse_demo,
    aes(x0 = x0, y0 = y0, a = a, b = b, angle = 0, fill = fill),
    alpha = 0.4, color = "white", linewidth = 0.3
  ) +
  scale_fill_identity() +
  # annotation arrows + labels
  annotate("segment", x = -0.37, xend = -0.28, y = 0.6, yend = 0.2,
           arrow = arrow(length = unit(0.015, "npc")), linewidth = 0.4) +
  annotate("text", x = -0.37, y = 0.63,
           label = "Circle size =\ndiet breadth",
           hjust = 0.5, vjust = 0, size = 4,
          fontface = "bold") +
  annotate("segment", x = 0.06, xend = 0.06, y = 0.32, yend = 0.09,
           arrow = arrow(length = unit(0.015, "npc")), linewidth = 0.4) +
  annotate("text", x = 0.06, y = 0.35,
           label = "Overlap =\nshared prey",
           hjust = 0.5, vjust = 0, size = 4,
          fontface = "bold") +
  annotate("segment", x = 0.5, xend = 0.3, y = 0, yend = 0,
           arrow = arrow(length = unit(0.015, "npc")), linewidth = 0.4) +
  annotate("text", x = 0.8, y = -0.04,
           label = "Smaller circle =\nnarrower diet",
           hjust = 0.5, vjust = 0, size = 4,
          fontface = "bold") +
  coord_fixed(xlim = c(-0.55, 1.1), ylim = c(-0.5, 0.75)) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0))

final_plot <- wrap_elements(full = plots) +
  inset_element(
    explanation_plot,
    left   = 0,
    bottom = 0,
    right  = 0.6,
    top    = 0.65,
    clip   = FALSE
  )

# add titles and caption
final_plot <- final_plot+
  plot_annotation(
    title   = "Diet overlap between predatory fishes in Florida",
    subtitle = "Current published sampling shows ***low*** dietary overlap among predators",
    caption = paste0(
      "Data: rfishbase and published literature | Viz: Louis M Penrod | #30DayChartChallenge",
      "\nGlyphs: PhyloPic via rphylopic (Gearty & Jones 2023)",
      " by E. Price, G. Montgomery, C. Kenaley,",
      " J. Hlavin, A. Distrubell, and A. Wilson, 2022"
    ),
  theme = theme(
    plot.title = element_text(hjust = 0, size = 17, face = "bold", 
                              margin = margin(b = 0, t = 1)),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(size = 9, color = "grey30", lineheight = 1.3,
                                margin = margin(t = 0),
                              hjust = 0.5),
    plot.caption.position = "plot",
  plot.margin = margin(t = 0, r = 0, b = 0, l = 0)))

final_plot


ggsave("../www/outputs/day05_diet_euler_matrix.png", 
      final_plot, 
      width = 8, 
      height = 5, 
      dpi = 300,
      bg = "white")
