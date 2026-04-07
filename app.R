# =============================================================================
# #30DayChartChallenge 2026
# =============================================================================

library(shiny)
library(bslib)

# =============================================================================
# THEME
# =============================================================================

app_theme <- bs_theme(
  version      = 5,
  bg           = "#EEEEEE",
  fg           = "#2D2D2D",
  primary      = "#555555",
  secondary    = "#888888",
  base_font    = font_google("IBM Plex Mono"),
  heading_font = font_google("IBM Plex Mono"),
  code_font    = font_google("IBM Plex Mono"),
  navbar_bg    = "#2D2D2D",
  navbar_fg    = "#EEEEEE"
)

# =============================================================================
# HELPER — day content area header
# =============================================================================

day_header <- function(day_num, topic, text = NULL) {
  tagList(
    tags$p(class = "day-number", paste0("Day ", day_num, " — ", topic)),
    tags$hr(),
    if (!is.null(text)) tags$p(class = "day-text", text)
  )
}

home_text <- "Welcome to my submissions for the 2026 #30DayChartChallenge!"
home_text2 <- "Most visualization are created using publicly available datasets. While I've done my best to source reliable data,"
home_text2.2 <- " figures are presented as-is and I make no guarantees regarding their accuracy or completeness."
home_text3 <- "Questions or feedback? Reach out or visit my website."

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title    = "#30DayChartChallenge 2026",
  theme    = app_theme,
  fillable = FALSE,
  lang     = "en",
  header   = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

# Home
nav_panel(
  "Home",
  p(home_text),
  p(home_text2, tags$br(),home_text2.2),
  p(
    home_text3,
    tags$a("louismpenrod@gmail.com", href = "mailto:louismpenrod@gmail.com"),
    " · ",
    tags$a("louismpenrod.github.io", href = "https://louismpenrod.github.io/", target = "_blank")
  ),
  imageOutput("home")
),

  # ===========================================================================
  # COMPARISONS
  # ===========================================================================

  nav_panel(
    "Comparisons",
    navset_pill_list(
      well   = FALSE,
      widths = c(2, 10),

      nav_panel(
        "1. Part-to-Whole",
        day_header(1, "Part-to-Whole"),
        p("Data is the US commercial fisheries landing for 2022 as published by:", 
        tags$br(),
        "National Marine Fisheries Service (2024). Fisheries of the United States, 2022.",
        tags$br(),
        "Department of Commerce, NOAA Current Fishery Statistics No. 2022."),
        p("I’m not a big fan of pie charts, but they fit the brief of “Part-to-Whole”.",
          tags$br(),
        "I've tried to elevate it by adding an illustration of a cast net, but I'm not sure if it adds to or distracts from the plot."),
        tags$div(
          style = "max-width: 900px;",
          imageOutput("day01_plot", height = "auto"))
      ),

      nav_panel(
        "2. Pictogram",
        day_header(2, "Pictogram"),
        p("Data is from the NOAA Restoration Atlas which are projects conducted by NOAAs Restoration Center."),
        p("Oysters are probably not the best subject for a pictogram since they are a bit blobby..."),
        imageOutput("day02_plot")
      ),

      nav_panel(
        "3. Mosaic",
        day_header(3, "Mosaic"),
        p("Data is from the USDA National Agricultural Statistics Service (NASS) Census of Agriculture, 2023"),
        p("This shows the sum of sales by aquaculture type by US census region."),
        imageOutput("day03_plot")
      ),

      nav_panel(
        "4. Slope",
        day_header(4, "Slope"),
        p("A line plot of the contestant tap-outs through time by season."),
        p("It's hard to make any comparisons across seasons due to different locations and casts,", 
        tags$br(),
          "but we can see some season are very different from the others"),
        p("Data is from the {alone} R package."),
        imageOutput("day04_plot")
      ),

      nav_panel(
        "5. Experimental",
        day_header(5, "Experimental"),
        p("Diet overlap of some predatory fishes in Florida."),
        p("I've wanted to make something like this for a while. Admittedly, the data is not the best, but what I could pull together for the challenge. The concept is what I want to present."),
        p("These are eulerr diagrams set up in a matrix arrangement. The size of the circle shows the size of the diet",
        tags$br(),
        "(scaled to the max diet size so the circle size is the same across plot for the same species). The overlap shows the proportion of prey shared."),
        p("Data is from a combination of {rfishbase} R pacakge and publications (citations in dataset on github)."),
        imageOutput("day05_plot")
      ),

      nav_panel(
        "6. Data Day: Reporters Without Borders",
        day_header(6, "Data Day: RSF"),
        p("Are countries that protect their press also more likely to protect their environment?"),
        p("To find out, I plotted the RSF Press Freedom Index against the Yale Environmental Performance Index."),
        p("Data: RSF & Yale EPI."),
        imageOutput("day06_plot")
      )
    )
  ),

  # ===========================================================================
  # DISTRIBUTIONS
  # ===========================================================================

  nav_panel(
    "Distributions",
    navset_pill_list(
      well   = FALSE,
      widths = c(2, 10),

      nav_panel(
        "7. Multiscale",
        day_header(7, "Multiscale"),
        htmlOutput("day07_plot")
      ),

      nav_panel(
        "8. Circular",
        day_header(8, "Circular"),
        # imageOutput("day08_plot")
      ),

      nav_panel(
        "9. Wealth",
        day_header(9, "Wealth"),
        # plotlyOutput("day09_plot")
      ),

      nav_panel(
        "10. Pop Culture",
        day_header(10, "Pop Culture"),
        # imageOutput("day10_plot")
      ),

      nav_panel(
        "11. Physical",
        day_header(11, "Physical"),
        # plotlyOutput("day11_plot")
      ),

      nav_panel(
        "12. Theme: FlowingData",
        day_header(12, "Theme: FlowingData"),
        # imageOutput("day12_plot")
      )
    )
  ),

  # ===========================================================================
  # RELATIONSHIPS
  # ===========================================================================

  nav_panel(
    "Relationships",
    navset_pill_list(
      well   = FALSE,
      widths = c(2, 10),

      nav_panel(
        "13. Ecosystems",
        day_header(13, "Ecosystems"),
        # leafletOutput("day13_plot")
      ),

      nav_panel(
        "14. Trade",
        day_header(14, "Trade"),
        # imageOutput("day14_plot")
      ),

      nav_panel(
        "15. Correlation",
        day_header(15, "Correlation"),
        # imageOutput("day15_plot")
      ),

      nav_panel(
        "16. Causation",
        day_header(16, "Causation"),
        # imageOutput("day16_plot")
      ),

      nav_panel(
        "17. Remake",
        day_header(17, "Remake"),
        # imageOutput("day17_plot")
      ),

      nav_panel(
        "18. Data Day: UNICEF",
        day_header(18, "Data Day: UNICEF"),
        # plotlyOutput("day18_plot")
      )
    )
  ),

  # ===========================================================================
  # TIMESERIES
  # ===========================================================================

  nav_panel(
    "Timeseries",
    navset_pill_list(
      well   = FALSE,
      widths = c(2, 10),

      nav_panel(
        "19. Evolution",
        day_header(19, "Evolution"),
        # imageOutput("day19_plot")
      ),

      nav_panel(
        "20. Global Change",
        day_header(20, "Global Change"),
        # plotlyOutput("day20_plot")
      ),

      nav_panel(
        "21. Historical",
        day_header(21, "Historical"),
        # leafletOutput("day21_plot")
      ),

      nav_panel(
        "22. New Tool",
        day_header(22, "New Tool"),
        # r2d3Output("day22_plot")
      ),

      nav_panel(
        "23. Seasons",
        day_header(23, "Seasons"),
        # imageOutput("day23_plot")
      ),

      nav_panel(
        "24. Theme: SCMP",
        day_header(24, "Theme: SCMP"),
        # imageOutput("day24_plot")
      )
    )
  ),

  # ===========================================================================
  # UNCERTAINTIES
  # ===========================================================================

  nav_panel(
    "Uncertainties",
    navset_pill_list(
      well   = FALSE,
      widths = c(2, 10),

      nav_panel(
        "25. Space",
        day_header(25, "Space"),
        # imageOutput("day25_plot")
      ),

      nav_panel(
        "26. Trend",
        day_header(26, "Trend"),
        # plotlyOutput("day26_plot")
      ),

      nav_panel(
        "27. Animation",
        day_header(27, "Animation"),
        # imageOutput("day27_plot")
      ),

      nav_panel(
        "28. Modeling",
        day_header(28, "Modeling"),
        # plotlyOutput("day28_plot")
      ),

      nav_panel(
        "29. Monochrome",
        day_header(29, "Monochrome"),
        # imageOutput("day29_plot")
      ),

      nav_panel(
        "30. Data Day: GHDx",
        day_header(30, "Data Day: GHDx"),
        # plotlyOutput("day30_plot")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

 output$home <- renderImage({list(src = "www/challenges.jfif", width = "50%", height = "auto")}, deleteFile=FALSE)


  # COMPARISONS
  output$day01_plot <- renderImage({list(src = "www/outputs/day01_part_to_whole_landings.png", width = "100%", height = "auto")}, deleteFile=FALSE)
  output$day02_plot <- renderImage({list(src = "www/outputs/day02_pictogram_oysters.png", width = "100%", height = "auto")}, deleteFile=FALSE)
  output$day03_plot <- renderImage({list(src = "www/outputs/day03_mosaic_aq_prod.png", width = "70%", height = "auto")}, deleteFile=FALSE)
  output$day04_plot <- renderImage({list(src = "www/outputs/day04_slope_alone.png", width = "70%", height = "auto")}, deleteFile=FALSE)
  output$day05_plot <- renderImage({list(src = "www/outputs/day05_diet_euler_matrix.png", width = "100%", height = "auto")}, deleteFile=FALSE)
  output$day06_plot <- renderImage({list(src = "www/outputs/day06_data_rsf_v_epi.png", width = "100%", height = "auto")}, deleteFile=FALSE)

  # DISTRIBUTIONS
  output$day07_plot <- renderUI({
  tagList(
    tags$iframe(
      id     = "day07_frame",
      src    = "outputs/day07_multiscale.html",
      width  = "90%",
      height = "850px",
      style  = "border: none; background-color: #EEEEEE;"
    ),
    tags$script(HTML("
      document.getElementById('day07_frame').onload = function() {
        this.contentDocument.body.style.backgroundColor = '#EEEEEE';
      };
    "))
  )
})
  
  # output$day08_plot <- renderImage()
  # output$day09_plot <- renderImage()
  # output$day10_plot <- renderImage()
  # output$day11_plot <- renderImage()
  # output$day12_plot <- renderImage()

  # RELATIONSHIPS
  # output$day13_plot <- renderImage()
  # output$day14_plot <- renderImage()
  # output$day15_plot <- renderImage()
  # output$day16_plot <- renderImage()
  # output$day17_plot <- renderImage()
  # output$day18_plot <- renderImage()

  # TIMESERIES
  # output$day19_plot <- renderImage()
  # output$day20_plot <- renderImage()
  # output$day21_plot <- renderImage()
  # output$day22_plot <- renderImage()
  # output$day23_plot <- renderImage()
  # output$day24_plot <- renderImage()

  # UNCERTAINTIES
  # output$day25_plot <- renderImage()
  # output$day26_plot <- renderImage()
  # output$day27_plot <- renderImage()
  # output$day28_plot <- renderImage()
  # output$day29_plot <- renderImage()
  # output$day30_plot <- renderImage()

}

shinyApp(ui, server)