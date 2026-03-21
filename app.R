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
        # imageOutput("day01_plot")
        tags$div(
          style = "max-width: 900px;",
          imageOutput("day01_plot", height = "auto"))
      ),

      nav_panel(
        "2. Pictogram",
        day_header(2, "Pictogram"),
        # imageOutput("day02_plot")
      ),

      nav_panel(
        "3. Mosaic",
        day_header(3, "Mosaic"),
        # imageOutput("day03_plot")
      ),

      nav_panel(
        "4. Slope",
        day_header(4, "Slope"),
        # plotlyOutput("day04_plot")
      ),

      nav_panel(
        "5. Experimental",
        day_header(5, "Experimental"),
        # imageOutput("day05_plot")
      ),

      nav_panel(
        "6. Data Day: RSF",
        day_header(6, "Data Day: RSF"),
        # plotlyOutput("day06_plot")
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
        # uiOutput("day07_plot")
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

  # COMPARISONS
  # output$day01_plot <- renderImage({list(src = "www/assets/XXXXX.png", width = "100%", height = "auto")}, deleteFile=FALSE)
  # output$day02_plot <- renderImage()
  # output$day03_plot <- renderImage()
  # output$day04_plot <- renderImage()
  # output$day05_plot <- renderImage()
  # output$day06_plot <- renderImage()

  # DISTRIBUTIONS
  # output$day07_plot <- renderImage()
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