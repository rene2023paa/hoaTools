library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(yaml)

# Define UI with bslib
ui <- page_navbar(
  title = "HOA Assessment Tools",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#3498db",
    base_font = font_google("Roboto")
  ),

  # Page 1: Assessment Analysis
  nav_panel(
    title = "Assessment Analysis",
    icon = icon("chart-line"),

    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",

        fileInput(
          "yaml_file",
          "Upload YAML File (optional)",
          accept = c(".yaml", ".yml")
        ),

        selectInput(
          "unit_type",
          "Select Unit Type:",
          choices = c("All Units" = "all"),
          selected = "all"
        ),

        numericInput(
          "manual_rate",
          "Annual Increase Rate (%):",
          value = NULL,
          step = 0.1,
          min = 0,
          max = 20
        ),

        actionButton(
          "reset_rate",
          "Use Calculated Average",
          class = "btn-primary"
        ),

        hr(),

        h5("Summary Statistics"),
        verbatimTextOutput("stats_summary")
      ),

      navset_card_tab(
        nav_panel(
          "Percent Increase",
          plotOutput("percent_plot", height = "400px"),
          br(),
          card(
            card_header("Period Summary"),
            tableOutput("increase_table")
          )
        ),

        nav_panel(
          "Forecast Chart",
          plotOutput("forecast_plot", height = "500px"),
          card(
            card_body(
              markdown(
                "**Note:** Solid points and lines represent historical data. 
                       Dashed lines represent forecasted values based on the selected annual increase rate."
              )
            )
          )
        ),

        nav_panel(
          "Forecast Table",
          card(
            card_header("5-Year Forecast Details"),
            tableOutput("forecast_table")
          )
        )
      )
    )
  ),

  # Page 2: Raw Data
  nav_panel(
    title = "Raw Data",
    icon = icon("table"),

    card(
      card_header("Assessment Data by Unit Type"),
      card_body(
        tableOutput("raw_data_table")
      )
    )
  ),

  # Page 3: About
  nav_panel(
    title = "About",
    icon = icon("info-circle"),

    layout_columns(
      col_widths = c(12, 12),

      card(
        card_header("About This Application"),
        card_body(
          h4("HOA Assessment Analysis Tool"),
          p(
            "This application helps analyze and forecast HOA assessment increases over time."
          ),
          h5("Features:"),
          tags$ul(
            tags$li(
              "Upload YAML files with assessment data or use embedded sample data"
            ),
            tags$li("Calculate historical percentage increases (annualized)"),
            tags$li("Visualize trends across different unit types"),
            tags$li("Generate 5-year forecasts based on historical trends"),
            tags$li("Adjust forecast parameters interactively")
          )
        )
      ),

      card(
        card_header("How to Use"),
        card_body(
          h5("Step 1: Load Data"),
          p("Upload your YAML file or use the built-in sample data."),

          h5("Step 2: Explore Analysis"),
          p("Navigate to the 'Assessment Analysis' page to view:"),
          tags$ul(
            tags$li("Annual percentage increases by period"),
            tags$li("Visual forecasts for the next 5 years"),
            tags$li("Detailed forecast tables")
          ),

          h5("Step 3: Adjust Parameters"),
          p("Use the sidebar controls to:"),
          tags$ul(
            tags$li("Filter by specific unit types"),
            tags$li("Override the calculated average rate with custom values"),
            tags$li("Reset to use the calculated average")
          )
        )
      ),

      card(
        card_header("Data Format"),
        card_body(
          p("Your YAML file should follow this structure:"),
          pre(
            'units:
  - unit_size: "1 bed 1 bath"
    no_of_unit: 60
    2020: 236.77
    2023: 251.16
    2024: 275.77'
          )
        )
      )
    )
  ),

  # Footer
  nav_spacer(),
  nav_item(
    tags$a(
      icon("github"),
      "Source Code",
      href = "#",
      target = "_blank"
    )
  )
)
