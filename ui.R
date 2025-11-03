
# ui.R
library(shiny)
library(shinydashboard)
source("global.R")

dashboardPage(
  dashboardHeader(title = "Global Major Cities Housing Analysis"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",                tabName = "overview",  icon = icon("home")),
      menuItem("Descriptive Insights",    tabName = "insights",  icon = icon("chart-line")),
      menuItem("Financial Analysis",      tabName = "financial", icon = icon("dollar-sign"))
    )
  ),
  dashboardBody(
    
    # ---- Small style tweaks (map reset button over the map) ----
    tags$style(HTML("
      .leaflet-control.custom-reset {
        background: white; padding: 6px 10px; border-radius: 6px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.25);
      }
      .map-reset-container {
        position: absolute; right: 18px; top: 18px; z-index: 500;
      }
    ")),
    
    tabItems(
      
      # ==================================================================
      # OVERVIEW TAB
      # ==================================================================
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_min_year", width = 3),
          valueBoxOutput("vb_countries", width = 3),
          valueBoxOutput("vb_cities", width = 3),
          valueBoxOutput("vb_rows", width = 3)
        ),
        fluidRow(
          box(
            title = "Scope & Purpose", width = 12, status = "primary", solidHeader = TRUE,
            tags$strong("Global Major Cities Housing Purchase Analysis"),
            tags$p("This app focuses on housing purchase data across major global cities."),
            uiOutput("overview_intro"),
            br(),
            uiOutput("overview_image_placeholder")
          )
        )
      ),
      
      # ==================================================================
      # DESCRIPTIVE MARKET INSIGHTS (with Interactive Map)
      # ==================================================================
      tabItem(
        tabName = "insights",
        
        # Controls row (currency + filters)
        fluidRow(
          box(
            title = "Currency & Filters", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "currency_basis", "Price display",
                  choices = c("Local currency (nominal)", "USD (FX, nominal)"),
                  selected = "Local currency (nominal)"
                ),
                uiOutput("fx_note")
              ),
              column(
                width = 4,
                selectInput(
                  "flt_country", "Country",
                  choices = c("All", sort(unique(house$country))),
                  selected = "All", multiple = FALSE, selectize = TRUE
                )
              ),
              column(
                width = 4,
                selectInput(
                  "flt_property", "Property type",
                  choices = c("All", sort(unique(house$property_type))),
                  selected = "All", multiple = FALSE, selectize = TRUE
                )
              )
            )
          )
        ),
        
        # Size vs Price & bin summary
        fluidRow(
          box(
            title = "Property Size vs. Price (bin & compare)", width = 8,
            status = "info", solidHeader = TRUE,
            plotlyOutput("plot_size_price", height = 420)
          ),
          box(
            title = "Selected Bin Summary", width = 4,
            status = "info", solidHeader = TRUE,
            uiOutput("bin_summary")
          )
        ),
        
        # Amenities pies and Furnishing pies by selected quartile
        fluidRow(
          box(
            title = "Amenities in Selected Price Quartile", width = 6,
            status = "warning", solidHeader = TRUE,
            selectInput(
              "amen_quartile", "Price quartile",
              choices = c("Q1", "Q2", "Q3", "Q4"), selected = "Q4"
            ),
            plotlyOutput("pie_amenities", height = 360)
          ),
          box(
            title = "Furnishing Status in Selected Price Quartile", width = 6,
            status = "warning", solidHeader = TRUE,
            plotlyOutput("pie_furnish", height = 360)
          )
        ),
        
        # Age of house impact on value + reset
        fluidRow(
          box(
            title = "Age of House Impact on Value", width = 12,
            status = "success", solidHeader = TRUE,
            div(
              style = "display:flex; gap:10px; align-items:center; margin-bottom:8px;",
              actionButton("btn_reset_age", "Reset view", icon = icon("undo"))
            ),
            plotlyOutput("plot_age_impact", height = 420)
          )
        ),
        
        # Interactive Map with reset overlay
        fluidRow(
          box(
            title = "Interactive Map — Average Prices by Country/City", width = 12,
            status = "primary", solidHeader = TRUE,
            div(class = "map-reset-container",
                tags$div(class = "leaflet-control custom-reset",
                         actionButton("btn_reset_map", "Back to global", icon = icon("globe"))
                )
            ),
            leafletOutput("map_prices", height = 520)
          )
        )
      ),
      
      # ==================================================================
      # FINANCIAL ANALYSIS
      # ==================================================================
      tabItem(
        tabName = "financial",
        
        # Controls
        fluidRow(
          box(
            title = "Filters", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "fin_country", "Country",
                  choices = c("All", sort(unique(house$country))),
                  selected = "All", multiple = FALSE, selectize = TRUE
                )
              ),
              column(
                width = 4,
                selectInput(
                  "fin_property", "Property type",
                  choices = c("All", sort(unique(house$property_type))),
                  selected = "All", multiple = FALSE, selectize = TRUE
                )
              ),
              column(
                width = 4,
                selectInput(
                  "fin_xvar", "X-axis for scatter",
                  choices = c("salary", "loan", "emi"),
                  selected = "salary"
                )
              )
            ),
            sliderInput(
              "fin_salary_bins", "Salary bucket count (for purchased-only bar chart)",
              min = 4, max = 20, value = 8, step = 1
            )
          )
        ),
        
        # Charts
        fluidRow(
          box(
            title = "Salary / Loan / EMI vs Price (colored by decision)", width = 12,
            status = "info", solidHeader = TRUE,
            plotlyOutput("fin_scatter", height = 420)
          )
        ),
        fluidRow(
          box(
            title = "Which income groups buy more expensive properties? (purchases only)",
            width = 12, status = "warning", solidHeader = TRUE,
            plotlyOutput("fin_salary_buckets", height = 380)
          )
        ),
        fluidRow(
          box(
            title = "Affordability Index (Price / Salary) over Year Built",
            width = 12, status = "success", solidHeader = TRUE,
            plotlyOutput("fin_affordability", height = 380)
          )
        ),
        fluidRow(
          box(
            title = "EMI-to-Income Ratio vs Purchase Rate (binned)", width = 12,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("fin_emi_heat", height = 260),
            br(),
            tableOutput("fin_emi_table")
          )
        )
      )
      
    )
  )
)
