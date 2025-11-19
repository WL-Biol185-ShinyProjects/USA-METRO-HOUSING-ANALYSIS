
# ui.R

ui <- dashboardPage(
  dashboardHeader(title = "Redfin Monthly Housing Explorer", titleWidth = 400),
  
  dashboardSidebar(
    width = 400,
    sidebarMenu(
      menuItem(
        text    = "Market Overview and Summary",
        tabName = "overview",
        icon    = icon("chart-line")
      )
      # Additional menuItem() entries for other tabs will go here later
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------------------------
      # TAB 1: OVERVIEW
      # ---------------------------
      tabItem(
        tabName = "overview",
        
        fluidRow(
          
          # Filters box
          box(
            title       = "Filters",
            status      = "primary",
            solidHeader = TRUE,
            width       = 3,
            
            pickerInput(
              inputId  = "ov_regions",
              label    = "Select Regions:",
              choices  = region_choices,
              selected = c(
                "National"
              ),
              multiple = TRUE,
              options  = list(
                `actions-box` = TRUE,  # Select All / Deselect All buttons
                `live-search` = TRUE   # search box inside dropdown
              )
            ),
            
            sliderInput(
              inputId = "ov_date_range",
              label   = "Select Date Range:",
              min     = min(redfin$month_end, na.rm = TRUE),
              max     = max(redfin$month_end, na.rm = TRUE),
              value   = c(
                min(redfin$month_end, na.rm = TRUE),
                max(redfin$month_end, na.rm = TRUE)
              ),
              timeFormat = "%b %Y"
            )
          ),
          
          # Main plots box
          box(
            title       = "Market Overview",
            status      = "primary",
            solidHeader = TRUE,
            width       = 9,
            
            # Main headline plot
            plotOutput("ov_price_plot", height = "350px"),
            
            # Secondary plots
            fluidRow(
              column(4, plotOutput("ov_inventory_plot",  height = "250px")),
              column(4, plotOutput("ov_dom_plot",        height = "250px")),
              column(4, plotOutput("ov_saletolist_plot", height = "250px"))
            ),
            
            hr(),
            h4("Market Summary"),
            textOutput("ov_summary")
          )
        )
      )
    )
  )
)
