
# global.R — shared by ui.R and server.R
# Loads data once and defines helpers used across the app

library(shiny)
library(tidyverse)
library(leaflet)
library(shinydashboard)
library(plotly)
library(lubridate)
library(janitor)
library(readr)
library(shinyWidgets)
library(stringr)

# =============================================================================
# Color Palette and Theme Configuration
# =============================================================================

# Primary color palette (professional blue-based scheme)
app_colors <- list(
  primary      = "#2C3E50",
  secondary    = "#3498DB",
  accent       = "#E74C3C",
  success      = "#27AE60",
  warning      = "#F39C12",
  neutral      = "#95A5A6",
  background   = "#ECF0F1",
  text_dark    = "#2C3E50",
  text_light   = "#7F8C8D"
)

# Region-specific colors for consistent identification across plots
region_colors <- c(
  "National"                      = "#2C3E50",
  "Boston, MA metro area"         = "#3498DB",
  "Chicago, IL metro area"        = "#E74C3C",
  "Los Angeles, CA metro area"    = "#F39C12",
  "Philadelphia, PA metro area"   = "#9B59B6",
  "Seattle, WA metro area"        = "#1ABC9C",
  "Washington, DC metro area"     = "#E67E22"
)

# Global plot theme with refined aesthetics
theme_app <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      # Title and text
      plot.title = element_text(
        face   = "bold",
        size   = base_size + 2,
        color  = app_colors$text_dark,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size   = base_size,
        color  = app_colors$text_light,
        margin = margin(b = 15)
      ),
      
      # Axes
      axis.title = element_text(
        size  = base_size,
        color = app_colors$text_dark
      ),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(
        size  = base_size - 4,
        color = app_colors$text_light
      ),
      axis.line = element_line(
        color     = app_colors$neutral,
        linewidth = 0.5
      ),
      
      # Grid
      panel.grid.major = element_line(
        color     = "#E0E0E0",
        linewidth = 0.3
      ),
      panel.grid.minor = element_blank(),
      
      # Legend
      legend.title = element_text(
        size  = base_size - 1,
        face  = "bold",
        color = app_colors$text_dark
      ),
      legend.text = element_text(
        size  = base_size - 4,
        color = app_colors$text_light
      ),
      legend.position  = "bottom",
      legend.key.size  = unit(1.2, "lines"),
      legend.box.margin = margin(t = 10),
      
      # Background
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

# Compact theme for secondary/smaller plots
theme_app_compact <- function(base_size = 12) {
  theme_app(base_size = base_size) +
    theme(
      legend.position = "none",
      plot.margin     = margin(10, 10, 10, 10),
      axis.title.x    = element_blank()
    )
}

# =============================================================================
# Helper Functions
# =============================================================================

# Helper: parse prices like "$159K", "$1.2M", "250000"
parse_price <- function(x) {
  base <- readr::parse_number(x)
  multiplier <- case_when(
    str_detect(x, regex("k", ignore_case = TRUE)) ~ 1e3,
    str_detect(x, regex("m", ignore_case = TRUE)) ~ 1e6,
    TRUE ~ 1
  )
  base * multiplier
}

# Helper: parse percentage strings like "-6.40%" into numeric percent (-6.40)
parse_percent_numeric <- function(x) {
  readr::parse_number(x)
}

# Helper: format currency for display
format_currency <- function(x, accuracy = 1) {
  scales::label_dollar(accuracy = accuracy, scale = 1e-3, suffix = "k")(x)
}

# Helper: format percentage for display
format_pct <- function(x, accuracy = 0.1) {
  scales::label_percent(accuracy = accuracy, scale = 1)(x)
}

# Helper: create a value box style metric card
create_metric_html <- function(value, label, change = NULL, change_label = NULL) {
  change_html <- ""
  if (!is.null(change) && !is.na(change)) {
    change_color <- if (change >= 0) app_colors$success else app_colors$accent
    change_icon  <- if (change >= 0) "▲" else "▼"
    change_html  <- sprintf(
      '<span style="color: %s; font-size: 0.9em;">%s %.1f%% %s</span>',
      change_color, change_icon, abs(change),
      ifelse(!is.null(change_label), change_label, "")
    )
  }
  
  sprintf(
    '<div class="metric-card">
       <div class="metric-value">%s</div>
       <div class="metric-label">%s</div>
       %s
     </div>',
    value, label, change_html
  )
}

# =============================================================================
# Data Loading and Cleaning
# =============================================================================

# 1) Read Redfin data (treat everything as text first for full control)
redfin_raw <- read.csv(
  "redfin-monthly-housing-data.csv",
  colClasses       = "character",
  na.strings       = c("", "NA"),
  stringsAsFactors = FALSE,
  check.names      = FALSE
)

# 2) Clean column names and normalize the MoM/YoY names
redfin <- redfin_raw %>%
  clean_names() %>%
  rename(
    median_sale_price_mom    = median_sale_price_mo_m,
    median_sale_price_yoy    = median_sale_price_yo_y,
    homes_sold_mom           = homes_sold_mo_m,
    homes_sold_yoy           = homes_sold_yo_y,
    new_listings_mom         = new_listings_mo_m,
    new_listings_yoy         = new_listings_yo_y,
    inventory_mom            = inventory_mo_m,
    inventory_yoy            = inventory_yo_y,
    days_on_market_mom       = days_on_market_mo_m,
    days_on_market_yoy       = days_on_market_yo_y,
    average_sale_to_list_mom = average_sale_to_list_mo_m,
    average_sale_to_list_yoy = average_sale_to_list_yo_y
  )

# 3) Parse date, region, and numeric fields
redfin <- redfin %>%
  mutate(
    # "Jan-12" -> date
    month_end = lubridate::my(month_of_period_end),
    region    = factor(stringr::str_squish(region)),
    
    # Core level variables
    median_sale_price = parse_price(median_sale_price),
    homes_sold        = readr::parse_number(homes_sold),
    new_listings      = readr::parse_number(new_listings),
    inventory         = readr::parse_number(inventory),
    days_on_market    = readr::parse_number(days_on_market),
    
    # MoM / YoY deltas for price, volume, supply
    median_sale_price_mom = parse_percent_numeric(median_sale_price_mom),
    median_sale_price_yoy = parse_percent_numeric(median_sale_price_yoy),
    
    homes_sold_mom = parse_percent_numeric(homes_sold_mom),
    homes_sold_yoy = parse_percent_numeric(homes_sold_yoy),
    
    new_listings_mom = parse_percent_numeric(new_listings_mom),
    new_listings_yoy = parse_percent_numeric(new_listings_yoy),
    
    inventory_mom = parse_percent_numeric(inventory_mom),
    inventory_yoy = parse_percent_numeric(inventory_yoy),
    
    # Days on market MoM/YoY are absolute changes
    days_on_market_mom = readr::parse_number(days_on_market_mom),
    days_on_market_yoy = readr::parse_number(days_on_market_yoy),
    
    # Average sale-to-list and its changes
    average_sale_to_list     = parse_percent_numeric(average_sale_to_list),
    average_sale_to_list_mom = parse_percent_numeric(average_sale_to_list_mom),
    average_sale_to_list_yoy = parse_percent_numeric(average_sale_to_list_yoy)
  )

# 4) Derived metrics for later tabs
redfin <- redfin %>%
  mutate(
    tightness_ratio = homes_sold / new_listings,
    months_supply   = inventory / homes_sold
  )

# =============================================================================
# Helper Objects for UI
# =============================================================================

# Distinct regions from data, alphabetically
regions_vec <- redfin %>%
  distinct(region) %>%
  arrange(region) %>%
  pull(region)

# Build choices for the UI: (All), then National, then others alphabetically
region_choices <- c(
  "(All)",
  "National",
  setdiff(regions_vec, "National")
)

regions      <- regions_vec
latest_month <- max(redfin$month_end, na.rm = TRUE)
earliest_month <- min(redfin$month_end, na.rm = TRUE)

# =============================================================================
# Custom CSS for Dashboard Styling
# =============================================================================

app_css <- "
/* Main dashboard styling */
.skin-blue .main-header .logo {
  background-color: #2C3E50;
  font-weight: 600;
}
.skin-blue .main-header .navbar {
  background-color: #2C3E50;
}
.skin-blue .main-header .logo:hover {
  background-color: #34495E;
}
.skin-blue .main-sidebar {
  background-color: #34495E;
}
.skin-blue .sidebar-menu > li.active > a,
.skin-blue .sidebar-menu > li:hover > a {
  background-color: #2C3E50;
  border-left-color: #3498DB;
}
.skin-blue .sidebar-menu > li > a {
  border-left: 3px solid transparent;
}

/* Box styling */
.box {
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  border-top: none;
}
.box.box-primary {
  border-top: 3px solid #3498DB;
}
.box.box-success {
  border-top: 3px solid #27AE60;
}
.box.box-warning {
  border-top: 3px solid #F39C12;
}
.box.box-danger {
  border-top: 3px solid #E74C3C;
}
.box-header {
  padding: 15px 15px 10px 15px;
}
.box-header .box-title {
  font-size: 16px;
  font-weight: 600;
  color: #2C3E50;
}
.box-body {
  padding: 15px;
}

/* Content wrapper */
.content-wrapper {
  background-color: #ECF0F1;
}

/* Metric cards */
.metric-card {
  text-align: center;
  padding: 15px;
  background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
  border-radius: 8px;
  border: 1px solid #e0e0e0;
  margin-bottom: 10px;
}
.metric-value {
  font-size: 24px;
  font-weight: 700;
  color: #2C3E50;
  margin-bottom: 5px;
}
.metric-label {
  font-size: 12px;
  color: #7F8C8D;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

/* Value boxes */
.small-box {
  border-radius: 8px;
}
.small-box h3 {
  font-size: 28px;
  font-weight: 700;
}
.small-box p {
  font-size: 14px;
}

/* Picker input styling */
.bootstrap-select .dropdown-toggle {
  border-radius: 6px;
  border-color: #ddd;
}
.bootstrap-select .dropdown-toggle:focus {
  border-color: #3498DB;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
}

/* Slider styling */
.irs--shiny .irs-bar {
  background: #3498DB;
  border-top: 1px solid #2980B9;
  border-bottom: 1px solid #2980B9;
}
.irs--shiny .irs-handle {
  background-color: #3498DB;
  border: 2px solid #2980B9;
}
.irs--shiny .irs-from,
.irs--shiny .irs-to,
.irs--shiny .irs-single {
  background-color: #3498DB;
}

/* Summary text styling */
.summary-text {
  background-color: #f8f9fa;
  border-left: 4px solid #3498DB;
  padding: 15px 20px;
  border-radius: 0 8px 8px 0;
  font-size: 14px;
  line-height: 1.6;
  color: #2C3E50;
}

/* Tab styling */
.nav-tabs-custom > .nav-tabs > li.active {
  border-top-color: #3498DB;
}

/* Responsive adjustments */
@media (max-width: 768px) {
  .metric-value {
    font-size: 20px;
  }
  .box-header .box-title {
    font-size: 14px;
  }
}

/* Explore button hover effect */
.btn-explore:hover {
  background-color: #C0392B !important;
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(231, 76, 60, 0.5) !important;
}

/* Homepage stat cards hover */
.home-stat-card:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 25px rgba(0,0,0,0.2);
}
"
