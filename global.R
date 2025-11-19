
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

# Global plot theme with larger text
theme_app <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title   = element_text(face = "bold", size = 14),
      axis.title   = element_text(size = 14),
      axis.text    = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text  = element_text(size = 11)
    )
}

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
# (if you want proportions, divide by 100 after this)
parse_percent_numeric <- function(x) {
  readr::parse_number(x)
}

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
    median_sale_price_mom      = median_sale_price_mo_m,
    median_sale_price_yoy      = median_sale_price_yo_y,
    homes_sold_mom             = homes_sold_mo_m,
    homes_sold_yoy             = homes_sold_yo_y,
    new_listings_mom           = new_listings_mo_m,
    new_listings_yoy           = new_listings_yo_y,
    inventory_mom              = inventory_mo_m,
    inventory_yoy              = inventory_yo_y,
    days_on_market_mom         = days_on_market_mo_m,
    days_on_market_yoy         = days_on_market_yo_y,
    average_sale_to_list_mom   = average_sale_to_list_mo_m,
    average_sale_to_list_yoy   = average_sale_to_list_yo_y
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
    
    # MoM / YoY deltas for price, volume, supply (numeric percentage values)
    median_sale_price_mom = parse_percent_numeric(median_sale_price_mom),
    median_sale_price_yoy = parse_percent_numeric(median_sale_price_yoy),
    
    homes_sold_mom = parse_percent_numeric(homes_sold_mom),
    homes_sold_yoy = parse_percent_numeric(homes_sold_yoy),
    
    new_listings_mom = parse_percent_numeric(new_listings_mom),
    new_listings_yoy = parse_percent_numeric(new_listings_yoy),
    
    inventory_mom = parse_percent_numeric(inventory_mom),
    inventory_yoy = parse_percent_numeric(inventory_yoy),
    
    # Days on market MoM/YoY are absolute changes (e.g., 3, -8)
    days_on_market_mom = readr::parse_number(days_on_market_mom),
    days_on_market_yoy = readr::parse_number(days_on_market_yoy),
    
    # Average sale-to-list and its changes as numeric percentages
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

# 5) Helper objects to reuse

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
