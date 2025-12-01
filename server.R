
# server.R
# Enhanced server with improved visualizations and dynamic content

server <- function(input, output, session) {
  
  # ===========================================================================
  # NAVIGATION: Header click returns to home
  # ===========================================================================
  
  observeEvent(input$go_home, {
    updateTabItems(session, "tabs", "home")
  })
  
  observeEvent(input$go_overview, {
    updateTabItems(session, "tabs", "overview")
  })
  
  # ===========================================================================
  # OVERVIEW TAB: Reactive Values and Observers
  # ===========================================================================
  
  # Track whether "(All)" is currently considered active
  all_selected <- reactiveVal(FALSE)
  
  # Handle "(All)" behavior in the multi-select
  observeEvent(input$ov_regions, {
    all_label      <- "(All)"
    actual_regions <- setdiff(region_choices, all_label)
    selected       <- input$ov_regions
    
    if (is.null(selected)) {
      selected <- character(0)
    }
    
    has_all_now    <- all_label %in% selected
    had_all_before <- all_selected()
    
    if (has_all_now && !had_all_before) {
      all_selected(TRUE)
      updatePickerInput(
        session,
        "ov_regions",
        selected = c(all_label, actual_regions)
      )
    } else if (!has_all_now && had_all_before) {
      all_selected(FALSE)
      updatePickerInput(
        session,
        "ov_regions",
        selected = character(0)
      )
    } else {
      all_selected(has_all_now)
    }
  })
  
  # Quick date range buttons
  observeEvent(input$ov_last_year, {
    updateSliderInput(
      session,
      "ov_date_range",
      value = c(latest_month - years(1), latest_month)
    )
  })
  
  observeEvent(input$ov_last_3years, {
    updateSliderInput(
      session,
      "ov_date_range",
      value = c(latest_month - years(3), latest_month)
    )
  })
  
  observeEvent(input$ov_all_time, {
    updateSliderInput(
      session,
      "ov_date_range",
      value = c(earliest_month, latest_month)
    )
  })
  
  # ===========================================================================
  # OVERVIEW TAB: Filtered Data
  # ===========================================================================
  
  ov_data <- reactive({
    req(input$ov_regions)
    
    # Drop the "(All)" label; only real regions for filtering
    selected_regions <- setdiff(input$ov_regions, "(All)")
    req(length(selected_regions) > 0)
    
    redfin %>%
      filter(
        region %in% selected_regions,
        month_end >= input$ov_date_range[1],
        month_end <= input$ov_date_range[2]
      ) %>%
      arrange(region, month_end)
  })
  
  # Latest data for metrics
  ov_latest <- reactive({
    ov_data() %>%
      group_by(region) %>%
      filter(month_end == max(month_end)) %>%
      ungroup()
  })
  
  # ===========================================================================
  # OVERVIEW TAB: Legend Panel
  # ===========================================================================
  
  output$ov_legend_panel <- renderUI({
    req(input$ov_regions)
    
    # Get selected regions (excluding "(All)")
    selected_regions <- setdiff(input$ov_regions, "(All)")
    req(length(selected_regions) > 0)
    
    # Create legend items
    legend_items <- lapply(selected_regions, function(reg) {
      color <- region_colors[[reg]]
      if (is.null(color)) color <- "#666666"
      
      tags$div(
        style = "display: inline-flex; align-items: center; margin-right: 15px; margin-bottom: 8px;",
        tags$span(
          style = paste0(
            "width: 20px; height: 3px; background-color: ", color, 
            "; display: inline-block; margin-right: 6px; border-radius: 2px;"
          )
        ),
        tags$span(
          style = "font-size: 12px; color: #2C3E50;",
          reg
        )
      )
    })
    
    tags$div(
      style = "display: flex; flex-wrap: wrap; justify-content: flex-start;",
      legend_items
    )
  })
  
  # ===========================================================================
  # OVERVIEW TAB: Key Metrics Panel
  # ===========================================================================
  
  output$ov_metrics_panel <- renderUI({
    req(ov_latest())
    
    latest <- ov_latest()
    
    # If multiple regions, show aggregated stats
    if (nrow(latest) > 1) {
      avg_price    <- mean(latest$median_sale_price, na.rm = TRUE)
      avg_dom      <- mean(latest$days_on_market, na.rm = TRUE)
      avg_stl      <- mean(latest$average_sale_to_list, na.rm = TRUE)
      total_homes  <- sum(latest$homes_sold, na.rm = TRUE)
      
      tags$div(
        tags$div(
          class = "metric-card",
          tags$div(class = "metric-value", format_currency(avg_price)),
          tags$div(class = "metric-label", "Avg. Median Price")
        ),
        tags$div(
          class = "metric-card",
          tags$div(class = "metric-value", round(avg_dom, 0)),
          tags$div(class = "metric-label", "Avg. Days on Market")
        ),
        tags$div(
          class = "metric-card",
          tags$div(class = "metric-value", paste0(round(avg_stl, 1), "%")),
          tags$div(class = "metric-label", "Avg. Sale-to-List")
        ),
        tags$div(
          class = "metric-card",
          tags$div(
            class = "metric-value",
            scales::label_comma()(total_homes)
          ),
          tags$div(class = "metric-label", "Total Homes Sold")
        )
      )
    } else {
      # Single region: show with YoY changes
      r <- latest[1, ]
      
      price_change <- r$median_sale_price_yoy
      dom_change   <- r$days_on_market_yoy
      stl_change   <- r$average_sale_to_list_yoy
      
      tags$div(
        tags$div(
          class = "metric-card",
          tags$div(class = "metric-value", format_currency(r$median_sale_price)),
          tags$div(class = "metric-label", "Median Sale Price"),
          if (!is.na(price_change)) {
            change_color <- if (price_change >= 0) app_colors$success else app_colors$accent
            change_icon  <- if (price_change >= 0) "▲" else "▼"
            tags$span(
              style = paste0("color: ", change_color, "; font-size: 0.85em;"),
              paste(change_icon, abs(round(price_change, 1)), "% YoY")
            )
          }
        ),
        tags$div(
          class = "metric-card",
          tags$div(class = "metric-value", round(r$days_on_market, 0)),
          tags$div(class = "metric-label", "Days on Market"),
          if (!is.na(dom_change)) {
            # For DOM, negative is good (faster sales)
            change_color <- if (dom_change <= 0) app_colors$success else app_colors$accent
            change_icon  <- if (dom_change <= 0) "▼" else "▲"
            tags$span(
              style = paste0("color: ", change_color, "; font-size: 0.85em;"),
              paste(change_icon, abs(round(dom_change, 0)), "days YoY")
            )
          }
        ),
        tags$div(
          class = "metric-card",
          tags$div(class = "metric-value", paste0(round(r$average_sale_to_list, 1), "%")),
          tags$div(class = "metric-label", "Sale-to-List Ratio"),
          if (!is.na(stl_change)) {
            change_color <- if (stl_change >= 0) app_colors$success else app_colors$accent
            change_icon  <- if (stl_change >= 0) "▲" else "▼"
            tags$span(
              style = paste0("color: ", change_color, "; font-size: 0.85em;"),
              paste(change_icon, abs(round(stl_change, 2)), "pp YoY")
            )
          }
        ),
        tags$div(
          class = "metric-card",
          tags$div(
            class = "metric-value",
            scales::label_comma()(r$homes_sold)
          ),
          tags$div(class = "metric-label", "Homes Sold")
        )
      )
    }
  })
  
  # ===========================================================================
  # OVERVIEW TAB: Plots
  # ===========================================================================
  
  # Headline Median Sale Price chart (interactive with plotly)
  output$ov_price_plot <- renderPlotly({
    req(nrow(ov_data()) > 0)
    
    # Prepare data with formatted tooltip text
    plot_data <- ov_data() %>%
      mutate(
        tooltip_text = paste0(
          "<b>", region, "</b><br>",
          "<b>Month:</b> ", format(month_end, "%B %Y"), "<br>",
          "<b>Median Sale Price:</b> ", scales::dollar(median_sale_price), "<br>",
          "<b>Inventory:</b> ", scales::comma(inventory), "<br>",
          "<b>Days on Market:</b> ", round(days_on_market, 0), "<br>",
          "<b>Sale-to-List:</b> ", round(average_sale_to_list, 1), "%"
        )
      )
    
    # Calculate date range span to determine appropriate axis breaks
    date_range_days <- as.numeric(
      difftime(input$ov_date_range[2], input$ov_date_range[1], units = "days")
    )
    
    # Set date breaks and format based on range
    if (date_range_days <= 365) {
      # 1 year or less: show every month
      date_breaks <- "1 month"
      date_format <- "%b '%y"
    } else if (date_range_days <= 730) {
      # 1-2 years: show every 2 months
      date_breaks <- "2 months"
      date_format <- "%b '%y"
    } else if (date_range_days <= 1825) {
      # 2-5 years: show every 6 months
      date_breaks <- "6 months"
      date_format <- "%b '%y"
    } else {
      # More than 5 years: show every year
      date_breaks <- "1 year"
      date_format <- "%b '%y"
    }
    
    # Create ggplot (lines only, no points)
    p <- ggplot(
      plot_data,
      aes(
        x     = month_end,
        y     = median_sale_price,
        color = region,
        text  = tooltip_text,
        group = region
      )
    ) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(
        values = region_colors,
        name   = NULL
      ) +
      scale_y_continuous(
        limits = c(0, NA),
        labels = scales::label_dollar(scale = 1e-3, suffix = "k"),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks
      ) +
      labs(
        x = NULL,
        y = "Median Sale Price"
      ) +
      theme_app() +
      theme(
        legend.position = "none",
        axis.text.x     = element_text(angle = 45, hjust = 1)
      )
    
    # Convert to plotly with custom tooltip (no legend in plot)
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          font        = list(family = "Arial", size = 12, color = "#2C3E50"),
          bordercolor = "transparent"
        ),
        showlegend = FALSE,
        margin     = list(b = 80, t = 20)
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
          "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
  })
  
  # Inventory over time (exclude National when other regions selected)
  output$ov_inventory_plot <- renderPlot({
    req(nrow(ov_data()) > 0)
    
    # Get selected regions excluding National if others are present
    plot_data <- ov_data()
    selected_regions <- unique(plot_data$region)
    
    if (length(selected_regions) > 1 && "National" %in% selected_regions) {
      plot_data <- plot_data %>%
        filter(region != "National")
    }
    
    ggplot(
      plot_data,
      aes(x = month_end, y = inventory, color = region)
    ) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = region_colors) +
      scale_y_continuous(
        labels = scales::label_comma(scale = 1e-3, suffix = "k"),
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      scale_x_date(date_labels = "%Y") +
      labs(x = NULL, y = NULL) +
      theme_app_compact()
  }, res = 96)
  
  # Days on Market over time
  output$ov_dom_plot <- renderPlot({
    req(nrow(ov_data()) > 0)
    
    ggplot(
      ov_data(),
      aes(x = month_end, y = days_on_market, color = region)
    ) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = region_colors) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      scale_x_date(date_labels = "%Y") +
      labs(x = NULL, y = NULL) +
      theme_app_compact()
  }, res = 96)
  
  # Average Sale-to-List over time
  output$ov_saletolist_plot <- renderPlot({
    req(nrow(ov_data()) > 0)
    
    ggplot(
      ov_data(),
      aes(x = month_end, y = average_sale_to_list, color = region)
    ) +
      geom_hline(
        yintercept = 100,
        linetype   = "dashed",
        color      = app_colors$neutral,
        linewidth  = 0.5
      ) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = region_colors) +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      scale_x_date(date_labels = "%Y") +
      labs(x = NULL, y = NULL) +
      theme_app_compact()
  }, res = 96)
  
  # ===========================================================================
  # OVERVIEW TAB: Dynamic Summary
  # ===========================================================================
  
  output$ov_summary <- renderText({
    req(ov_data(), ov_latest())
    
    data   <- ov_data()
    latest <- ov_latest()
    
    # Get date range
    date_start <- format(min(data$month_end), "%B %Y")
    date_end   <- format(max(data$month_end), "%B %Y")
    
    # Find highest and lowest price regions
    if (nrow(latest) > 1) {
      highest_price <- latest %>%
        filter(median_sale_price == max(median_sale_price, na.rm = TRUE)) %>%
        pull(region) %>%
        as.character() %>%
        first()
      
      lowest_price <- latest %>%
        filter(median_sale_price == min(median_sale_price, na.rm = TRUE)) %>%
        pull(region) %>%
        as.character() %>%
        first()
      
      highest_price_val <- latest %>%
        filter(region == highest_price) %>%
        pull(median_sale_price) %>%
        format_currency()
      
      lowest_price_val <- latest %>%
        filter(region == lowest_price) %>%
        pull(median_sale_price) %>%
        format_currency()
      
      # Market conditions
      fastest_market <- latest %>%
        filter(days_on_market == min(days_on_market, na.rm = TRUE)) %>%
        pull(region) %>%
        as.character() %>%
        first()
      
      fastest_dom <- latest %>%
        filter(region == fastest_market) %>%
        pull(days_on_market) %>%
        round(0)
      
      # Hottest market (highest sale-to-list)
      hottest <- latest %>%
        filter(average_sale_to_list == max(average_sale_to_list, na.rm = TRUE)) %>%
        pull(region) %>%
        as.character() %>%
        first()
      
      hottest_stl <- latest %>%
        filter(region == hottest) %>%
        pull(average_sale_to_list) %>%
        round(1)
      
      paste0(
        "From ", date_start, " to ", date_end, ", ",
        highest_price, " shows the highest median sale price at ", highest_price_val,
        ", while ", lowest_price, " remains the most affordable at ", lowest_price_val, ". ",
        fastest_market, " has the fastest-moving market with homes averaging just ",
        fastest_dom, " days on market. ",
        if (hottest_stl >= 100) {
          paste0(
            hottest, " appears hottest with homes selling at ",
            hottest_stl, "% of list price on average."
          )
        } else {
          paste0(
            "Markets are generally balanced, with ",
            hottest, " showing the strongest sale-to-list ratio at ",
            hottest_stl, "%."
          )
        }
      )
      
    } else {
      # Single region summary
      r <- latest[1, ]
      region_name <- as.character(r$region)
      
      # Calculate price trend
      first_price <- data %>%
        filter(region == region_name) %>%
        filter(month_end == min(month_end)) %>%
        pull(median_sale_price)
      
      last_price <- r$median_sale_price
      
      price_change_pct <- ((last_price - first_price) / first_price) * 100
      price_direction  <- if (price_change_pct > 0) "increased" else "decreased"
      
      # Market heat assessment
      market_heat <- if (r$average_sale_to_list >= 100 && r$days_on_market < 60) {
        "a seller's market with strong demand"
      } else if (r$average_sale_to_list < 98 || r$days_on_market > 90) {
        "a buyer-friendly market with more negotiating room"
      } else {
        "a balanced market"
      }
      
      paste0(
        "In ", region_name, ", the median sale price has ", price_direction,
        " by ", abs(round(price_change_pct, 1)), "% over the selected period, ",
        "from ", format_currency(first_price), " to ", format_currency(last_price), ". ",
        "As of ", format(r$month_end, "%B %Y"), ", homes are averaging ",
        round(r$days_on_market, 0), " days on market ",
        "and selling at ", round(r$average_sale_to_list, 1), "% of list price, ",
        "indicating ", market_heat, "."
      )
    }
  })
  
  # ===========================================================================
  # REGION EXPLORER TAB
  # ===========================================================================
  
  # Quick date range buttons
  
  observeEvent(input$re_last_year, {
    updateSliderInput(
      session,
      "re_date_range",
      value = c(latest_month - years(1), latest_month)
    )
  })
  
  observeEvent(input$re_last_3years, {
    updateSliderInput(
      session,
      "re_date_range",
      value = c(latest_month - years(3), latest_month)
    )
  })
  
  observeEvent(input$re_last_5years, {
    updateSliderInput(
      session,
      "re_date_range",
      value = c(latest_month - years(5), latest_month)
    )
  })
  
  observeEvent(input$re_all_time, {
    updateSliderInput(
      session,
      "re_date_range",
      value = c(earliest_month, latest_month)
    )
  })
  
  # Filtered data for Region Explorer
  re_data <- reactive({
    req(input$re_regions)
    req(length(input$re_regions) > 0)
    
    redfin %>%
      filter(
        region %in% input$re_regions,
        month_end >= input$re_date_range[1],
        month_end <= input$re_date_range[2]
      ) %>%
      arrange(region, month_end)
  })
  
  # Latest data for metrics
  re_latest <- reactive({
    re_data() %>%
      group_by(region) %>%
      filter(month_end == max(month_end)) %>%
      ungroup()
  })
  
  # Legend panel
  output$re_legend_panel <- renderUI({
    req(input$re_regions)
    
    legend_items <- lapply(input$re_regions, function(reg) {
      color <- region_colors[[reg]]
      if (is.null(color)) color <- "#666666"
      
      tags$div(
        style = "display: inline-flex; align-items: center; margin-right: 15px; margin-bottom: 8px;",
        tags$span(
          style = paste0(
            "width: 20px; height: 3px; background-color: ", color,
            "; display: inline-block; margin-right: 6px; border-radius: 2px;"
          )
        ),
        tags$span(
          style = "font-size: 12px; color: #2C3E50;",
          reg
        )
      )
    })
    
    tags$div(
      style = "display: flex; flex-wrap: wrap; justify-content: flex-start;",
      legend_items
    )
  })
  
  # Metric labels helper
  metric_labels <- c(
    "median_sale_price"       = "Median Sale Price",
    "median_sale_price_mom"   = "Price MoM Change (%)",
    "median_sale_price_yoy"   = "Price YoY Change (%)",
    "homes_sold"              = "Homes Sold",
    "homes_sold_mom"          = "Homes Sold MoM (%)",
    "homes_sold_yoy"          = "Homes Sold YoY (%)",
    "new_listings"            = "New Listings",
    "new_listings_mom"        = "New Listings MoM (%)",
    "new_listings_yoy"        = "New Listings YoY (%)",
    "inventory"               = "Inventory",
    "inventory_mom"           = "Inventory MoM (%)",
    "inventory_yoy"           = "Inventory YoY (%)",
    "days_on_market"          = "Days on Market",
    "days_on_market_mom"      = "DOM MoM Change",
    "days_on_market_yoy"      = "DOM YoY Change",
    "average_sale_to_list"    = "Sale-to-List Ratio (%)",
    "average_sale_to_list_mom" = "Sale-to-List MoM",
    "average_sale_to_list_yoy" = "Sale-to-List YoY"
  )
  
  # Dynamic chart title
  output$re_main_chart_title <- renderUI({
    metric_name <- metric_labels[[input$re_metric]]
    tagList(icon("chart-area"), paste(" ", metric_name, "Over Time"))
  })
  
  # Summary metric cards
  output$re_card_price <- renderUI({
    req(re_latest())
    latest <- re_latest()
    
    if (nrow(latest) == 1) {
      val <- format_currency(latest$median_sale_price[1])
      change <- latest$median_sale_price_yoy[1]
    } else {
      val <- format_currency(mean(latest$median_sale_price, na.rm = TRUE))
      change <- mean(latest$median_sale_price_yoy, na.rm = TRUE)
    }
    
    change_color <- if (!is.na(change) && change >= 0) app_colors$success else app_colors$accent
    change_icon <- if (!is.na(change) && change >= 0) "▲" else "▼"
    
    tags$div(
      style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); border-left: 4px solid #3498DB;",
      tags$div(style = "font-size: 11px; color: #7F8C8D; text-transform: uppercase;", "Median Price"),
      tags$div(style = "font-size: 22px; font-weight: 700; color: #2C3E50;", val),
      if (!is.na(change)) {
        tags$div(
          style = paste0("font-size: 12px; color: ", change_color, ";"),
          paste(change_icon, abs(round(change, 1)), "% YoY")
        )
      }
    )
  })
  
  output$re_card_dom <- renderUI({
    req(re_latest())
    latest <- re_latest()
    
    if (nrow(latest) == 1) {
      val <- round(latest$days_on_market[1], 0)
      change <- latest$days_on_market_yoy[1]
    } else {
      val <- round(mean(latest$days_on_market, na.rm = TRUE), 0)
      change <- mean(latest$days_on_market_yoy, na.rm = TRUE)
    }
    
    # For DOM, negative is good
    change_color <- if (!is.na(change) && change <= 0) app_colors$success else app_colors$accent
    change_icon <- if (!is.na(change) && change <= 0) "▼" else "▲"
    
    tags$div(
      style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); border-left: 4px solid #9B59B6;",
      tags$div(style = "font-size: 11px; color: #7F8C8D; text-transform: uppercase;", "Days on Market"),
      tags$div(style = "font-size: 22px; font-weight: 700; color: #2C3E50;", val),
      if (!is.na(change)) {
        tags$div(
          style = paste0("font-size: 12px; color: ", change_color, ";"),
          paste(change_icon, abs(round(change, 0)), "days YoY")
        )
      }
    )
  })
  
  output$re_card_stl <- renderUI({
    req(re_latest())
    latest <- re_latest()
    
    if (nrow(latest) == 1) {
      val <- round(latest$average_sale_to_list[1], 1)
      change <- latest$average_sale_to_list_yoy[1]
    } else {
      val <- round(mean(latest$average_sale_to_list, na.rm = TRUE), 1)
      change <- mean(latest$average_sale_to_list_yoy, na.rm = TRUE)
    }
    
    change_color <- if (!is.na(change) && change >= 0) app_colors$success else app_colors$accent
    change_icon <- if (!is.na(change) && change >= 0) "▲" else "▼"
    
    tags$div(
      style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); border-left: 4px solid #1ABC9C;",
      tags$div(style = "font-size: 11px; color: #7F8C8D; text-transform: uppercase;", "Sale-to-List"),
      tags$div(style = "font-size: 22px; font-weight: 700; color: #2C3E50;", paste0(val, "%")),
      if (!is.na(change)) {
        tags$div(
          style = paste0("font-size: 12px; color: ", change_color, ";"),
          paste(change_icon, abs(round(change, 2)), "pp YoY")
        )
      }
    )
  })
  
  output$re_card_volume <- renderUI({
    req(re_latest())
    latest <- re_latest()
    
    if (nrow(latest) == 1) {
      val <- scales::comma(latest$homes_sold[1])
      change <- latest$homes_sold_yoy[1]
    } else {
      val <- scales::comma(sum(latest$homes_sold, na.rm = TRUE))
      change <- mean(latest$homes_sold_yoy, na.rm = TRUE)
    }
    
    change_color <- if (!is.na(change) && change >= 0) app_colors$success else app_colors$accent
    change_icon <- if (!is.na(change) && change >= 0) "▲" else "▼"
    
    tags$div(
      style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); border-left: 4px solid #27AE60;",
      tags$div(style = "font-size: 11px; color: #7F8C8D; text-transform: uppercase;", "Homes Sold"),
      tags$div(style = "font-size: 22px; font-weight: 700; color: #2C3E50;", val),
      if (!is.na(change)) {
        tags$div(
          style = paste0("font-size: 12px; color: ", change_color, ";"),
          paste(change_icon, abs(round(change, 1)), "% YoY")
        )
      }
    )
  })
  
  # Main metric plot (interactive)
  output$re_main_plot <- renderPlotly({
    req(nrow(re_data()) > 0)
    
    metric <- input$re_metric
    metric_label <- metric_labels[[metric]]
    
    plot_data <- re_data() %>%
      mutate(
        metric_value = .data[[metric]],
        tooltip_text = paste0(
          "<b>", region, "</b><br>",
          "<b>Month:</b> ", format(month_end, "%B %Y"), "<br>",
          "<b>", metric_label, ":</b> ",
          if (metric == "median_sale_price") {
            scales::dollar(metric_value)
          } else if (grepl("_mom|_yoy", metric) && !grepl("days_on_market", metric)) {
            paste0(round(metric_value, 1), "%")
          } else if (metric == "average_sale_to_list") {
            paste0(round(metric_value, 1), "%")
          } else if (grepl("days_on_market", metric)) {
            round(metric_value, 0)
          } else {
            scales::comma(metric_value)
          }
        )
      )
    
    # Calculate date range for axis breaks
    date_range_days <- as.numeric(
      difftime(input$re_date_range[2], input$re_date_range[1], units = "days")
    )
    
    if (date_range_days <= 365) {
      date_breaks <- "1 month"
      date_format <- "%b '%y"
    } else if (date_range_days <= 730) {
      date_breaks <- "2 months"
      date_format <- "%b '%y"
    } else if (date_range_days <= 1825) {
      date_breaks <- "6 months"
      date_format <- "%b '%y"
    } else {
      date_breaks <- "1 year"
      date_format <- "%b '%y"
    }
    
    # Determine y-axis formatting
    y_labels <- if (metric == "median_sale_price") {
      scales::label_dollar(scale = 1e-3, suffix = "k")
    } else if (grepl("_mom|_yoy", metric) && !grepl("days_on_market", metric)) {
      function(x) paste0(x, "%")
    } else if (metric == "average_sale_to_list") {
      function(x) paste0(x, "%")
    } else if (metric %in% c("inventory", "homes_sold", "new_listings")) {
      scales::label_comma(scale = 1e-3, suffix = "k")
    } else {
      scales::label_comma()
    }
    
    # Determine if y should start at 0
    y_limits <- if (grepl("_mom|_yoy", metric)) {
      c(NA, NA)
    } else if (metric == "average_sale_to_list") {
      c(NA, NA)
    } else {
      c(0, NA)
    }
    
    p <- ggplot(
      plot_data,
      aes(
        x     = month_end,
        y     = metric_value,
        color = region,
        text  = tooltip_text,
        group = region
      )
    ) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = region_colors) +
      scale_y_continuous(
        labels = y_labels,
        limits = y_limits,
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks
      ) +
      labs(x = NULL, y = NULL) +
      theme_app() +
      theme(
        legend.position = "none",
        axis.text.x     = element_text(angle = 45, hjust = 1)
      )
    
    # Add reference line for percentage metrics
    if (grepl("_mom|_yoy", metric)) {
      p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "#95A5A6", linewidth = 0.5)
    } else if (metric == "average_sale_to_list") {
      p <- p + geom_hline(yintercept = 100, linetype = "dashed", color = "#95A5A6", linewidth = 0.5)
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          font        = list(family = "Arial", size = 12, color = "#2C3E50"),
          bordercolor = "transparent"
        ),
        showlegend = FALSE,
        margin     = list(b = 80, t = 20)
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
          "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
  })
  
  # Secondary plots
  output$re_price_plot <- renderPlot({
    req(nrow(re_data()) > 0)
    
    ggplot(re_data(), aes(x = month_end, y = median_sale_price, color = region)) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = region_colors) +
      scale_y_continuous(
        labels = scales::label_dollar(scale = 1e-3, suffix = "k"),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_x_date(date_labels = "%Y") +
      labs(x = NULL, y = NULL) +
      theme_app_compact()
  }, res = 96)
  
  output$re_dom_plot <- renderPlot({
    req(nrow(re_data()) > 0)
    
    ggplot(re_data(), aes(x = month_end, y = days_on_market, color = region)) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = region_colors) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      scale_x_date(date_labels = "%Y") +
      labs(x = NULL, y = NULL) +
      theme_app_compact()
  }, res = 96)
  
  output$re_inventory_plot <- renderPlot({
    req(nrow(re_data()) > 0)
    
    # Exclude National if other regions are selected
    plot_data <- re_data()
    selected_regions <- unique(plot_data$region)
    
    if (length(selected_regions) > 1 && "National" %in% selected_regions) {
      plot_data <- plot_data %>%
        filter(region != "National")
    }
    
    ggplot(plot_data, aes(x = month_end, y = inventory, color = region)) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = region_colors) +
      scale_y_continuous(
        labels = scales::label_comma(scale = 1e-3, suffix = "k"),
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      scale_x_date(date_labels = "%Y") +
      labs(x = NULL, y = NULL) +
      theme_app_compact()
  }, res = 96)
  
  # Region summary
  output$re_summary <- renderUI({
    req(re_data(), re_latest())
    
    data <- re_data()
    latest <- re_latest()
    regions <- input$re_regions
    
    date_start <- format(min(data$month_end), "%B %Y")
    date_end <- format(max(data$month_end), "%B %Y")
    
    if (length(regions) == 1) {
      # Single region summary
      r <- latest[1, ]
      region_name <- as.character(r$region)
      
      first_data <- data %>%
        filter(region == region_name, month_end == min(month_end))
      
      price_change <- ((r$median_sale_price - first_data$median_sale_price) / first_data$median_sale_price) * 100
      price_direction <- if (price_change > 0) "increased" else "decreased"
      
      market_heat <- if (r$average_sale_to_list >= 100 && r$days_on_market < 60) {
        "a seller's market with strong demand"
      } else if (r$average_sale_to_list < 98 || r$days_on_market > 90) {
        "a buyer-friendly market with more negotiating room"
      } else {
        "a balanced market"
      }
      
      HTML(paste0(
        "<p>In <strong>", region_name, "</strong>, the median sale price has ", price_direction,
        " by <strong>", abs(round(price_change, 1)), "%</strong> over the selected period (",
        date_start, " to ", date_end, "), from ",
        format_currency(first_data$median_sale_price), " to ",
        format_currency(r$median_sale_price), ".</p>",
        "<p>As of ", format(r$month_end, "%B %Y"), ", homes are averaging <strong>",
        round(r$days_on_market, 0), " days on market</strong> and selling at <strong>",
        round(r$average_sale_to_list, 1), "%</strong> of list price, indicating ", market_heat, ".</p>"
      ))
      
    } else {
      # Multi-region comparison
      highest_price <- latest %>%
        filter(median_sale_price == max(median_sale_price, na.rm = TRUE)) %>%
        slice(1)
      
      lowest_dom <- latest %>%
        filter(days_on_market == min(days_on_market, na.rm = TRUE)) %>%
        slice(1)
      
      hottest_stl <- latest %>%
        filter(average_sale_to_list == max(average_sale_to_list, na.rm = TRUE)) %>%
        slice(1)
      
      HTML(paste0(
        "<p>Comparing <strong>", paste(regions, collapse = ", "), "</strong> from ",
        date_start, " to ", date_end, ":</p>",
        "<ul style='margin: 10px 0; padding-left: 20px;'>",
        "<li><strong>", highest_price$region, "</strong> has the highest median sale price at ",
        format_currency(highest_price$median_sale_price), "</li>",
        "<li><strong>", lowest_dom$region, "</strong> has the fastest-moving market at ",
        round(lowest_dom$days_on_market, 0), " days on market</li>",
        "<li><strong>", hottest_stl$region, "</strong> shows the strongest competition with ",
        round(hottest_stl$average_sale_to_list, 1), "% sale-to-list ratio</li>",
        "</ul>"
      ))
    }
  })
  
  # ===========================================================================
  # MARKET HEAT TAB
  # ===========================================================================
  
  # Quick date range buttons
  observeEvent(input$mh_last_3years, {
    updateSliderInput(session, "mh_date_range", value = c(latest_month - years(3), latest_month))
  })
  
  observeEvent(input$mh_last_5years, {
    updateSliderInput(session, "mh_date_range", value = c(latest_month - years(5), latest_month))
  })
  
  observeEvent(input$mh_all_time, {
    updateSliderInput(session, "mh_date_range", value = c(earliest_month, latest_month))
  })
  
  # Compute market heat data with regime classification
  mh_data <- reactive({
    req(input$mh_regions)
    req(length(input$mh_regions) > 0)
    
    # Filter data
    data <- redfin %>%
      filter(
        region %in% input$mh_regions,
        month_end >= input$mh_date_range[1],
        month_end <= input$mh_date_range[2]
      )
    
    # Calculate percentile thresholds for each region
    data <- data %>%
      group_by(region) %>%
      mutate(
        dom_percentile = percent_rank(days_on_market),
        stl_percentile = percent_rank(average_sale_to_list),
        inv_percentile = percent_rank(inventory),
        price_yoy_percentile = percent_rank(median_sale_price_yoy)
      ) %>%
      ungroup()
    
    # Calculate heat index (higher = hotter/seller's market)
    # Components: high price YoY, low DOM, high sale-to-list, low inventory
    data <- data %>%
      mutate(
        heat_index = (
          (1 - dom_percentile) * 0.3 +
            stl_percentile * 0.3 +
            (1 - inv_percentile) * 0.2 +
            price_yoy_percentile * 0.2
        ) * 100
      )
    
    # Classify into regimes
    data <- data %>%
      mutate(
        regime = case_when(
          # Seller's market: high sale-to-list AND fast sales
          average_sale_to_list >= 100 & dom_percentile <= 0.33 ~ "Seller",
          # Buyer's market: low sale-to-list OR slow sales
          average_sale_to_list < 99 | dom_percentile >= 0.67 ~ "Buyer",
          # Everything else is neutral
          TRUE ~ "Neutral"
        ),
        regime = factor(regime, levels = c("Seller", "Neutral", "Buyer"))
      )
    
    data
  })
  
  # Main heatmap
  output$mh_heatmap <- renderPlotly({
    req(nrow(mh_data()) > 0)
    
    data <- mh_data()
    view_type <- input$mh_view_type
    
    if (view_type == "regime") {
      # Categorical heatmap
      plot_data <- data %>%
        mutate(
          regime_num = case_when(
            regime == "Seller" ~ 3,
            regime == "Neutral" ~ 2,
            regime == "Buyer" ~ 1
          ),
          tooltip_text = paste0(
            "<b>", region, "</b><br>",
            "<b>Month:</b> ", format(month_end, "%B %Y"), "<br>",
            "<b>Regime:</b> ", regime, "'s Market<br>",
            "<b>Sale-to-List:</b> ", round(average_sale_to_list, 1), "%<br>",
            "<b>Days on Market:</b> ", round(days_on_market, 0)
          )
        )
      
      p <- ggplot(
        plot_data,
        aes(
          x    = month_end,
          y    = region,
          fill = regime,
          text = tooltip_text
        )
      ) +
        geom_tile(color = "white", linewidth = 0.3) +
        scale_fill_manual(
          values = c("Seller" = "#E74C3C", "Neutral" = "#F39C12", "Buyer" = "#3498DB"),
          name   = "Market Regime"
        ) +
        scale_x_date(date_labels = "%b '%y", date_breaks = "6 months") +
        labs(x = NULL, y = NULL) +
        theme_app() +
        theme(
          legend.position = "none",
          axis.text.x     = element_text(angle = 45, hjust = 1),
          panel.grid      = element_blank()
        )
      
    } else {
      # Continuous heat index
      plot_data <- data %>%
        mutate(
          tooltip_text = paste0(
            "<b>", region, "</b><br>",
            "<b>Month:</b> ", format(month_end, "%B %Y"), "<br>",
            "<b>Heat Index:</b> ", round(heat_index, 1), "<br>",
            "<b>Sale-to-List:</b> ", round(average_sale_to_list, 1), "%<br>",
            "<b>Days on Market:</b> ", round(days_on_market, 0)
          )
        )
      
      p <- ggplot(
        plot_data,
        aes(
          x    = month_end,
          y    = region,
          fill = heat_index,
          text = tooltip_text
        )
      ) +
        geom_tile(color = "white", linewidth = 0.3) +
        scale_fill_gradient2(
          low      = "#3498DB",
          mid      = "#F39C12",
          high     = "#E74C3C",
          midpoint = 50,
          name     = "Heat Index",
          limits   = c(0, 100)
        ) +
        scale_x_date(date_labels = "%b '%y", date_breaks = "6 months") +
        labs(x = NULL, y = NULL) +
        theme_app() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid  = element_blank()
        )
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          font        = list(family = "Arial", size = 12, color = "#2C3E50"),
          bordercolor = "transparent"
        ),
        margin = list(b = 80, l = 150)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Regime distribution bar chart
  output$mh_regime_bars <- renderPlot({
    req(nrow(mh_data()) > 0)
    
    regime_summary <- mh_data() %>%
      group_by(region, regime) %>%
      summarize(months = n(), .groups = "drop") %>%
      group_by(region) %>%
      mutate(pct = months / sum(months) * 100) %>%
      ungroup()
    
    ggplot(
      regime_summary,
      aes(x = reorder(region, pct * (regime == "Seller")), y = pct, fill = regime)
    ) +
      geom_col(position = "stack", width = 0.7) +
      coord_flip() +
      scale_fill_manual(
        values = c("Seller" = "#E74C3C", "Neutral" = "#F39C12", "Buyer" = "#3498DB"),
        name   = "Regime"
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
      labs(x = NULL, y = "Percentage of Months") +
      theme_app() +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank()
      )
  }, res = 96)
  
  # Heat index trend over time
  output$mh_heat_trend <- renderPlot({
    req(nrow(mh_data()) > 0)
    
    ggplot(mh_data(), aes(x = month_end, y = heat_index, color = region)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "#95A5A6", linewidth = 0.5) +
      scale_color_manual(values = region_colors) +
      scale_y_continuous(limits = c(0, 100), expand = c(0.02, 0)) +
      scale_x_date(date_labels = "%Y") +
      labs(x = NULL, y = "Heat Index") +
      theme_app() +
      theme(legend.position = "none")
  }, res = 96)
  
  # Market heat summary
  output$mh_summary <- renderUI({
    req(nrow(mh_data()) > 0)
    
    data <- mh_data()
    
    # Calculate regime percentages by region
    regime_pct <- data %>%
      group_by(region, regime) %>%
      summarize(months = n(), .groups = "drop") %>%
      group_by(region) %>%
      mutate(pct = months / sum(months) * 100) %>%
      ungroup()
    
    # Find hottest region (most seller's market months)
    seller_pct <- regime_pct %>%
      filter(regime == "Seller") %>%
      arrange(desc(pct))
    
    hottest_region <- if (nrow(seller_pct) > 0) {
      seller_pct$region[1]
    } else {
      NA
    }
    
    hottest_pct <- if (nrow(seller_pct) > 0) {
      round(seller_pct$pct[1], 1)
    } else {
      0
    }
    
    # Find coolest region (most buyer's market months)
    buyer_pct <- regime_pct %>%
      filter(regime == "Buyer") %>%
      arrange(desc(pct))
    
    coolest_region <- if (nrow(buyer_pct) > 0) {
      buyer_pct$region[1]
    } else {
      NA
    }
    
    coolest_pct <- if (nrow(buyer_pct) > 0) {
      round(buyer_pct$pct[1], 1)
    } else {
      0
    }
    
    # Current conditions (most recent month)
    current <- data %>%
      group_by(region) %>%
      filter(month_end == max(month_end)) %>%
      ungroup()
    
    current_hot <- current %>%
      filter(regime == "Seller") %>%
      pull(region)
    
    current_cool <- current %>%
      filter(regime == "Buyer") %>%
      pull(region)
    
    date_range <- paste(
      format(min(data$month_end), "%B %Y"),
      "to",
      format(max(data$month_end), "%B %Y")
    )
    
    HTML(paste0(
      "<p>Over the period from <strong>", date_range, "</strong>:</p>",
      "<ul style='margin: 10px 0; padding-left: 20px;'>",
      if (!is.na(hottest_region)) {
        paste0(
          "<li><strong>", hottest_region, "</strong> has been the hottest market, ",
          "spending <strong>", hottest_pct, "%</strong> of months in a seller's market regime.</li>"
        )
      } else "",
      if (!is.na(coolest_region)) {
        paste0(
          "<li><strong>", coolest_region, "</strong> has been the most buyer-friendly, ",
          "with <strong>", coolest_pct, "%</strong> of months classified as a buyer's market.</li>"
        )
      } else "",
      "</ul>",
      "<p><strong>Current Conditions:</strong> ",
      if (length(current_hot) > 0) {
        paste0(paste(current_hot, collapse = ", "), " currently in seller's market. ")
      } else {
        "No regions currently in seller's market. "
      },
      if (length(current_cool) > 0) {
        paste0(paste(current_cool, collapse = ", "), " currently in buyer's market.")
      } else {
        "No regions currently in buyer's market."
      },
      "</p>"
    ))
  })
  
  # ===========================================================================
  # CROSS-SECTION SNAPSHOT TAB
  # ===========================================================================
  
  # Quick month buttons
  observeEvent(input$ss_latest, {
    updateSliderInput(session, "ss_month", value = latest_month)
  })
  
  observeEvent(input$ss_year_ago, {
    updateSliderInput(session, "ss_month", value = as.Date(latest_month) - years(1))
  })
  
  observeEvent(input$ss_3years_ago, {
    updateSliderInput(session, "ss_month", value = as.Date(latest_month) - years(3))
  })
  
  # Filtered snapshot data
  ss_data <- reactive({
    req(input$ss_regions)
    req(length(input$ss_regions) > 0)
    req(input$ss_month)
    
    # Find the closest available month to selected
    selected_month <- as.Date(input$ss_month)
    
    redfin %>%
      filter(region %in% input$ss_regions) %>%
      group_by(region) %>%
      mutate(month_diff = abs(as.numeric(difftime(month_end, selected_month, units = "days")))) %>%
      filter(month_diff == min(month_diff)) %>%
      ungroup() %>%
      select(-month_diff)
  })
  
  # Period info panel
  output$ss_period_info <- renderUI({
    req(ss_data())
    
    data <- ss_data()
    actual_month <- unique(data$month_end)[1]
    
    tags$div(
      tags$div(
        style = "text-align: center; padding: 10px;",
        tags$div(
          style = "font-size: 24px; font-weight: 700; color: #2C3E50;",
          format(actual_month, "%B %Y")
        ),
        tags$div(
          style = "font-size: 12px; color: #7F8C8D; margin-top: 5px;",
          paste(nrow(data), "regions")
        )
      )
    )
  })
  
  # Price bar chart
  output$ss_price_bars <- renderPlotly({
    req(nrow(ss_data()) > 0)
    
    plot_data <- ss_data() %>%
      mutate(
        tooltip_text = paste0(
          "<b>", region, "</b><br>",
          "<b>Median Price:</b> ", scales::dollar(median_sale_price), "<br>",
          "<b>YoY Change:</b> ", round(median_sale_price_yoy, 1), "%"
        )
      ) %>%
      arrange(desc(median_sale_price))
    
    p <- ggplot(
      plot_data,
      aes(
        x    = reorder(region, median_sale_price),
        y    = median_sale_price,
        fill = region,
        text = tooltip_text
      )
    ) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = region_colors) +
      scale_y_continuous(
        labels = scales::label_dollar(scale = 1e-3, suffix = "k"),
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(x = NULL, y = NULL) +
      theme_app() +
      theme(
        legend.position    = "none",
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12, color = "#2C3E50")),
        margin     = list(l = 120)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Days on Market bar chart
  output$ss_dom_bars <- renderPlotly({
    req(nrow(ss_data()) > 0)
    
    plot_data <- ss_data() %>%
      mutate(
        tooltip_text = paste0(
          "<b>", region, "</b><br>",
          "<b>Days on Market:</b> ", round(days_on_market, 0), "<br>",
          "<b>YoY Change:</b> ", round(days_on_market_yoy, 0), " days"
        )
      ) %>%
      arrange(desc(days_on_market))
    
    p <- ggplot(
      plot_data,
      aes(
        x    = reorder(region, days_on_market),
        y    = days_on_market,
        fill = region,
        text = tooltip_text
      )
    ) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = region_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = NULL) +
      theme_app() +
      theme(
        legend.position    = "none",
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12, color = "#2C3E50")),
        margin     = list(l = 120)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Inventory bar chart (exclude National when others selected)
  output$ss_inventory_bars <- renderPlotly({
    req(nrow(ss_data()) > 0)
    
    plot_data <- ss_data()
    
    # Exclude National if other regions are present
    if (nrow(plot_data) > 1 && "National" %in% plot_data$region) {
      plot_data <- plot_data %>% filter(region != "National")
    }
    
    plot_data <- plot_data %>%
      mutate(
        tooltip_text = paste0(
          "<b>", region, "</b><br>",
          "<b>Inventory:</b> ", scales::comma(inventory), "<br>",
          "<b>YoY Change:</b> ", round(inventory_yoy, 1), "%"
        )
      ) %>%
      arrange(desc(inventory))
    
    p <- ggplot(
      plot_data,
      aes(
        x    = reorder(region, inventory),
        y    = inventory,
        fill = region,
        text = tooltip_text
      )
    ) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = region_colors) +
      scale_y_continuous(
        labels = scales::label_comma(scale = 1e-3, suffix = "k"),
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(x = NULL, y = NULL) +
      theme_app() +
      theme(
        legend.position    = "none",
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12, color = "#2C3E50")),
        margin     = list(l = 120)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Sale-to-List bar chart
  output$ss_stl_bars <- renderPlotly({
    req(nrow(ss_data()) > 0)
    
    plot_data <- ss_data() %>%
      mutate(
        tooltip_text = paste0(
          "<b>", region, "</b><br>",
          "<b>Sale-to-List:</b> ", round(average_sale_to_list, 1), "%<br>",
          "<b>YoY Change:</b> ", round(average_sale_to_list_yoy, 2), " pp"
        )
      ) %>%
      arrange(desc(average_sale_to_list))
    
    p <- ggplot(
      plot_data,
      aes(
        x    = reorder(region, average_sale_to_list),
        y    = average_sale_to_list,
        fill = region,
        text = tooltip_text
      )
    ) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "#95A5A6", linewidth = 0.5) +
      coord_flip() +
      scale_fill_manual(values = region_colors) +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(x = NULL, y = NULL) +
      theme_app() +
      theme(
        legend.position    = "none",
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12, color = "#2C3E50")),
        margin     = list(l = 120)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Data table
  output$ss_table <- renderTable({
    req(nrow(ss_data()) > 0)
    
    ss_data() %>%
      select(
        Region              = region,
        `Median Price`      = median_sale_price,
        `Price YoY`         = median_sale_price_yoy,
        `Homes Sold`        = homes_sold,
        `New Listings`      = new_listings,
        Inventory           = inventory,
        `Days on Market`    = days_on_market,
        `Sale-to-List`      = average_sale_to_list
      ) %>%
      mutate(
        `Median Price` = scales::dollar(`Median Price`),
        `Price YoY`    = paste0(round(`Price YoY`, 1), "%"),
        `Homes Sold`   = scales::comma(`Homes Sold`),
        `New Listings` = scales::comma(`New Listings`),
        Inventory      = scales::comma(Inventory),
        `Days on Market` = round(`Days on Market`, 0),
        `Sale-to-List` = paste0(round(`Sale-to-List`, 1), "%")
      ) %>%
      arrange(Region)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # Snapshot summary
  output$ss_summary <- renderUI({
    req(nrow(ss_data()) > 0)
    
    data <- ss_data()
    month_str <- format(unique(data$month_end)[1], "%B %Y")
    
    # Find extremes
    highest_price <- data %>% filter(median_sale_price == max(median_sale_price, na.rm = TRUE)) %>% slice(1)
    lowest_price  <- data %>% filter(median_sale_price == min(median_sale_price, na.rm = TRUE)) %>% slice(1)
    fastest_dom   <- data %>% filter(days_on_market == min(days_on_market, na.rm = TRUE)) %>% slice(1)
    slowest_dom   <- data %>% filter(days_on_market == max(days_on_market, na.rm = TRUE)) %>% slice(1)
    hottest_stl   <- data %>% filter(average_sale_to_list == max(average_sale_to_list, na.rm = TRUE)) %>% slice(1)
    
    HTML(paste0(
      "<p>As of <strong>", month_str, "</strong>:</p>",
      "<ul style='margin: 10px 0; padding-left: 20px;'>",
      
      "<li><strong>Highest Price:</strong> ", highest_price$region, " at ",
      scales::dollar(highest_price$median_sale_price),
      " (", ifelse(highest_price$median_sale_price_yoy >= 0, "+", ""),
      round(highest_price$median_sale_price_yoy, 1), "% YoY)</li>",
      
      "<li><strong>Most Affordable:</strong> ", lowest_price$region, " at ",
      scales::dollar(lowest_price$median_sale_price), "</li>",
      
      "<li><strong>Fastest Market:</strong> ", fastest_dom$region, " with homes averaging ",
      round(fastest_dom$days_on_market, 0), " days on market</li>",
      
      "<li><strong>Slowest Market:</strong> ", slowest_dom$region, " at ",
      round(slowest_dom$days_on_market, 0), " days on market</li>",
      
      "<li><strong>Hottest Competition:</strong> ", hottest_stl$region, " with ",
      round(hottest_stl$average_sale_to_list, 1), "% sale-to-list ratio</li>",
      
      "</ul>"
    ))
  })
  
  # ===========================================================================
  # PRICING GUIDANCE TAB
  # ===========================================================================
  
  # Quick month buttons
  observeEvent(input$pg_latest, {
    updateSliderInput(session, "pg_month", value = latest_month)
  })
  
  observeEvent(input$pg_year_ago, {
    updateSliderInput(session, "pg_month", value = as.Date(latest_month) - years(1))
  })
  
  # Get data for selected region
  pg_region_data <- reactive({
    req(input$pg_region)
    
    redfin %>%
      filter(region == input$pg_region) %>%
      arrange(month_end)
  })
  
  # Get data for selected month
  pg_month_data <- reactive({
    req(input$pg_region)
    req(input$pg_month)
    
    selected_month <- as.Date(input$pg_month)
    
    pg_region_data() %>%
      mutate(month_diff = abs(as.numeric(difftime(month_end, selected_month, units = "days")))) %>%
      filter(month_diff == min(month_diff)) %>%
      slice(1) %>%
      select(-month_diff)
  })
  
  # Calculate market heat classification and price band
  pg_market_class <- reactive({
    req(nrow(pg_month_data()) > 0)
    
    current <- pg_month_data()
    region_hist <- pg_region_data()
    
    # Calculate percentiles for this region
    dom_percentile <- mean(region_hist$days_on_market <= current$days_on_market, na.rm = TRUE)
    stl_percentile <- mean(region_hist$average_sale_to_list <= current$average_sale_to_list, na.rm = TRUE)
    
    # Classify market
    if (current$average_sale_to_list >= 100 && dom_percentile <= 0.33) {
      market_class <- "Hot"
      lower_mult <- 1.00
      upper_mult <- 1.05
      color <- "#E74C3C"
      icon_name <- "fire"
      description <- "Seller's market with strong demand"
    } else if (current$average_sale_to_list < 99 || dom_percentile >= 0.67) {
      market_class <- "Cool"
      lower_mult <- 0.95
      upper_mult <- 1.00
      color <- "#3498DB"
      icon_name <- "snowflake"
      description <- "Buyer's market with more negotiating room"
    } else {
      market_class <- "Neutral"
      lower_mult <- 0.98
      upper_mult <- 1.02
      color <- "#F39C12"
      icon_name <- "balance-scale"
      description <- "Balanced market conditions"
    }
    
    median_price <- current$median_sale_price
    lower_bound <- median_price * lower_mult
    upper_bound <- median_price * upper_mult
    
    list(
      market_class = market_class,
      color        = color,
      icon         = icon_name,
      description  = description,
      median_price = median_price,
      lower_bound  = lower_bound,
      upper_bound  = upper_bound,
      lower_mult   = lower_mult,
      upper_mult   = upper_mult,
      dom_percentile = dom_percentile,
      stl_percentile = stl_percentile
    )
  })
  
  # Main price card
  output$pg_price_card <- renderUI({
    req(pg_month_data(), pg_market_class())
    
    current <- pg_month_data()
    mc <- pg_market_class()
    month_str <- format(current$month_end, "%B %Y")
    
    tags$div(
      style = paste0(
        "background: linear-gradient(135deg, ", mc$color, " 0%, ", 
        adjustcolor(mc$color, alpha.f = 0.8), " 100%);",
        "color: white; border-radius: 12px; padding: 30px; margin-bottom: 20px;",
        "box-shadow: 0 8px 25px rgba(0,0,0,0.15);"
      ),
      
      fluidRow(
        column(
          width = 6,
          tags$div(
            style = "border-right: 1px solid rgba(255,255,255,0.3); padding-right: 30px;",
            
            tags$div(
              style = "font-size: 14px; opacity: 0.9; margin-bottom: 5px;",
              input$pg_region, " · ", month_str
            ),
            
            tags$div(
              style = "font-size: 16px; margin-bottom: 15px;",
              icon(mc$icon, style = "margin-right: 8px;"),
              tags$strong(mc$market_class, " Market"),
              " — ", mc$description
            ),
            
            tags$div(
              style = "font-size: 14px; opacity: 0.9;",
              "Median Sale Price"
            ),
            tags$div(
              style = "font-size: 36px; font-weight: 700;",
              scales::dollar(mc$median_price)
            )
          )
        ),
        
        column(
          width = 6,
          tags$div(
            style = "padding-left: 30px;",
            
            tags$div(
              style = "font-size: 14px; opacity: 0.9; margin-bottom: 10px;",
              "Suggested Listing Context Range"
            ),
            
            tags$div(
              style = "font-size: 32px; font-weight: 700; margin-bottom: 10px;",
              scales::dollar(mc$lower_bound), " — ", scales::dollar(mc$upper_bound)
            ),
            
            tags$div(
              style = "font-size: 13px; opacity: 0.85;",
              paste0(
                "(", round(mc$lower_mult * 100), "% – ", round(mc$upper_mult * 100), "% of median)"
              )
            ),
            
            tags$div(
              style = "margin-top: 15px; padding: 10px; background: rgba(255,255,255,0.15); border-radius: 6px; font-size: 12px;",
              icon("exclamation-triangle", style = "margin-right: 5px;"),
              "This is a contextual range, not an appraisal. Individual property factors will vary."
            )
          )
        )
      )
    )
  })
  
  # Market indicators
  output$pg_indicator_price <- renderUI({
    req(pg_month_data())
    current <- pg_month_data()
    
    change <- current$median_sale_price_yoy
    change_color <- if (!is.na(change) && change >= 0) app_colors$success else app_colors$accent
    change_icon <- if (!is.na(change) && change >= 0) "▲" else "▼"
    
    tags$div(
      style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); text-align: center;",
      tags$div(style = "font-size: 11px; color: #7F8C8D; text-transform: uppercase;", "Price YoY"),
      tags$div(
        style = paste0("font-size: 22px; font-weight: 700; color: ", change_color, ";"),
        paste0(change_icon, " ", abs(round(change, 1)), "%")
      )
    )
  })
  
  output$pg_indicator_dom <- renderUI({
    req(pg_month_data())
    current <- pg_month_data()
    
    tags$div(
      style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); text-align: center;",
      tags$div(style = "font-size: 11px; color: #7F8C8D; text-transform: uppercase;", "Days on Market"),
      tags$div(
        style = "font-size: 22px; font-weight: 700; color: #2C3E50;",
        round(current$days_on_market, 0)
      )
    )
  })
  
  output$pg_indicator_stl <- renderUI({
    req(pg_month_data())
    current <- pg_month_data()
    
    stl <- current$average_sale_to_list
    stl_color <- if (stl >= 100) app_colors$accent else if (stl >= 99) app_colors$warning else app_colors$secondary
    
    tags$div(
      style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); text-align: center;",
      tags$div(style = "font-size: 11px; color: #7F8C8D; text-transform: uppercase;", "Sale-to-List"),
      tags$div(
        style = paste0("font-size: 22px; font-weight: 700; color: ", stl_color, ";"),
        paste0(round(stl, 1), "%")
      )
    )
  })
  
  output$pg_indicator_inventory <- renderUI({
    req(pg_month_data())
    current <- pg_month_data()
    
    change <- current$inventory_yoy
    change_color <- if (!is.na(change) && change <= 0) app_colors$accent else app_colors$success
    change_icon <- if (!is.na(change) && change <= 0) "▼" else "▲"
    
    tags$div(
      style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); text-align: center;",
      tags$div(style = "font-size: 11px; color: #7F8C8D; text-transform: uppercase;", "Inventory YoY"),
      tags$div(
        style = paste0("font-size: 22px; font-weight: 700; color: ", change_color, ";"),
        paste0(change_icon, " ", abs(round(change, 1)), "%")
      )
    )
  })
  
  # Context chart - price history with range band
  output$pg_context_chart <- renderPlotly({
    req(nrow(pg_region_data()) > 0, pg_month_data(), pg_market_class())
    
    region_data <- pg_region_data()
    current <- pg_month_data()
    mc <- pg_market_class()
    
    # Get last 24 months of data
    recent_data <- region_data %>%
      filter(month_end >= (current$month_end - months(24)))
    
    # Create tooltip
    recent_data <- recent_data %>%
      mutate(
        tooltip_text = paste0(
          "<b>", format(month_end, "%B %Y"), "</b><br>",
          "<b>Median Price:</b> ", scales::dollar(median_sale_price), "<br>",
          "<b>YoY Change:</b> ", round(median_sale_price_yoy, 1), "%"
        )
      )
    
    p <- ggplot(recent_data, aes(x = month_end, y = median_sale_price)) +
      # Suggested range band for current month
      annotate(
        "rect",
        xmin  = current$month_end - days(15),
        xmax  = current$month_end + days(15),
        ymin  = mc$lower_bound,
        ymax  = mc$upper_bound,
        fill  = mc$color,
        alpha = 0.3
      ) +
      # Price line
      geom_line(aes(text = tooltip_text), color = app_colors$primary, linewidth = 1.2) +
      # Current month point
      geom_point(
        data = current,
        aes(x = month_end, y = median_sale_price),
        color = mc$color,
        size = 4
      ) +
      # Median reference line
      geom_hline(
        yintercept = mc$median_price,
        linetype   = "dashed",
        color      = "#95A5A6",
        linewidth  = 0.5
      ) +
      scale_y_continuous(
        labels = scales::label_dollar(scale = 1e-3, suffix = "k"),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_x_date(date_labels = "%b '%y", date_breaks = "3 months") +
      labs(x = NULL, y = "Median Sale Price") +
      theme_app() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12, color = "#2C3E50")),
        margin     = list(b = 80),
        annotations = list(
          list(
            x         = as.numeric(current$month_end) * 86400000,
            y         = mc$upper_bound,
            text      = "Suggested Range",
            showarrow = FALSE,
            yshift    = 15,
            font      = list(size = 10, color = mc$color)
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Interpretation text
  output$pg_interpretation <- renderUI({
    req(pg_month_data(), pg_market_class())
    
    current <- pg_month_data()
    mc <- pg_market_class()
    month_str <- format(current$month_end, "%B %Y")
    region <- input$pg_region
    
    # Build interpretation
    dom_context <- if (mc$dom_percentile <= 0.33) {
      "faster than typical for this market"
    } else if (mc$dom_percentile >= 0.67) {
      "slower than typical for this market"
    } else {
      "around average for this market"
    }
    
    stl_context <- if (current$average_sale_to_list >= 100) {
      paste0("with homes selling at ", round(current$average_sale_to_list, 1), "% of list price on average, indicating buyers are competing and often paying above asking")
    } else if (current$average_sale_to_list >= 99) {
      paste0("with homes selling at ", round(current$average_sale_to_list, 1), "% of list price, close to asking")
    } else {
      paste0("with homes selling at ", round(current$average_sale_to_list, 1), "% of list price, suggesting room for negotiation")
    }
    
    price_trend <- if (!is.na(current$median_sale_price_yoy) && current$median_sale_price_yoy > 0) {
      paste0("Prices have risen ", round(current$median_sale_price_yoy, 1), "% year-over-year.")
    } else if (!is.na(current$median_sale_price_yoy) && current$median_sale_price_yoy < 0) {
      paste0("Prices have declined ", abs(round(current$median_sale_price_yoy, 1)), "% year-over-year.")
    } else {
      "Prices have remained relatively stable year-over-year."
    }
    
    HTML(paste0(
      "<p>In <strong>", month_str, "</strong>, the median sale price in <strong>", region,
      "</strong> was <strong>", scales::dollar(mc$median_price), "</strong>. ",
      "Homes are averaging <strong>", round(current$days_on_market, 0), " days on market</strong>, which is ",
      dom_context, ", ", stl_context, ".</p>",
      
      "<p>", price_trend, "</p>",
      
      "<p>Based on these conditions, the market appears <strong>", tolower(mc$market_class),
      "</strong>. A reasonable listing context would be approximately <strong>",
      scales::dollar(mc$lower_bound), " to ", scales::dollar(mc$upper_bound),
      "</strong> (", round(mc$lower_mult * 100), "% – ", round(mc$upper_mult * 100), "% of the median), ",
      "though individual property characteristics, location within the region, and condition will significantly affect appropriate pricing.</p>"
    ))
  })
  
}
