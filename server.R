
# server.R

server <- function(input, output, session) {
  
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
  
  # Filtered data for Overview tab
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
      arrange(month_end)
  })
  
  # Headline Median Sale Price chart
  output$ov_price_plot <- renderPlot({
    ggplot(ov_data(), aes(x = month_end, y = median_sale_price, color = region)) +
      geom_line(size = 1.1) +
      scale_y_continuous(
        limits = c(0, NA),  # always start at 0
        labels = scales::label_dollar(
          accuracy = 1,
          scale    = 1e-3,   # 1000 -> 1k
          suffix   = "k"
        )
      ) +
      labs(
        title = "Median Sale Price Over Time",
        x     = "Month of Period End",
        y     = "Median Sale Price (USD)",
        color = "Region"
      ) +
      theme_app()
  })
  
  # Inventory over time
  output$ov_inventory_plot <- renderPlot({
    ggplot(ov_data(), aes(x = month_end, y = inventory, color = region)) +
      geom_line() +
      labs(
        title = "Inventory Over Time",
        x     = "",
        y     = "Inventory"
      ) +
      theme_app() +
      theme(legend.position = "none")
  })
  
  # Days on Market over time
  output$ov_dom_plot <- renderPlot({
    ggplot(ov_data(), aes(x = month_end, y = days_on_market, color = region)) +
      geom_line() +
      labs(
        title = "Days on Market Over Time",
        x     = "",
        y     = "Days on Market"
      ) +
      theme_app() +
      theme(legend.position = "none")
  })
  
  # Average Sale-to-List over time
  output$ov_saletolist_plot <- renderPlot({
    ggplot(ov_data(), aes(x = month_end, y = average_sale_to_list, color = region)) +
      geom_line() +
      labs(
        title = "Average Sale-to-List Ratio Over Time",
        x     = "",
        y     = "Sale-to-List (%)"
      ) +
      theme_app() +
      theme(legend.position = "none")
  })
  
  # Summary placeholder
  output$ov_summary <- renderText({
    req(ov_data())
    "Summary placeholder. Interpretation will go here."
  })
}
