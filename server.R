
# server.R
source("global.R")

shinyServer(function(input, output, session) {
  # ====================================================================
  # OVERVIEW TAB
  # ====================================================================
  output$overview_image_placeholder <- renderUI({
    tags$div(
      style = paste(
        "height: 240px; border: 2px dashed #bbb;",
        "border-radius: 8px; display: flex; align-items: center;",
        "justify-content: center; background-color: #fcfcfc;"
      ),
      tags$span(style = "color:#888;", "Image placeholder — add visualization or header image here")
    )
  })
  
  output$overview_intro <- renderUI({
    tags$p(
      "Use this application to explore housing purchase patterns across major global cities. ",
      "The Descriptive Market Insights tab summarizes prices, size–price trends, amenities in high-value homes, ",
      "and the relationship between home age and value. An interactive map lets you drill from country to city."
    )
  })
  
  output$vb_min_year <- renderValueBox({
    valueBox(subtitle = "Earliest Build Year", value = dataset_stats$min_constructed_year,
             icon = icon("calendar"), color = "teal")
  })
  output$vb_countries <- renderValueBox({
    valueBox(subtitle = "Countries", value = dataset_stats$n_countries,
             icon = icon("globe"), color = "aqua")
  })
  output$vb_cities <- renderValueBox({
    valueBox(subtitle = "Cities", value = dataset_stats$n_cities,
             icon = icon("city"), color = "blue")
  })
  output$vb_rows <- renderValueBox({
    valueBox(subtitle = "Datapoints", value = format(dataset_stats$n_rows, big.mark = ","),
             icon = icon("database"), color = "purple")
  })
  
  # ====================================================================
  # CURRENCY NORMALIZATION
  # ====================================================================
  base_df <- reactive({
    house %>%
      dplyr::left_join(currency_map, by = "country") %>%
      dplyr::left_join(fx_rates,     by = "currency") %>%
      dplyr::mutate(rate_to_usd = dplyr::coalesce(rate_to_usd, 1))
  })
  
  output$fx_note <- renderUI({
    if (identical(input$currency_basis, "USD (FX, nominal)") && !fx_available) {
      tags$div(
        style = "padding:6px 10px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
        tags$small("USD conversion needs an FX table (fx_rates_static.csv). Showing local currency instead.")
      )
    } else NULL
  })
  
  df_price <- reactive({
    df <- base_df()
    use_usd <- identical(input$currency_basis, "USD (FX, nominal)") && fx_available
    df %>%
      dplyr::mutate(
        price_display = if (use_usd) price * rate_to_usd else price,
        price_is_usd  = use_usd
      )
  })
  
  # --------------------------------------------------------------------
  # Compact currency formatting helpers
  # --------------------------------------------------------------------
  compact_num <- function(x) {
    x[!is.finite(x)] <- NA_real_
    out <- rep(NA_character_, length(x))
    idx <- !is.na(x); if (!any(idx)) return(out)
    
    y <- x[idx]; abs_y <- abs(y)
    unit <- character(length(y)); val <- numeric(length(y))
    
    m_m <- abs_y >= 1e6; unit[m_m] <- "M"; val[m_m] <- y[m_m] / 1e6
    m_k <- abs_y >= 1e3 & !m_m; unit[m_k] <- "K"; val[m_k] <- y[m_k] / 1e3
    m_p <- !m_m & !m_k; unit[m_p] <- "";  val[m_p] <- y[m_p]
    
    lab <- paste0(scales::number(val, accuracy = 0.1), unit)
    out[idx] <- lab
    out
  }
  
  compact_num_prec <- function(x, digits = 3) {
    x[!is.finite(x)] <- NA_real_
    out <- rep(NA_character_, length(x))
    idx <- !is.na(x); if (!any(idx)) return(out)
    
    y <- x[idx]; abs_y <- abs(y)
    unit <- character(length(y)); val <- numeric(length(y))
    
    m_m <- abs_y >= 1e6; unit[m_m] <- "M"; val[m_m] <- y[m_m] / 1e6
    m_k <- abs_y >= 1e3 & !m_m; unit[m_k] <- "K"; val[m_k] <- y[m_k] / 1e3
    m_p <- !m_m & !m_k; unit[m_p] <- "";  val[m_p] <- y[m_p]
    
    acc <- 10^(-digits)
    lab <- paste0(scales::number(val, accuracy = acc), unit)
    out[idx] <- lab
    out
  }
  
  local_symbol <- reactive({
    sel <- input$flt_country
    if (is.null(sel) || sel == "All") {
      "$"
    } else {
      sym <- currency_map %>% dplyr::filter(country == sel) %>% dplyr::pull(currency_symbol)
      if (length(sym) == 1) sym else "$"
    }
  })
  
  fmt_compact_money <- function(x) {
    if (isTruthy(input$currency_basis) &&
        identical(input$currency_basis, "USD (FX, nominal)") &&
        fx_available) {
      paste0("$", compact_num(x))
    } else {
      paste0(local_symbol(), compact_num(x))
    }
  }
  
  fmt_compact_money_prec <- function(x, digits = 3) {
    if (isTruthy(input$currency_basis) &&
        identical(input$currency_basis, "USD (FX, nominal)") &&
        fx_available) {
      paste0("$", compact_num_prec(x, digits = digits))
    } else {
      paste0(local_symbol(), compact_num_prec(x, digits = digits))
    }
  }
  
  # ====================================================================
  # SHARED FILTERS (Country + Property type) for Insights tab
  # ====================================================================
  df_filtered <- reactive({
    df <- df_price()
    if (!is.null(input$flt_country) && input$flt_country != "All") {
      df <- df %>% dplyr::filter(country == input$flt_country)
    }
    if (!is.null(input$flt_property) && input$flt_property != "All") {
      df <- df %>% dplyr::filter(property_type == input$flt_property)
    }
    df
  })
  
  # Utility for Plotly y-axis labels with compact money (choose precision)
  make_yaxis_compact <- function(yvals, digits = 1) {
    rng   <- range(yvals, na.rm = TRUE)
    ticks <- pretty(rng, n = 6)
    list(
      tickmode = "array",
      tickvals = ticks,
      ticktext = fmt_compact_money_prec(ticks, digits = digits)
    )
  }
  
  # ====================================================================
  # SIZE vs PRICE (Plotly) — hover + click-to-lock bin
  # ====================================================================
  bin_clicked <- reactiveVal(NULL)
  
  output$plot_size_price <- renderPlotly({
    df <- df_filtered(); req(nrow(df) > 0)
    
    qs <- stats::quantile(df$property_size_sqft, probs = seq(0, 1, 0.05), na.rm = TRUE)
    qs <- unique(qs[is.finite(qs)])
    if (length(qs) < 2) {
      p <- plotly::plot_ly() %>% layout(
        annotations = list(
          x = 0.5, y = 0.5, text = "Not enough data to bin by size.",
          showarrow = FALSE, xref = "paper", yref = "paper"
        )
      )
      p <- plotly::event_register(p, 'plotly_click')
      return(p)
    }
    
    binned <- df %>%
      dplyr::mutate(size_bin = cut(property_size_sqft, breaks = qs, include.lowest = TRUE, dig.lab = 8)) %>%
      dplyr::group_by(size_bin) %>%
      dplyr::summarise(
        avg_price = mean(price_display, na.rm = TRUE),
        avg_sqft  = mean(property_size_sqft, na.rm = TRUE),
        n         = dplyr::n(), .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(avg_price), is.finite(avg_sqft)) %>%
      dplyr::arrange(avg_sqft) %>%
      dplyr::mutate(
        bin_id = as.character(size_bin),
        hover  = paste0(
          "Bin: ", bin_id,
          "<br>Avg sqft: ", round(avg_sqft, 0),
          "<br>Avg price: ", fmt_compact_money(avg_price),
          "<br>Obs: ", n
        )
      )
    
    p <- plotly::plot_ly(
      data = binned, source = "sp",
      x = ~avg_sqft, y = ~avg_price, text = ~hover, key = ~bin_id,
      type = "scatter", mode = "lines+markers", hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Average sqft (bin center)"),
        yaxis = c(list(title = ""), make_yaxis_compact(binned$avg_price, digits = 1)),
        hovermode = "closest"
      )
    
    p <- plotly::event_register(p, 'plotly_click')
    p
  })
  
  observeEvent(plotly::event_data("plotly_click", source = "sp"), {
    ed <- plotly::event_data("plotly_click", source = "sp")
    if (!is.null(ed) && !is.null(ed$key) && length(ed$key) >= 1) {
      bin_clicked(ed$key[[1]])
    }
  }, ignoreInit = TRUE)
  
  output$bin_summary <- renderUI({
    df <- df_filtered(); req(nrow(df) > 0)
    
    qs <- stats::quantile(df$property_size_sqft, probs = seq(0, 1, 0.05), na.rm = TRUE)
    qs <- unique(qs[is.finite(qs)])
    if (length(qs) < 2) return(tags$p("Not enough data to bin by size."))
    
    binned <- df %>%
      dplyr::mutate(size_bin = cut(property_size_sqft, breaks = qs, include.lowest = TRUE, dig.lab = 8)) %>%
      dplyr::group_by(size_bin) %>%
      dplyr::summarise(
        avg_price = mean(price_display, na.rm = TRUE),
        avg_sqft  = mean(property_size_sqft, na.rm = TRUE),
        n         = dplyr::n(), .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(avg_price), is.finite(avg_sqft)) %>%
      dplyr::arrange(avg_sqft) %>%
      dplyr::mutate(bin_id = as.character(size_bin))
    
    sel <- bin_clicked()
    if (is.null(sel) || !(sel %in% binned$bin_id)) {
      return(tags$p("Click a point on the chart to see its summary here."))
    }
    
    row <- binned %>% dplyr::filter(bin_id == sel) %>% dplyr::slice(1)
    tags$div(
      tags$p(tags$b("Selected bin: "), sel),
      tags$p(tags$b("Avg sqft: "), round(row$avg_sqft, 0)),
      tags$p(tags$b("Avg price: "), fmt_compact_money(row$avg_price)),
      tags$p(tags$b("Observations: "), row$n)
    )
  })
  
  # ====================================================================
  # AMENITIES PIE + FURNISHING PIE (Plotly) by filtered quartile
  # ====================================================================
  quartile_filter <- reactive({
    df <- df_filtered()
    qs <- stats::quantile(df$price_display, probs = c(.25, .5, .75), na.rm = TRUE)
    switch(input$amen_quartile,
           Q1 = df$price_display <= qs[1],
           Q2 = df$price_display > qs[1] & df$price_display <= qs[2],
           Q3 = df$price_display > qs[2] & df$price_display <= qs[3],
           Q4 = df$price_display > qs[3]
    )
  })
  
  # Updated to include mutually exclusive slices incl. "None"
  output$pie_amenities <- renderPlotly({
    df <- df_filtered(); req(nrow(df) > 0)
    
    idx <- quartile_filter(); if (is.null(idx)) return(plotly::plot_ly())
    
    dfx <- df %>%
      dplyr::filter(idx) %>%
      dplyr::mutate(
        garage_present = garage %in% 1,
        garden_present = garden %in% 1,
        AmenityGroup = dplyr::case_when(
          garage_present & garden_present ~ "Both",
          garage_present & !garden_present ~ "Garage only",
          !garage_present & garden_present ~ "Garden only",
          TRUE ~ "None"
        )
      ) %>%
      dplyr::count(AmenityGroup, name = "n") %>%
      dplyr::mutate(Share = n / sum(n)) %>%
      dplyr::arrange(factor(AmenityGroup, levels = c("None", "Garage only", "Garden only", "Both")))
    
    if (nrow(dfx) == 0) {
      return(plotly::plot_ly() %>% layout(
        annotations = list(x = 0.5, y = 0.5, text = "No data for selected filters/quartile",
                           showarrow = FALSE, xref = "paper", yref = "paper")
      ))
    }
    
    plotly::plot_ly(
      data = dfx,
      labels = ~AmenityGroup, values = ~Share,
      type = "pie",
      textinfo = "label+percent",
      hovertemplate = "%{label}: %{percent:.1%}<extra></extra>"
    )
  })
  
  output$pie_furnish <- renderPlotly({
    df <- df_filtered(); req(nrow(df) > 0)
    
    idx <- quartile_filter(); if (is.null(idx)) return(plotly::plot_ly())
    
    dfx <- df %>%
      dplyr::filter(idx) %>%
      dplyr::count(furnishing_status, name = "n") %>%
      dplyr::mutate(
        Share = n / sum(n),
        Label = ifelse(is.na(furnishing_status), "Unknown", furnishing_status)
      ) %>%
      dplyr::filter(!is.na(Share))
    
    if (nrow(dfx) == 0) {
      return(plotly::plot_ly() %>% layout(
        annotations = list(x = 0.5, y = 0.5, text = "No data for selected filters/quartile",
                           showarrow = FALSE, xref = "paper", yref = "paper")
      ))
    }
    
    plotly::plot_ly(
      data = dfx,
      labels = ~Label, values = ~Share,
      type = "pie",
      textinfo = "label+percent",
      hovertemplate = "%{label}: %{percent:.1%}<extra></extra>"
    )
  })
  
  # ====================================================================
  # AGE OF HOUSE IMPACT (Plotly) — drag-select to ZOOM + Reset (3-decimals)
  # ====================================================================
  selected_age_range <- reactiveVal(NULL)
  
  output$plot_age_impact <- renderPlotly({
    df <- df_filtered(); req(nrow(df) > 0)
    
    agg <- df %>%
      dplyr::group_by(constructed_year) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(is.finite(constructed_year), is.finite(avg_price)) %>%
      dplyr::arrange(constructed_year)
    
    # If there are no rows after aggregation, return an empty plot
    if (nrow(agg) == 0) {
      p <- plotly::plot_ly() %>% layout(
        annotations = list(
          x = 0.5, y = 0.5, text = "No data available under current filters",
          showarrow = FALSE, xref = "paper", yref = "paper"
        )
      )
      p <- plotly::event_register(p, 'plotly_selected')  # register brush event even on empty plots
      return(p)
    }
    
    sel <- selected_age_range()  # NULL or c(x0, x1)
    
    p <- plotly::plot_ly(
      data = agg, source = "age",
      x = ~constructed_year, y = ~avg_price,
      type = "scatter", mode = "lines+markers", hoverinfo = "text",
      text = ~paste0(
        "Year: ", constructed_year,
        "<br>Avg price: ", fmt_compact_money_prec(avg_price, digits = 3)
      )
    ) %>%
      layout(
        dragmode = "select",
        xaxis = c(list(title = "Constructed year"),
                  if (!is.null(sel) && all(is.finite(sel))) list(range = sel) else NULL),
        yaxis = c(list(title = ""),
                  make_yaxis_compact(agg$avg_price, digits = 3))
      )
    
    p <- plotly::event_register(p, 'plotly_selected')
    p
  })
  
  observeEvent(plotly::event_data("plotly_selected", source = "age"), {
    ed <- plotly::event_data("plotly_selected", source = "age")
    if (is.null(ed) || !isTRUE(NROW(ed) > 0) || all(is.na(ed[["x"]]))) {
      selected_age_range(NULL); return()
    }
    xr <- range(ed[["x"]], na.rm = TRUE)
    if (all(is.finite(xr))) selected_age_range(xr)
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset_age, {
    selected_age_range(NULL)
  })
  
  # ====================================================================
  # INTERACTIVE MAP (country view -> click -> city view) + legend
  # ====================================================================
  output$map_prices <- renderLeaflet({
    df_c <- df_price() %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(is.finite(avg_price)) %>%
      dplyr::inner_join(country_coords, by = "country")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        data    = df_c,
        lng     = ~lng, lat = ~lat,
        radius  = ~scales::rescale(avg_price, to = c(6, 16)),
        label   = ~paste0(country, ": ", fmt_compact_money(round(avg_price, 0))),
        layerId = ~country,
        stroke  = FALSE, fillOpacity = 0.7
      ) %>%
      addControl(html = HTML(
        '<div style="background:white;padding:8px;border-radius:6px;box-shadow:0 1px 3px rgba(0,0,0,0.2);">
           <b>Bubble size: avg price</b><br/>
           <svg width="160" height="38">
             <circle cx="25" cy="20" r="6"  fill="#3388ff" fill-opacity="0.7"></circle>
             <text x="45" y="24" font-size="12">lower</text>
             <circle cx="110" cy="20" r="16" fill="#3388ff" fill-opacity="0.7"></circle>
             <text x="135" y="24" font-size="12">higher</text>
           </svg>
         </div>'
      ), position = "bottomleft")
  })
  
  observeEvent(input$map_prices_marker_click, {
    click <- input$map_prices_marker_click
    req(click$id %in% country_coords$country)
    
    df_city <- df_price() %>%
      dplyr::group_by(country, city) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(country == click$id, is.finite(avg_price)) %>%
      dplyr::inner_join(city_coords, by = c("country", "city"))
    
    if (nrow(df_city) == 0) return()
    
    leafletProxy("map_prices") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data    = df_city,
        lng     = ~lng, lat = ~lat,
        radius  = ~scales::rescale(avg_price, to = c(5, 14)),
        label   = ~paste0(city, ": ", fmt_compact_money(round(avg_price, 0))),
        popup   = ~paste0("<b>", city, "</b><br/>Avg price: ", fmt_compact_money(round(avg_price, 0))),
        stroke  = FALSE, fillOpacity = 0.75
      ) %>%
      fitBounds(lng1 = min(df_city$lng), lat1 = min(df_city$lat),
                lng2 = max(df_city$lng), lat2 = max(df_city$lat))
  })
  
  observeEvent(input$btn_reset_map, {
    df_c <- df_price() %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(is.finite(avg_price)) %>%
      dplyr::inner_join(country_coords, by = "country")
    
    leafletProxy("map_prices") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data    = df_c,
        lng     = ~lng, lat = ~lat,
        radius  = ~scales::rescale(avg_price, to = c(6, 16)),
        label   = ~paste0(country, ": ", fmt_compact_money(round(avg_price, 0))),
        layerId = ~country,
        stroke  = FALSE, fillOpacity = 0.7
      ) %>%
      setView(lng = 10, lat = 20, zoom = 2)
  })
  
  # ====================================================================
  # FINANCIAL ANALYSIS TAB
  # ====================================================================
  
  # ---- Column resolver (handles alternative column names safely) ----
  resolve_col <- function(prefs, in_names = names(house)) {
    hit <- prefs[prefs %in% in_names]
    if (length(hit)) hit[[1]] else NA_character_
  }
  
  col_salary   <- resolve_col(c("customer_salary", "salary", "income"))
  col_loan     <- resolve_col(c("loan_amount", "loan", "loan_amt"))
  col_emi      <- resolve_col(c("emi_to_income", "emi_to_income_ratio", "emi_ratio", "emi"))
  col_decision <- resolve_col(c("decision", "purchase_decision", "purchased"))
  
  # --- Ensure EMI mapping is correct (force to the known CSV column if present)
  if (is.na(col_emi) || is.null(col_emi) || !(col_emi %in% names(house))) {
    if ("emi_to_income_ratio" %in% names(house)) {
      col_emi <- "emi_to_income_ratio"
    }
  }
  
  # Helper: robust decision -> 0/1
  decision_to_binary <- function(x) {
    d <- tolower(as.character(x))
    dplyr::case_when(
      d %in% c("1","true","t","yes","y","purchased","buy","bought") ~ 1,
      d %in% c("0","false","f","no","n","not purchased","reject","decline") ~ 0,
      TRUE ~ suppressWarnings(as.numeric(d))
    )
  }
  
  # Build a "financial base" with standardized columns if they exist
  df_financial_base <- reactive({
    df <- df_price()
    df %>%
      dplyr::mutate(
        salary   = if (!is.na(col_salary))   suppressWarnings(as.numeric(.data[[col_salary]]))   else NA_real_,
        loan     = if (!is.na(col_loan))     suppressWarnings(as.numeric(.data[[col_loan]]))     else NA_real_,
        emi      = if (!is.na(col_emi)) {
          v <- suppressWarnings(as.numeric(.data[[col_emi]]))
          # Clamp impossible negatives to 0; keep NA for non-finite
          dplyr::if_else(is.finite(v) & v < 0, 0, v, missing = NA_real_)
        } else NA_real_,
        decision = if (!is.na(col_decision)) decision_to_binary(.data[[col_decision]])           else NA_real_
      )
  })
  
  # Filters for the Financial tab (independent from Insights)
  df_financial <- reactive({
    df <- df_financial_base()
    if (!is.null(input$fin_country)  && input$fin_country  != "All") df <- df %>% dplyr::filter(country == input$fin_country)
    if (!is.null(input$fin_property) && input$fin_property != "All") df <- df %>% dplyr::filter(property_type == input$fin_property)
    df
  })
  
  # Axis label helper for y = price_display
  yaxis_money <- function(yvals, digits = 1) {
    c(list(title = ""), make_yaxis_compact(yvals, digits = digits))
  }
  
  # 1) Relationship scatter (salary/loan/EMI vs price, colored by decision)
  output$fin_scatter <- renderPlotly({
    df <- df_financial()
    req(nrow(df) > 0, !all(is.na(df$price_display)))
    
    xkey <- switch(input$fin_xvar,
                   salary = "salary",
                   loan   = "loan",
                   emi    = "emi",
                   "salary")
    req(xkey %in% names(df))
    
    dfx <- df %>%
      dplyr::filter(is.finite(.data[[xkey]]), is.finite(price_display)) %>%
      dplyr::mutate(
        decision_f = dplyr::case_when(
          !is.na(decision) & decision %in% c(1) ~ "Purchased",
          !is.na(decision) & decision %in% c(0) ~ "Not purchased",
          TRUE ~ "Unknown"
        )
      )
    
    if (nrow(dfx) == 0) return(plotly::plot_ly())
    
    xlab <- switch(xkey,
                   salary = "Customer salary",
                   loan   = "Loan amount",
                   emi    = "EMI-to-income ratio",
                   xkey)
    
    plotly::plot_ly(
      data = dfx,
      x = ~.data[[xkey]], y = ~price_display, color = ~decision_f,
      type = "scatter", mode = "markers",
      text = ~paste0(
        xlab, ": ",
        if (xkey == "emi") round(.data[[xkey]], 3) else scales::number(.data[[xkey]], accuracy = 1),
        "<br>Price: ", fmt_compact_money(price_display),
        "<br>Decision: ", decision_f
      ),
      hoverinfo = "text",
      alpha = 0.6
    ) %>%
      layout(
        xaxis = list(title = xlab),
        yaxis = yaxis_money(dfx$price_display, digits = 1),
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # 2) Salary buckets → avg price (purchases only)
  output$fin_salary_buckets <- renderPlotly({
    df <- df_financial()
    req(nrow(df) > 0, !all(is.na(df$salary)), !all(is.na(df$price_display)))
    
    dfx <- df %>%
      dplyr::filter(decision %in% c(1), is.finite(salary), is.finite(price_display))
    
    if (nrow(dfx) == 0) {
      return(plotly::plot_ly() %>% layout(
        annotations = list(x = 0.5, y = 0.5, text = "No purchased records under current filters",
                           showarrow = FALSE, xref = "paper", yref = "paper")
      ))
    }
    
    k <- ifelse(is.null(input$fin_salary_bins), 8, input$fin_salary_bins)
    probs <- seq(0, 1, length.out = k + 1)
    brks  <- unique(stats::quantile(dfx$salary, probs = probs, na.rm = TRUE))
    if (length(brks) < 2) {
      return(plotly::plot_ly() %>% layout(
        annotations = list(x = 0.5, y = 0.5, text = "Not enough variation in salary to bucket",
                           showarrow = FALSE, xref = "paper", yref = "paper")
      ))
    }
    
    agg <- dfx %>%
      dplyr::mutate(sal_bin = cut(salary, breaks = brks, include.lowest = TRUE, dig.lab = 8)) %>%
      dplyr::group_by(sal_bin) %>%
      dplyr::summarise(
        avg_price = mean(price_display, na.rm = TRUE),
        n = dplyr::n(), .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(avg_price))
    
    plotly::plot_ly(
      data = agg,
      x = ~sal_bin, y = ~avg_price, type = "bar",
      text = ~paste0("Avg price: ", fmt_compact_money(avg_price), "<br>Obs: ", n),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Salary buckets (quantiles)"),
        yaxis = yaxis_money(agg$avg_price, digits = 1),
        bargap = 0.15
      )
  })
  
  # 3) Affordability index = price / salary (over constructed year)
  output$fin_affordability <- renderPlotly({
    df <- df_financial()
    req(nrow(df) > 0, "constructed_year" %in% names(df))
    
    dfx <- df %>%
      dplyr::filter(is.finite(price_display), is.finite(salary), is.finite(constructed_year)) %>%
      dplyr::mutate(afford_idx = price_display / salary) %>%
      dplyr::group_by(constructed_year) %>%
      dplyr::summarise(afford_idx = mean(afford_idx, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(constructed_year)
    
    if (nrow(dfx) == 0) return(plotly::plot_ly())
    
    plotly::plot_ly(
      data = dfx,
      x = ~constructed_year, y = ~afford_idx,
      type = "scatter", mode = "lines+markers",
      text = ~paste0("Year: ", constructed_year, "<br>Affordability index: ", round(afford_idx, 3)),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Constructed year"),
        yaxis = list(title = "Price / Salary", tickformat = ".3f")
      )
  })
  
  # 4) EMI-to-income ratio — Purchase rate (top) + Bin counts (bottom) with named legend
  output$fin_emi_rate_bins <- renderPlotly({
    df <- df_financial()
    req(nrow(df) > 0, "emi" %in% names(df), "decision" %in% names(df))
    
    dfx <- df %>% dplyr::filter(is.finite(emi), emi >= 0, !is.na(decision))
    if (nrow(dfx) == 0) return(plotly::plot_ly())
    
    breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.80, 1.00, Inf)
    labels <- c("0.00–0.10","0.10–0.20","0.20–0.30","0.30–0.40",
                "0.40–0.50","0.50–0.60","0.60–0.80","0.80–1.00",">1.00")
    
    agg <- dfx %>%
      dplyr::mutate(
        emi_bin = cut(
          emi,
          breaks = breaks,
          include.lowest = TRUE,   # include 0 in the first interval
          right = FALSE,           # [a, b) so 0 -> "0.00–0.10"
          labels = labels
        ),
        purchased = decision == 1
      ) %>%
      dplyr::group_by(emi_bin) %>%
      dplyr::summarise(
        purchase_rate = mean(purchased, na.rm = TRUE),
        n             = dplyr::n(), .groups = "drop"
      ) %>%
      dplyr::mutate(emi_bin = factor(emi_bin, levels = labels)) %>%
      tidyr::complete(emi_bin, fill = list(purchase_rate = NA_real_, n = 0)) %>%
      dplyr::arrange(emi_bin)
    
    # Top: purchase rate line
    p_rate <- plotly::plot_ly(
      data = agg,
      x = ~emi_bin, y = ~purchase_rate,
      type = "scatter", mode = "lines+markers",
      name = "Purchase Rate", showlegend = TRUE,
      text = ~paste0(
        "EMI bin: ", emi_bin,
        "<br>Purchase rate: ",
        ifelse(is.na(purchase_rate), "—", scales::percent(purchase_rate, accuracy = 1)),
        "<br>Obs: ", n
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        yaxis = list(title = "Purchase rate", tickformat = ".0%"),
        margin = list(t = 20)
      )
    
    # Bottom: observation counts
    p_count <- plotly::plot_ly(
      data = agg,
      x = ~emi_bin, y = ~n,
      type = "bar",
      name = "Number of Decisions", showlegend = TRUE,
      text = ~paste0("EMI bin: ", emi_bin, "<br>Obs: ", n),
      hoverinfo = "text"
    ) %>%
      layout(
        yaxis = list(title = "Observations"),
        margin = list(t = 10)
      )
    
    plotly::subplot(p_rate, p_count, nrows = 2, shareX = TRUE, heights = c(0.6, 0.4)) %>%
      layout(
        xaxis = list(title = "EMI-to-income bin"),
        legend = list(orientation = "h", y = -0.15)
      )
  })
  # ===========================
  # MODELING — helpers
  # ===========================
  
  # Keep city choices synced to chosen country in the predictor UI
  observeEvent(input$pred_country, {
    opts <- house %>%
      dplyr::filter(country == input$pred_country) %>%
      dplyr::pull(city) %>%
      unique() %>% sort()
    updateSelectInput(session, "pred_city", choices = opts, selected = dplyr::first(opts))
  }, ignoreInit = TRUE)
  
  # Build a modeling frame aligned to the current currency display
  df_modeling <- reactive({
    # Use the same currency-normalized data as elsewhere
    df <- df_price()
    df %>%
      dplyr::mutate(
        # Basic sanitization of booleans and factors used below
        garage = as.integer(garage %in% 1),
        garden = as.integer(garden %in% 1),
        furnishing_status = dplyr::case_when(
          is.na(furnishing_status) ~ "Unknown",
          TRUE ~ as.character(furnishing_status)
        )
      )
  })
  
  # ===========================
  # Recommendation
  # ===========================
  # ----- Modeling: recommendation engine (suggestive) -----
  recommend_suggestions <- eventReactive(input$btn_recommend, {
    df <- df_modeling()
    if (nrow(df) == 0) return(NULL)
    
    # Base filter from user prefs (suggestive, not overconstrained)
    dfx <- df
    if (!is.null(input$mdl_property) && input$mdl_property != "All") {
      dfx <- dfx %>% dplyr::filter(property_type == input$mdl_property)
    }
    if (isTRUE(input$mdl_garage)) dfx <- dfx %>% dplyr::filter(garage == 1)
    if (isTRUE(input$mdl_garden)) dfx <- dfx %>% dplyr::filter(garden == 1)
    if (!is.null(input$mdl_furnish) && input$mdl_furnish != "Any") {
      dfx <- dfx %>% dplyr::filter(furnishing_status == input$mdl_furnish)
    }
    
    # Budget window in display currency
    pr <- input$mdl_price_range
    if (length(pr) == 2 && all(is.finite(pr))) {
      dfx <- dfx %>% dplyr::filter(price_display >= pr[1], price_display <= pr[2])
    }
    
    # Optional salary band [0.6x, 1.4x] if a salary column exists
    if (is.finite(input$mdl_salary)) {
      sal_col <- intersect(c("customer_salary","salary","income"), names(dfx))
      if (length(sal_col) == 1) {
        dfx <- dfx %>%
          dplyr::mutate(sal_num = suppressWarnings(as.numeric(.data[[sal_col]]))) %>%
          dplyr::filter(is.finite(sal_num)) %>%
          dplyr::filter(sal_num >= 0.6 * input$mdl_salary,
                        sal_num <= 1.4 * input$mdl_salary)
      }
    }
    
    if (nrow(dfx) == 0) return(list(ok = FALSE))
    
    # 1) Recommend SQFT from successful purchases within the filtered set
    dfp <- dfx %>% dplyr::filter(is.finite(property_size_sqft))
    dfp_buy <- dfp %>% dplyr::filter(!is.na(decision) & decision == 1)
    base_sqft <- if (nrow(dfp_buy) >= 50) dfp_buy else dfp
    if (nrow(base_sqft) == 0) {
      rec_sqft <- NA_real_; lo_sqft <- NA_real_; hi_sqft <- NA_real_
    } else {
      qs <- stats::quantile(base_sqft$property_size_sqft, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      lo_sqft <- as.numeric(qs[1]); rec_sqft <- as.numeric(qs[2]); hi_sqft <- as.numeric(qs[3])
    }
    
    # 2) Recommend COUNTRY by smoothed purchase rate
    global_rate <- df %>%
      dplyr::filter(!is.na(decision)) %>%
      dplyr::summarise(r = mean(decision == 1), .groups = "drop") %>% dplyr::pull(r)
    if (!is.finite(global_rate)) global_rate <- 0.5
    alpha <- 5
    
    agg_country <- dfx %>%
      dplyr::filter(!is.na(country), !is.na(decision)) %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(n = dplyr::n(),
                       buys = sum(decision == 1),
                       .groups = "drop") %>%
      dplyr::mutate(rate = (buys + alpha * global_rate) / (n + alpha)) %>%
      dplyr::arrange(dplyr::desc(rate), dplyr::desc(n))
    
    rec_country <- if (nrow(agg_country) == 0) NA_character_ else agg_country$country[1]
    
    # 3) Top cities within the recommended country
    agg_city <- dfx %>%
      { if (is.na(rec_country)) . else dplyr::filter(., country == rec_country) } %>%
      dplyr::filter(!is.na(city), !is.na(decision)) %>%
      dplyr::group_by(country, city) %>%
      dplyr::summarise(n = dplyr::n(), buys = sum(decision == 1), .groups = "drop") %>%
      dplyr::mutate(rate = (buys + alpha * global_rate) / (n + alpha)) %>%
      dplyr::arrange(dplyr::desc(rate), dplyr::desc(n)) %>%
      dplyr::slice(1:12)
    
    list(
      ok = TRUE,
      rec_sqft = rec_sqft, lo_sqft = lo_sqft, hi_sqft = hi_sqft,
      rec_country = rec_country,
      agg_city = agg_city
    )
  })
  
  # Suggested SQFT valueBox
  output$rec_sqft <- renderValueBox({
    res <- recommend_suggestions()
    if (is.null(res) || !isTRUE(res$ok) || !is.finite(res$rec_sqft)) {
      return(valueBox("—", "Recommended size (sqft)", icon = icon("ruler-combined"), color = "light-blue"))
    }
    label <- if (is.finite(res$lo_sqft) && is.finite(res$hi_sqft)) {
      paste0(round(res$rec_sqft), " (IQR ", round(res$lo_sqft), "–", round(res$hi_sqft), ")")
    } else as.character(round(res$rec_sqft))
    valueBox(label, "Recommended size (sqft)", icon = icon("ruler-combined"), color = "light-blue")
  })
  
  # Suggested COUNTRY valueBox
  output$rec_country <- renderValueBox({
    res <- recommend_suggestions()
    if (is.null(res) || !isTRUE(res$ok) || is.na(res$rec_country)) {
      return(valueBox("—", "Recommended country", icon = icon("globe"), color = "teal"))
    }
    valueBox(res$rec_country, "Recommended country", icon = icon("globe"), color = "teal")
  })
  
  # City bar chart in recommended country
  output$mdl_city_bar <- renderPlotly({
    res <- recommend_suggestions()
    if (is.null(res) || !isTRUE(res$ok) || nrow(res$agg_city) == 0) {
      return(plotly::plot_ly() %>% layout(
        annotations = list(x = 0.5, y = 0.5, text = "No matching cities", showarrow = FALSE,
                           xref = "paper", yref = "paper")
      ))
    }
    plotly::plot_ly(
      data = res$agg_city,
      x = ~paste0(city, " (", country, ")"),
      y = ~rate, type = "bar",
      text = ~paste0(
        city, " — ", country,
        "<br>Purchase rate: ", scales::percent(rate, 1),
        "<br>Obs: ", n
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "City"),
        yaxis = list(title = "Estimated purchase rate", tickformat = ".0%")
      )
  })
  
  # ===========================
  # Price Prediction (log-linear baseline with CIs)
  # ===========================
  predict_price <- eventReactive(input$btn_predict, {
    df <- df_modeling()
    
    # --- training frame ---
    train <- df %>%
      dplyr::mutate(
        y         = log(pmax(price_display, 1)),
        ln_sqft   = log(pmax(property_size_sqft, 1)),
        garage    = factor(garage, levels = c(0, 1)),
        garden    = factor(garden, levels = c(0, 1)),
        furnishing_status = factor(
          dplyr::coalesce(as.character(furnishing_status), "Unknown")
        ),
        country       = factor(country),
        city          = factor(city),
        property_type = factor(property_type)
      ) %>%
      dplyr::filter(
        is.finite(y), is.finite(ln_sqft),
        !is.na(country), !is.na(city), !is.na(property_type), !is.na(constructed_year)
      ) %>%
      droplevels()
    
    if (nrow(train) < 50) return(NULL)
    
    fmla <- y ~ ln_sqft + country + city + property_type + constructed_year +
      garage + garden + furnishing_status
    fit <- stats::lm(fmla, data = train)
    
    # --- build new-data row from inputs ---
    nd <- tibble::tibble(
      property_size_sqft = input$pred_sqft,
      ln_sqft            = log(pmax(input$pred_sqft, 1)),
      country            = as.character(input$pred_country),
      city               = as.character(input$pred_city),
      property_type      = as.character(input$pred_property),
      constructed_year   = input$pred_year,
      garage             = as.integer(isTRUE(input$pred_garage)),
      garden             = as.integer(isTRUE(input$pred_garden)),
      furnishing_status  = as.character(input$pred_furnish)
    )
    
    # helper: align a character column in nd to training factor levels safely
    align_factor <- function(nd, train, var) {
      lv <- levels(train[[var]])
      if (is.null(lv) || length(lv) == 0) {
        # fall back to unique non-NA values observed in training
        lv <- sort(unique(as.character(train[[var]])))
      }
      if (length(lv) == 0) {
        # no valid levels at all; return NA factor with empty levels (model will error later)
        nd[[var]] <- factor(NA_character_, levels = character(0))
        return(nd)
      }
      val <- as.character(nd[[var]][1])
      if (!isTRUE(val %in% lv)) val <- lv[1]
      nd[[var]] <- factor(val, levels = lv)
      nd
    }
    
    nd <- align_factor(nd, train, "country")
    nd <- align_factor(nd, train, "city")
    nd <- align_factor(nd, train, "property_type")
    nd <- align_factor(nd, train, "furnishing_status")
    nd$garage <- factor(nd$garage, levels = c(0, 1))
    nd$garden <- factor(nd$garden, levels = c(0, 1))
    
    # guard: if any aligned factor ended up with 0 levels, bail gracefully
    bad <- any(vapply(c("country","city","property_type","furnishing_status"),
                      function(v) length(levels(nd[[v]])) == 0, logical(1)))
    if (bad) return(NULL)
    
    # --- predict on log scale, invert, and return ---
    pr <- stats::predict(fit, newdata = nd, interval = "prediction", level = 0.80, se.fit = TRUE)
    out <- tibble::tibble(
      pred_lo = exp(pr$fit[,"lwr"]),
      pred    = exp(pr$fit[,"fit"]),
      pred_hi = exp(pr$fit[,"upr"])
    )
    
    list(pred_df = out, model = fit)
  })
  
  output$predicted_price_text <- renderText({
    res <- predict_price()
    if (is.null(res)) return("Not enough data to train a model under current settings.")
    p  <- res$pred_df$pred
    lo <- res$pred_df$pred_lo
    hi <- res$pred_df$pred_hi
    paste0("Point estimate: ", fmt_compact_money(p),
           "   |   80% prediction interval: ",
           fmt_compact_money(lo), " — ", fmt_compact_money(hi))
  })
  
  output$predicted_price_ci <- renderPlotly({
    res <- predict_price()
    if (is.null(res)) return(plotly::plot_ly())
    
    p  <- res$pred_df$pred
    lo <- res$pred_df$pred_lo
    hi <- res$pred_df$pred_hi
    
    plotly::plot_ly() %>%
      add_trace(
        x = c(0, 1), y = c(p, p), type = "scatter", mode = "lines",
        name = "Predicted", hoverinfo = "skip"
      ) %>%
      add_trace(
        x = c(0, 1, 1, 0, 0),
        y = c(lo, lo, hi, hi, lo),
        type = "scatter", mode = "lines", fill = "toself",
        name = "80% PI", hoverinfo = "skip", opacity = 0.3
      ) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(showticklabels = FALSE, title = ""),
        yaxis = c(list(title = ""), make_yaxis_compact(c(lo, p, hi), digits = 1))
      )
  })
  # ---------------------------
  # Data Table: dynamic filter UIs
  # ---------------------------
  
  # City input appears only when a specific country is selected
  output$dt_city_ui <- renderUI({
    req(input$dt_country)
    if (identical(input$dt_country, "All")) return(NULL)
    
    dfc <- df_price() %>% dplyr::filter(country == input$dt_country)
    choices <- c("All", sort(unique(dfc$city)))
    selectInput("dt_city", "City", choices = choices, selected = "All")
  })
  
  # Price range slider (based on current display currency across all data)
  output$dt_value_ui <- renderUI({
    df <- df_price()
    rng <- range(df$price_display, na.rm = TRUE)
    if (!all(is.finite(rng))) rng <- c(0, 1)
    
    sliderInput(
      "dt_value", "House value range (display currency)",
      min = floor(rng[1]), max = ceiling(rng[2]),
      value = rng, step = max(1, round((rng[2] - rng[1]) / 200))
    )
  })
  
  # ---------------------------
  # Data Table: filtered data
  # ---------------------------
  df_table <- reactive({
    df <- df_price()  # respects currency toggle used elsewhere
    if (nrow(df) == 0) return(df)
    
    # Apply filters
    if (!identical(input$dt_country, "All")) {
      df <- df %>% dplyr::filter(country == input$dt_country)
    }
    if (!is.null(input$dt_city) && !identical(input$dt_city, "All")) {
      df <- df %>% dplyr::filter(city == input$dt_city)
    }
    if (!is.null(input$dt_value) && length(input$dt_value) == 2 && all(is.finite(input$dt_value))) {
      df <- df %>% dplyr::filter(price_display >= input$dt_value[1],
                                 price_display <= input$dt_value[2])
    }
    if (!is.null(input$dt_decision) && !identical(input$dt_decision, "All") && "decision" %in% names(df)) {
      if (identical(input$dt_decision, "Purchased")) {
        df <- df %>% dplyr::filter(decision == 1)
      } else if (identical(input$dt_decision, "Not purchased")) {
        df <- df %>% dplyr::filter(decision == 0)
      }
    }
    
    df
  })
  
  # ---------------------------
  # Data Table: render
  # ---------------------------
  output$dt_table <- DT::renderDT({
    df <- df_table()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No rows under current filters"),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    
    # Safe Yes/No mapping
    yes_no <- function(x) dplyr::case_when(
      is.na(x)          ~ "Unknown",
      x %in% c(1, "1")  ~ "Yes",
      x %in% c(0, "0")  ~ "No",
      TRUE              ~ as.character(x)
    )
    
    # Build display frame in desired order and names
    display <- df %>%
      dplyr::mutate(
        `Garage Access` = yes_no(garage),
        `Garden Access` = yes_no(garden),
        Price           = fmt_compact_money(price_display)  # uses display currency
      ) %>%
      dplyr::transmute(
        `Property #`             = property_id,
        Country                  = country,
        City                     = city,
        `Property Type`          = property_type,
        `Furnishing Status`      = furnishing_status,
        `Property Size (sqft)`   = property_size_sqft,
        Price,  # from mutated column above
        `Year of Construction`   = constructed_year,
        `# of Previous Owners`   = previous_owners,
        `# of Rooms`             = rooms,
        `# of Bathrooms`         = bathrooms,
        `Garage Access`,
        `Garden Access`
      )
    
    DT::datatable(
      display,
      rownames = FALSE,
      filter   = "top",
      options  = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        order = list()
      )
    )
  })
})
