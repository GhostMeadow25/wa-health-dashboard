# R/server.R - All Server Logic

setup_server <- function(input, output, session) {
  
  # ── Helper: half-open interval legend labels ────────────────────
  make_legend_labels <- function(breaks) {
    n      <- length(breaks)
    labels <- character(n - 1)
    for (i in seq_len(n - 1)) {
      if (i == 1) {
        labels[i] <- paste0("< ", breaks[i + 1], "%")
      } else if (i == n - 1) {
        labels[i] <- paste0("\u2265 ", breaks[i], "%")
      } else {
        labels[i] <- paste0(breaks[i], " to < ", breaks[i + 1], "%")
      }
    }
    labels
  }
  
  # ── Reactive: NULL = state selected, string = county selected ───────────────
  selected_county <- reactive({
    req(input$selected_county)
    if (input$selected_county == STATE_NAME) NULL else input$selected_county
  })
  
  # ══ DATA CARD ════════════════════════════════════════════════════════════════
  
  output$datacard_ui <- renderUI({
    req(input$selected_county)
    loc <- input$selected_county
    
    if (loc == STATE_NAME) {
      pct <- state_pct
      num <- state_num
      compare_tag <- div(class = "stat-compare",
                         paste0("State total: ", format_number(num), " uninsured children"))
    } else {
      row <- county_data %>% filter(County == loc)
      if (nrow(row) == 0) return(div("No data available."))
      row <- row[1, ]
      pct <- row$Percent
      num <- row$Number
      
      compare_tag <- div(class = "stat-compare",
                         paste0("County total: ", format_number(num), " uninsured children"))
    }
    
    div(class = "stat-card",
        div(class = "stat-value", format_percent(pct)),
        div(class = "stat-label",
            paste0("of children (under 19) are not covered by any health insurance programs (public or private) \u2014 ", loc)),
        compare_tag
    )
  })
  
  # ══ MAP ══════════════════════════════════════════════════════════════════════
  
  output$map_plot <- renderLeaflet({
    leaflet(options = leafletOptions(
      minZoom   = 5,
      maxBounds = list(WA_BOUNDS$sw, WA_BOUNDS$ne)
    )) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = WA_CENTER[2], lat = WA_CENTER[1], zoom = WA_ZOOM) %>%
      setMaxBounds(lng1 = WA_BOUNDS$sw[2], lat1 = WA_BOUNDS$sw[1],
                   lng2 = WA_BOUNDS$ne[2], lat2 = WA_BOUNDS$ne[1])
  })
  
  observe({
    req(input$selected_county)
    county <- selected_county()
    
    if (is.null(county)) {
      # ── State view: county choropleth ────────────────────────────────────
      if (is.null(wa_counties)) return()
      
      merged <- wa_counties %>%
        left_join(county_data, by = c("NAMELSAD" = "County")) %>%
        sf::st_transform(crs = 4326)
      
      breaks     <- WA_BREAKS
      pal        <- colorBin("Blues", domain = county_data$Percent * 100,
                             bins = breaks, na.color = "#cccccc")
      labels_leg <- make_legend_labels(breaks)
      bin_colors <- pal((breaks[-length(breaks)] + breaks[-1]) / 2)
      
      tooltips <- paste0(
        "<div style='font-family:sans-serif;font-size:13px;'>",
        "<strong style='font-size:14px;'>", merged$NAMELSAD, "</strong><br/>",
        "Uninsured: <strong>",
        ifelse(is.na(merged$PctDisplay), "N/A", merged$PctDisplay),
        "</strong><br/>",
        "Count: ", ifelse(is.na(merged$NumDisplay), "N/A", merged$NumDisplay),
        "<br/><span style='color:#888;font-size:11px;'>WA avg: ",
        format_percent(state_pct), "</span></div>"
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map_plot") %>%
        clearShapes() %>% clearControls() %>%
        setView(lng = WA_CENTER[2], lat = WA_CENTER[1], zoom = WA_ZOOM) %>%
        addPolygons(
          data         = merged,
          fillColor    = ~pal(Percent * 100),
          fillOpacity  = 0.72,
          color        = "white", weight = 1.5,
          label        = tooltips,
          labelOptions = labelOptions(
            style     = list("font-family" = "Poppins, sans-serif"),
            direction = "auto"),
          highlight = highlightOptions(
            weight = 2.5, color = PRIMARY_COLOR,
            fillOpacity = 0.9, bringToFront = TRUE)
        ) %>%
        addLegend(position = "bottomright", colors = bin_colors,
                  labels = labels_leg, title = "% Uninsured<br/>(Under 19)",
                  opacity = 0.85)
      
    } else {
      # ── County view: cousub choropleth ───────────────────────────────────
      if (is.null(wa_cousub_shp) || is.null(wa_cousub_data)) {
        leafletProxy("map_plot") %>% clearShapes() %>% clearControls() %>%
          addControl("<b>Subdivision data not available.</b>", position = "topright")
        return()
      }
      
      cousub_county <- wa_cousub_data %>% filter(County == county)
      shp_county    <- wa_cousub_shp %>%
        left_join(cousub_county, by = c("GEOID" = "cousub_geoid")) %>%
        sf::st_transform(crs = 4326)
      
      county_outline <- wa_counties %>%
        filter(NAMELSAD == county) %>%
        sf::st_transform(crs = 4326)
      
      vals <- shp_county$Percent[!is.na(shp_county$Percent)] * 100
      
      if (length(vals) == 0 || all(is.na(vals))) {
        leafletProxy("map_plot") %>% clearShapes() %>% clearControls() %>%
          addControl(paste0("<b>No subdivision data for ", county, "</b>"),
                     position = "topright")
        return()
      }
      
      breaks     <- WA_BREAKS
      pal        <- colorBin("Blues",
                             domain = c(county_data$Percent * 100,
                                        wa_cousub_data$Percent * 100),
                             bins   = breaks, na.color = "#cccccc")
      labels_leg <- make_legend_labels(breaks)
      bin_colors <- pal((breaks[-length(breaks)] + breaks[-1]) / 2)
      
      county_pct     <- county_data %>% filter(County == county) %>% pull(Percent)
      county_avg_str <- paste0(gsub(" County$", "", county),
                               " avg: ", round(county_pct * 100, 1), "%")
      
      tooltips <- paste0(
        "<div style='font-family:sans-serif;font-size:13px;'>",
        "<strong style='font-size:14px;'>", shp_county$SubdivName, "</strong><br/>",
        ifelse(shp_county$PctDisplay == "Suppressed",
               "<em style='color:#999;'>Data not available (insufficient sample size)</em>",
               paste0("Uninsured: <strong>", shp_county$PctDisplay, "</strong><br/>",
                      "Count: ", shp_county$NumDisplay)),
        "<br/><span style='color:#888;font-size:11px;'>",
        county_avg_str, "</span></div>"
      ) %>% lapply(htmltools::HTML)
      
      center <- sf::st_centroid(county_outline) %>% sf::st_coordinates()
      
      leafletProxy("map_plot") %>%
        clearShapes() %>% clearControls() %>%
        setView(lng = center[1], lat = center[2], zoom = 9) %>%
        addPolygons(
          data         = shp_county,
          fillColor    = ~pal(Percent * 100),
          fillOpacity  = 0.72,
          color        = "white", weight = 1,
          label        = tooltips,
          labelOptions = labelOptions(
            style     = list("font-family" = "Poppins, sans-serif"),
            direction = "auto"),
          highlight = highlightOptions(
            weight = 2.5, color = PRIMARY_COLOR,
            fillOpacity = 0.9, bringToFront = TRUE)
        ) %>%
        addPolygons(
          data    = county_outline,
          fill    = FALSE,
          color   = PRIMARY_COLOR,
          weight  = 2.5,
          opacity = 1
        ) %>%
        addLegend(position = "bottomright", colors = bin_colors,
                  labels = labels_leg, title = "% Uninsured<br/>(Under 19)",
                  opacity = 0.85)
    }
  })
  
  # ══ BAR CHART ════════════════════════════════════════════════════════════════
  
  output$bar_chart <- renderPlotly({
    tryCatch({
      county   <- selected_county()
      cd       <- county_data %>% arrange(Percent)
      state_pp <- round(state_pct * 100, 1)
      
      # Check is.null(county), then do vectorized comparisons
      if (is.null(county)) {
        bar_colors  <- ifelse(cd$Percent > state_pct, "#c0392b", "#2E9E84")
        bar_opacity <- rep(0.85, nrow(cd))
        sel_label   <- rep("", nrow(cd))
      } else {
        bar_colors  <- ifelse(cd$County == county, "#F39C12",
                              ifelse(cd$Percent > state_pct, "#c0392b", "#2E9E84"))
        bar_opacity <- ifelse(cd$County == county, 1.0, 0.35)
        sel_label   <- ifelse(cd$County == county, "<br><b>\u2190 Selected</b>", "")
      }
      
      plot_ly(
        data        = cd,
        x           = ~round(Percent * 100, 2),
        y           = ~ShortName,
        type        = "bar",
        orientation = "h",
        marker      = list(color = bar_colors, opacity = bar_opacity),
        text        = ~paste0(
          "<b>", County, "</b><br>",
          "Uninsured rate: ", PctDisplay, "<br>",
          "Children uninsured: ", NumDisplay,
          sel_label
        ),
        textposition = "none",
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "Uninsured Rate (%)", ticksuffix = "%",
                       gridcolor = "#e8f0f7"),
          yaxis = list(title = "", automargin = TRUE,
                       tickfont = list(size = 9),
                       tickmode = "array",
                       tickvals = cd$ShortName,
                       ticktext = paste0(cd$ShortName, "   ")),
          shapes = list(list(
            type = "line", x0 = state_pp, x1 = state_pp, y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "#1B6CA8", width = 1.5, dash = "dash")
          )),
          annotations = list(list(
            x = state_pp, y = 1, xref = "x", yref = "paper",
            text = paste0("WA avg: ", state_pp, "%"),
            showarrow = FALSE,
            font    = list(size = 10, color = "#1B6CA8"),
            xanchor = "left", yanchor = "bottom",
            bgcolor = "rgba(255,255,255,0.7)"
          )),
          margin       = list(l = 10, r = 20, t = 30, b = 40),
          hoverlabel   = list(bgcolor = "white",
                              font = list(size = 11, family = "Poppins, sans-serif")),
          plot_bgcolor  = "#f5f9fc",
          paper_bgcolor = "#f5f9fc"
        ) %>%
        config(displayModeBar = FALSE)
      
    }, error = function(e) {
      plotly::plot_ly() %>%
        layout(title = list(text = paste("Chart error:", e$message)),
               xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    })
  })
}