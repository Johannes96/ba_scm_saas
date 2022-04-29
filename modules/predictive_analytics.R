
predictive_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width=12, verbatimTextOutput(ns("report_info")))),
    fluidRow(
      box(title="Controls", status='primary', width=12, collapsible = TRUE, collapsed = FALSE,
          column(width=2, selectizeInput(ns("KundenArt"), label="Kunden: Art", choices = NULL, multiple=TRUE)),
          column(width=2, selectizeInput(ns("KundenGrp"), label="Kunden: Gruppe", choices = NULL, multiple=TRUE)),
          column(width=2, selectizeInput(ns("KundenTyp"), label="Kunden: Typ", choices = NULL, multiple=TRUE)),
          column(width=2, selectizeInput(ns("Forecast"), label="Forecast", choices = c("EXPO-1", "EXPO-2", "ETSM", "ARIMA", "Neuro"), selected = "EXPO-1", multiple=FALSE)))),
    fluidRow(
      uiOutput(ns("box_map")),
      uiOutput(ns("box_forecast_week"))),
    fluidRow(
      uiOutput(ns("box_forecast_day"))
    )
  )
}

predictive_analytics <- function(input, output, session) {
  

# Add variables -----------------------------------------------------------
  selected <- reactiveValues()
  
  filter <- reactiveValues(Kart = unique(shpm$Kart),
                           Kgrp = unique(shpm$Kgruppe),
                           Ktyp = unique(shpm$Ktyp))
  

# Generate GUI ------------------------------------------------------------
  observeEvent(eventExpr = input$KundenArt, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Kart <- input$KundenArt
    filter$Kart <- if(is.null(selected$Kart)) unique(shpm$Kart) else selected$Kart
  })
  
  observeEvent(eventExpr = input$KundenGrp, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Kgrp <- input$KundenGrp
    filter$Kgrp <- if(is.null(selected$Kgrp)) unique(shpm$Kgruppe) else selected$Kgrp
  })
  
  observeEvent(eventExpr = input$KundenTyp, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Ktyp <- input$KundenTyp
    filter$Ktyp <- if(is.null(selected$Ktyp)) unique(shpm$Ktyp) else selected$Ktyp
  })
  
  
  # Input Boxes
  updateSelectizeInput(session, "KundenArt", label="Kunden: Art", choices = unique(shpm$Kart), server=TRUE)
  updateSelectizeInput(session, "KundenGrp", label="Kunden: Gruppe", choices = unique(shpm$Kgruppe), server=TRUE)
  updateSelectizeInput(session, "KundenTyp", label="Kunden: Typ", choices = unique(shpm$Ktyp), server=TRUE)
  
  
  output$report_info <- renderText({
    HTML(
      "This report makes a forecast concerning the shipment volumes for the last weeks of the year.")
  })
  
  
  # Box: Map & Pie
  output$box_map <- renderUI({
    ns <- session$ns
    
    box(title="Customer Locations", status="primary", width=4,
        leafletOutput(ns("map")))
  })
  
  output$box_forecast_week <- renderUI({
    ns <- session$ns
    
    box(title="Customer Orders", status="primary", width=8,
        plotOutput(ns("forecast_week")))
  })
  
  output$box_forecast_day <- renderUI({
    ns <- session$ns
    
    box(title="Customer Orders: Day", status="primary", width=12,
        plotlyOutput(ns("forecast_day")))
  })
  

# Generate Data -----------------------------------------------------------S
  map_data <- reactive({
    
    colorRange <- input$sld_Bestellvolumen
    colorRange[1] <- if(is.null(colorRange[1])) 0 else colorRange[1]
    
    temp <- shpm %>%
      filter(Kart %in% filter$Kart, Kgruppe %in% filter$Kgrp, Ktyp %in% filter$Ktyp) %>%
      group_by(customer_ID, Kname, Kstrasse, KPlz, Kart, Kort, Ktyp, Kgruppe, customer_Latitude, customer_Longitude) %>%
      summarise(Pallets = sum(Pallets), GWkg = sum(GWkg)) %>%
      ungroup() %>%
      mutate(color_lbls = ifelse(GWkg/1000 < colorRange[1], "green", "red"),
             popup_content = paste(sep='<br>',
                                   paste0('<b>', Kname, '</b>', Kstrasse, KPlz, Kort, Kart, Kgruppe, Ktyp,
                                          round(GWkg/1000, digits=0))))
  })
  
  
  output$map <- renderLeaflet({
    leaflet(data=shpm) %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>%
      fitBounds(~min(customer_Longitude), ~min(customer_Latitude),
                ~max(customer_Longitude), ~max(customer_Latitude))
  })
  
  
  observe(
    leafletProxy("map", data=map_data()) %>%
      clearMarkers() %>% clearMarkerClusters() %>%
      addAwesomeMarkers(clusterOptions = markerClusterOptions(),
                        icon = awesomeIcons(markerColor = map_data()$color_lbls),
                        lng=~customer_Longitude, lat= ~customer_Latitude, popup=~popup_content)
  )
  
  
  forecast_data <- reactive({
    
    bounds <- input$map_bounds
    
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    temp_1 <- shpm %>%
      filter(Kart %in% filter$Kart, Kgruppe %in% filter$Kgrp, Ktyp %in% filter$Ktyp,
             customer_Latitude >= latRng[1], customer_Latitude <= latRng[2],
             customer_Longitude >= lngRng[1], customer_Longitude <= lngRng[2])
  })
  
  
  
  output$forecast_week <- renderPlot({
    
    temp <- forecast_data() %>%
      mutate(cal_week = week(ymd(del_date))) %>%
      group_by(cal_week) %>%
      summarise(tons = sum(GWkg)/1000) %>%
      ungroup()
    
    anz_total <- nrow(temp)
    anz_train <- round(0.8 * anz_total)
    anz_test <- anz_total - anz_train
    
    train <- temp %>% select(tons) %>% slice(1:anz_train) %>% as.ts()
    
    fc_1 <- ses(train, h=anz_test)
    fc_2 <- holt(train, h=anz_test)
    fc_3 <- forecast(ets(train), h=anz_test)
    fc_4 <- forecast(auto.arima(train), h=anz_test)
    fc_5 <- forecast(nnetar(train), h=anz_test)
    
    fc_sel <- switch(input$Forecast,
                     "EXPO-1" = fc_1, "EXPO-2" = fc_2,
                     "ETSM" = fc_3, "ARIMA" = fc_4,
                     "Neuro" = fc_5)
    
    autoplot(fc_sel) +
      autolayer(fc_sel$mean, series = "Forecast")
  })
  

  output$forecast_day <- renderPlotly({
    
    temp <- forecast_data()
    
    temp <- shpm %>%
      group_by(del_date) %>%
      summarise(TO_total = sum(GWkg)/1000) %>%
      ungroup() %>%
      arrange(del_date) %>%
      mutate(week_day = weekdays(del_date))
    
    working_day <- seq(from=1, to = nrow(temp))
    
    temp_1 <- cbind(working_day, temp) %>%
      select(-del_date) %>%
      mutate(Di = ifelse(week_day == "Dienstag", 1, 0),
             Mi = ifelse(week_day == "Mittwoch", 1, 0),
             Do = ifelse(week_day == "Donnerstag", 1, 0),
             Fr = ifelse(week_day == "Freitag", 1, 0)) %>%
      select(2, 1, 4:7)
    
    share_train <- 0.8
    
    set.seed(100)
    shpm_train_rows <- sample(1:nrow(temp_1), share_train * nrow(temp_1))
    shpm_TrainData <- temp_1[shpm_train_rows,]
    shpm_TestData <- temp_1[-shpm_train_rows,]
    
    
    mod_1 <- lm(data = shpm_TrainData, formula = TO_total ~ working_day)
    res_1 <- predict(mod_1, shpm_TestData)
    
    mod_2 <- lm(data = shpm_TrainData, formula = TO_total ~ working_day + Di + Mi + Do + Fr)
    res_2 <- predict(mod_2, shpm_TestData)
    
    mod_3 <- lm(data = shpm_TrainData, formula = TO_total ~ Di + Mi + Do + Fr)
    res_3 <- predict(mod_3, shpm_TestData)
    
    
    inp_ens <- data.frame(cbind(y = shpm_TestData$TO_total, res_1, res_2, res_3))
    mod_ens <- lm(data = inp_ens, formula = y ~ -1 + res_1 + res_2 + res_3)
    res_ens <- predict(mod_ens, inp_ens)
    
    actuals_preds <- data.frame(cbind(actuals = shpm_TestData$TO_total, res_1, res_2, res_3, res_ens))
    mape <- mean(abs((actuals_preds$res_ens - actuals_preds$actuals)/actuals_preds$actuals))
    
    
    res <- data.frame(cbind(actuals = actuals_preds$actuals,
                            res_1, res_2, res_3, res_ens))
    
    xaxis <- seq(1, nrow(res))
    
    res <- cbind(xaxis, res)
    
    plot <- ggplot(data=res, aes(x=xaxis)) +
      geom_line(aes(y=actuals, colour="Actual"), size=0.8) +
      geom_line(aes(y=res_1, colour="Model 1"), size=0.8) +
      geom_line(aes(y=res_2, colour="Model 2"), size=0.8) +
      geom_line(aes(y=res_3, colour="Model 3"), size=0.8) +
      geom_line(aes(y=res_ens, colour="Ensemble"), size=0.8) +
      labs(x = "Test Data: x", y = "Test Data: y") +
      scale_colour_manual(name="Models",
                          values=c("red", "green", "blue", "orange", "purple"))
    
    plot %>% ggplotly()
  })
}