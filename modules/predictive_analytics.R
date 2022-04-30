
predictive_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    sidebarLayout(
      sidebarPanel(
        
        titlePanel("Filters"),
        #shinythemes::themeSelector(),
        fluidRow(column(12,
                        
                        # Select which Customer-industry(s) to plot
                        selectizeInput(inputId = ns("CustomerIndustry"),
                                       label = "Customer-Industry",
                                       choices = NULL,
                                       multiple=TRUE
                        ))),
        fluidRow(column(6,
                        # Select which Sales Channel and type to plot
                        selectizeInput(inputId = ns("SalesChannel"),
                                    label = "Sales Channel",
                                    choices=NULL, 
                                    multiple=TRUE)
        ),
        column(6, ofset = 3,
               selectizeInput(inputId = ns("SalesTyp"),
                           label = "Sales-typ",
                           choices=NULL, 
                           multiple=TRUE)
        )),
        fluidRow(column(12,
                        # Select which Product-type(s) to plot
                        checkboxGroupInput(inputId = ns("ProductType"),
                                           label = "Product-type(s):",
                                           choices = unique(saas_data$ProductType),
                                           selected = c("Cloud" = "Cloud", "On Premises", "Hybrid")),
                        
                        # Set Billing Intervall
                        sliderInput(inputId = ns("BillingInterval"),
                                    label = "Billing intervall [Month]",
                                    min = min(saas_data$BillingInterval),
                                    max = max(saas_data$BillingInterval),
                                    value = c(min(saas_data$BillingInterval), max(saas_data$BillingInterval)))
        )), width=3),
      
      mainPanel(
      )
    )
  )
  
}


predictive_analytics <- function(input, output, session) {
  
  
  selected <- reactiveValues(SalesChannel = NULL,
                             SalesTyp = NULL,
                             CustomerIndustry = NULL,
                             ProductType=NULL,
                             BillingInterval=NULL)
  
  filter <- reactiveValues(SalesChannel = unique(saas_data$SalesChannel),
                           SalesTyp = unique(saas_data$SalesTyp),
                           CustomerIndustry = unique(saas_data$CustomerIndustry),
                           ProductType = unique(saas_data$ProductType),
                           BillingInterval = unique(saas_data$BillingInterval))
  
  
  observeEvent(eventExpr = input$SalesChannel, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$SalesChannel <- input$SalesChannel
    filter$SalesChannel <- if(is.null(selected$SalesChannel)) unique(saas_data$SalesChannel) else selected$SalesChannel
  })
  
  observeEvent(eventExpr = input$SalesTyp, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$SalesTyp <- input$SalesTyp
    filter$SalesTyp <- if(is.null(selected$SalesTyp)) unique(saas_data$SalesTyp) else selected$SalesTyp
  })
  
  observeEvent(eventExpr = input$CustomerIndustry, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$CustomerIndustry <- input$CustomerIndustry
    filter$CustomerIndustry <- if(is.null(selected$CustomerIndustry)) unique(saas_data$CustomerIndustry) else selected$CustomerIndustry
  })
  
  observeEvent(eventExpr = input$ProductType, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$ProductType <- input$ProductType
    filter$ProductType <- if(is.null(selected$ProductType)) unique(saas_data$ProductType) else selected$ProductType
  })
 
  observeEvent(eventExpr = input$BillingInterval, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$BillingInterval <- input$BillingInterval
    filter$BillingInterval <- if(is.null(selected$BillingInterval)) unique(saas_data$BillingInterval) else selected$BillingInterval
  }) 
  
  
  # Generate UI -------------------------------------------------------------
  
  
  # Input Boxes
  updateSelectizeInput(session, "SalesChannel", label="Sales Channel", choices = unique(saas_data$SalesChannel), server=TRUE)
  updateSelectizeInput(session, "SalesTyp", label="Sales-typ", choices = unique(saas_data$SalesTyp), server=TRUE)
  updateSelectizeInput(session, "CustomerIndustry", label="Customer-Industry", choices = unique(saas_data$CustomerIndustry), server=TRUE)
  updateSliderInput(session, "BillingInterval", label="Billing intervall [Month]", min = min(saas_data$BillingInterval), max = max(saas_data$BillingInterval), value = c(min(saas_data$BillingInterval), max(saas_data$BillingInterval)))
  updateCheckboxGroupInput(session, "ProductType", label="Product-type(s)", choices = unique(saas_data$ProductType))
  

  
  
#   # Box: Map & Pie
#   output$box_map <- renderUI({
#     ns <- session$ns
#     
#     box(title="Customer Locations", status="primary", width=4,
#         leafletOutput(ns("map")))
#   })
#   
#   output$box_forecast_week <- renderUI({
#     ns <- session$ns
#     
#     box(title="Customer Orders", status="primary", width=8,
#         plotOutput(ns("forecast_week")))
#   })
#   
#   output$box_forecast_day <- renderUI({
#     ns <- session$ns
#     
#     box(title="Customer Orders: Day", status="primary", width=12,
#         plotlyOutput(ns("forecast_day")))
#   })
#   
# 
# # Generate Data -----------------------------------------------------------S
#   map_data <- reactive({
#     
#     colorRange <- input$sld_Bestellvolumen
#     colorRange[1] <- if(is.null(colorRange[1])) 0 else colorRange[1]
#     
#     temp <- shpm %>%
#       filter(Kart %in% filter$Kart, Kgruppe %in% filter$Kgrp, Ktyp %in% filter$Ktyp) %>%
#       group_by(customer_ID, Kname, Kstrasse, KPlz, Kart, Kort, Ktyp, Kgruppe, customer_Latitude, customer_Longitude) %>%
#       summarise(Pallets = sum(Pallets), GWkg = sum(GWkg)) %>%
#       ungroup() %>%
#       mutate(color_lbls = ifelse(GWkg/1000 < colorRange[1], "green", "red"),
#              popup_content = paste(sep='<br>',
#                                    paste0('<b>', Kname, '</b>', Kstrasse, KPlz, Kort, Kart, Kgruppe, Ktyp,
#                                           round(GWkg/1000, digits=0))))
#   })
#   
#   
#   output$map <- renderLeaflet({
#     leaflet(data=shpm) %>%
#       addProviderTiles(providers$OpenStreetMap.DE) %>%
#       fitBounds(~min(customer_Longitude), ~min(customer_Latitude),
#                 ~max(customer_Longitude), ~max(customer_Latitude))
#   })
#   
#   
#   observe(
#     leafletProxy("map", data=map_data()) %>%
#       clearMarkers() %>% clearMarkerClusters() %>%
#       addAwesomeMarkers(clusterOptions = markerClusterOptions(),
#                         icon = awesomeIcons(markerColor = map_data()$color_lbls),
#                         lng=~customer_Longitude, lat= ~customer_Latitude, popup=~popup_content)
#   )
#   
#   
#   forecast_data <- reactive({
#     
#     bounds <- input$map_bounds
#     
#     latRng <- range(bounds$north, bounds$south)
#     lngRng <- range(bounds$east, bounds$west)
#     
#     temp_1 <- shpm %>%
#       filter(Kart %in% filter$Kart, Kgruppe %in% filter$Kgrp, Ktyp %in% filter$Ktyp,
#              customer_Latitude >= latRng[1], customer_Latitude <= latRng[2],
#              customer_Longitude >= lngRng[1], customer_Longitude <= lngRng[2])
#   })
#   
#   
#   
#   output$forecast_week <- renderPlot({
#     
#     temp <- forecast_data() %>%
#       mutate(cal_week = week(ymd(del_date))) %>%
#       group_by(cal_week) %>%
#       summarise(tons = sum(GWkg)/1000) %>%
#       ungroup()
#     
#     anz_total <- nrow(temp)
#     anz_train <- round(0.8 * anz_total)
#     anz_test <- anz_total - anz_train
#     
#     train <- temp %>% select(tons) %>% slice(1:anz_train) %>% as.ts()
#     
#     fc_1 <- ses(train, h=anz_test)
#     fc_2 <- holt(train, h=anz_test)
#     fc_3 <- forecast(ets(train), h=anz_test)
#     fc_4 <- forecast(auto.arima(train), h=anz_test)
#     fc_5 <- forecast(nnetar(train), h=anz_test)
#     
#     fc_sel <- switch(input$Forecast,
#                      "EXPO-1" = fc_1, "EXPO-2" = fc_2,
#                      "ETSM" = fc_3, "ARIMA" = fc_4,
#                      "Neuro" = fc_5)
#     
#     autoplot(fc_sel) +
#       autolayer(fc_sel$mean, series = "Forecast")
#   })
#   
# 
#   output$forecast_day <- renderPlotly({
#     
#     temp <- forecast_data()
#     
#     temp <- shpm %>%
#       group_by(del_date) %>%
#       summarise(TO_total = sum(GWkg)/1000) %>%
#       ungroup() %>%
#       arrange(del_date) %>%
#       mutate(week_day = weekdays(del_date))
#     
#     working_day <- seq(from=1, to = nrow(temp))
#     
#     temp_1 <- cbind(working_day, temp) %>%
#       select(-del_date) %>%
#       mutate(Di = ifelse(week_day == "Dienstag", 1, 0),
#              Mi = ifelse(week_day == "Mittwoch", 1, 0),
#              Do = ifelse(week_day == "Donnerstag", 1, 0),
#              Fr = ifelse(week_day == "Freitag", 1, 0)) %>%
#       select(2, 1, 4:7)
#     
#     share_train <- 0.8
#     
#     set.seed(100)
#     shpm_train_rows <- sample(1:nrow(temp_1), share_train * nrow(temp_1))
#     shpm_TrainData <- temp_1[shpm_train_rows,]
#     shpm_TestData <- temp_1[-shpm_train_rows,]
#     
#     
#     mod_1 <- lm(data = shpm_TrainData, formula = TO_total ~ working_day)
#     res_1 <- predict(mod_1, shpm_TestData)
#     
#     mod_2 <- lm(data = shpm_TrainData, formula = TO_total ~ working_day + Di + Mi + Do + Fr)
#     res_2 <- predict(mod_2, shpm_TestData)
#     
#     mod_3 <- lm(data = shpm_TrainData, formula = TO_total ~ Di + Mi + Do + Fr)
#     res_3 <- predict(mod_3, shpm_TestData)
#     
#     
#     inp_ens <- data.frame(cbind(y = shpm_TestData$TO_total, res_1, res_2, res_3))
#     mod_ens <- lm(data = inp_ens, formula = y ~ -1 + res_1 + res_2 + res_3)
#     res_ens <- predict(mod_ens, inp_ens)
#     
#     actuals_preds <- data.frame(cbind(actuals = shpm_TestData$TO_total, res_1, res_2, res_3, res_ens))
#     mape <- mean(abs((actuals_preds$res_ens - actuals_preds$actuals)/actuals_preds$actuals))
#     
#     
#     res <- data.frame(cbind(actuals = actuals_preds$actuals,
#                             res_1, res_2, res_3, res_ens))
#     
#     xaxis <- seq(1, nrow(res))
#     
#     res <- cbind(xaxis, res)
#     
#     plot <- ggplot(data=res, aes(x=xaxis)) +
#       geom_line(aes(y=actuals, colour="Actual"), size=0.8) +
#       geom_line(aes(y=res_1, colour="Model 1"), size=0.8) +
#       geom_line(aes(y=res_2, colour="Model 2"), size=0.8) +
#       geom_line(aes(y=res_3, colour="Model 3"), size=0.8) +
#       geom_line(aes(y=res_ens, colour="Ensemble"), size=0.8) +
#       labs(x = "Test Data: x", y = "Test Data: y") +
#       scale_colour_manual(name="Models",
#                           values=c("red", "green", "blue", "orange", "purple"))
#     
#     plot %>% ggplotly()
#   })
 }