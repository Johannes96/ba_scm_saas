
predictive_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    sidebarLayout(
      sidebarPanel(
        
        titlePanel("Filters"),
        #shinythemes::themeSelector(),
        fluidRow(column(12,
                        
                        # Select which Forecast-type to plot
                        selectizeInput(ns("Forecast"), 
                                       label="Forecast", 
                                       choices=c("EXPO-1", "EXPO-2", "ETSM", "ARIMA", "Neuro"), 
                                       selected="EXPO-1", 
                                       multiple=FALSE))),
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
                           label = "Sales-type",
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
        uiOutput(ns("box_forecast"))
      )
    )
  )
  
}

# Add Variable ------------------------------------------------------------

predictive_analytics <- function(input, output, session) {
  
  
  selected <- reactiveValues()
  
  filter <- reactiveValues(SalesChannel = unique(saas_data$SalesChannel),
                           SalesTyp = unique(saas_data$SalesTyp),
                           CustomerIndustry = unique(saas_data$CustomerIndustry),
                           ProductType = unique(saas_data$ProductType),
                           BillingInterval = unique(saas_data$BillingInterval))
  
  # Generate GUI ------------------------------------------------------------
  
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
  updateSelectizeInput(session, "SalesTyp", label="Sales-type", choices = unique(saas_data$SalesTyp), server=TRUE)
  updateSelectizeInput(session, "CustomerIndustry", label="Customer-Industry", choices = unique(saas_data$CustomerIndustry), server=TRUE)
  updateSliderInput(session, "BillingInterval", label="Billing intervall [Month]", min = min(saas_data$BillingInterval), max = max(saas_data$BillingInterval), value = c(min(saas_data$BillingInterval), max(saas_data$BillingInterval)))
  updateCheckboxGroupInput(session, "ProductType", label="Product-type(s)", choices = unique(saas_data$ProductType))
  

  
  
   # Box: Forecast plot


   output$box_forecast <- renderUI({
     ns <- session$ns

     box(title="ARR-Forecast", status="primary", width=12,
         plotOutput(ns("forecast")))
   })


 
 # Generate Data -----------------------------------------------------------S
   
   forecast_data <- reactive({

     
     temp_1 <- saas_data %>%
       filter(BillingInterval %in% filter$BillingInterval, ProductType %in% filter$ProductType, CustomerIndustry %in% filter$CustomerIndustry, SalesTyp %in% filter$SalesTyp, SalesChannel %in% filter$SalesChannel)
   })
   
   
  # Forecast  ------------------------------------------------------------
   
   output$forecast <- renderPlot({
     
     temp_1 <- forecast_data() %>%
       group_by(Period)  %>%
       summarise(TotalARR = sum(ARR)) %>%
       ungroup()
     
     anz_total <- nrow(temp_1)
     anz_train <- round(0.8 * anz_total)
     anz_test <- anz_total - anz_train
     
     SUMARR <- temp_1 %>% select(TotalARR) %>% slice(1:anz_train) %>% as.ts()
     
     fc_1 <- ses(SUMARR, h=anz_test)
     fc_2 <- holt(SUMARR, h=anz_test)
     fc_3 <- forecast(ets(SUMARR), h=anz_test)
     fc_4 <- forecast(auto.arima(SUMARR), h=anz_test)
     fc_5 <- forecast(nnetar(SUMARR), h=anz_test)
     
     fc_sel <- switch(input$Forecast,
                      "EXPO-1" = fc_1, 
                      "EXPO-2" = fc_2,
                      "ETSM" = fc_3, 
                      "ARIMA" = fc_4,
                      "Neuro" = fc_5)
     
     autoplot(fc_sel) +
       autolayer(fc_sel$mean, series = "Forecast")
   })
   
 

 }