
yearly_analytics_UI <- function(id) {
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
        plotlyOutput(ns("yearlyplot"))
      )
    )
  )
  
}


yearly_analytics <- function(input, output, session) {
  
  
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
  

# Diagramm ----------------------------------------------------------------

  histo_data <- reactive({
    
    temp_1 <- saas_data %>%
      filter(BillingInterval %in% filter$BillingInterval, ProductType %in% filter$ProductType, CustomerIndustry %in% filter$CustomerIndustry, SalesTyp %in% filter$SalesTyp, SalesChannel %in% filter$SalesChannel)
    
    
  })
  
  output$yearlyplot <- renderPlotly({
    
    
    temp_1 <- histo_data() %>%
      group_by(Period)  %>%
      summarise(TotalARR = sum(ARR)) %>%
      ungroup()
    
    
    # fit <- lm(data=temp_1, TotalARR ~ Period)
    
    plot_ly() %>%
      add_trace(data = temp_1, type="bar", x= ~Period, y= ~TotalARR, name="ARR",
                hoverinfo="text", text=~format(round(TotalARR, digits=0), big.mark=".", decimal.mark=",")) %>%
      # add_lines(y=fitted(fit), name="Regression Line") %>%
      layout(title="ARR-movement", showlegend=TRUE)
    
    
  })
  
  
  # Generate UI-------------------------------------------------------------
  
  
  # Input Boxes
  updateSelectizeInput(session, "SalesChannel", label="Sales Channel", choices = unique(saas_data$SalesChannel), server=TRUE)
  updateSelectizeInput(session, "SalesTyp", label="Sales-type", choices = unique(saas_data$SalesTyp), server=TRUE)
  updateSelectizeInput(session, "CustomerIndustry", label="Customer-Industry", choices = unique(saas_data$CustomerIndustry), server=TRUE)
  updateSliderInput(session, "BillingInterval", label="Billing intervall [Month]", min = min(saas_data$BillingInterval), max = max(saas_data$BillingInterval), value = c(min(saas_data$BillingInterval), max(saas_data$BillingInterval)))
  updateCheckboxGroupInput(session, "ProductType", label="Product-type(s)", choices = unique(saas_data$ProductType))
  
  
  
  
  
}


