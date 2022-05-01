
descriptive_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(

    sidebarLayout(
      sidebarPanel(
        titlePanel("Filters"),
        #shinythemes::themeSelector(),
        fluidRow(column(12,
                        
                        # Select Period
                        selectInput(inputId = ns("Period"),
                                    label = "Period",
                                    choices = unique(saas_data$Period),
                                    selected = "2022-03-01",
                        
                        ),
                        
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
                                           choices = unique(saas_data$ProductType)),
                        
                        # Set Billing Intervall
                        sliderInput(inputId = ns("BillingInterval"),
                                    label = "Billing intervall [Month]",
                                    min = min(saas_data$BillingInterval),
                                    max = max(saas_data$BillingInterval),
                                    value = c(min(saas_data$BillingInterval), max(saas_data$BillingInterval)))
        )), width=3),
      
      mainPanel(
        leafletOutput(ns("map"), height = "600"),
        textOutput(ns("debug_text"))
        )
      )
    )

}

descriptive_analytics <- function(input, output, session) {
  
  
  selected <- reactiveValues(SalesChannel = NULL,
                             SalesTyp = NULL,
                             CustomerIndustry = NULL,
                             ProductType=NULL,
                             BillingInterval=NULL,
                             Period=NULL)
  
  filter <- reactiveValues(SalesChannel = unique(saas_data$SalesChannel),
                           SalesTyp = unique(saas_data$SalesTyp),
                           CustomerIndustry = unique(saas_data$CustomerIndustry),
                           ProductType = unique(saas_data$ProductType),
                           BillingInterval = unique(saas_data$BillingInterval),
                           Period = unique(saas_data$Period))
  
  
  observeEvent(eventExpr = input$Period, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Period <- input$Period
    filter$Period <- if(is.null(selected$Period)) unique(saas_data$Period) else selected$Period
  })
  
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
  

# Plot --------------------------------------------------------------------

 # output$plt_test <- renderPlot({
 # 
 #   df_plt_temp <- saas_data %>%
 #     filter(Industries %in% c(input$CustomerIndustry))
 # 
 #   p <- ggplot(df_plt_temp, aes(ARR)) +
 #     geom_histogram()
 # 
 #   print(p)
 #  })
  

# map ---------------------------------------------------------------------

  output$map <- renderLeaflet({
    #TODO
    # check if country names of saas data coincide with world_spdf
    # map data from saas (group by country and aggregate f.e. number of customers...)
    
    mybins <- c(0,10,20,50,100,500,Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$POP2005, na.color="transparent", bins=mybins)
    
    # Prepare the text for tooltips
    mytext <- paste(
      "Country: ", world_spdf@data$NAME,"<br/>", 
      "Area: ", world_spdf@data$AREA, "<br/>", 
      "Population: ", round(world_spdf@data$POP2005, 2), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # create map
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor = ~mypalette(POP2005), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~POP2005, opacity=0.9, title = "Population (M)", position = "bottomleft" )
    
  })
  
  
  # text output for debugging  
  output$debug_text <- renderText({

    paste("input customer industry: ", input$IndustryFinder)

  })



  
# Generate UI -------------------------------------------------------------
  
  # Input Boxes
  updateSelectizeInput(session, "SalesChannel", label="Sales Channel", choices = unique(saas_data$SalesChannel), server=TRUE)
  updateSelectizeInput(session, "SalesTyp", label="Sales-type", choices = unique(saas_data$SalesTyp), server=TRUE)
  updateSelectizeInput(session, "CustomerIndustry", label="Customer-Industry", choices = unique(saas_data$CustomerIndustry), server=TRUE)
  updateSliderInput(session, "BillingInterval", label="Billing intervall [Month]", min = min(saas_data$BillingInterval), max = max(saas_data$BillingInterval), value = c(min(saas_data$BillingInterval), max(saas_data$BillingInterval)))
  updateSelectInput(session, "Period", label="Period", choices = unique(saas_data$Period))
  updateCheckboxGroupInput(session, "ProductType", label="Product-type(s)", choices = unique(saas_data$ProductType), selected = c("Cloud" = "Cloud", "On Premises", "Hybrid"))
  
}

