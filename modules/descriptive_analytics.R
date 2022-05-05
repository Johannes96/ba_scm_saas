
descriptive_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(

    sidebarLayout(
      sidebarPanel(
        titlePanel("Filters"),
        #shinythemes::themeSelector(),
        
                        # Create DataRangeInput for Period
                        dateRangeInput(inputId = ns("Period"),
                                       label = "Period",
                                       start = min(saas_data$Period),
                                       end = max(saas_data$Period),
                                       min = min(saas_data$Period),
                                       max = max(saas_data$Period)),
                        
                        # Create SelectInput for Customer Industry
                        selectizeInput(inputId = ns("CustomerIndustry"),
                                       label = "Customer industry",
                                       choices = unique(saas_data$CustomerIndustry),
                                       multiple=TRUE),

                        # Create Checkboxes for Product type
                        checkboxGroupInput(inputId = ns("ProductType"),
                                           label = "Product type",
                                           choices = unique(saas_data$ProductType)
                                           ),
                        
                        # Create SliderInput for Billing Interval
                        sliderInput(inputId = ns("BillingInterval"),
                                    label = "Billing intervall [Month]",
                                    min = min(saas_data$BillingInterval),
                                    max = max(saas_data$BillingInterval),
                                    value = c(min(saas_data$BillingInterval), max(saas_data$BillingInterval))),
        
                        actionButton(ns("btn_debug"), "debug", icon = icon("bug"))
        , width=3),
      
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Choropleth", icon = icon("globe-europe"), leafletOutput(ns("map"), height = "600")),
          tabPanel(title = "Map Timo", icon = icon("map-marked"))
        )
        )
      )
    )

}

descriptive_analytics <- function(input, output, session) {

# map ---------------------------------------------------------------------

  output$map <- renderLeaflet({
    
    # create temporary data frame
    saas_data_temp <- saas_data
    
    # filter temp data if customer industry is selected
    if (!is.null(input$CustomerIndustry)) {
      saas_data_temp <- saas_data_temp %>%
        dplyr::filter(CustomerIndustry %in% input$CustomerIndustry)
    }
    
    # filter temp data if product types are deselected
    if (!is.null(input$ProductType)) {
      saas_data_temp <- saas_data_temp %>%
        dplyr::filter(ProductType %in% input$ProductType)
    }
    
    # join spatial data with saas data
    world_spdf@data <- world_spdf@data %>%
      left_join(saas_data_temp %>%
                  dplyr::filter(between(Period, input$Period[1],input$Period[2])) %>%
                  dplyr::filter(between(BillingInterval, input$BillingInterval[1],input$BillingInterval[2])) %>%
                  dplyr::group_by(Country) %>%
                  dplyr::summarise(n_customers = n_distinct(CustomerID),
                                   avg_ARR = mean(ARR),
                                   sum_ARR = sum(ARR)) %>%
                  dplyr::rename(NAME = Country) %>%
                  dplyr::ungroup(),
                by = "NAME")
    
    # create bins and color palette
    mybins <- c(0,1000,2000,3000,4000,5000,6000,7000,Inf)
    mypalette <- colorBin(palette="YlOrBr", domain=world_spdf@data$avg_ARR, na.color="transparent", bins=mybins)
    
    # Prepare the text for tooltips
    mytext <- paste(
      "Land: ", world_spdf@data$NAME,"<br/>", 
      "Kunden: ", world_spdf@data$n_customers, "<br/>", 
      "Average ARR: ", round(world_spdf@data$avg_ARR, 2), "<br/>",
      "Summe ARR: ", round(world_spdf@data$sum_ARR, 2),
      sep="") %>%
      lapply(htmltools::HTML)
    
    # create map
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor = ~mypalette(avg_ARR), 
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
      addLegend( pal=mypalette, values=~avg_ARR, opacity=0.9, title = "Average ARR (€)", position = "bottomleft")
    
  })
  

  # button for debugging
  observeEvent(input$btn_debug, {
    browser()
    
  })

}

