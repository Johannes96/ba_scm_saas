
descriptive_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(

    sidebarLayout(
      sidebarPanel(
        
        titlePanel("Filters"),
        #shinythemes::themeSelector(),
        fluidRow(column(12,
                        
                        # Select Period
                        selectInput(inputId = "PeriodFinder",
                                    label = "Select Period",
                                    choices = levels(Periods),
                                    selected = "2022-03-01",
                        
                        ),
                        
                        # Select which Customer-industry(s) to plot
                        selectizeInput(inputId = "IndustryFinder",
                                      label = "Select Customer-Industry(s):",
                                      choices = levels(Industries),
                                      multiple=TRUE,
                                      selected=character(0)
                                      ))),
          fluidRow(column(5,
                          # Select which Sales Channel and type
                          selectInput(inputId = ns("SalesChannelFinder"),
                                      label = "Select Sales Channel",
                                      choices=NULL, 
                                      multiple=TRUE)
                 ),
                 column(5, ofset = 3,
                        selectInput(inputId = ns("SalesTypFinder"),
                                  label = "Select Saley-typ",
                                  choices=NULL, 
                                  multiple=TRUE)
                 )),
               
                       # Select which Region(s) to plot
                       checkboxGroupInput(inputId = "ProductTypeFinder",
                                          label = "Select Product-type(s):",
                                          choices = c("Cloud" = "Cloud", "On Premises", "Hybrid", "Support"),
                                          selected = c("Cloud" = "Cloud", "On Premises", "Hybrid")),
               
                      # Set Billing Intervall
                            sliderInput(inputId = "BillingIntervall",
                                  label = "Select Billing intervall",
                                  min = 1,
                                  max = 36,
                                  value = c(1,36))
        ),
      mainPanel(
        )
      )
    )

}


descriptive_analytics <- function(input, output, session) {
  
  
  selected <- reactiveValues(SalesChannel = NULL,
                             SalesTyp = NULL)
  
  filter <- reactiveValues(SalesChannel = unique(saas_data$SalesChannel),
                           SalesTyp = unique(saas_data$SalesTyp))
  
  
  observeEvent(eventExpr = input$SalesChannelFinder, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$SalesChannel <- input$SalesChannelFinder
    filter$SalesChannel <- if(is.null(selected$SalesChannel)) unique(saas_data$SalesChannel) else selected$Kart
  })
  
  observeEvent(eventExpr = input$SalesTypFinder, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Kgrp <- input$SalesTypFinder
    filter$SalesTyp <- if(is.null(selected$SalesTyp)) unique(saas_data$SalesTyp) else selected$SalesTyp
  })
  

  
  
  
  

# Generate UI -------------------------------------------------------------
  output$report_info <- renderText({
    HTML(
      "This report analyzes the order volumes of different customers in the distribution network of an existing manufacturer of fast-moving consumer goods.")
  })
  
  # Input Boxes
  updateSelectizeInput(session, "SalesChannelFinder", label="Select Sales Channel", choices = unique(saas_data$SalesChannel), server=TRUE)
  updateSelectizeInput(session, "SalesTypFinder", label="Select Saley-typ", choices = unique(saas_data$SalesTyp), server=TRUE)

  
  # Slider
  output$Bestellvolumen <- renderUI({
    ns <- session$ns
    
    temp <- shpm %>%
      group_by(customer_ID) %>%
      summarise(GWkg = sum(GWkg)/1000) %>%
      ungroup()
    
    gewicht_max <- round(max(temp$GWkg), digits = 0)
    
    sliderInput(ns("sld_Bestellvolumen"), "Bestellvolumen [to]",
                   min=0, max=gewicht_max, value=gewicht_max/2, step=100)
  })
  
  # Box: Map & Pie
  output$box_map <- renderUI({
    ns <- session$ns
    
    box(title="Customer Locations", status="primary", width=8,
        leafletOutput(ns("map")))
  })
  
  output$box_pie <- renderUI({
    ns <- session$ns
    
    box(title="Customer Orders", status="primary", width=4,
        plotlyOutput(ns("pie")))
  })
  

# Data Processing ---------------------------------------------------------
  
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
  

# Pie & Histo -------------------------------------------------------------

  histo_data <- reactive({
    
    bounds <- input$map_bounds
    
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
  
    temp_1 <- shpm %>%
      filter(Kart %in% filter$Kart, Kgruppe %in% filter$Kgrp, Ktyp %in% filter$Ktyp,
             customer_Latitude >= latRng[1], customer_Latitude <= latRng[2],
             customer_Longitude >= lngRng[1], customer_Longitude <= lngRng[2])
  })
  
  
  output$pie <- renderPlotly({
    
    temp <- histo_data() %>%
      group_by(Kart) %>%
      summarise(tons = sum(GWkg)/1000) %>%
      ungroup()
    
    plot_ly() %>%
      add_pie(data=temp, labels= ~Kart, values= ~tons, name='Order shares', rotation=90, textinfo='percent',
              hoverinfo='text', text= ~paste0("Sendungsvolument [to]", format(round(tons, digits=0), big.mark='.', decimal.mark=','))) %>%
      layout(title='', showlegend=TRUE)
  })
  
  
  output$histo <- renderPlotly({
    
    temp <- histo_data() %>%
      mutate(week = week(ymd(del_date))) %>%
      group_by(week) %>%
      summarise(tons = sum(GWkg)/1000) %>%
      ungroup()
    
    fit <- lm(data = temp, tons ~ week)
    
    plot_ly(x = ~week) %>%
      add_trace(data=temp, type='bar', y=~tons, name='Order Volume',
                hoverinfo='text', text= ~format(round(tons, digits=0), big.mark='.', decimal.mark=',')) %>%
      add_lines(y = fitted(fit), name = 'Regression Line') %>%
      layout(title='', showlegend=TRUE)
  })
}