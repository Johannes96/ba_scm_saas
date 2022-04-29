
yearly_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    sidebarLayout(
      sidebarPanel(
        
        titlePanel("Filters"),
        #shinythemes::themeSelector(),
        fluidRow(column(12,
                        
                        # Select which Customer-industry(s) to plot
                        selectizeInput(inputId = "IndustryFinder",
                                       label = "Select Customer-Industry(s):",
                                       choices = levels(Industries),
                                       multiple=TRUE,
                                       selected=character(0)
                        ))),
        fluidRow(column(5,
                        # Select which Sales Channel and type to plot
                        selectInput(inputId = ns("SalesChannelFinder"),
                                    label = "Select Sales Channel",
                                    choices=NULL, 
                                    multiple=TRUE)
        ),
        column(5, ofset = 3,
               selectInput(inputId = ns("SalesTypFinder"),
                           label = "Select Sales-typ",
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
    filter$SalesChannel <- if(is.null(selected$SalesChannel)) unique(saas_data$SalesChannel) else selected$SalesChannel
  })
  
  observeEvent(eventExpr = input$SalesTypFinder, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$SalesTyp <- input$SalesTypFinder
    filter$SalesTyp <- if(is.null(selected$SalesTyp)) unique(saas_data$SalesTyp) else selected$SalesTyp
  })
  
  
  
  
  
  
  
  # Generate UI -------------------------------------------------------------
  
  
  # Input Boxes
  updateSelectizeInput(session, "SalesChannelFinder", label="Select Sales Channel", choices = unique(saas_data$SalesChannel), server=TRUE)
  updateSelectizeInput(session, "SalesTypFinder", label="Select Sales-typ", choices = unique(saas_data$SalesTyp), server=TRUE)
  
  
}