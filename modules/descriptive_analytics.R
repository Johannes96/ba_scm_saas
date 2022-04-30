
descriptive_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(

    sidebarLayout(
      sidebarPanel(
        titlePanel("Filters"),
        #shinythemes::themeSelector(),
        fluidRow(column(12,
                        
                        # Select Period
                        selectInput(inputId = ns("PeriodFinder"),
                                    label = "Period",
                                    choices = levels(Periods),
                                    selected = "2022-03-01",
                        
                        ),
                        
                        # Select which Customer-industry(s) to plot
                        selectizeInput(inputId = ns("IndustryFinder"),
                                      label = "Customer-Industry(s):",
                                      choices = levels(Industries),
                                      multiple=TRUE,
                                      selected=character(0)
                                      ))),
          fluidRow(column(5,
                          # Select which Sales Channel and type to plot
                          selectInput(inputId = ns("SalesChannelFinder"),
                                      label = "Sales Channel",
                                      choices=NULL, 
                                      multiple=TRUE)
                 ),
                 column(5, ofset = 3,
                        selectInput(inputId = ns("SalesTypFinder"),
                                  label = "Sales-type",
                                  choices=NULL, 
                                  multiple=TRUE)
                 )),
        fluidRow(column(12,
                        # Select which Region(s) to plot
                        checkboxGroupInput(inputId = ns("ProductTypeFinder"),
                                           label = "Product-type:",
                                           choices = unique(saas_data$ProductType),
                                           selected = c("Cloud" = "Cloud", "On Premises", "Hybrid")),
                        # Set Billing Intervall
                        sliderInput(inputId = ns("BillingIntervall"),
                                    label = "Billing intervall",
                                    min = min(saas_data$BillingInterval),
                                    max = max(saas_data$BillingInterval),
                                    value = c(min(saas_data$BillingInterval), max(saas_data$BillingInterval)))
                  ))
               
        ),
      mainPanel(
        plotOutput(ns("plt_test")),
        textOutput(ns("debug_text"))
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
  
output$plt_test <- renderPlot({
  
  df_plt_temp <- saas_data %>%
    filter(Industries %in% c(input$IndustryFinder))
  
  p <- ggplot(df_plt_temp, aes(ARR)) +
    geom_histogram()
  
  print(p)
})

output$debug_text <- renderText({
  
  paste("input customer industry: ", input$IndustryFinder)
  
})
  
  
# Generate UI -------------------------------------------------------------

  
  # Input Boxes
  updateSelectizeInput(session, "SalesChannelFinder", label="Sales Channel", choices = unique(saas_data$SalesChannel), server=TRUE)
  updateSelectizeInput(session, "SalesTypFinder", label="Sales type", choices = unique(saas_data$SalesTyp), server=TRUE)
  
}