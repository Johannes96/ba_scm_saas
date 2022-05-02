
# UI ----------------------------------------------------------------------

bridge_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        titlePanel("Filter"),
        fluidRow(column(12,
                        # Select Month
                        selectInput(inputId = ns("Month"),
                                    label = "Period",
                                    choices = unique(saas_data$Period),
                                    selected = "2020-10-01")
                ) 
              )               
          , width=2),
          mainPanel(
            plotOutput(ns("ARRBridge"))
      )
    )
  )
}

## Server ------------------------------------------------------------------

bridge <- function(input, output, session) {
  
  selected <- reactiveValues()
  filter <- reactiveValues(Month = unique(saas_data$Period))
                           
                             
  observeEvent(eventExpr = input$Month, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Month <- input$Month
    filter$Month <- if(is.null(selected$Month)) unique(saas_data$Period) else selected$Period
  })
  
  # Input Boxes
  updateSelectInput(session, "Month", label="Period", choices = unique(saas_data$Period), selected = "2020-10-01")
  

# Prepare dataframe -------------------------------------------------------

saas_data_temp <- saas_data %>% select(CustomerID, Period, CustomerIndustry, SalesChannel, SalesTyp, ARR)
saas_data_temp <- saas_data_temp %>% 
                  group_by(CustomerID, CustomerIndustry, SalesChannel, SalesTyp, Period) %>%
                  summarise(TotalARR = sum(ARR)) %>%
                  ungroup()


# Prepare Bridge ----------------------------------------------------------

bridge <- pivot_wider(saas_data_temp, names_from = Period, values_from = TotalARR) 

bridge[is.na(bridge)] <- 0.00
TotalARR <- format(round(saas_data_temp$TotalARR, 2), nsmall = 2)

#Calculate differences

period_data <- reactive({
  filter(Month %in% filter$Month)
})


# alt <-observe({ifelse(input$Month == "2020-10-01", bridge$"2020-09-01",
#              ifelse(input$Month == "2020-11-01", bridge$"2020-10-01",
#                     ifelse(input$Month == "2020-12-01", bridge$"2020-11-01",
#                            ifelse(input$Month == "2021-01-01", bridge$"2020-12-01",
#                                   ifelse(input$Month == "2021-02-01", bridge$"2021-01-01",
#                                          ifelse(input$Month == "2021-03-01", bridge$"2021-02-01",
#                                                 ifelse(input$Month == "2021-04-01", bridge$"2021-03-01",
#                                                        ifelse(input$Month == "2021-05-01", bridge$"2021-04-01",
#                                                               ifelse(input$Month == "2021-06-01", bridge$"2021-05-01",
#                                                                      ifelse(input$Month == "2021-07-01", bridge$"2021-06-01",
#                                                                             ifelse(input$Month == "2021-08-01", bridge$"2021-07-01",
#                                                                                    ifelse(input$Month == "2021-09-01", bridge$"2021-08-01",
#                                                                                           ifelse(input$Month == "2021-10-01", bridge$"2021-09-01",
#                                                                                                  ifelse(input$Month == "2021-11-01", bridge$"2021-10-01",
#                                                                                                         ifelse(input$Month == "2021-12-01", bridge$"2021-11-01",
#                                                                                                                ifelse(input$Month == "2022-01-01", bridge$"2021-12-01",
#                                                                                                                       ifelse(input$Month == "2022-02-01", bridge$"2022-01-01",
#                                                                                                                              ifelse(input$Month == "2021-03-01", bridge$"2021-02-01",bridge$"2020-09-01"
#                                                                                                                                     
#              
#       ))))))))))))))))))})
# 
# 
# neu <-observe({ifelse(input$Month == "2020-10-01", bridge$"2020-10-01",
#              ifelse(input$Month == "2020-11-01", bridge$"2020-11-01",
#                     ifelse(input$Month == "2020-12-01", bridge$"2020-12-01",
#                            ifelse(input$Month == "2021-01-01", bridge$"2021-01-01",
#                                   ifelse(input$Month == "2021-02-01", bridge$"2021-02-01",
#                                          ifelse(input$Month == "2021-03-01", bridge$"2021-03-01",
#                                                 ifelse(input$Month == "2021-04-01", bridge$"2021-04-01",
#                                                        ifelse(input$Month == "2021-05-01", bridge$"2021-05-01",
#                                                               ifelse(input$Month == "2021-06-01", bridge$"2021-06-01",
#                                                                      ifelse(input$Month == "2021-07-01", bridge$"2021-07-01",
#                                                                             ifelse(input$Month == "2021-08-01", bridge$"2021-08-01",
#                                                                                    ifelse(input$Month == "2021-09-01", bridge$"2021-09-01",
#                                                                                           ifelse(input$Month == "2021-10-01", bridge$"2021-10-01",
#                                                                                                  ifelse(input$Month == "2021-11-01", bridge$"2021-11-01",
#                                                                                                         ifelse(input$Month == "2021-12-01", bridge$"2021-12-01",
#                                                                                                                ifelse(input$Month == "2022-01-01", bridge$"2022-01-01",
#                                                                                                                       ifelse(input$Month == "2022-02-01", bridge$"2022-02-01",
#                                                                                                                              ifelse(input$Month == "2021-03-01", bridge$"2021-03-01", bridge$"2020-10-01"
#                                                                                                                                     
#                                                                                                                                     
#       ))))))))))))))))))})

alt <- bridge$"2022-02-01"
neu <- bridge$"2022-03-01"


bridge$"Dif" <- (neu - alt)
transform(bridge, Dif = as.numeric(Dif))
     

#Create categories

bridge$"Change" <- ifelse((alt > neu & neu != 0), "Contraction",
                                ifelse((alt < neu & alt != 0), "Expansion",
                                       ifelse((alt ==  0 & bridge$"Dif" != 0 & neu != 0), "New",
                                              ifelse((neu ==  0 & bridge$"Dif" != 0 & alt != 0), "Churn", "No change"))))
#Sum ARR Last Month
LM <- data.table(Change="Last Month",PeriodARR=sum(alt))


# Create bridge -----------------------------------------------------------

bridgetable <- bridge %>% 
                  group_by(Change) %>%
                  summarise(ARRGrouped = sum(Dif)) %>%
                  ungroup() 

bridgetable <- rbindlist(list(as.list(LM), bridgetable), use.names=FALSE)


# Create plot -------------------------------------------------------------

output$ARRBridge <- renderPlot({
  
  waterfall(bridgetable, calc_total = TRUE, total_axis_text="Actual", draw_lines = FALSE, fill_by_sign = FALSE, fill_colours = 3:8, rect_border = NA)
   
})



}