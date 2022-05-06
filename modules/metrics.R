
# UI ----------------------------------------------------------------------

metrics_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        titlePanel("Filter"),
        fluidRow(column(12,
                        # Select Month
                        selectInput(inputId = ns("Month"),
                                    label = "Period",
                                    choices = c("2020-10", "2020-11", "2020-12", "2021-01", "2021-02", "2021-03", "2021-04", "2021-05", "2021-06", "2021-07", "2021-08", "2021-09", "2021-10", "2021-11", "2021-12", "2022-01", "2022-02", "2022-03"),
                                    selected = "2022-03")
        ) 
        )               
        , width=3),
      mainPanel(
        titlePanel("Metrics"),
        DT::dataTableOutput(ns("Metrics"))
      )
    )
  )
}

## Server ------------------------------------------------------------------

metrics <- function(input, output, session) {
  
  selected <- reactiveValues()
  filter <- reactiveValues(Month = unique(saas_data$Period))
  
  
  observeEvent(eventExpr = input$Month, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Month <- input$Month
    filter$Month <- if(is.null(selected$Month)) unique(saas_data$Period) else selected$Month
  })
  
  # Input Boxes
  updateSelectInput(session, "Month", 
                    label="Period", 
                    choices = c("2020-10", "2020-11", "2020-12", "2021-01", "2021-02", "2021-03", "2021-04", "2021-05", "2021-06", "2021-07", "2021-08", "2021-09", "2021-10", "2021-11", "2021-12", "2022-01", "2022-02", "2022-03"), 
                    selected = "2022-03")
  
  
  # Prepare dataframe -------------------------------------------------------
  
  saas_data_temp <- saas_data %>% select(CustomerID, Period, CustomerIndustry, SalesChannel, SalesTyp, ARR)
  saas_data_temp <- saas_data_temp %>% 
    group_by(CustomerID, CustomerIndustry, SalesChannel, SalesTyp, Period) %>%
    summarise(TotalARR = sum(ARR)) %>%
    ungroup()
  
  
  # Preparation ----------------------------------------------------------
  
  bridge <- pivot_wider(saas_data_temp, names_from = Period, values_from = TotalARR) 
  
  bridge[is.na(bridge)] <- 0.00
  TotalARR <- format(round(saas_data_temp$TotalARR, 2), nsmall = 2)
  
  #Calculate differences
  

  dat <- reactive({
    UserInput <-
      ifelse(input$Month == "2020-10", "2020-10-01",
             ifelse(input$Month == "2020-11", "2020-11-01",
                    ifelse(input$Month == "2020-12", "2020-12-01",
                           ifelse(input$Month == "2021-01", "2021-01-01",
                                  ifelse(input$Month == "2021-02", "2021-02-01",
                                         ifelse(input$Month == "2021-03", "2021-03-01",
                                                ifelse(input$Month == "2021-04", "2021-04-01",
                                                       ifelse(input$Month == "2021-05", "2021-05-01",
                                                              ifelse(input$Month == "2021-06", "2021-06-01",
                                                                     ifelse(input$Month == "2021-07", "2021-07-01",
                                                                            ifelse(input$Month == "2021-08", "2021-08-01",
                                                                                   ifelse(input$Month == "2021-09", "2021-09-01",
                                                                                          ifelse(input$Month == "2021-10", "2021-10-01",
                                                                                                 ifelse(input$Month == "2021-11", "2021-11-01",
                                                                                                        ifelse(input$Month == "2021-12", "2021-12-01",
                                                                                                               ifelse(input$Month == "2022-01", "2022-01-01",
                                                                                                                      ifelse(input$Month == "2022-02", "2022-02-01",
                                                                                                                             ifelse(input$Month == "2022-03", "2022-03-01", "2020-10-01"
                                                                                                                                    
                                                                                                                                    
                                                                                                                             ))))))))))))))))))
    
    
    
    alt <-
      ifelse(UserInput == "2020-10-01", bridge %>% select("2020-09-01"),
             ifelse(UserInput == "2020-11-01", bridge %>% select("2020-10-01"),
                    ifelse(UserInput == "2020-12-01", bridge %>% select("2020-11-01"),
                           ifelse(UserInput == "2021-01-01", bridge %>% select("2020-12-01"),
                                  ifelse(UserInput == "2021-02-01", bridge %>% select("2021-01-01"),
                                         ifelse(UserInput == "2021-03-01", bridge %>% select("2021-02-01"),
                                                ifelse(UserInput == "2021-04-01", bridge %>% select("2021-03-01"),
                                                       ifelse(UserInput == "2021-05-01", bridge %>% select("2021-04-01"),
                                                              ifelse(UserInput == "2021-06-01", bridge %>% select("2021-05-01"),
                                                                     ifelse(UserInput == "2021-07-01", bridge %>% select("2021-06-01"),
                                                                            ifelse(UserInput == "2021-08-01", bridge %>% select("2021-07-01"),
                                                                                   ifelse(UserInput == "2021-09-01", bridge %>% select("2021-08-01"),
                                                                                          ifelse(UserInput == "2021-10-01", bridge %>% select("2021-09-01"),
                                                                                                 ifelse(UserInput == "2021-11-01", bridge %>% select("2021-10-01"),
                                                                                                        ifelse(UserInput == "2021-12-01", bridge %>% select("2021-11-01"),
                                                                                                               ifelse(UserInput == "2022-01-01", bridge %>% select("2021-12-01"),
                                                                                                                      ifelse(UserInput == "2022-02-01", bridge %>% select("2022-01-01"),
                                                                                                                             ifelse(UserInput == "2022-03-01", bridge %>% select("2022-02-01"), bridge %>% select("2020-09-01")
                                                                                                                                    
                                                                                                                                    
                                                                                                                             ))))))))))))))))))
    
    
    
    neu <-
      ifelse(UserInput == "2020-10-01", bridge %>% select("2020-10-01"),
             ifelse(UserInput == "2020-11-01", bridge %>% select("2020-11-01"),
                    ifelse(UserInput == "2020-12-01", bridge %>% select("2020-12-01"),
                           ifelse(UserInput == "2021-01-01", bridge %>% select("2021-01-01"),
                                  ifelse(UserInput == "2021-02-01", bridge %>% select("2021-02-01"),
                                         ifelse(UserInput == "2021-03-01", bridge %>% select("2021-03-01"),
                                                ifelse(UserInput == "2021-04-01", bridge %>% select("2021-04-01"),
                                                       ifelse(UserInput == "2021-05-01", bridge %>% select("2021-05-01"),
                                                              ifelse(UserInput == "2021-06-01", bridge %>% select("2021-06-01"),
                                                                     ifelse(UserInput == "2021-07-01", bridge %>% select("2021-07-01"),
                                                                            ifelse(UserInput == "2021-08-01", bridge %>% select("2021-08-01"),
                                                                                   ifelse(UserInput == "2021-09-01", bridge %>% select("2021-09-01"),
                                                                                          ifelse(UserInput == "2021-10-01", bridge %>% select("2021-10-01"),
                                                                                                 ifelse(UserInput == "2021-11-01", bridge %>% select("2021-11-01"),
                                                                                                        ifelse(UserInput == "2021-12-01", bridge %>% select("2021-12-01"),
                                                                                                               ifelse(UserInput == "2022-01-01", bridge %>% select("2022-01-01"),
                                                                                                                      ifelse(UserInput == "2022-02-01", bridge %>% select("2022-02-01"),
                                                                                                                             ifelse(UserInput == "2022-03-01", bridge %>% select("2022-03-01"), bridge %>% select("2020-10-01")
                                                                                                                                    
                                                                                                                                    
                                                                                                                             ))))))))))))))))))
    
    
    
    alt <- data.frame(alt = matrix(unlist(alt), nrow=472, byrow=TRUE),stringsAsFactors=FALSE)
    neu <- data.frame(neu = matrix(unlist(neu), nrow=472, byrow=TRUE),stringsAsFactors=FALSE)
    
    
    
    bridge$"Dif" <- (neu - alt)
    
    
    #Create categories
    
    bridge$"Change" <- ifelse((alt > neu & neu != 0), "Contraction",
                              ifelse((alt < neu & alt != 0), "Expansion",
                                     ifelse((alt ==  0 & bridge$"Dif" != 0 & neu != 0), "New",
                                            ifelse((neu ==  0 & bridge$"Dif" != 0 & alt != 0), "Churn", "No change"))))
    #Sum ARR Last Month
    LM <- data.table(Change="Last Month",PeriodARR=sum(alt))
    
    Cost <- data.table("CostType" = c("Personal expenses Sales", "Referral commission", "Personal expenses Marketing", "Marketing cost"),
                       "Expense"=c(100000,20000,30000,70000))
    
    TotalCost <- with(Cost, sum(Expense[CostType == "Personal expenses Sales" | CostType == "Referral commission" | CostType == "Personal expenses Marketing" | CostType == "Marketing cost"]))
    TotalCost <- data.table(Change="Costs of ARR Acquisistion", TotalCost = TotalCost)
    
    Cost <- rbindlist(list(as.list(Cost),as.list(TotalCost)), use.names=FALSE)
    
    # Create metrics table -----------------------------------------------------------
    
    metrics <- bridge %>% 
      filter(Change == c("New", "Expansion")) %>% 
      group_by(Change) %>%
      summarise(ARRGrouped = sum(Dif)) %>%
      ungroup() 
    
    NewExpand <- with(metrics, sum(ARRGrouped[Change == "New" | Change == "Expansion"]))
    
    NewExpand <- data.table(KPI ="New & Expand ARR", Value = NewExpand)
    
    #metrics <- rbindlist(list(metrics, as.list(NewExpand), as.list(Cost)), use.names=FALSE)
    
    metrics <- rbindlist(list(as.list(NewExpand), as.list(TotalCost)), use.names=FALSE)
    
    #calculate Combined CAC Ratio
    
    CombinedCACRatio <- metrics[2, -1] / metrics[1, -1]
    
    CombinedCACRatio <- data.table(KPI = "Combined CAC Ratio", Value = CombinedCACRatio)
    
    metrics <- rbindlist(list(metrics, CombinedCACRatio), use.names=FALSE)
    
    metrics$Value <- format(round(metrics$Value, 4), nsmall = 4)
    
    metrics
    
  })
  
  
  # Create plot -------------------------------------------------------------
  

  
  output$Metrics = DT::renderDataTable({
    tmp <- dat()
    tmp} ,options = list(paging = FALSE, 
                         searching = FALSE
    ))
  
}
