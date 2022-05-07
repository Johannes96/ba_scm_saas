
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
                                    choices =  c("2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01", "2022-03-01"),
                                    selected = "2022-03-01")
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
                    choices = c("2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01", "2022-03-01"), 
                    selected = "2022-03-01")
  
  
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
      ifelse(input$Month == "2020-10-01", "2020-10-01",
             ifelse(input$Month == "2020-11-01", "2020-11-01",
                    ifelse(input$Month == "2020-12-01", "2020-12-01",
                           ifelse(input$Month == "2021-01-01", "2021-01-01",
                                  ifelse(input$Month == "2021-02-01", "2021-02-01",
                                         ifelse(input$Month == "2021-03-01", "2021-03-01",
                                                ifelse(input$Month == "2021-04-01", "2021-04-01",
                                                       ifelse(input$Month == "2021-05-01", "2021-05-01",
                                                              ifelse(input$Month == "2021-06-01", "2021-06-01",
                                                                     ifelse(input$Month == "2021-07-01", "2021-07-01",
                                                                            ifelse(input$Month == "2021-08-01", "2021-08-01",
                                                                                   ifelse(input$Month == "2021-09-01", "2021-09-01",
                                                                                          ifelse(input$Month == "2021-10-01", "2021-10-01",
                                                                                                 ifelse(input$Month == "2021-11-01", "2021-11-01",
                                                                                                        ifelse(input$Month == "2021-12-01", "2021-12-01",
                                                                                                               ifelse(input$Month == "2022-01-01", "2022-01-01",
                                                                                                                      ifelse(input$Month == "2022-02-01", "2022-02-01",
                                                                                                                             ifelse(input$Month == "2022-03-01", "2022-03-01", "2020-10-01"
                                                                                                                                    
                                                                                                                                    
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

    Cost <-
      ifelse(UserInput == "2020-10-01", saas_exp %>% select("01.10.2020"),
             ifelse(UserInput == "2020-11-01", saas_exp %>% select("01.11.2020"),
                    ifelse(UserInput == "2020-12-01", saas_exp %>% select("01.12.2020"),
                           ifelse(UserInput == "2021-01-01", saas_exp %>% select("01.01.2021"),
                                  ifelse(UserInput == "2021-02-01", saas_exp %>% select("01.02.2021"),
                                         ifelse(UserInput == "2021-03-01", saas_exp %>% select("01.03.2021"),
                                                ifelse(UserInput == "2021-04-01", saas_exp %>% select("01.04.2021"),
                                                       ifelse(UserInput == "2021-05-01", saas_exp %>% select("01.05.2021"),
                                                              ifelse(UserInput == "2021-06-01", saas_exp %>% select("01.06.2021"),
                                                                     ifelse(UserInput == "2021-07-01", saas_exp %>% select("01.07.2021"),
                                                                            ifelse(UserInput == "2021-08-01", saas_exp %>% select("01.08.2021"),
                                                                                   ifelse(UserInput == "2021-09-01", saas_exp %>% select("01.09.2021"),
                                                                                          ifelse(UserInput == "2021-10-01", saas_exp %>% select("01.10.2021"),
                                                                                                 ifelse(UserInput == "2021-11-01", saas_exp %>% select("01.11.2021"),
                                                                                                        ifelse(UserInput == "2021-12-01", saas_exp %>% select("01.12.2021"),
                                                                                                               ifelse(UserInput == "2022-01-01", saas_exp %>% select("01.01.2022"),
                                                                                                                      ifelse(UserInput == "2022-02-01", saas_exp %>% select("01.02.2022"),
                                                                                                                             ifelse(UserInput == "2022-03-01", saas_exp %>% select("01.03.2022"), saas_exp %>% select("01.10.2020")


                                                                                                                             ))))))))))))))))))
    
    
    alt <- data.frame(alt = matrix(unlist(alt), nrow=472, byrow=TRUE),stringsAsFactors=FALSE)
    neu <- data.frame(neu = matrix(unlist(neu), nrow=472, byrow=TRUE),stringsAsFactors=FALSE)
    Cost <- data.frame(neu = matrix(unlist(Cost), nrow=4, byrow=TRUE),stringsAsFactors=FALSE)
    
    
    
    bridge$"Dif" <- (neu - alt)
    
    
    #Create categories
    
    bridge$"Change" <- ifelse((alt > neu & neu != 0), "Contraction",
                              ifelse((alt < neu & alt != 0), "Expansion",
                                     ifelse((alt ==  0 & bridge$"Dif" != 0 & neu != 0), "New",
                                            ifelse((neu ==  0 & bridge$"Dif" != 0 & alt != 0), "Churn", "No change"))))
    #Sum ARR Last Month
    LM <- data.table(Change="Last Month",PeriodARR=sum(alt))
    
    #Cost <- data.table("CostType" = c("Personal expenses Sales", "Referral commission", "Personal expenses Marketing", "Marketing cost"),
     #                  "Expense"=c(100000,20000,30000,70000))
    
    Cost <- data.table("CostType" = c("Personal expenses Sales", "Referral commission", "Personal expenses Marketing", "Marketing cost"),
                        "Expense"=Cost)
    
    TotalCost <- with(Cost, sum(Expense.neu[CostType == "Personal expenses Sales" | CostType == "Referral commission" | CostType == "Personal expenses Marketing" | CostType == "Marketing cost"]))
    TotalCost <- data.table(Change="Costs of ARR Acquisistion", TotalCost = TotalCost)
    
    Cost <- rbindlist(list(as.list(Cost),as.list(TotalCost)), use.names=FALSE)
    
    # Create metrics table -----------------------------------------------------------
    
    metrics <- bridge %>% 
      filter(Change !="No change") %>% 
      group_by(Change) %>%
      summarise(ARRGrouped = sum(Dif)) %>%
      ungroup() 
    
    NewExpand <- with(metrics, sum(ARRGrouped[Change == "New" | Change == "Expansion"]))
    
    NewExpand <- data.table(KPI ="New & Expand ARR", Value = NewExpand)
    
    TM <- data.table(KPI="Total ARR [This Month]", Value=sum(neu))
    
    #metrics <- rbindlist(list(metrics, as.list(NewExpand), as.list(Cost)), use.names=FALSE)
    
    metrics <- rbindlist(list(as.list(TM), as.list(NewExpand), as.list(TotalCost)), use.names=FALSE)
    
    #calculate Combined CAC Ratio
    
    CombinedCACRatio <- metrics[3, -1] / metrics[2, -1]
    
    CombinedCACRatio <- data.table(KPI = "Combined CAC Ratio", Value = CombinedCACRatio)
    
    metrics <- rbindlist(list(metrics, CombinedCACRatio), use.names=FALSE)
    
    
    #Format values
    
    metrics$Value <- format(round(metrics$Value, 3), nsmall = 3)

    
    #Calculate Avg. ARR per Customer
    
    NrCustomers <- saas_data
    
    if (!is.null(input$Month)) {
      NrCustomers <- NrCustomers %>%
        dplyr::filter(Period %in% input$Month)}
     
    NrCustomers <- NrCustomers %>%
                      dplyr::summarise(n_customers = n_distinct(CustomerID))

      
    NrCustomers <- data.table(KPI = "Number of customers", Value = NrCustomers)
    
    AvARRpC <- TM[1, -1] / NrCustomers[1, -1]
    AvARRpC <- data.table(KPI = "Avg. ARR per Customer", Value = AvARRpC)
    
    metrics <- rbindlist(list(metrics, NrCustomers, AvARRpC), use.names=FALSE)
    
    
    #Print Table
    
    metrics
    
  })
  
  
  # Create plot -------------------------------------------------------------
  

  
  output$Metrics = DT::renderDataTable({
    tmp <- datatable(dat(), options = list(paging = FALSE, searching = FALSE)) %>%
      #formatStyle('KPI',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
      formatStyle(1, target="row", fontWeight = styleEqual(c("Avg. ARR per Customer", "Combined CAC Ratio", "Costs of ARR Acquisistion"),"bold"), backgroundColor = styleEqual(c("Avg. ARR per Customer", "Combined CAC Ratio", "Costs of ARR Acquisistion"),"palegreen"))
    return(tmp)

  })
  
}
