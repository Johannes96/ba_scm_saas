
# UI ----------------------------------------------------------------------

commission_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        titlePanel("Filter"),
        downloadButton(ns('downloadData'),"Download Table"),
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
        DT::dataTableOutput(ns("commission"))
          
      )
    )
  )
}

## Server ------------------------------------------------------------------

commission <- function(input, output, session) {
  
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
  
  saas_data_temp <- saas_data %>% select(CustomerID, SalesRep, Period, ARR)
  saas_data_temp <- saas_data_temp %>% 
    group_by(CustomerID, SalesRep, Period) %>%
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
    
    
    
    alt <- data.frame(alt = matrix(unlist(alt), nrow=522, byrow=TRUE),stringsAsFactors=FALSE)
    neu <- data.frame(neu = matrix(unlist(neu), nrow=522, byrow=TRUE),stringsAsFactors=FALSE)

    
    bridge$"Dif" <- (neu - alt)
    
    
    #Create categories
    
    bridge$"Change" <- ifelse((alt > neu & neu != 0), "Contraction",
                              ifelse((alt < neu & alt != 0), "Expansion",
                                     ifelse((alt ==  0 & bridge$"Dif" != 0 & neu != 0), "New",
                                            ifelse((neu ==  0 & bridge$"Dif" != 0 & alt != 0), "Churn", "No change"))))

    
    # Create commission calculation table -----------------------------------------------------------
    
    commission <- bridge %>% 
      filter(Change ==c("New", "Expansion")) %>% 
      select("SalesRep", "CustomerID", "Commission base"="Dif", "Change")
    
    
   # CommissionRate <- data.table("Type" = c("New", "Expansion", "Else"), "Rate" = c(0.06, 0.12, 0.00))
    
    commission$"Commission Rate" <- ifelse(commission %>% select("Change") == "Expansion", 0.06,
                                           ifelse(commission %>% select("Change") == "New", 0.12, 0))
    
    commission$"Commission" <- (commission[, 3] * commission[, 5])
    
    commission <- commission %>% 
      arrange_(.dots="SalesRep")
    
    #Print Table
    
    commission
    
  })
  
  
  # Create plot -------------------------------------------------------------
  
  
  
  output$commission = DT::renderDataTable({
    tmp <- datatable(dat(), rownames = FALSE, options = list(paging = FALSE, searching = FALSE)) %>%
      formatCurrency(c("Commission base","Commission"), '\U20AC', before = FALSE, interval = 3, mark = ".", dec.mark = ",", digits = 2,) %>%
      formatPercentage("Commission Rate", digits = 0, interval = 3, mark = ",")
    
        return(tmp)
    
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$Month, "_commission_calculation.csv", sep = "")
    },
    content = function(file) {
      write.table(dat(), row.names = FALSE, dec = ",", sep = ";",  quote = FALSE, file)
    }
  )

}


