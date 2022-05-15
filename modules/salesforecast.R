# UI ----------------------------------------------------------------------

salesforecast_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fluidRow(column(12,
                        
        ) 
        )               
        , width=3),
      mainPanel(
        titlePanel("Quarterly Sales Forecast"),
        DT::dataTableOutput(ns("salesforecast"))
        
      )
    )
  )
}

## Server ------------------------------------------------------------------

salesforecast <- function(input, output, session) {
  
  opportunities <- saas_opp %>% select(SalesStage, SalesRep, CustomerIndustry, SalesChannel, SalesTyp, ProductType, Change, Payment, ARR)
  opportunities <- opportunities %>% 
    group_by(SalesStage) %>%
    summarise(TotalARR = sum(ARR)) %>%
    ungroup()
  
  #probablity <- data.table("Probability" = c("Pipeline", "Upside", "Most Likely", "Commit", "Order Received", "Closed Won"), "Rate" = c(0.30, 0.50, 0.75, 0.90, 1.00, 1.00)) 
  
  opportunities$"Probability" <- ifelse(opportunities %>% select("SalesStage") == "Pipeline", 0.3,
                                        ifelse(opportunities %>% select("SalesStage") == "Upside", 0.5,
                                               ifelse(opportunities %>% select("SalesStage") == "Most Likely", 0.75,
                                                      ifelse(opportunities %>% select("SalesStage") == "Commit", 0.9,
                                                             ifelse(opportunities %>% select("SalesStage") == "Order Received", 1.0,
                                                                    ifelse(opportunities %>% select("SalesStage") == "Closed Won", 1.0, 0))))))
  
  opportunities$"Weighted Total" <- (opportunities[, 2] * opportunities[, 3])
  
  # WeightedTotal <- as.double(colSums(opportunities[ , 4], na.rm=TRUE))
  # 
  # Total <- data.table("SalesStage" = "Total", "TotalARR" = "", "Probability" = "", "Weighted Total" = WeightedTotal)
  # 
  # opportunities <- rbindlist(list(opportunities, as.list(Total)), use.names=FALSE)
  
  opportunities <- opportunities %>% 
    arrange_(.dots="Probability")
  
  # Create plot -------------------------------------------------------------
  
  
  
  output$salesforecast = DT::renderDataTable({
    tmpF <- datatable(opportunities, rownames = FALSE, options = list(paging = FALSE, searching = FALSE)) %>%
      formatCurrency(c( "TotalARR","Weighted Total"), '\U20AC', before = FALSE, interval = 3, mark = ".", dec.mark = ",", digits = 2,) %>%
      formatPercentage("Probability", digits = 0, interval = 3, mark = ",")
    
    return(tmpF)
    
  })
  
  
  
  
}
