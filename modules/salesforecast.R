# UI ----------------------------------------------------------------------

salesforecast_UI <- function(id) {
  ns <- NS(id)
  tagList(
        titlePanel("Quarterly Sales Forecast"),
        DT::dataTableOutput(ns("salesforecast")),
        verbatimTextOutput(ns("txtout")),
        plotlyOutput(ns("salesfunnel"))
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
  
  opportunities$"Weighted Total" <- as.numeric(unlist((opportunities[, 2] * opportunities[, 3])))
  
  WeightedTotal <- as.double(sum(opportunities[which(opportunities$Probability >= 0.75), 4]))
  
  
  opportunities <- opportunities %>% 
    arrange_(.dots="Probability")
  
  
  
  # Create plot -------------------------------------------------------------
  
  
  output$salesforecast = DT::renderDataTable({
    tmpF <- datatable(opportunities, rownames = FALSE, options = list(paging = FALSE, searching = FALSE, info = FALSE)) %>%
      formatCurrency(c( "TotalARR","Weighted Total"), '\U20AC', before = FALSE, interval = 3, mark = ".", dec.mark = ",", digits = 2,) %>%
      formatPercentage("Probability", digits = 0, interval = 3, mark = ",")
    
    return(tmpF)
    
  })
  
  output$txtout <- renderText({
    paste("Weighted [> 75%] Forecast:" , WeightedTotal, "EURO", sep = " "), align = 'right'
  })
  
  output$salesfunnel <- renderPlotly({
    
  fig <- plot_ly(
    type = "funnelarea",
    text = c("Pipeline", "Upside", "Most Likely", "Commit", "Order Received", "Closed Won"),
    values = opportunities$"TotalARR",
    title = list(position = "top center", size=5, text = "Sum of ARR (EUR)"),
    marker = list(colors = c("deepskyblue", "lightsalmon", "tan", "lightgreen", "silver", "floralwhite")),
    showlegend = TRUE
    )
  
  fig
  }
  )
  
  
}
