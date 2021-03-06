industry_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      rpivotTableOutput(ns("pivot_saas"))   
    )
  )
  
}

industry_analytics_server <- function(input, output, session) {
  
  
  output$pivot_saas <-renderRpivotTable({
    rpivotTable(saas_data, 
                rows = "CustomerIndustry",
                cols = "Period",
                aggregatorName = "Sum",
                vals = "ARR",
                rendererName = "Stacked Bar Chart")
  })
  
}