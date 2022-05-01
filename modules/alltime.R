alltime_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      rpivotTableOutput(ns("pivot_saas"))   
    )
  )
  
}

alltime_server <- function(input, output, session) {
  
  
  output$pivot_saas <-renderRpivotTable({
    rpivotTable(saas_data, 
                rows = "CustomerIndustry",
                cols = "Period",
                aggregatorName = "Sum",
                vals = "ARR",
                rendererName = "Stacked Bar Chart")
  })
  
}