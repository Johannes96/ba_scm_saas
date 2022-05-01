customer_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(

    fluidRow(
      box(title="Pivot Table", status = "primary", width=12, collapsible = TRUE, collapsed = FALSE,
          rpivotTableOutput(ns("pivot_saas")))    
  )
)

}

customer_analytics_server <- function(input, output, session) {
  
  
  output$pivot_saas <-renderRpivotTable({
    rpivotTable(saas_data, 
                rows = c("SalesChannel", "SalesTyp", "SalesRep"),
                cols = "Period",
                aggregatorName = "Sum",
                vals = "ARR")
  })

}