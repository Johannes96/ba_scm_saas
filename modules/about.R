about_UI <- function(id) {
  
  fluidRow(
    column(12,
           
           h3(strong("About")),
           br(),
           h4(strong("Usage")),
           br(),
           h5(p("With this tool you can analyse the ARR-movement of your company."),
              p("Under the menu item", strong("Monthly Analysis"), "you can view the inventory of contracts for a specific month, and select desired filters to view the performance e.g. a specific customer-industry, a sales channel or a product-type."),
              p(strong("Yearly Analysis"), "shows the movement of your inventory over the whole period of time when data is available."),
              p("With the help of", strong("Forecast"), "you are able to predict your expected future ARR and react prior if need be."),
              p("Under", strong("Employee success"), "you can monitor the performance of your employees.")),
           br(),
           h4(strong("ARR Definition")),
           br(),
           h5(".")
           
           
      
    )
  ) 
  
}