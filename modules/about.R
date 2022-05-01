about_UI <- function(id) {
  
  fluidRow(
    column(6,
           
           h3(strong("About")),
           br(),
           h4(strong("Usage")),
           br(),
           h5(p("With this tool you can analyse the ARR-movement of your company."),
              p("Under the menu item", strong("Monthly Analysis"), "you can view the inventory of contracts for a specific month,"), 
              p("and select desired filters to view the performance e.g. a specific customer-industry, a sales channel or a product-type."),
              p(strong("Yearly Analysis"), "shows the movement of your inventory over the whole period of time when data is available."),
              p("With the help of", strong("Forecast"), "you are able to predict your expected future ARR and react prior if need be."),
              p("Under", strong("Employee success"), "you can monitor the performance of your employees.")),
           br()),
    column(6,
           br(),
           br(),
           br(),
           h4(strong("ARR Definition")),
           br(),
           h5(p("In Software-as-a-Service companies the Annual Recurring Revenue, short ARR, is the central controlling metric."),
              p("It shows the money that is earned every year for the life of a subscription or contract."),
              p("To be exact, ARR is the value of the recurring revenue of subscriptions normalized for a single calendar year."),
              p("Because ARR is a recurring amount it allows measurement of company development and business forecasts."),
              p("It also enables to measure momentum in areas such as new sales, expensions, and price increases, as well as lost off momentum in contractions and customer churn."),
              p("Source:", a("Zuora: Annual Recurring Revenue: What is ARR & How to Calculate It", href = "https://www.zuora.com/billing-topics/annual-recurring-revenue/"), ".")),
          p(style="color:black;text-align:justify"),
          withMathJax(),
          p('$$ARR = (Total ~contract ~value / ~billing ~intervall) * 12$$',style="color:black;border:1px solid black;background-color:white"),
          br(),
          p("Non-recurring revenues, meaning one-time purchases of services/support, are excluded from ARR calculation."
           )
           
           
      
    )
   
  
}

about <- function(input, output, session) {

}