about_UI <- function(id) {
  
  fluidRow(
    column(6,
           
           h3(strong("About")),
           br(),
           h4(strong("Usage")),
           br(),
           h5(p("With this tool you can analyse the ARR-movement of your company."),
              p("Under the menu item", strong("Geographical analysis"), "you can view the inventory of contracts (Summe ARR), Avarage ARR and number of customers, and select desired filters to view the performance in a specific country."),
              p(strong("All time Analysis"), "shows the movement of your inventory over the whole period of time when data is available."),
              p("Under", strong("Employee performance"), "you can monitor the ARR-contribution of your employees."),
              p(strong("ARR-Bridge"), "illustrates the new, expanded, contracted and churned customers measured in ARR."),
              p(strong("Commission calculation"), "displays two tables in two tabs, one for controlling and one for accounting. In both tables the commission amount that the employees earned is calculated, based on the new and expanded business for a month. Controlling employees can see more details for the calculation so they can check if everything is correct, and for the accounting employees the tables shows the amounts grouped by sales representatives which they can include in the payroll."),
              p(strong("Metrics"), "contains further metrics, like Combined Customer Acquisition Ratio or Avarage ARR per Customer."),
              p("With the help of two different types of", strong("Forecasts"), "you are able to predict your expected future ARR and react prior if need be."),
              ),
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
          h5(p("Non-recurring revenues, meaning one-time purchases of services/support, are excluded from ARR calculation.")
           ))
           
           
      
    )
   
  
}

about <- function(input, output, session) {

}