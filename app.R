source('global.R')

ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage(
    theme = "flatly",
    "SaaS Data Analysis",
    tabPanel("Monthly analysis", descriptive_analytics_UI("descriptive_analytics"), icon = icon("calendar", "f073")),
    tabPanel("All time analysis", yearly_analytics_UI("yearly_analytics"), icon = icon("chart-bar")),
    tabPanel("Forecast", predictive_analytics_UI("predictive_analytics"), icon = icon("chart-line")),
    tabPanel("Employee performance", customer_analytics_UI("customer_analytics"), icon = icon("users")),
    navbarMenu("More", icon = icon("cogs"),
               tabPanel("About", about_UI("about"), icon = icon("info")),
               tabPanel("panel 4b", "four-b"),
               tabPanel("panel 4c", "four-c")
    )
  )
)

server <- function(input, output) {

    callModule(descriptive_analytics, "descriptive_analytics")
    callModule(yearly_analytics, "yearly_analytics")
    callModule(predictive_analytics, "predictive_analytics")
    callModule(customer_analytics_server, "customer_analytics")
    callModule(about, "about")
}

shinyApp(ui = ui, server = server)
