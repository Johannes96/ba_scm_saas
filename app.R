source('global.R')

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage(
    theme = "flatly",
    "SaaS Data Analysis",
    tabPanel("Monthly analysis", descriptive_analytics_UI("descriptive_analytics"), icon = icon("calendar", "f073")),
    tabPanel("Yearly analysis", yearly_analytics_UI("yearly_analytics"), icon = icon("calendar")),
    tabPanel("Forecast", predictive_analytics_UI("predictive_analytics"), icon = icon("chart-line")),
    tabPanel("table", customer_analytics_UI("customer_analytics"), icon = icon("table")),
    navbarMenu("", icon = icon("table"),
               tabPanel("panel 4a", "four-a"),
               tabPanel("panel 4b", "four-b"),
               tabPanel("panel 4c", "four-c")
    )
  )
)

server <- function(input, output) {

    callModule(descriptive_analytics, "descriptive_analytics")
    callModule(descriptive_analytics, "yearly_analytics")
    callModule(predictive_analytics, "predictive_analytics")
    callModule(customer_analytics_server, "customer_analytics")
}

shinyApp(ui = ui, server = server)

# old version
# ui <- dashboardPage(skin="blue",
#                     dashboardHeader(title = "Software as a Service"),
#                     dashboardSidebar(
#                       sidebarMenu(
#                         menuItem("Descriptive Analytics", tabName="descriptive_analytics", icon=icon("dashboard")),
#                         menuItem("Predictive Analytics", tabName="predictive_analytics", icon=icon("chart-line")),
#                         menuItem("Data Tables", tabName="customer_analytics", icon=icon("table"))
#                       )
#                     ),
#                     dashboardBody(
#                       tabItems(
#                         tabItem(tabName="descriptive_analytics", descriptive_analytics_UI("descriptive_analytics")),
#                         tabItem(tabName="predictive_analytics", predictive_analytics_UI("predictive_analytics")),
#                         tabItem(tabName="customer_analytics", customer_analytics_UI("customer_analytics"))
#                       )
#                     )
# )