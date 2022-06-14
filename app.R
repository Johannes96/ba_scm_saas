source('global.R')

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # add css file to customize appearance
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
           
    navbarPage(
    theme = "cosmo",
    "Dive Insight",
    
    tabPanel("Welcome", welcome_UI("welcome"), icon = icon("font-awesome")),
    
    navbarMenu("Descriptive analytics", icon = icon("deezer"),
                tabPanel("Geographical analysis", geographical_analytics_UI("geographical_analytics"), icon = icon("globe")),
                tabPanel("Industry analysis", industry_analytics_UI("industry_analytics"), icon = icon("chart-bar")),
                tabPanel("Employee performance", employee_analytics_UI("employee_analytics"), icon = icon("users")),
                tabPanel("ARR-Bridge", bridge_UI("bridge"), icon = icon("bold")),
                tabPanel("Commission calculation", icon = icon("euro-sign"),
                         titlePanel("Commission calculation"),
                         tabsetPanel(
                           tabPanel(title = "For Controlling", commission_UI("commission")),
                           tabPanel(title = "For Accounting", commission2_UI("commission2")))),
                tabPanel("Metrics", metrics_UI("metrics"), icon = icon("calculator"))),
    
    navbarMenu("Predictive analytics", icon = icon("chart-line"),
              tabPanel("Modell-based Forecast", predictive_analytics_UI("predictive_analytics"), icon = icon("signal")),
              tabPanel("Sales-Forecast", salesforecast_UI("salesforecast"), icon = icon("comment-dollar"))),
    
    navbarMenu("Machine Learning", icon = icon("robot"),
               tabPanel("Clustering", icon = icon("hubspot"), clustering_UI("clustering"))
               ),
    
    navbarMenu("More", icon = icon("cogs"),
               tabPanel("About", about_UI("about"), icon = icon("info")),
               tabPanel("Glossar", glossar_UI("glossar"), icon = icon("book")))
  )
)

server <- function(input, output) {

    callModule(welcome, "welcome")
    callModule(geographical_analytics, "geographical_analytics")
    callModule(industry_analytics_server, "industry_analytics")
    callModule(predictive_analytics, "predictive_analytics")
    callModule(salesforecast, "salesforecast")
    callModule(employee_analytics_server, "employee_analytics")
    callModule(bridge, "bridge")
    callModule(commission, "commission")
    callModule(commission2, "commission2")
    callModule(metrics, "metrics")
    callModule(about, "about")
    callModule(clustering_server, "clustering")
    callModule(glossar_server, "glossar")
}

shinyApp(ui = ui, server = server)
