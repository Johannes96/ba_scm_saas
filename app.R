source('global.R')

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # add css file to customize appearance
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
           
    navbarPage(
    theme = "cosmo",
    "Dive Insight",
    
    tabPanel("Welcome", welcome_UI("welcome"), icon = icon("font-awesome")),
    
    navbarMenu("Descriptive analytics", icon = icon("deezer"),
                tabPanel("Geographical analysis", descriptive_analytics_UI("descriptive_analytics"), icon = icon("globe")),
                tabPanel("All time analysis", alltime_UI("alltime"), icon = icon("chart-bar")),
                tabPanel("Employee performance", customer_analytics_UI("customer_analytics"), icon = icon("users")),
                tabPanel("ARR-Bridge", bridge_UI("bridge"), icon = icon("bold")),
                tabPanel("Commission calculation", commission_UI("commission"), icon = icon("euro-sign")),
                tabPanel("Metrics", metrics_UI("metrics"), icon = icon("calculator"))),
    
    tabPanel("Forecast", predictive_analytics_UI("predictive_analytics"), icon = icon("chart-line")),
    
    navbarMenu("Machine Learning", icon = icon("robot"),
               tabPanel("Clustering", icon = icon("hubspot"), clustering_UI("clustering")),
               tabPanel("Classification", icon = icon("sitemap"))
               ),
    
    navbarMenu("More", icon = icon("cogs"),
               tabPanel("About", about_UI("about"), icon = icon("info")),
               tabPanel("panel 4b", "four-b"),
               tabPanel("panel 4c", "four-c"))
  )
)

server <- function(input, output) {

    callModule(welcome, "welcome")
    callModule(descriptive_analytics, "descriptive_analytics")
    callModule(alltime_server, "alltime")
    callModule(predictive_analytics, "predictive_analytics")
    callModule(customer_analytics_server, "customer_analytics")
    callModule(bridge, "bridge")
    callModule(commission, "commission")
    callModule(metrics, "metrics")
    callModule(about, "about")
}

shinyApp(ui = ui, server = server)
