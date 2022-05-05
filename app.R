source('global.R')

ui <- fluidPage(theme = shinytheme("cosmo"),
                # setBackgroundImage(
                #   src = "https://wallpaperbat.com/img/312147-aesthetic-white-geometric-wallpaper-top-free-aesthetic-white.png"
                # ),            
    navbarPage(
    theme = "cosmo",
    "Dive Insight",
    
    tabPanel("Welcome", welcome_UI("welcome"), icon = icon("globe")),
    
    navbarMenu("Descriptive analytics", icon = icon("deezer"),
                tabPanel("Monthly analysis", descriptive_analytics_UI("descriptive_analytics"), icon = icon("calendar", "f073")),
                tabPanel("All time analysis", alltime_UI("alltime"), icon = icon("chart-bar")),
                tabPanel("Employee performance", customer_analytics_UI("customer_analytics"), icon = icon("users")),
                tabPanel("ARR-Bridge", bridge_UI("bridge"), icon = icon("bold"))),
    
    tabPanel("Forecast", predictive_analytics_UI("predictive_analytics"), icon = icon("chart-line")),
    
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
    callModule(about, "about")
}

shinyApp(ui = ui, server = server)

