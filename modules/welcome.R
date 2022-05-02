welcome_UI <- function(id) {
  
  fluidRow(
    
    titlePanel(HTML("<h1><center><font size=16><strong> Welcome to </strong></font></center></h1>")),
    
    br(),
 
    img(src='DiveInsights.PNG', width=800, style="display: block; margin-left: auto; margin-right: auto;"),
    
    titlePanel(HTML("<h2><center> the data analytics tool for Software-as-a-Service enterprises. </center></h2>"))
  )
  
  
}

welcome <- function(input, output, session) {
  
}