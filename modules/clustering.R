
clustering_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    actionButton(ns("btn_debug"), "", icon = icon("bug")),
    br(),
    fluidRow(
      column(width = 3, align = "center",
        actionButton(ns("btn_pca"), "Principal Component Analysis", width = '100%'),
        h4(icon("arrow-down")),
        actionButton(ns("btn_n_cluster"), "Number of Cluster", width = '100%'),
        h4(icon("arrow-down")),
        actionButton(ns("btn_interpret"), "Interpretation", width = '100%')
      ),
      column(width = 9,
        
        
      )
    )
  )
  
  
  
}

clustering_server <- function(input, output, session) {
  
  observeEvent(input$btn_debug, {
    browser()
  })
  
  
  observeEvent(input$btn_pca, {
    cat("btn clicked!\n")
    shinyjs::disable("btn_pca")
    
    data_clustering.pca <- princomp(data_clustering[, c(6:10)], center = TRUE, scale. = TRUE)
    #TODO: Warum werden argumente center und scale verworfen?
    
  })
  
  
  
}

# TODO: 
# Solve bug that tooltip is freezed on button click... add tooltips again?