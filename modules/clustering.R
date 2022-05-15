
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
        actionButton(ns("btn_n_cluster"), "Number of Clusters", width = '100%'),
        h4(icon("arrow-down")),
        actionButton(ns("btn_interpret"), "Interpretation", width = '100%')
      ),
      column(width = 9,
             tabsetPanel(id = ns("tabs"),
               tabPanel(
                 "PCA",
                 fluidRow(
                   column(6,
                          
                          withSpinner(plotOutput(ns("pca_cum_variance")))
                          ),
                   column(6,
                          withSpinner(plotOutput(ns("pca_biplot")))
                          )
                     
                    )
                 ),
               tabPanel("Number of Clusters", verbatimTextOutput("summary")),
               tabPanel("Interpretation", tableOutput("table"))
             )
        )
        
      )
    )
  
  
  
  
}

clustering_server <- function(input, output, session) {
  
  hideTab(session = session, inputId = "tabs", target = "PCA")
  hideTab(session = session, inputId = "tabs", target = "Number of Clusters")
  hideTab(session = session, inputId = "tabs", target = "Interpretation")
  
  observeEvent(input$btn_debug, {
    browser()
    
  })
  

  
  
  observeEvent(input$btn_pca, {
    cat("btn clicked!\n")
    # disable button after first click
    shinyjs::disable("btn_pca")
    
    showTab(session = session, inputId = "tabs", target = "PCA", select = TRUE)
    
    data_clustering.pca <- prcomp(data_clustering[, c(6:10)], scale = TRUE)
    
    pca_cum_variance <- cumsum(data_clustering.pca$sdev^2 / sum(data_clustering.pca$sdev^2))
    df_pca_temp <- data.frame(pca = c("PC1", "PC2", "PC3", "PC4", "PC5"),
                              cum_variance = pca_cum_variance)
    
    
      
    output$pca_cum_variance <- renderPlot({
      p_cum_var <- ggplot(df_pca_temp, aes(x=pca, y=cum_variance, group=1, label = scales::percent(cum_variance))) +
        geom_line() +
        geom_point() +
        scale_y_continuous(labels = scales::percent) + 
        labs(x="Principal Components",y="Cumulative explained Variance") + 
        ggtitle("Cumulative Explained Variance per Principal Component") +
        theme(aspect.ratio=1/1)
      p_cum_var
    })
    
    output$pca_biplot <- renderPlot({
      biplot(data_clustering.pca, 
             xlim=c(-0.25, 0.1), 
             ylim=c(-0.2, 0.2), 
             xlabs = rep("*", nrow(data_clustering.pca$x)), 
             col=c("black", "grey"),
             main = "PCA Biplot",
             xlab = "Principal Component 1",
             ylab = "Principal Component 2")
    })
    
    #TODO: ggf. Tabelle einfügen
    
      
  })
  
  
  
  observeEvent(input$btn_n_cluster, {
    #TODO: display elbow-graph, show shilouetten coeff...
    
    
    
  })
  
  
  
}

# TODO: 
# Solve bug that tooltip is freezed on button click... add tooltips again?