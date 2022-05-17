
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
               tabPanel(
                 "Number of Clusters",
                 fluidRow(
                   column(6,
                          
                          withSpinner(plotOutput(ns("elbow_method")))
                   ),
                   column(6,
                          withSpinner(plotOutput(ns("silhouette_score")))
                   )
                   
                 )
                 ),
               tabPanel("Interpretation", tableOutput("table"))
             )
        )
        
      )
    )

}

clustering_server <- function(input, output, session) {
  
  # set initial state of tabs and buttons
  hideTab(session = session, inputId = "tabs", target = "PCA")
  hideTab(session = session, inputId = "tabs", target = "Number of Clusters")
  hideTab(session = session, inputId = "tabs", target = "Interpretation")
  shinyjs::disable("btn_n_cluster")
  shinyjs::disable("btn_interpret")
  
  cluster_results <- reactiveValues(df1 = 0, kmeans = 0)
  
  observeEvent(input$btn_debug, {
    browser()
    
  })
  
  
  observeEvent(input$btn_pca, {

    # disable button after first click and shot Tab
    shinyjs::disable("btn_pca")
    showTab(session = session, inputId = "tabs", target = "PCA", select = TRUE)
    
    data_clustering.pca <- prcomp(data_clustering[, c(6:10)], scale = TRUE)
    
    pca_cum_variance <- cumsum(data_clustering.pca$sdev^2 / sum(data_clustering.pca$sdev^2))
    df_pca_temp <- data.frame(pca = c("PC1", "PC2", "PC3", "PC4", "PC5"),
                              cum_variance = pca_cum_variance)
    
    cluster_results$df1 <- data_clustering %>%
      mutate(PC1 = data_clustering.pca$x[,1],
             PC2 = data_clustering.pca$x[,2])

    output$pca_cum_variance <- renderPlot({
      p_cum_var <- ggplot(df_pca_temp, aes(x=pca, y=cum_variance, group=1, label = scales::percent(cum_variance))) +
        geom_line() +
        geom_point() +
        scale_y_continuous(labels = scales::percent) + 
        labs(x="Principal Components",y="Cumulative explained Variance") + 
        ggtitle("Cumulative Explained Variance per Principal Component") +
        theme(aspect.ratio=1/1)
      print(p_cum_var)
    
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
    shinyjs::enable("btn_n_cluster")
      
  })
  
  
  
  observeEvent(input$btn_n_cluster, {
    
    shinyjs::disable("btn_n_cluster")
    showTab(session = session, inputId = "tabs", target = "Number of Clusters", select = TRUE)
    
    calc_kmeans <- function(x) {
      km_temp <- kmeans(cluster_results$df1[,11:12], centers = x, nstart = 10)
      
      # append cluster to existing data frame
      cluster_results$df1[ , ncol(cluster_results$df1) + 1] <- km_temp$cluster
      colnames(cluster_results$df1)[ncol(cluster_results$df1)] <- paste0("n_cluster_", x)
      return(km_temp)
    }
    
    cluster_results$kmeans <- sapply(1:10, calc_kmeans)
    
    # calculate silhouette score for 1 to 10 clusters
    calc_ss <- function(x) {
      #browser()
      ss <- cluster::silhouette(cluster_results$kmeans[,x]$cluster, dist(cluster_results$df1[,11:12]))
      return(mean(ss[, 3]))
    }
    
    avg_ss <- sapply(2:10, calc_ss)

    # calculate total within sum of squares for 1 to 10 clusters
    tot_wss <- sapply(2:10, function(x) {cluster_results$kmeans[,x]$tot.withinss})

    df_plot <- data.frame(n_cluster = 2:10,
                          avg_ss = avg_ss,
                          tot_wss = tot_wss)
    
    output$elbow_method <- renderPlot({
      p_tot_wss <- ggplot(df_plot, aes(x=n_cluster, y=tot_wss, group=1)) +
        geom_line() +
        geom_point() +
        labs(x="Number of Clusters", y="Total Within Sum of Squares") + 
        ggtitle("Elbow Method")
      print(p_tot_wss)
      
    })
    
    output$silhouette_score <- renderPlot({
      p_ss <- ggplot(df_plot, aes(x=n_cluster, y=avg_ss, group=1)) +
        geom_line() +
        geom_point() +
        labs(x="Number of Clusters", y="Silhoette Score") + 
        ggtitle("Silhouette Score")
      print(p_ss)
      
    })
    
    shinyjs::enable("btn_interpret")
    
  })
  
  #TODO: implement interpretation (scatterplot, choose number of clusters, cond. boxplots/violinplots)
  
  
}
