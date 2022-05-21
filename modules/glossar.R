glossar_UI <- function(id) {
  
  tabPanel(title = "Terminology",
           icon = shiny::icon(name = "book-reader"),
           wellPanel(id = "panel_2",
                     style = "background: white",
                     navlistPanel(
                       id = "insert",
                       well = FALSE,
                       widths = c(2, 10),
                       
                       tabPanel(
                         title = "Data",
                         icon = shiny::icon(name = "database"),
                         uiOutput("term_boxes_data")
                       ),
                       tabPanel(
                         title = "Statistics",
                         icon = shiny::icon(name = "chart-area"),
                         uiOutput("term_boxes_statistics")
                       ),
                       tabPanel(
                         title = "Infrastructure",
                         icon = shiny::icon(name = "server"),
                         uiOutput("term_boxes_infrastructure")
                       ),
                       tabPanel(
                         title = "Other",
                         icon = shiny::icon(name = "book-reader"),
                         uiOutput("term_boxes_other")
                       )
                       
                     ),
                     br(),
                     actionButton("btn_term_insert", "Add", icon = icon("plus-circle")),
                     actionButton("btn_term_delete", "Delete", icon = icon("times-circle"))
                     #actionButton("btn_debug", "", icon = icon("bug"))
           )
  )
   
  
}

glossar_server <- function(input, output, session) {
  
  create_boxes <- function(category) {
    
    sqlString <- paste0("SELECT * FROM terms_data WHERE category == '", category, "';")
    df_terms_temp <- dbGetQuery(con2, sqlString)
    
    v <- list()
    
    for (i in 1:nrow(df_terms_temp)){
      v[[i]] <- box(status = "primary", collapsible = TRUE, collapsed = TRUE,
                    title = h4(df_terms_temp$term[i], style = "display:inline"),
                    df_terms_temp$explanation[i]
      )
    }
    
    return(v)
  }
  
  # create boxes for each tab
  output$term_boxes_data <- renderUI(create_boxes("data"))
  output$term_boxes_statistics <- renderUI(create_boxes("statistics"))
  output$term_boxes_infrastructure <- renderUI(create_boxes("infrastructure"))
  output$term_boxes_other <- renderUI(create_boxes("other"))
  
  
  # insert button
  observeEvent(input$btn_term_insert, {
    showModal(modalDialog(
      textInput("term_question", "Question"),
      textAreaInput("term_answer", "Answer", rows = 5),
      selectizeInput("term_category", "Category", choices = c("data", "infrastructure", "statistics", "other")),
      size = "l",   easyClose = T,   footer = tagList(actionButton("btn_term_insert_final","Add", icon = icon("plus-circle")),
                                                      modalButton("Close", icon = icon("times-circle")))
    ))
  })
  
  # insert button inside pop-up executes SQL-Statement
  observeEvent(input$btn_term_insert_final, {
    
    sqlString <- paste0("INSERT INTO terms_data (term, explanation, category) VALUES ('",input$term_question,"', '", input$term_answer,"', '", input$term_category, "');")
    dbSendQuery(con2, sqlString)
    
    tab_name_temp <- paste0("term_boxes_", input$term_category)
    
    output[[tab_name_temp]] <- renderUI(create_boxes(input$term_category))
    
    removeModal()
    
    sendSweetAlert(session = session, title = "Definition added", text = " ", type = "success")
    
  })
  
  # delete button
  observeEvent(input$btn_term_delete, {
    sqlString <- paste0("SELECT * FROM terms_data")
    df_terms_temp <- dbGetQuery(con2, sqlString)
    
    showModal(modalDialog(
      selectizeInput("term_question_del", "Which question do you want to delete?", choices = df_terms_temp$term),
      size = "l",   easyClose = T,   footer = tagList(actionButton("btn_term_delete_final","Delete"),
                                                      modalButton("Close", icon = icon("times-circle")))
    ))
  })
  
  # delete button inside pop-up triggers confirm button
  observeEvent(input$btn_term_delete_final, {
    
    confirmSweetAlert(session = session, inputId = "btn_confirm_del", type = "warning", title = "Definition to be deleted. Are you sure?")
    
  })
  
  # confirm button executes SQL-Statement
  observeEvent(input$btn_confirm_del, {
    removeModal()
    
    if(input$btn_confirm_del) {
      sqlString <- paste0("DELETE FROM terms_data WHERE term = '", input$term_question_del, "';")
      sqlString_category <- paste0("SELECT category FROM terms_data WHERE term = '", input$term_question_del, "';")
      
      term_category_temp <- dbGetQuery(con2, sqlString_category)
      dbSendQuery(con2, sqlString)
      
      tab_name_temp <- paste0("term_boxes_", term_category_temp)
      
      output[[tab_name_temp]] <- renderUI(create_boxes(term_category_temp))
      
    }
    
  })
  

}