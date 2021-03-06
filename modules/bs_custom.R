
bs_card <- function(font_color, bg_color, header_text, title_text, body_text) {
  # uses bootstrap 5
  shiny::div(
    class=glue::glue("card text-{font_color} bg-{bg_color} mb-3"), 
    style="max-width: 18rem;", 
    
    shiny::div(
      class="card-header", 
      header_text
    ), 
    
    shiny::div(
      class="card-body", 
      
      shiny::h5(
        class="card-title", 
        title_text
      ), 
      
      shiny::p(
        class="card-text", 
        body_text
      )
      
    )
    
  )
  
}

bs_panel <- function(bg_color, panel_title, panel_text) {
  # uses bootstrap 3
  shiny::div(
    class=glue("panel panel-{bg_color}"),
    
    shiny::div(
      class="panel-heading",
      panel_title
    ),
    
    shiny::div(
      class="panel-body",
      panel_text
    )
  )
  
}