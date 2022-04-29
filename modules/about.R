about_UI <- function(id) {
  
  fluidRow(
    column(12,
           
           h4(p("Wiki")),
           h5(p("This project is intended to facilitate useful comparisons between colleges in the NCAA, based on swimming performance, location, and academic information.  Here a prospective student-athlete, or anyone else with an interest can find schools fitting a particular set of criterion relevant to them, for example, schools close to home, with times in a particular range, and of a specified academic profile.")),
           br(),
           h5(p("The project began as an attempt to combine my interest in swimming with a need to practice R, a programming language used primarily for analyzing and reporting data.  It has two components.  The first is this app, which queries a dataset to return information in the form of plots, data tables etc.  The second is the dataset itself, which I assembled by tying together information from the sources below.")),
           br(),
           h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at gpilgrim2607@gmail.com"),
              p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/gpilgrim2670/SwimMap"), "."))
           
           
    )
   
)
  
  
}