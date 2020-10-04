run.local.s.tab <-  
  tabItem("run_local", fluidRow(
    box(
      title = "How to Run FAM Locally",
      width = 12,
      
      p("Running FAM locally is quite simple you only need to folow this steps:"),
      
      tags$ol(
        tags$li("Download", tags$a(href ="https://cran.r-project.org ","R language"),
                "(environment for statistical computing)."),
        tags$li("Download", tags$a(href ="https://cran.r-project.org ","Rstudio Desktop"),
                "(integrated development environment for R)."),
        tags$li("Clone or download this app", tags$a(href ="https://cran.r-project.org ","this app"),
                " repository on github."),
        tags$li("Open Rstudio and open ui.R file (could be the server.R or global R too)."),
        tags$li("Run the script below in the console."
                ),
        tags$li("Click in Run App.")
      )
      ,
      column(width = 12, shiny::includeMarkdown("sidebar_tabs/run_local.md"))


      
      
      )
    )
  )