
ShowParametersUI <- function(id) {
  ns <- NS(id)
  box(
    class ="par_box",
    title = "Parameter estimation",
    width = 12,
    height = "310px",
    div(class = "par_choice",
        radioButtons(ns(
          "dist_choice_par"),
          label = "Theoretical Distribution:",
          choices = list(
            "Gamma  " = "gamma",
            "Gumbel" = "gumbel",
            "Pearson Type III " = "gamma3",
            "Log-Pearson Type III" = "lgamma3",
            "Generalized Extreme Value" = "gev",
            "Log-Normal (2 parameters)" = "lnorm",
            "Log-Normal (3 parameters)" = "ln3"
          ),
          selected = "gumbel")),
        div(class = "par_summary",
            div(class = "par_title" ,
                htmlOutput(ns('dist_title'))),
            splitLayout(
              div(class = "par_tab",
                  p('Method of Moments:'),
                  tableOutput(ns('param_table_mom'))),
              div(class = "par_tab",
                  p('Maximun Likelihood:'),
                  tableOutput(ns('param_table_mle'))),
              div(class = "par_tab",
                  p('Method of L-Moments:'),
                  tableOutput(ns('param_table_lmo')))
              ))
        
        
        )
}



ShowParameters <- function(input, output, session, param.values) {

  output$param_table_mom <- renderTable({
    if (!is.list(param.values[[input$dist_choice_par]])) {
      return(NULL)
    }
    if (!is.list(param.values[[input$dist_choice_par]][[1]])) {
      return(NULL)
    }
    
    dist.param <- param.values[[input$dist_choice_par]][[1]]
    nm <- names(dist.param$estimate)
    nm <- CapFirst(x = nm)
    nm <- paste0(nm, ":")
    est <- prettyNum(dist.param$estimate, decimal.mark = ".", digits = 4)
    dt <- data.frame(name = nm, est = est)
    
  }, include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)
    x)
  
  
  output$param_table_mle <- renderTable({
    if (!is.list(param.values[[input$dist_choice_par]])) {
      return(NULL)
    }
    if (!is.list(param.values[[input$dist_choice_par]][[2]])) {
      return(NULL)
    }
    
    dist.param <- param.values[[input$dist_choice_par]][[2]]
    nm <- names(dist.param$estimate)
    nm <- CapFirst(x = nm)
    nm <- paste0(nm, ":")
    est <- prettyNum(dist.param$estimate, decimal.mark = ".", digits = 4)
    dt <- data.frame(name = nm, est = est)
    
  }, include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)
    x)
  
  output$param_table_lmo <- renderTable({
    if (!is.list(param.values[[input$dist_choice_par]])) {
      return(NULL)
    }
    if (!is.list(param.values[[input$dist_choice_par]][[3]])) {
      return(NULL)
    }
    
    dist.param <- param.values[[input$dist_choice_par]][[3]]
    nm <- names(dist.param$estimate)
    nm <- CapFirst(x = nm)
    nm <- paste0(nm, ":")
    est <- prettyNum(dist.param$estimate, decimal.mark = ".", digits = 4)
    dt <- data.frame(name = nm, est = est)
    
  }, include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)
    x)
  
  
  
  output$dist_title <- renderText({
    paste0("<strong>",
           SummaryDistribution(dist = input$dist_choice_par),
           "</strong>")
    
    })

}


SummaryDistribution <- function(dist){
  
  name <- switch(dist,
                 "gumbel"   = "Gumbel (Extreme Value Type I):",
                 "gev"   = "Generalized Extreme Value:",
                 "gamma" = "Gamma Distribution:",
                 "lnorm" = "Log-Normal (2 Parameters):",
                 "ln3"   = "Log-Normal (3 Parameters):",
                 "Not Defined yet...")
  
  return(name)
}



# Cap first letters - Used only in parameters table -> go to module (future)
CapFirst <- function(x) {
  paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "")
}





