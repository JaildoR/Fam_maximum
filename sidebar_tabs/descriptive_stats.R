desc.stats <- 
  tabItem("descstats",
          fluidRow(
            box(title = "Conventional Moments",
                width = 4,
                tableOutput('conv')),
            box(title = "L-Moments",
                width = 4,
                tableOutput('lmom')),
            box(title = "Other Sample Statistics ",
                width = 4,
                tableOutput('sumario'))
          ),
          
          
          
          fluidRow(
            box(title = "Time series plot",
                width = 12,
            div(class = "tsplot",
                highchartOutput(
                  outputId = "plot1",
                  height  = "340px",
                  width = "84%"),
                
                highchartOutput(
                  outputId = "plot2",
                  height  = "340px",
                  width = "15.8%")
                )
            
          )))
  