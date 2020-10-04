plot.data.tab <- 
  tabItem("graf",
          fluidRow(
            box(title = "Plotting Postion",
                width = 4,
                height = 275,
                radioButtons(
                  'plotpos',
                  label = ("Formulae:"),
                  choices = list(
                    "Bloom" = "bloom",
                    "Cunnane" = "cunnane",
                    "Gringorten" = "gringorten",
                    "Hazen" = "hazen",
                    "Median" = "median",
                    "Weibull" = "weibull"
                  ),
                  selected = "weibull"
                )
            ),
            box(
              title = "Theoretical distributions",
              width = 8,
              height = 275,
              
              
              selectizeInput(inputId = "dist1",
                             label = "Choose a Theoretical Distribution to plot (max =3):",
                             choices = c(""),selected = "",
                             multiple = T,options = list(maxItems = 3)),
              br()
            ),
            box(title = " Frequency Analysis Plot",
                width = 12,
                
                highchartOutput(outputId = "freqplot")
                )

            )
          )
  
