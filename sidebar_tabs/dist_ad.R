dist.ad <- 
  tabItem("dist_ad", fluidRow(
    box(
      title = "L-Moments Ratio Diagram",
      width = 12,
      highchartOutput(outputId = "lmrd",height = '450px')),
    
    ShowParametersUI("parameter_box"),
    
    box(title = "Goodness of Fit Tests",
        width = 12,
      highchartOutput(outputId = "gofplot",height = '400px')),
    box(
      title = " Histogram argurments ",
      width = 4,
      radioButtons(
        'histclass',
        label = ("Rules for class intervals:"),
        choices = list(
          "Sturges" = "st",
          "Scott" = "sc",
          "Freedman-Diaconis" = "fd",
          "Knuth" = "kn",
          "Shimazaki-Shinomoto" = "ss"
        ),
        selected = "st"
      ),
      br(),
      radioButtons(
        'histtype',
        label = ("Histogram type:"),
        choices = list(
          "Frequency" = "od",
          "Cumulative Freq." = "cf",
          "Density" = "rf",
          "Cumulative Dens." = "crf"
        ),
        selected = "od"
      )),
    box(
      title = "Histogram Plot",
      width = 8,
      height = "406px",
      highchartOutput(outputId = "histogram", height = 312)
      )
    ))
