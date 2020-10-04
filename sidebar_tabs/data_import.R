data.import <- 
  tabItem("data_import",
          fluidRow(
            box(
              title = "App Information",
              width = 12,
              collapsible = F ,
              collapsed = F,
              id = "app_info",
              p(HTML(
                paste0(
                  "FAM (Hydrologic ",
                  strong("F"),
                  "requency ",
                  strong("A")
                  ,
                  "nalysis  of Annual  ",
                  strong("M"),
                  "axima) is a tool for hydrologists to conduct
                  an at-site frequency analysis for annual maxima data.
                  After the user upload the annual series,
                  the app will display four additional items in the sidebar.
                  They are:"
                )
              )),
              
              
              tags$ul(
                tags$li(
                  strong("Descriptive statistics"),
                  "– This sidebar provides
                  some statistics of uploaded data, such as the sample
                  moments, sample l-moments, interquartile range, maximum,
                  minimum, median, scatterplot, and box-plot."
                ),
                tags$li(
                  strong("Statistical hypothesis tests"),
                  " – This sidebar
                  shows the results of the main hypothesis tests in order
                  to verify fundamental assumptions of a frequency analysis
                  (randomness, independence, homogeneity, and stationarity)."
                ),
                tags$li(
                  strong("Distribution suitability"),
                  " – This item displays
                  the l-moments ratio diagram, estmates the parameters
                  probability distribution commonly used applied in
                  hydrology, and presents the goodness of fit
                  test p-values."
                ),
                tags$ul(
                  tags$li(
                    "Parameter estimation methods – Moments,
                    Maximum Likelihood, and L-moments;"
                  ),
                  tags$li(
                    "Probability distribution – Gumbel, Gamma,
                    Pearson Type III, Log-Pearson Type III, Log-Normal
                    (2 and 3 parameters), and Generalized Extreme Value."
                  ),
                  tags$li(
                    "The goodness of fit tests – Kolmogorov-Smirnov,
                    Anderson-Darling, and Chi-square (not implemented yet)."
                  )
                ),
                tags$li(
                  strong("Frequency Plot"),
                  " – This sidebar item shows
                  the frequency analysis plot with some options
                  for the users to select different types plot
                  position formulae and theoretical probability
                  distributions."
                )
              ),
              
              p(
                "How to use it: The user needs to import a .csv/.txt file
                with two columns. The first column must contain the years,
                and the second dedicate the hydrologic variables.
                To run an example, choose and download one of
                the available datasets and upload it."
              ),
              
              p(strong("Input data example:")),
              div(class = "download-button",
                  downloadButton("downloadData", "Download")),
              
              div(class = "input-download",
                  selectInput(
                    "dataset",
                    label = NULL,
                    choices = c(
      "BR - 40800001 - Ponte Nova - MG - Hidrologia Estatistica (2007).txt",
      "USGS - 01447500 - Lehigh River PA - Stoddartsville.txt",
      "Wabash River - IN - Flood Freq. Analysis (2000).txt"
                    )
                    
                  )
              )
            ),
            box(
              id = "up-box",
              height = "566px",
              title = "File Upload",
              fileInput(
                'file',
                label = 'Choose a file to import (.csv/.txt)',
                accept = c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv')
              ),
              
              checkboxInput(
                inputId = 'header',
                label =  'Header',
                value =  TRUE
              ),
              
              radioButtons(
                'sep',
                'Column Separator',
                c(
                  'Comma' = ',',
                  'Semicolon' = ';',
                  Tab = '\t'
                ),
                '\t'
              ),
              
              radioButtons('dec',
                           'Decimal Separator',
                           c('Comma' = ',',
                             'Dot' = '.'), '.'),
              
              radioButtons(
                'unidade',
                label = ("Units"),
                choices = list(
                  "Cubic metre per second" = 1,
                  "Millimeter" = 2,
                  "Inches" = 3,
                  "Cubic feet per second" = 4
                ),
                selected = 1
              )
            ),
            uiOutput("databox")
            
            
          )
  )





