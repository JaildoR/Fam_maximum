term.cond <-  
  tabItem("term_cond", fluidRow(
    box(
      title = "License:",
      width = 12, p("In regards some doubts around the use of highcharts (and others components) 
                    and its relation with
                    open source licenses, Fam don't have a license yet. It shoud be noted that
                    some links related to this app source codes leads to google.
                    In the next release  the source code will be completelly available as the
                    implementations related to Pearson and Log-Pearson Type III, Chi-square goodness of 
                    fit test.")
      ),
    box(
      title = "Disclaimer:",
      width = 12,
      div(
        class = "notes",
        p(
          'This aplication is intended for informational and educational purposes only.
        The authors does not take any legal responsibility for the accuracy,
        completeness, or usefulness of the information in (or produced by) this application.
        The authors and maintainers of this app will not save your data on our servers.
        However, we are not responsible for the confidentiality,
        availability, security, loss, misuse or misappropriation of any
        data you submit to this application (see hosting service notes).'
        ),
        p(
          'This aplication is intended for informational and educational purposes only.
        The authors does not take any legal responsibility for the accuracy,
        completeness, or usefulness of the information in (or produced by) this application.
        The authors and maintainers of this app will not save your data on our servers.
        However, we are not responsible for the confidentiality,
        availability, security, loss, misuse or misappropriation of any
        data you submit to this application (see third parties  - hosting services notes).'
        )
      )),
    box(
      title = "Third-Party Information:",
      width = 12,
      p(strong(
        'Hosting Services Notes: Shinyapps.io server'
              )
      ),
      div(
        class = "notes",
        p(
          'This application is hosted in a ',
          a(href = 'https://www.shinyapps.io/.', 
            'Shinyapps.io'
            ),
          'server. By using this app you are agreeing to the 
          terms of use as described by',
          a(href = 'https://www.rstudio.com/about/shinyapps-terms-use/',
            'Shinyapps.io terms of use'),'. 
            The Shinyapps server is not HIPAA compliant, 
            you must refrain from uploading confidential data with 
            this app. You may instead download the code and run the app 
            locally on your private computer and network (see
            how to run this app locally in the documentation).'
        )
      ),
      br(),
      p(strong('Third Party Components:')),
      div(
        class = "notes",
        p(
          'Highcharts API: FAM uses Highcharts in its plots.',
          a(href = 'www.highcharts.com', 'Highcharts'),
          'is a Highsoft software product which is not free for 
          commercial and Governmental use.'
        ),
        p(
          'Shiny Package: This applicantion uses shiny package which 
          includes other open source software components. 
          Click ',
          a(href = 'https://github.com/rstudio/shiny/blob/master/LICENSE', 'here'),
          ' to see the components lists and its licenses.'
        )
      )
    )
  )
)