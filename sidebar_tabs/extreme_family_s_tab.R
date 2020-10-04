extreme.s.tab <-  
  tabItem("extreme_family", fluidRow(
    box(
      title = "Gumbel Distribution (Extreme Value Type I)",
      width = 12,
      p(strong('Probability Density Function:')),
      p(
        '$$ f(x) = \\frac{1} {\\alpha} exp \\left[ - \\frac{x-\\beta}
        {\\alpha} - exp \\left( -\\frac{x-\\beta}{\\alpha} \\right) \\right] $$'
      ),
      
      div(
        class = "eq_notes",
        p(
          '$$ \\begin{multline}
                 \\shoveright{\\text{Where,} \\quad \\beta = location}\\\\
                 \\shoveright{\\alpha = scale}
                 \\end{multline}$$'
        )
      ),
      #p(strong("Quantile Estimation:")),
      p(strong('Parameter Estimators:')),
      tags$ul(
        tags$li(
          "Method of Moments:",
          p(
            a(href = "https://www.google.com/", "gumfit.mme(...)"),
            ",
                             script based on Naghettini (2017)."
          )
        ),
        tags$li(
          "Method of Maximum Likelihood: ",
          p(
            a(
              href = "https://www.google.com/",
              "fitdistrplus::fitdist(..., FAdist::dgumble, method = 'mle')"
            ),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of L-Moments:",
          p(
            a(href = "https://www.google.com/", "lmom::pelgum(...) "),
            ", for further information consult Hosking (2000)."
          )
        )
      )
    ),
    box(
      title = "Generalized Extreme Value Distribution",
      width = 12,
      p(strong('Probability Density Function:')),
      p(
        '$$ f(x) = \\frac{1} {\\alpha} \\left[1+\\kappa \\left( 
        \\frac{x-\\beta}{\\alpha} \\right) \\right]^{-1/\\kappa-1}
        exp \\left\\{ - \\left[1+\\kappa \\left( 
        \\frac{x-\\beta}{\\alpha}\\right) \\right]^{-1/\\kappa} \\right\\}$$'
      ),
      
      div(
        class = "eq_notes",
        p(
          '$$ \\begin{multline}
                 \\shoveright{\\text{Where,} \\quad  \\beta = location}\\\\
                 \\shoveright{\\alpha = scale }\\\\
                 \\shoveright{\\kappa = shape}
                 \\end{multline}$$'
        )
      ),
      
      #p(strong("Quantile Estimation:*")),
      p(strong('Parameter Estimators:*')),
      tags$ul(
        tags$li(
          "Method of Moments:",
          p(
            a(href = "https://www.google.com/", "gevfit.mme()"),
            ",
                              script present in Naghettini (2017) with minor changes."
          )
        ),
        tags$li(
          "Method of Maximum Likelihood: ",
          p(
            a(
              href = "https://www.google.com/",
              "fitdistrplus::fitdist(..., FAdist::dgev, method = 'mle')"
            ),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of L-Moments:",
          p(
            a(href = "https://www.google.com/", "lmom::pelgev(...)"),
            ", for further information consult Hosking (2000)."
          )
        )
      )
    )
    
    
  ))