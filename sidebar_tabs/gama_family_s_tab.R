gamma.s.tab <-  
  tabItem("gamma_family", fluidRow(
    box(
      title = "Gamma Distribution",
      width = 12,
      p(strong('Probability Density Function:')),
      withMathJax(
        '$$f(x) =  \\frac{1} {\\alpha^ \\cdot
        \\Gamma(\\beta)} x^{\\beta - 1} \\cdot exp \\left( -
        \\frac{x} {\\alpha } \\right) $$'
      ),
      
      div(
        class = "eq_notes",
        p(
          '$$ \\begin{multline}
                 \\shoveright{\\text{Where,} \\quad \\alpha = \\frac{1} {rate}}\\\\
                 \\shoveright{\\beta  = shape }
                 \\end{multline}$$'
        )
      ),
      
      #p(strong("Quantile Estimation:")),
      p(strong('Parameter Estimators:*')),
      tags$ul(
        tags$li(
          "Method of Moments:",
          p(
            a(href = "https://www.google.com/", "fitdistrplus::fitdist(..., dgamma, method = 'mme')"),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of Maximum Likelihood: ",
          p(
            a(href = "https://www.google.com/", "fitdistrplus::fitdist(..., dgamma, method = 'mle')"),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of L-Moments:",
          p(
            a(href = "https://www.google.com/", "gammafit.lmo(...) "),
            ", script based on Rao and Hamed (2000)."
          )
        )
      )
    ),
    box(
      title = "Pearson Type III Distribution*",
      width = 12,
      p(strong('Probability Density Function:*'))
      ,
      p(
        '$$f(x) =  \\frac{1} {\\alpha \\cdot
        \\Gamma(\\beta)} \\left(\\frac {x - \\xi }{\\alpha} \\right)^{\\beta-1}
        exp \\left(-\\frac{x} {\\alpha} \\right) $$'),

      div(
        class = "eq_notes",
        p(
          '$$ \\begin{multline}
                 \\shoveright{\\text{Where,} \\quad \\alpha = \\frac{1} {rate}}\\\\
                 \\shoveright{\\beta  = shape }\\\\
                 \\shoveright{\\xi  = thres }\\\\
                 \\end{multline}$$'
        )
      ),

      # p(strong("Quantile Estimation:*")),
      p(strong('Parameter Estimators:*')),
      tags$ul(
        tags$li(
          "Method of Moments:",
          p(
            a(href = "https://www.google.com/", "fitdistrplus::fitdist(..., dlnorm, method = 'mme')"),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of Maximum Likelihood: ",
          p(
            a(href = "https://www.google.com/", "fitdistrplus::fitdist(..., dlnorm, method = 'mle')"),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of L-Moments:",
          p(
            a(href = "https://www.google.com/", "lnormfit.lmo(...) "),
            ", script based on Rao and Hamed (2000)."
          )
        )
      )
      
    ),
    box(
      title = "Log-Pearson Type III Distribution*",
      width = 12,
      p(strong('Probability Density Function:*')) 
      ,
      p(
        '$$f(x) =  \\frac{1} {\\alpha \\cdot x \\cdot
        \\Gamma(\\beta)} \\left[\\frac { \\ln(x) - \\xi }{\\alpha} \\right]^{\\beta-1}
        exp \\left[-\\frac{ \\ln(x) -\\xi } {\\alpha} \\right] $$'),

      div(
        class = "eq_notes",
        p(
          '$$ \\begin{multline}
                 \\shoveright{\\text{Where,} \\quad \\alpha = \\frac{1} {rate}}\\\\
                 \\shoveright{\\beta  = shape }\\\\
                 \\shoveright{\\xi  = thres }\\\\
                 \\end{multline}$$'
        )
      ),

      p(strong("Quantile Estimation:*")),
      p(strong('Parameter Estimators:*')),
      tags$ul(
        tags$li(
          "Method of Moments:",
          p(
            a(href = "https://www.google.com/", "fitdistrplus::fitdist(..., dlnorm, method = 'mme')"),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of Maximum Likelihood: ",
          p(
            a(href = "https://www.google.com/", "fitdistrplus::fitdist(..., dlnorm, method = 'mle')"),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of L-Moments:",
          p(
            a(href = "https://www.google.com/", "lnormfit.lmo(...) "),
            ", script based on Rao and Hamed (2000)."
          )
        )
      )

      
    )
    
    
    ))