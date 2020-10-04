normal.s.tab <-  
  tabItem("normal_family", fluidRow(
    box(
      title = "Log-normal (2 parameter) Distribution",
      width = 12,
      p(strong('Probability Density Function:')),
      
      withMathJax(
        '$$ f(x) = \\frac{1} {x \\cdot \\sigma_y \\sqrt{2\\pi}}
        \\cdot exp \\left\\{ - \\frac{1}{2} \\left[ \\frac{\\ln(x) - \\mu_y)} 
        {\\sigma_y} \\right]^2 \\right\\} $$'
      ),
      
      div(
        class = "eq_notes",
        p(
          '$$ \\begin{multline}
                  \\shoveright{\\text{Where,} \\quad  Y = \\ln(X)}\\\\     
                 \\shoveright{\\text{Where,} \\quad \\mu_y = meanlog}\\\\
                 \\shoveright{ \\sigma_y = sdlog}
                 \\end{multline}$$'
        )
      ),
      
      #p(strong("Quantile Estimation:")),
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
      title = "Log-normal (3 parameter) Distribution",
      width = 12,
      p(strong('Probability Density Function:')),
      p(
        '$$ f(x) = \\frac{1} { (x - \\alpha) \\cdot \\sigma_y \\cdot \\sqrt{2\\pi}}
        \\cdot exp \\left\\{  - \\frac{1} {2} \\cdot 
        \\left[ \\frac{ ln(x - \\alpha ) - \\mu_y}{\\sigma_y} \\right]^2  \\right\\} $$'
      ),
      
      div(
        class = "eq_notes",
        p(
          '$$ \\begin{multline}
                 \\shoveright{\\text{Where,} \\quad  Y = \\ln(X - \\alpha)}\\\\                 
                 \\shoveright{\\alpha = location}\\\\
                 \\shoveright{\\mu_y = scale}\\\\
                 \\shoveright{\\sigma_y = shape}
                 \\end{multline}$$'
        )
      ),
      
      #p(strong("Quantile Estimation:*")),
      p(strong('Parameter Estimators:*')),
      
      tags$ul(
        tags$li(
          "Method of Moments:*",
          p(
            a(href = "https://www.google.com/", "gumfit.mme(...)"),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of Maximum Likelihood:* ",
          p(
            a(href = "https://www.google.com/", "fitdistrplus::fitdist(..., dlnorm, method = 'mle')"),
            ",
                              consult Delignette-Muller and Dutang (2015) for further information."
          )
        ),
        tags$li(
          "Method of L-Moments:",
          p(
            a(href = "https://www.google.com/", "lmom::pelln3(..., bound = NULL) "),
            ", for further information consult Hosking (2000)."
          )
        )
      )
      
    )
    
    
  ))