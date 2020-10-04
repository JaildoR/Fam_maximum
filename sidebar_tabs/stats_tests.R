stats.tests <- 
  tabItem("stattest",
          fluidRow(
            withMathJax(),
            box(
              title = "Tests for Randomness/Independence/Stationarity",
              width = 12,
              highchartOutput(outputId = "pvalues", height = 370)
            ),
            
            tabBox(
              title = "Tests Report",
              width = 12,
              side = "right",
              height = "472px",
              selected = "spr",
              tabPanel(
                title = "Spearman's \u03C1" ,
                value = "spr",
                
                div(
                  class = "table_summary",
                  div(class = "table_title", 
                      p(tags$strong("Spearman's \u03C1 Test Results:")
                  )),
                  div(class = "table_text", tableOutput('spearman'))
                ),
                
                div(
                  class = "description_summary",
                  div(class = "description_title",
                      p(tags$strong("Description:"))),
                  div(
                    class = "description_text",
                    HTML(
                      "The Spearman's \u03C1 test is a non-parametric test 
                      used to assess the significance of a monotonic trend. 
                      In general terms, this test consists of computing the
                      spearman correlation coefficient \\((r_s)\\)
                      between two  rankings. In this case, the
                      rankings are the observed data rank
                      \\((Rx_i)\\) and its time indices \\((i)\\):"
                      
                    ),
                    
                    withMathJax(
                      '$$ r_s = 1 - \\left[ 6 \\cdot
                      \\sum_{i=1}^n \\left( Rx_i - i
                      \\right)^{2}  \\cdot \\left(N^3 - N
                      \\right)^{-1} \\right] $$'
                    ),
                    
                    HTML("and then calculate its test statistic \\((T)\\):"),
                    
                    withMathJax(
                      '$$ T = r_s \\cdot \\sqrt{ \\left( n-2 \\right) /
                      \\left(1- r_s^2 \\right)} $$'
                    ),
                    HTML(
                      "Under the null hypothesis, for \\(n \\gt 10\\) the
                      standardized test statistic \\(T\\) follows
                      approximately the Student's t distribution with
                      \\(df  = n-2 \\). It should be noted that some
                      in some cases (e.g. ties), the test computation
                      may differ from the above description.
                      For further information, consult Naghettini (2017)
                      and the test code."
                    )
                  ),
                  
                  div(
                    class = "implementation",
                    span("Test code:"),
                    tags$a(href ="https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/cor.test.R",
                           "stats::cor.test(..., method = 'spearman')")
                  )
                )
              ),
              
              
              tabPanel(
                title = "Mann-Kendall",
                div(
                  class = "table_summary",
                  div(class = "table_title", p(
                    tags$strong("Mann-Kendall Test Results:")
                  )),
                  div(class = "table_text",
                      tableOutput('mannkendall'))
                ),
                
                div(
                  class = "description_summary",
                  div(class = "description_title",
                      p(tags$strong("Description:"))),
                  div(
                    class = "description_text",
                    HTML(
                      "The Mann Kendall test
                      is a non paramentric test used to assess the
                      significance of a monotonic trend. Given a
                      sequence of observed data
                      \\( x_1, x_2,\\dotsc, x_n \\), the statistic
                      \\(S\\) is calculated as:"
                    ),
                    withMathJax(
                      '$$ S = \\sum_{k=1}^{n-1}  \\sum_{j=k+1}^n sign
                                   \\left( x_j - x_k \\right)^{-1} $$'
                    ),
                    
                    HTML(
                      "Under the null hypothesis, the distribution of
                      \\(S\\) is approximately normally distributed with:"
                    ),
                    
                    withMathJax(
                      '$$  \\mathrm{E}[S] = 0; \\qquad  \\mathrm{Var}[S]
                      = n \\cdot \\left(n -1 \\right) \\cdot
                      \\left(2\\cdot n + 5 \\right)\\; / \\;18;  $$'
                    ),
                    HTML(
                      "Hence the standardized statistic is given
                      by \\(Z = S\\;/\\;\\mathrm{Var}[S]^{0.5}\\).
                      It should be noted that in some cases (e.g. ties),
                      the test computation may differ from
                      the above description. For further
                      information, consult Chiew and Siriwardena (2005)
                      and the test code."
                    )
                  ),
                  div(
                    class = "implementation",
                    span("Test code:"),
                    tags$a(href ="https://github.com/cran/trend/blob/master/R/mk.test.R",
                           "trend::mk.test(...)")
                  )
                )
              ),
              
              
              tabPanel(
                "Wald-Wolfowitz",
                div(
                  class = "table_summary",
                  div(class = "table_title", p(
                    tags$strong("Wald-Wolfowitz Test Results:")
                  )),
                  div(class = "table_text", tableOutput('waldwolf'))
                ),
                
                div(
                  class = "description_summary",
                  div(class = "description_title",
                      p(tags$strong("Description:"))),
                  div(
                    class = "description_text",
                    HTML(
                      "The Wald-Wolfowitz test is used to indicate whether
                      there is significant evidence to (not) reject the
                      null hypothesis (the sample data are independent).
                      Given a time series data\\( x_1, x_2
                      ,\\dotsc, x_n \\), \\( y_i = x_i - \\bar{x} \\).
                      The test statistic \\(R\\) is:"
                    ),
                    withMathJax('$$ R = \\sum_{i=1}^{n-1} y_i \\cdot y_{i+1}
                                + y_1 \\cdot y_n $$'),
                    HTML("The \\(R\\) expected value is given by:"),
                    withMathJax(
                      '$$  \\mathrm{E}[R] = \\frac{-s_2}{n-1}
                      \\qquad \\text{where:} \\quad  s_r =
                      \\sum_{i=1}^{n} \\left(y_i\\right)^r  $$'
                    ),
                    
                    HTML("The \\(R\\) variance is given by:"),
                    withMathJax(
                      '$$ \\mathrm{Var}[R] = \\frac{1}{n-1} \\cdot
                                   \\left( s_2^2- s_4 +
                                   \\frac{s_2^2 - 2s_4}{n-2} -
                                   \\frac{s_2^2 }{n-1}\\right) $$'
                    ),
                    HTML(
                      "Under the null hypothesis and for \\(n \\gt 10\\),
                      the standardized test statistic \\(Z\\) is: "
                    ),
                    withMathJax(
                      '$$ Z = \\frac{ R - \\mathrm{E}[R]}
                      {\\mathrm{Var}[R]^{0.5}}
                      \\qquad Z \\sim \\mathcal{N}(0,1)  $$'
                    ),
                    HTML(
                      'For further information, consult Naghettini (2017),
                      Wald and Wolfowitz (1943) and the test code. However,
                      it should be noted the test uses "\\(y\\)"
                      variable as input in the test code.'
                    )
                    
                  ),
                  div(
                    class = "implementation",
                    span("Test code:"),
                    tags$a(href ="https://github.com/cran/trend/blob/master/R/ww.test.R",
                           "trend::ww.test(...)")
                  )
                )
              ),
              
              tabPanel(
                title = "Turning Points",
                div(
                  class = "table_summary",
                  div(class = "table_title", p(
                    tags$strong("Turning Points Test Results:")
                  )),
                  div(class = "table_text", tableOutput('tp'))
                ),
                
                div(
                  class = "description_summary",
                  div(class = "description_title",
                      p(tags$strong("Description:"))),
                  div(
                    class = "description_text",
                    
                    HTML(
                      "The Turning Points test, a non parametric test,
                      used toassess ramdomness in time series observations.
                      Given a data sample\\( x_1, x_2,\\dotsc, x_n \\),
                      the turning points \\((p_i)\\) are computed as: "
                    ),
                    
                    withMathJax(
                      '$$ p_i = \\begin{cases}
                      1, &  \\text{if} x_{i-1} \\lt x_i \\gt x_{i+1}
                      \\text{ (peak)}  \\\\
                      1, &  \\text{if} x_{i-1} \\gt x_i \\lt x_{i+1}
                      \\text{ (trough)} \\\\
                      0, &  \\text{otherwise} \\end{cases}\\! $$'
                    ),
                    
                    HTML(
                      "Under the null hypothesis, the number of turning
                      points \\((p)\\) expected value and variance
                      are given by:"
                    ),
                    
                    withMathJax(
                      '$$  \\mathrm{E}[p] = 2 \\cdot \\left(n -1 \\right)
                      \\; / \\;3 \\qquad\\mathrm{Var}[p] =
                      \\left(16n - 29 \\right) \\; / \\;18$$'
                    ),
                    
                    HTML("For \\(n \\gt 30\\) the standardized test
                         statistic \\(Z\\) is: "),
                    withMathJax(
                      '$$ Z = \\frac{ p - \\mathrm{E}[p]}
                      {\\mathrm{Var}[p]^{0.5}} \\qquad
                      \\qquad Z \\sim \\mathcal{N}(0,1)  $$'
                    )
                  ),
                  div(
                    class = "implementation",
                    span("Test code:"),
                    tags$a(href =
                             "https://github.com/cran/randtests/blob/master/R/turning.point.test.R",
                           "randtests::turning.point.test(...)")
                  )
                )
              ),
              
              
              tabPanel(
                "Median Crossing",
                div(
                  class = "table_summary",
                  div(class = "table_title", p(
                    tags$strong("Median Crossing Test Results:")
                  )),
                  div(class = "table_text", tableOutput('mc'))
                ),
                
                div(
                  class = "description_summary",
                  div(class = "description_title",
                      p(tags$strong("Description:"))),
                  div(
                    class = "description_text",
                    HTML(
                      "The Median Crossing test is a non-parametric test
                      used to assess the randomness of sampled data.
                      Given a sequence \\( x_1, x_2,\\dotsc, x_n \\),
                      the number of times that the
                      median  is crossed in the time serie \\((C)\\)
                      is calculated as*:"
                    ),
                    withMathJax(
                      '$$ C = \\sum_{i=1}^{n-1}| c_{i+1} - c_i |
                      \\qquad \\text{where:} \\;  c_i = \\begin{cases}
                      1, &  \\text{if} \\quad x_{i}  \\gt \\tilde{x}  \\\\
                      0, &  \\text{otherwise} \\end{cases}$$ '
                    ),
                    HTML("The statistic \\(C\\) expected value and
                         variance are given by: "),
                    withMathJax(
                      '$$  \\mathrm{E}[C] = \\left(n -1 \\right) \\;
                      / \\;2 \\qquad
                                 \\mathrm{Var}[p] = \\left(n - 1 \\right)
                      \\; / \\;4  $$'
                    ),
                    HTML(
                      "Under the null hypothesis, the standardized test
                      statistic \\(Z = S\\;/\\;\\mathrm{Var}[S]^{0.5}\\)
                      follows the standard normal distribution.
                      For further information, review Chiew and
                      Siriwardena (2005) and the test code."
                    )
                  ),
                  
                  div(
                    class = "implementation",
                    span("Test code:"),
                    tags$a(href ="https://www.google.com/",
                           "mc.test(...)*")
                  )
                )
              )
            ),
            
            uiOutput("twosamp")
          )
        )

