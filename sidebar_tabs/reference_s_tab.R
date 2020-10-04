reference.s.tab <-  
  tabItem("reference", fluidRow(
    box(
      title = "App Components",
      width = 12,
      collapsible = F,
      collapsed = T,
      
      p(strong('R:')),
      
      p(
        'R Core Team (2018). R: A language and environment for statistical computing.
        R Foundation for Statistical Computing, Vienna, Austria.
        URL https://www.R-project.org/ ',
        style = "text-align:justify"
      ),
      br(),

      p(strong('Libraries (R Packages) and Functions:')),
      
      p(
        'Carlos J. Gil Bellosta (2011). ADGofTest: Anderson-Darling GoF test.
        R package version 0.3. https://CRAN.R-project.org/package=ADGofTest',
        style = "text-align:justify"
      ),
      
      p(
        'Francois Aucoin (2015). FAdist: Distributions that are Sometimes Used in Hydrology.
        R package version 2.2. https://CRAN.R-project.org/package=FAdist',
        style = "text-align:justify"
      ),
      
      p(
        'Frederico Caeiro and Ayana Mateus (2014). randtests: Testing randomness in R.
        R package version 1.0. https://CRAN.R-project.org/package=randtests',
        style = "text-align:justify"
      ),
      
      p(
        'Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2018). dplyr:
        A Grammar of Data Manipulation.
        R package version 0.7.6. https://CRAN.R-project.org/package=dplyr',
        style = "text-align:justify"
      ),
      
      p(
        "Joshua Kunst (2017). highcharter: A Wrapper for the 'Highcharts' Library.
        R package version 0.5.0. https://CRAN.R-project.org/package=highcharter",
        style = "text-align:justify"
      ),
      
      p(
        'J. R. M. Hosking (2017). L-Moments.
        R package, version 2.6. URL: https://CRAN.R-project.org/package=lmom.',
        style = "text-align:justify"
      ),
      
      p(
        "K. H. Knuth (2006). Optimal Data-Based Binning for Histograms, function OPTBINS (Matlab)
        arXiv:physics/0605197 [physics.data-an].",
        style = "text-align:justify"
      ),
      
      p(
        'Lukasz Komsta and Frederick Novomestky (2015). moments: Moments,
        cumulants, skewness, kurtosis and related tests.
        R package version 0.14. https://CRAN.R-project.org/package=moments',
        style = "text-align:justify"
      ),
      
      p(
        'Marie Laure Delignette-Muller, Christophe Dutang (2015).
        fitdistrplus: An R Package for Fitting Distributions.
        Journal of Statistical Software, 64(4), 1-34. URL http://www.jstatsoft.org/v64/i04/.',
        style = "text-align:justify"
      ),
      
      p(
        'Hideaki Shimazaki (2006). Bin-width Optimization.Function sshist.
        Department of Physics, Kyoto University
        http://176.32.89.45/~hideaki/res/histogram.html',
        style = "text-align:justify"
      ),
      
      p(
        'Thorsten Pohlert (2018). trend: Non-Parametric Trend Tests and Change-Point Detection.
        R package version 1.1.0. https://CRAN.R-project.org/package=trend',
        style = "text-align:justify"
      ),
      
      p(
        'Winston Chang (2016). shinythemes: Themes for Shiny.
        R package version 1.1.1. https://CRAN.R-project.org/package=shinythemes',
        style = "text-align:justify"
      ),
      
      p(
        "Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: Create Dashboards with 'Shiny'.
        R package version 0.7.0. https://CRAN.R-project.org/package=shinydashboard",
        style = "text-align:justify"
      ),
      
      p(
        "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2018).
        shiny: Web Application Framework for R.
        R package version 1.1.0. https://CRAN.R-project.org/package=shiny",
        style = "text-align:justify"
      ),
      
      
      p(
        "Yihui Xie (2018). DT: A Wrapper of the JavaScript Library 'DataTables'
        . R package version 0.4. https://CRAN.R-project.org/package=DT",
        style = "text-align:justify"
      )
    ),
    
    box(
      title = "Books",
      width = 12,
      collapsible = F,
      p(strong('Books:')),
      
      
      p(
        'Chiew, F. and L. Siriwardena (2005). Trend/change detection software and user guide.
        Cooperative Research Centre for Catchment Hydrology, Australia, 23 pp.',
        style = "text-align:justify"
      ),
      
      p(
        'Grayson, R.B., Argent, R.M., Nathan, R.J., McMahon, T.A. and Mein, R. (1996).
        Hydrological Recipes: Estimation Techniques in Australian Hydrology.
        Cooperative Research Centre for Catchment Hydrology, Australia, 125 pp.',
        style = "text-align:justify"
      ),
      
      p(
        'Kundzewicz, Z.W. and Robson, A. (2000).
        Detecting Trend and Other Changes in Hydrological Data. World Climate
        Program - Water, WMO/UNESCO, WCDMP-45, WMO/TD 1013, Geneva, 157 pp.',
        style = "text-align:justify"
      ),
      
      p(
        'Naghettini, M. (Ed.) (2017). Fundamentals of Statistical Hydrology.
        Springer International. Switzerland, 660 pp.',
        style = "text-align:justify"
      ),
      
      p(
        'Rao A.R and Hamed K.H (2000). Flood frequency analysis. CRC Press, Boca Raton, FL, 356 pp.',
        style = "text-align:justify"
      )
    )
    
  ))