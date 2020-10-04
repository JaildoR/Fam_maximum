# Source functions --------------------------------------------------------
# Parameters estimators

source(file = "parameters_estimators/gamma.R", encoding = "UTF-8", local = T)
source(file = "parameters_estimators/gev.R", encoding = "UTF-8", local = T)
source(file = "parameters_estimators/gumbel.R", encoding = "UTF-8", local = T)
source(file = "parameters_estimators/lnorm.R", encoding = "UTF-8", local = T)
source(file = "parameters_estimators/ln3.R", encoding = "UTF-8", local = T)

# Htests functions
source(file = "Htests/mann_kendal.R", encoding = "UTF-8", local = T)
source(file = "Htests/mann_whit.R", encoding = "UTF-8", local = T)
source(file = "Htests/median_cross.R", encoding = "UTF-8", local = T)
source(file = "Htests/spearman.R", encoding = "UTF-8", local = T)
source(file = "Htests/turning_points.R", encoding = "UTF-8", local = T)
source(file = "Htests/wald_wolf.R", encoding = "UTF-8", local = T)



# Load data ---------------------------------------------------------------
# L-ratio diagram Plot
hc_lmrd <- readRDS("data/lmrd.rds")

# Vector Frequency plot (RT)
probability.df <- readRDS("data/probability_df.rds")

# categories grouped
categories_grouped <- readRDS("data/cat_grouped.rds")

# Server ------------------------------------------------------------------

shinyServer(function(input, output, session) {
  # Data Manipulation -------------------------------------------------------
  
  # File upload - affects --- everywhere
  user_file <- reactive({
    req(input$file)
    
    status_colnumber <- F
    status_numeric <- F
    status_l20 <- F
    status_l25 <- F
    status_l40 <- F
    
    case_comp <- NULL

    inFile <- input$file
    raw_file <- read.csv(inFile$datapath,
                         header = input$header,
                         sep = input$sep,
                         dec = input$dec)
    
    if (ncol(raw_file) == 2) {
      colnames(raw_file) <- c("Year", "Variable")
      status_colnumber <- T

      
      if ( is.numeric(raw_file$Year) & is.numeric(raw_file$Variable)) {
        colnames(raw_file) <- c("Year", "Variable")
        status_numeric <- T
        
        if (is.integer(raw_file$Variable)) {
          raw_file$Variable <- as.numeric(raw_file$Variable)
          #integer == numeric != damm R
        }
        
        case_comp <- raw_file[complete.cases(raw_file), ]
        status_l20 <- length(case_comp$Variable) >= 20
        status_l25 <- length(case_comp$Variable) >= 25
        status_l40 <- length(case_comp$Variable) >= 40
        
      } else {
        status_numeric <- F
        status_l20 <- F
        status_l25 <- F
        status_l40 <- F
        case_comp <- NULL
        
      }
    } else {
      status_colnumber <- F
      status_numeric <- F
      status_l20 <- F
      status_l25 <- F
      status_l40 <- F
      case_comp <- NULL
    }
    

    return(list(raw_file         = raw_file,
                status_colnumber = status_colnumber,
                status_numeric   = status_numeric,
                case_comp        = case_comp,
                status_l20       = status_l20,
                status_l25       = status_l25,
                status_l40       = status_l40,
                file_name        = inFile$name ))
  })
  
  # set debouncer for input range
  range_d <- reactive({
    req(input$range,user_file()[['status_l40']])

    
    input$range
  }) %>% debounce(millis = 1000,priority = 6)
  
  h_test_two_sample <- reactive({
    
    req(user_file()[['case_comp']],input$range)
    req(user_file()[['status_l40']])
    
    
    arq <- user_file()[['case_comp']]
    
    sel.year <- intersect(range_d()[1]:range_d()[2], arq$Year)
    arq$rank <- rank(arq$Variable, ties.method = "average")
    n1 <- length(sel.year)
    n2 <- length(arq$Year) - n1
    
    if (n1 > n2) {
      arq$subsample <- arq$Year %in% sel.year
      
    } else {
      arq$subsample <- !(arq$Year %in% sel.year)
    }
    
    
    mw_test  <- mw.test(dt = arq)

    return(list(mw_test = mw_test))
    
  })
  
  ## Download examples - 1/2
  output$downloadData <- downloadHandler(filename <- function() {
    input$dataset
  },
  
  ## Download examples - 2/2
  content <- function(file) {
    file.copy(from = paste("example/", input$dataset, sep = ""), file)
  })
  
  # Observe functions -------------------------------------------------------
  
  # Show notifications 
  observeEvent(eventExpr = user_file(), {

    if (isFALSE(user_file()[['status_colnumber']])) {
      
      showNotification(paste(user_file()[['file_name']],
                             "must have two columns "),
                       duration = 4,
                       type = "error")
      
    } else if (isFALSE(user_file()[['status_numeric']])) {
      
      showNotification(
        paste(user_file()[['file_name']],
              "must have only numeric values"),
        duration = 4,
        type = "error")
      
      
    } else if (isFALSE(user_file()[['status_l20']])) {
      
      showNotification(
        paste("Sample must have at least 20 values to perform computations"),
                       duration = 4,
                       type = "error")
      
    } else if (isFALSE(user_file()[['status_l25']])) {
      showNotification(
        paste("Warning: ", 
              user_file()[['file_name']], 
              "have a small sample size. Calculating..."),
                       duration = 4,
                       type = "warning")
    }  else {
      showNotification(
        paste(user_file()[['file_name']],
              " was properly imported. Calculating..."),
        duration = 4,
        type = "message")
    
    }

  },
  priority = 4)
  
  # Update Selectizer
  observe({
    av <-  FillData()[['available_dist']]
    
    updateSelectizeInput(session,
                         "dist1",
                         choices = c("",av))
  },
  priority = 4)
  
  # Range observer
  observeEvent(input$range, {
    req(user_file()[['status_l40']])
    
    arq <- user_file()[['case_comp']]
    
    n.x <- max(c(20, floor(length(arq$Year) / 2 - 1)))
    n.y <- length(arq$Year) - n.x
    elements <- intersect(input$range[1]:input$range[2], 
                          arq$Year)
    start <-which(arq$Year == min(arq$Year[arq$Year >= input$range[1]]))
    end <- which(arq$Year == max(arq$Year[arq$Year <= input$range[2]]))
    
    if (elements[1] > elements[length(elements)] - n.x)
      updateSliderInput(session,
                        "range", 
                        value = c(arq$Year[start], 
                                  arq$Year[start + n.x]))
    
    if (length(arq$Year) < start + n.x)
      updateSliderInput(session, 
                        "range", 
                        value = c(arq$Year[end - n.x], 
                                  arq$Year[end]))
    
    if (elements[1] < elements[length(elements)] - n.y)
      updateSliderInput(session, 
                        "range", 
                        value = c(arq$Year[start], 
                                  arq$Year[start + n.y]))
  },
  ignoreInit = T,
  priority = 1)
  
  # Reactive functions ------------------------------------------------------
  # Reactive  - FitData
  FitData <- reactive({
    req(user_file()[['case_comp']])
    
    gumbel <- gumfit(data   = user_file()[['case_comp']]$Variable)
    gev    <- gevfit(data   = user_file()[['case_comp']]$Variable)
    gamma  <- gammafit(data = user_file()[['case_comp']]$Variable)
    lnorm  <- lnormfit(data = user_file()[['case_comp']]$Variable)
    ln3    <- ln3fit(data = user_file()[['case_comp']]$Variable)
    
    return(list(gumbel = gumbel,
                gev    = gev,
                gamma  = gamma,
                lnorm  = lnorm,
                ln3    = ln3))
  })

  # Fill in reactive values - data
  FillData <- reactive({
    req(user_file()[['case_comp']])
    req(FitData())
    
    arq <- user_file()[['case_comp']]
    
    fill.data <- PlotPosition(data = arq$Variable,
                          method = input$plotpos)
    

    fill.data <- rbind.data.frame(fill.data,probability.df)
    
    fill.data <- fill.data[order(fill.data$probability,decreasing = F ),]
    
    fill.data$rt <- 1/(1 - fill.data$probability)
    
    # create a vector of distribution available
    available_dist <- c()
    
    # get fit data
    gumbel <- FitData()[["gumbel"]]
    gev    <- FitData()[["gev"]]
    gamma  <- FitData()[["gamma"]]
    lnorm  <- FitData()[["lnorm"]]
    ln3    <- FitData()[["ln3"]]
    
    # gumbel fill
    if(is.list(gumbel)){
      if(is.list(gumbel[[1]])){
        available_dist <- c(available_dist,"Gumbel - Method of Moments Estimators")
        
        fill.data$gum_mme <- qgumbel(p = fill.data$probability,
                                     scale = gumbel[[1]]$estimate[1],
                                     location = gumbel[[1]]$estimate[2])  
      }
      
      if(is.list(gumbel[[2]])){
        available_dist <- c(available_dist,"Gumbel - Maximum Likelihood Estimators")
        
        fill.data$gum_mle <- qgumbel(p = fill.data$probability,
                                     scale = gumbel[[2]]$estimate[1],
                                     location = gumbel[[2]]$estimate[2])
      }
      
      
      if(is.list(gumbel[[3]])){
        available_dist <- c(available_dist,"Gumbel - L-moments Estimators")
        
        fill.data$gum_lmo <- qgumbel(p = fill.data$probability,
                                     scale = gumbel[[3]]$estimate[1],
                                     location = gumbel[[3]]$estimate[2])
      }
      
    }
    
    # gev fill
    if(is.list(gev)){
      if(is.list(gev[[1]])){
        available_dist <- c(available_dist,"GEV - Method of Moments Estimators")
        
        fill.data$gev_mme <- qgev(p = fill.data$probability,
                                  location =gev[[1]]$estimate[1],
                                  scale = gev[[1]]$estimate[2],
                                  shape = gev[[1]]$estimate[3])
        
      }
      if(is.list(gev[[2]])){
        available_dist <- c(available_dist,"GEV - Maximum Likelihood Estimators")
        
        fill.data$gev_mle <- qgev(p = fill.data$probability,
                                  location =gev[[2]]$estimate[1],
                                  scale = gev[[2]]$estimate[2],
                                  shape = gev[[2]]$estimate[3])
        
        
      }
      if(is.list(gev[[3]])){
        available_dist <- c(available_dist,"GEV - L-moments Estimators")
        
        fill.data$gev_lmo <- qgev(p = fill.data$probability,
                                  location =gev[[3]]$estimate[1],
                                  scale = gev[[3]]$estimate[2],
                                  shape = gev[[3]]$estimate[3])
        
      }
    }
    
    # gama fill
    if(is.list(gamma)){
      if(is.list(gamma[[1]])){
        available_dist <- c(available_dist,"Gamma - Method of Moments Estimators")
        
        fill.data$gamma_mme <- qgamma(p = fill.data$probability,
                                      shape = gamma[[1]]$estimate[1],
                                      rate  = gamma[[1]]$estimate[2])
        
      }
      if(is.list(gamma[[2]])){
        available_dist <- c(available_dist,"Gamma - Maximum Likelihood Estimators")
        
        fill.data$gamma_mle <- qgamma(p = fill.data$probability,
                                      shape = gamma[[2]]$estimate[1],
                                      rate  = gamma[[2]]$estimate[2])
        
        
      }
      if(is.list(gamma[[3]])){
        available_dist <- c(available_dist,"Gamma - L-moments Estimators")
        
        fill.data$gamma_lmo <- qgamma(p = fill.data$probability,
                                      shape = gamma[[3]]$estimate[1],
                                      rate  = gamma[[3]]$estimate[2])
      }
    }
    
    #lnorm
    if(is.list(lnorm)){
      if(is.list(lnorm[[1]])){
        available_dist <- c(available_dist,"Log Normal (2P) - Method of Moments Estimators")
        
        fill.data$lnorm_mme <- qlnorm(p =  fill.data$probability,
                                      meanlog = lnorm[[1]]$estimate[1], 
                                      sdlog = lnorm[[1]]$estimate[2])
        
      }
      if(is.list(lnorm[[2]])){
        available_dist <- c(available_dist,"Log Normal (2P) - Maximum Likelihood Estimators")
        
        fill.data$lnorm_mle <- qlnorm(p = fill.data$probability,
                                      meanlog = lnorm[[2]]$estimate[1], 
                                      sdlog = lnorm[[2]]$estimate[2])
      }
      if(is.list(lnorm[[3]])){
        available_dist <- c(available_dist,"Log Normal (2P) - L-moments Estimators")
        
        fill.data$lnorm_lmo <- qlnorm(p = fill.data$probability,
                                      meanlog = lnorm[[3]]$estimate[1], 
                                      sdlog = lnorm[[3]]$estimate[2])
        
      }
    }
    
    #lnorm3
    if(is.list(ln3)){
      if(is.list(ln3[[1]])){
        available_dist <- c(available_dist,"Log Normal (3P) - Method of Moments Estimators")
        
        fill.data$ln3_mme <- qlnorm3(p     =  fill.data$probability,
                                     shape = ln3[[1]]$estimate[1],
                                     scale = ln3[[1]]$estimate[2],
                                     thres =  ln3[[1]]$estimate[3])
        
      }
      if(is.list(ln3[[2]])){
        available_dist <- c(available_dist,"Log Normal (3P) - Maximum Likelihood Estimators")
        
        fill.data$ln3_mle <-  qlnorm3(p     =  fill.data$probability,
                                      shape = ln3[[2]]$estimate[1],
                                      scale = ln3[[2]]$estimate[2],
                                      thres =  ln3[[2]]$estimate[3])
      }
      if(is.list(ln3[[3]])){
        available_dist <- c(available_dist,"Log Normal (3P) - L-moments Estimators")
        
        fill.data$ln3_lmo <-  qlnorm3(p     =  fill.data$probability,
                                      shape = ln3[[3]]$estimate[1],
                                      scale = ln3[[3]]$estimate[2],
                                      thres =  ln3[[3]]$estimate[3])
        
      }
    }
    
    return(list(available_dist = available_dist,
                fill.data = fill.data))
  })
  
  # Render Functions --------------------------------------------------------
  # Render menuitems
  output$menu <- renderMenu({
    menu_list <- NULL
    
    req(user_file()[['case_comp']])

    menu_list[[1]] <- menuItem("Descriptive Statistics",
                               tabName = "descstats",
                               icon = icon("list-ul",
                                           class = "fas",
                                           lib = "font-awesome")) 
    
    menu_list[[2]] <- menuItem("Assumptions Checking",
                               tabName = "stattest",
                               icon = icon("clipboard-check",
                                           class = "fas",
                                           lib = "font-awesome"))
    
    menu_list[[3]] <- menuItem("Distributions Suitability",
                               tabName = "dist_ad",
                               icon = icon("stethoscope",
                                            class = "fas",
                                            lib = "font-awesome"))
    
    menu_list[[4]] <- menuItem("Frequency Analysis Plot",
                               tabName = "graf",
                               icon = icon("chart-area",lib = "font-awesome")) 
    
    sidebarMenu(menu_list)
  })
  
  # Ui output dtabox
  output$databox <- renderUI({
    if (is.null(user_file()[['raw_file']]))
      return()
    box(height = "566px", id = "data_box",
      title = "Data Uploaded",
      DT::dataTableOutput('dtdata',height = "474px"))
    
  })
  
  output$dtdata = DT::renderDataTable(
    user_file()[['raw_file']], 
    style = "bootstrap",
    options = list(
      bLengthChange = 0,
      bFilter = 0,
      bInfo = 0,
      bSortable = 0,
      bOrderable = 0,
      pagingType = 'simple',
      scrollY = 370,
      scrollX= T
    ),
    rownames = FALSE,
    extensions = c('Responsive')
  )
  
  output$twosamp <- renderUI({
    
    
    req(user_file()[['case_comp']], user_file()[['status_l40']])
    
    arq <- user_file()[['case_comp']]
    
    box(
      title = " Mann-Whitney Test",
      width = 12,
      footer = HTML(paste(span("Test code:"),tags$a(href = "https://www.google.com/",
                      "mw.test(...)*"))),
             div(id = "mw_text",
                 p(id ="mw_d_title" ,tags$strong("Description:")),
                 div(id = "mw_d_text",
                     withMathJax(),
                   p("The Mann-Whitney test is applied to indicate whether 
                                      there is significant evidence to (not) reject the sample data homogenity. 
                                      Given a time series data of size \\(N\\) splitted in two of size \\(N_1\\) 
                                      and \\(N_2\\) with \\(N_1 \\lt N_2\\), the 
                                      test statistic \\(V\\) is  given by:"),
                   p('$$ V = min \\begin{cases} 
                                            V_1 = N_1 \\cdot N_2 + \\frac{N_1 \\cdot (N_1 +1)}{2} - R_1  \\\\
                                            V_2 = N_1 \\cdot N_2 - V_1 \\end{cases}$$ '),
                   
                   
                   p("Where \\(R_1\\) is the sum of the ranking orders of elements from the subsample with \\(N_1\\) size. 
                      The  expected Value and variance of \\(V\\) are calculated by:"),
                   p(
                     '$$  \\begin{align} 
                                            \\mathrm{E}[V] & = \\frac{ N_1 \\cdot N_2 }{2} \\\\
                                            \\mathrm{Var}[V] & = \\frac{ N_1 \\cdot N_2 (N_1 + N_2 +1 ) }{12} \\end{align} $$'),
                   
                   
                   p("Under the null hypothesis and for \\(N_1 \\gt 20\\), 
                                      the standardized test statistic \\(T\\) is: "),
                   p('$$ Z = \\frac{ V - \\mathrm{E}[V]}{\\mathrm{Var}[V]^{0.5}}
                                             \\qquad Z \\sim \\mathcal{N}(0,1)  $$'),
                   p('For further information, consult Naghettini (2017) and the test code.')
                   )),
      div(id = "mw_content",
        sliderInput(
          inputId = "range",
          "Subsample:",
          sep = "",
          step = 1,
          min = arq$Year[1],
          max = arq$Year[length(arq$Year)],
          value = c(arq$Year[1], arq$Year[20])),
        # p(tags$strong("Ranksum Test:")),
        # tableOutput('ranksum'),
        br(),
        div(id = "mw_results",
        p(tags$strong("Mann-Whitney Results:")),
        tableOutput('mw')),
        div(id ="mw_plot", highchartOutput(outputId = "pvalues2", height = 364, width = "98%"))))
  })
  

  # Descriptive stats table -------------------------------------------------

  output$conv <- renderTable({
    
    req(user_file()[['case_comp']])
    
    ConvMoment(variavel = user_file()[['case_comp']]$Variable,
                unidade = input$unidade)
    },
    include.rownames = FALSE,
    include.colnames = FALSE ,
    sanitize.text.function = function(x)x)
  
  output$lmom <- renderTable({
    req(user_file()[['case_comp']])
    
    LMoment(variavel = user_file()[['case_comp']]$Variable,
             unidade = input$unidade)
    },
    include.rownames = FALSE,
    include.colnames = FALSE,
    sanitize.text.function = function(x) x)
  
  
  output$sumario <- renderTable({
    
    req(user_file()[['case_comp']])
    
    sumario(variavel = user_file()[['case_comp']]$Variable,
            unidade = input$unidade)
    },
    include.rownames = FALSE,
    include.colnames = FALSE,
    sanitize.text.function = function(x)
    x)
  

  # H-tests tables  ---------------------------------------------------------
  
  
  h_test <- reactive({
    req(user_file()[['case_comp']])
    
    tp_test <- tp.test(variavel = user_file()[['case_comp']]$Variable)
    
    mc_test <- mc.test(variavel = user_file()[['case_comp']]$Variable)
    
    mk_test <- mannkendall.test(variavel = user_file()[['case_comp']]$Variable)
    
    sp_test <- spearman.test(var  = user_file()[['case_comp']]$Variable,
                             year = user_file()[['case_comp']]$Year)
    
    ######## Same as description

    ww_test <- waldwolf.test(variavel = user_file()[['case_comp']]$Variable - 
                               mean(user_file()[['case_comp']]$Variable))

    return(list(tp_test = tp_test,
                mc_test = mc_test,
                ww_test = ww_test,
                mk_test = mk_test,
                sp_test = sp_test))
  })
  
  output$tp <- renderTable({
    
    req(h_test())
    
    h_test()[['tp_test']][[1]]
  },
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)x)
  
  output$mc <- renderTable({
    
    req(h_test())
    
    h_test()[['mc_test']][[1]]
    
  },
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)x)
  
  output$waldwolf <- renderTable({
    
    req(h_test())
    
    h_test()[['ww_test']][[1]]
    
  },
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)x)
  
  output$mannkendall <- renderTable({
    
    req(h_test())
    
    h_test()[['mk_test']][[1]]
    
  },
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)x)
  
  output$spearman <- renderTable({
    
    req(h_test())
    
    h_test()[['sp_test']][[1]]
    
  },
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)x)
  
  output$mw <- renderTable({
    req(h_test_two_sample(), range_d() )
    
    h_test_two_sample()[['mw_test']][[1]]
    
  },
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x)x)
  
  # Plots -------------------------------------------------------------------

  output$plot1 <- renderHighchart({
    if (is.null(user_file()[['case_comp']])) {
      return(NULL)
    }
    arq <- user_file()[['case_comp']]
    
    arq$v_lab <- labfy(x = arq$Variable,
                       sufix = paste("</strong></td><td>",HtmlUnit(input$unidade)),
                       digit = 4,
                       dec.mark = ".")
    
    
    hc <- highchart()  %>% 
      hc_add_theme(hc_thm = hc_theme_scatter())  %>% 
      hc_add_series(arq,
                    "line",
                    hcaes(x = Year, y = Variable, lab = v_lab),
                    name = "Time Series",
                    tooltip = list(headerFormat = "",
                                   pointFormat = paste("<small>Year: {point.Year}</small> <br>",
                                                       TypeUnit(unidade = input$unidade),
                                                       "<b>{point.lab}</b>",sep = " "))
      ) %>%
      hc_yAxis(
        title = list(text = AxisNameUnit(unidade = input$unidade),
                     style = list(color = '#333333'),
                     margin = 15))  %>%
      hc_xAxis(
        title = list(text = "<b>Date (year)</b>",
                     style = list(color = '#333333'),
                     margin=15))  %>%
      hc_legend(list(enable = FALSE)) %>%
      hc_plotOptions(series = list(
        marker = (list(enabled = T)),
        lineWidth = 1,
        states = list(hover = list(lineWidthPlus = 1))
      ))
    
  })
  
  output$plot2 <- renderHighchart({
    if (is.null(user_file()[['case_comp']])) {
      return(NULL)
    }
    arq <- user_file()[['case_comp']]
    unit <- HtmlUnit(unidade = input$unidade)
    
    
    hc <- hcboxplot(x = arq$Variable,outliers = F, color = "#333333") %>%
      hc_tooltip(useHTML = T, 
                 outside = T,
                 backgroundColor = NULL,
                 borderWidth = NULL,
                 followPointer = T,
                 headerFormat = "<div class ='tooltip_bp' 
                                                style = '
                                                background: rgba(247,247,247,0.85);
                                                border: 1px solid #333333;
                                                border-radius: 3px;
                                                box-shadow: 1px 1px 2px rgba(0, 0, 0, 0.15);
                                                padding: 8px;'> <small> Box-plot Values </small> <table>",
                 pointFormat =
      "<tr><td> Min:              </td><td style='padding: 0 5px 0 1px'> <strong>{point.low_lab}     </tr>
       <tr><td> 1<sup>st</sup> Q: </td><td style='padding: 0 5px 0 1px'> <strong>{point.q1_lab}      </tr>
       <tr><td> 2<sup>nd</sup> Q: </td><td style='padding: 0 5px 0 1px'> <strong>{point.median_lab}  </tr>
       <tr><td> 3<sup>rd</sup> Q: </td><td style='padding: 0 5px 0 1px'> <strong>{point.q3_lab}      </tr>
       <tr><td> Max:              </td><td style='padding: 0 5px 0 1px'> <strong>{point.high_lab}    </tr>",
                 footerFormat ="</table></div>") %>%
      hc_legend(list(enable = FALSE)) %>%
      hc_chart(type = "column", backgroundColor = "transparent",
               spacingBottom = 5) %>%
      hc_xAxis(title=list(text = "<b>0</b>",
                          margin = 15,
                          style =list(color = "#ffffff00")),
               gridLineColor = 'transparent',
               lineColor = "#ffffff00",
               tickColor = "#ffffff00",
               labels = list(style = list(color = "#ffffff00"))) %>%
      hc_yAxis(visible = F)
    
    
    
    
    # Workaround - Prettyfy tooltip
    low_lab <- labfy(x = hc$x$hc_opts$series[[1]]$data[[1]]$low,
                     sufix = paste("</strong></td><td><strong>",unit,"<td/>"),
                     digit = 4,
                     dec.mark = ".")
    
    q1_lab <- labfy(x = hc$x$hc_opts$series[[1]]$data[[1]]$q1,
                    sufix = paste("</strong></td><td><strong>",unit,"<td/>"),
                    digit = 4,
                    dec.mark = ".")
    
    
    median_lab <- labfy(x = hc$x$hc_opts$series[[1]]$data[[1]]$median,
                        sufix = paste("</strong></td><td><strong>",unit,"<td/>"),
                        digit = 4,
                        dec.mark = ".")
    
    
    q3_lab <- labfy(x = hc$x$hc_opts$series[[1]]$data[[1]]$q3,
                    sufix = paste("</strong></td><td><strong>",unit,"<td/>"),
                    digit = 4,
                    dec.mark = ".")
    
    high_lab <- labfy(x = hc$x$hc_opts$series[[1]]$data[[1]]$high,
                      sufix = paste("</strong></td><td><strong>",unit,"<td/>"),
                      digit = 4,
                      dec.mark = ".")
    
    # New values to pretty tooltip
    hc$x$hc_opts$series[[1]]$data[[1]]$low_lab <- low_lab
    hc$x$hc_opts$series[[1]]$data[[1]]$q1_lab <- q1_lab
    hc$x$hc_opts$series[[1]]$data[[1]]$median_lab <- median_lab
    hc$x$hc_opts$series[[1]]$data[[1]]$q3_lab <- q3_lab
    hc$x$hc_opts$series[[1]]$data[[1]]$high_lab <- high_lab
    
    
    stats_bp <- boxplot.stats(arq$Variable)
    if(length(stats_bp$out)!=0){
      outliers <- data.frame(x=0,
                             out = stats_bp$out,
                             out_lab= labfy(x = stats_bp$out ,
                                            sufix = paste(unit,"</strong>"),
                                            digit = 4,
                                            dec.mark = "."),stringsAsFactors = F)
      
      hc <- hc %>% hc_add_series(data = outliers, color = "#333333",
                                 hcaes(x=x,y=out,labs=out_lab),
                                 name = "Outliers",
                                 type = "scatter",
                                 tooltip = list(headerFormat = "<div class ='tooltip_bp' 
                                                style = '
                                                background: rgba(247,247,247,0.85);
                                                border: 1px solid #333333;
                                                border-radius: 3px;
                                                box-shadow: 1px 1px 2px rgba(0, 0, 0, 0.15);
                                                padding: 8px;'>",
                                                pointFormat = "
                                                Out: <strong>{point.out_lab}<br/>",
                                                footerFormat ="</div>"))
    }
    
  })
  
  output$pvalues <- renderHighchart({
    req(h_test())
    
    df <- data.frame(
      name = c("Turning Point Test",
               "Median Crossing Test",
               "Wald-Wolfowitz Test",
               "Spearman's Rho Test",
               "Mann-Kendall Test"),
      pvalue = c(h_test()[['tp_test']][[2]], 
                 h_test()[['mc_test']][[2]],
                 h_test()[['ww_test']][[2]],
                 h_test()[['sp_test']][[2]],
                h_test()[['mk_test']][[2]]),
      ref = c("Naghettini 2017)","Trend Software (2005)",
              "Naghettini (2017)",
              "Naghettini (2017)","Trend Software (2005)"),
      
      cat = c("Turning<br>Points Test",
              "Median<br>Cross. Test",
              "Wald<br>Wolfowitz Test",
              "Spearman's \u03C1 <br> Test",
              "Mann<br>Kendall Test"),stringsAsFactors = F)
    
    if ( all(df$pvalue > 0.25)) {
      df$colors <-  "#333333"
    } else{
      df$colors <-  PvalueColor(pval = df$pvalue)
    }
    
    df <- df[order(df$pvalue, decreasing = F), ]
    
    hc <- highchart()  %>% 
      hc_add_series(df, "bar",
                    hcaes(y = pvalue, 
                          color = colors,
                          text = ref),
                    
                    name = "") %>%
      hc_chart(spacingLeft   = 2,
               spacingRight  = 8,
               spacingBottom = 5)  %>% 
      
      hc_yAxis(title = list(text = "<b>P-Value</b>",
                            style = list(color = '#333333'),
                            margin=15),
               max = 1,
               tickPixelInterval = 100,
               labels = list(style = list(color = '#555555')),
               gridLineColor = '#eeeeee',
               tickLength = 0,
               tickColor = '#eeeeee',
               lineWidth = 0)  %>%
      hc_legend(list(enable = FALSE)) %>%
      hc_tooltip(
        headerFormat = "",
        valueDecimals = 3,
        pointFormat = "<b>{point.name}<b>
                             <br>P-value: <b>{point.y}</b> 
                             <br> Ref.: {point.text}"
      )  %>%
      hc_xAxis(categories = df$cat,
               labels = list(style = list(color = '#555555')),
               tickLength = 5,
               tickColor = '#888888',
               gridLineWidth = 0,
               tickWidth = 1,
               lineColor = '#888888',
               lineWidth= 1)
  })
  
  output$pvalues2 <- renderHighchart({
    req(h_test_two_sample(),range_d())
    
    df <- data.frame(
      name   = c("Mann-Whitney Test"),
      pvalue = c(h_test_two_sample()[['mw_test']][[2]]),
      ref    = c("Naghettini (2017)"),
      cat    = c("Mann-Whitney<br>Test"),
      stringsAsFactors = F)

    if ( all(df$pvalue > 0.18)) {
      df$colors <-  "#333333"
    } else{
      df$colors <-  PvalueColor(pval = df$pvalue)
    }
    
    hc <- highchart()  %>% 
      hc_add_series(df,"column",
                    hcaes(y = pvalue,color = colors,text = ref),
                    pointPadding= 0.25,
                    name = "") %>%
      hc_chart(spacingLeft = 0, spacingRight = 0, spacingBottom = 5)  %>% 
      hc_yAxis(title = list(style = list(color = '#333333'),margin=10),
               labels = list(style = list(color = '#555555')),
               lineColor = '#888888',
               gridLineColor = '#eeeeee',
               tickColor = '#888888',
               tickWidth = 0,
               max = 1)  %>%
      hc_legend(list(enable = FALSE)) %>%
      hc_tooltip(
        headerFormat = "",
        valueDecimals = 3,
        pointFormat = "<b>{point.name}<b><br>P-value: 
        <b>{point.y}</b> <br> Ref.: {point.text}")  %>%
      hc_xAxis(labels = list(style = list(color = '#555555'),
                             format = "Mann-Whitney<br>Test"),
               tickLength = 0,
               tickColor  = '#888888',
               tickWidth  = 1,
               lineColor  = '#888888',
               gridLineWidth = 0,
               lineWidth  = 1)
  })
  
  output$lmrd <- renderHighchart({
    if (is.null(user_file()[['case_comp']])) {
      return(NULL)
    }
    arq <- user_file()[['case_comp']]
    
    lmom.sample <- lmom:::samlmu(x = arq$Variable)
    sample <-
      data.frame(X1 = lmom.sample[3],
                 X2 = lmom.sample[4],
                 name = "Sample")
    hc_lmrd <- hc_lmrd %>% hc_add_series(
      sample,
      "scatter",
      hcaes(x = X1, y = X2, group = name),
      color = "#CC2936",
      marker = list(symbol = "cross", radius = 6),
      states =list(hover = list(halo = list(size = 11)))
    ) %>%
      hc_add_theme(hc_thm = hc_theme_scatter())  %>% 

      
      hc_yAxis(
        title = list(text = "<b><i>L</i>-Kurtosis</b>",
                     style = list(color = '#333333'),
                     margin=15),
        max = 1,
        min = -0.5)  %>%
      
      hc_xAxis(
        title = list(text = "<b><i>L</i>-Skewness</b>",
                     style = list(color = '#333333'),
                     margin=15),
        min = -1,
        max = 1)
    hc_lmrd
  })
  
  # if lower not hit threeshold -- set color ks /ad
  output$gofplot <-  renderHighchart({
    
    if (is.null(user_file()[['case_comp']])) {
      return(NULL)
    }
    
    gumbel <- FitData()[["gumbel"]]
    gev    <- FitData()[["gev"]]
    gamma  <- FitData()[["gamma"]]
    lnorm  <- FitData()[["lnorm"]]
    ln3    <- FitData()[["ln3"]]
    
    
    if (is.null(gumbel) & is.null(gev)) {
      return(NULL)
    }
    
    df_ks <- data.frame(matrix(ncol = 4, nrow = 0))
    
    if (is.list(gumbel)) {
      gumbel <- Filter(Negate(is.null), gumbel)
      if ((length(gumbel) != 0)) {
        gum.names <- as.data.frame(t(sapply(gumbel,
                                            function(x)
                                              c(x$distname, x$method), simplify = T)), stringsAsFactors = F)
        
        
        ks_gum <- sapply(gumbel,
                         function(x)
                           ks.test(
                             x = x$data,
                             "pgumbel",
                             scale = x$estimate[1],
                             location = x$estimate[2]
                           ), simplify = F)
        gum.ks.val <-
          as.data.frame(t(sapply(ks_gum, function(x)
            c(x$p.value, x$statistic))))
        
        gum.ks.data <- cbind(gum.names, gum.ks.val)
        
        df_ks <- rbind(df_ks, gum.ks.data)
        
        
      }
      
    }
    
    if (is.list(gev)) {
      gev <- Filter(Negate(is.null), gev)
      if ((length(gev) != 0)) {
        gev.names <- as.data.frame(t(sapply(gev,
                                            function(x)
                                              c(x$distname, x$method), simplify = T)), stringsAsFactors = F)
        
        
        ks_gev <-  sapply(gev,
                          function(x)
                            ks.test(
                              x = x$data,
                              "pgev",
                              location = x$estimate[1],
                              scale = x$estimate[2],
                              shape = x$estimate[3]
                            ), simplify = F)
        
        
        
        gev.ks.val <-
          as.data.frame(t(sapply(ks_gev, function(x)
            c(x$p.value, x$statistic))))
        
        
        gev.ks.data <- cbind(gev.names, gev.ks.val)
        
        df_ks <- rbind(df_ks, gev.ks.data)
        
        
        
      }
    }
    
    if (is.list(gamma)) {
      gamma <- Filter(Negate(is.null), gamma)
      if ((length(gamma) != 0)) {
        gamma.names <- as.data.frame(t(sapply(gamma,
                                            function(x)
                                              c(x$distname, x$method), simplify = T)), stringsAsFactors = F)
        
        
        ks_gamma <- sapply(gamma,
                         function(x)
                           ks.test(
                             x = x$data,
                             "pgamma",
                             shape = x$estimate[1],
                             rate = x$estimate[2]
                           ), simplify = F)
        gamma.ks.val <-
          as.data.frame(t(sapply(ks_gamma, function(x)
            c(x$p.value, x$statistic))))
        
        gamma.ks.data <- cbind(gamma.names, gamma.ks.val)
        
        df_ks <- rbind(df_ks, gamma.ks.data)
        
        
      }
      
    }
    
    if (is.list(lnorm)) {
      lnorm <- Filter(Negate(is.null), lnorm)
      if ((length(lnorm) != 0)) {
        lnorm.names <- as.data.frame(t(sapply(lnorm,
                                              function(x)
                                                c(x$distname, x$method), simplify = T)), stringsAsFactors = F)
        
        
        ks_lnorm <- sapply(lnorm,
                           function(x)
                             ks.test(
                               x = x$data,
                               "plnorm",
                               meanlog = x$estimate[1],
                               sdlog = x$estimate[2]
                             ), simplify = F)
        lnorm.ks.val <-
          as.data.frame(t(sapply(ks_lnorm, function(x)
            c(x$p.value, x$statistic))))
        
        lnorm.ks.data <- cbind(lnorm.names, lnorm.ks.val)
        
        df_ks <- rbind(df_ks, lnorm.ks.data)
        
        
      }
      
    }
    
    # if (is.list(ln3)) {
    #   ln3 <- Filter(Negate(is.null), ln3)
    #   if ((length(ln3) != 0)) {
    #     ln3.names <- as.data.frame(t(sapply(ln3,
    #                                           function(x)
    #                                             c(x$distname, x$method), simplify = T)), stringsAsFactors = F)
    #     
    #     
    #     ks_ln3 <- sapply(ln3,
    #                        function(x)
    #                          ks.test(
    #                            x = x$data,
    #                            "plnorm3",
    #                            shape = x$estimate[1],
    #                            scale = x$estimate[2],
    #                            thres = x$estimete[3]
    #                          ), simplify = F)
    #     ln3.ks.val <-
    #       as.data.frame(t(sapply(ks_ln3, function(x)
    #         c(x$p.value, x$statistic))))
    #     
    #     ln3.ks.data <- cbind(ln3.names, ln3.ks.val)
    #     
    #     df_ks <- rbind(df_ks, ln3.ks.data)
    #     
    #     
    #   }
    #   
    # }
    
    names(df_ks) <- c("name", "method", "pvalue", "stat")
    
    df_ks$stat <- signif(x = df_ks$stat, digits = 3)
    
    
    df_ad <- data.frame(matrix(ncol = 4, nrow = 0))
    
    if(is.list(gumbel)){
      gumbel <- Filter(Negate(is.null), gumbel)
      if((length(gumbel)!= 0 )){
        
        gum.names <- as.data.frame(t(sapply(gumbel, 
                                            function(x) c(x$distname,x$method),simplify = T)),stringsAsFactors = F)
        
        
        ad_gum <- sapply(gumbel, 
                         function(x) ad.test(x = x$data,
                                             distr.fun =pgumbel,
                                             scale = x$estimate[1],
                                             location = x$estimate[2]),simplify = F)
        gum.ad.val <-  as.data.frame(t(sapply(ad_gum,function(x) c(x$p.value,x$statistic))))
        
        gum.ad.data <- cbind(gum.names,gum.ad.val)
        
        df_ad <- rbind(df_ad,gum.ad.data)
        
        
      }
      
    }
    
    if(is.list(gev)){
      gev <- Filter(Negate(is.null), gev)
      if((length(gev)!= 0 )){
        
        gev.names <- as.data.frame(t(sapply(gev, 
                                            function(x) c(x$distname,x$method),simplify = T)),stringsAsFactors = F)
        
        
        ad_gev <-  sapply(gev, 
                          function(x) ad.test(x = x$data,
                                              distr.fun = pgev,
                                              location = x$estimate[1],
                                              scale = x$estimate[2],
                                              shape = x$estimate[3]),simplify = F)
        
        
        
        gev.ad.val <-  as.data.frame(t(sapply(ad_gev,function(x) c(x$p.value,x$statistic))))
        
        
        gev.ad.data <- cbind(gev.names,gev.ad.val)
        
        df_ad <- rbind(df_ad,gev.ad.data)
        
        
        
      }
    }
    
    if(is.list(gamma)){
      gamma <- Filter(Negate(is.null), gamma)
      if((length(gamma)!= 0 )){
        
        gamma.names <- as.data.frame(t(sapply(gamma, 
                                              function(x) c(x$distname,x$method),simplify = T)),stringsAsFactors = F)
        
        
        ad_gamma <- sapply(gamma, 
                           function(x) ad.test(x = x$data,
                                               distr.fun =pgamma,
                                               shape = x$estimate[1],
                                               rate = x$estimate[2]),simplify = F)
        gamma.ad.val <-  as.data.frame(t(sapply(ad_gamma,function(x) c(x$p.value,x$statistic))))
        
        gamma.ad.data <- cbind(gamma.names,gamma.ad.val)
        
        df_ad <- rbind(df_ad,gamma.ad.data)
        
        
      }
      
    }
    
    if(is.list(lnorm)){
      lnorm <- Filter(Negate(is.null), lnorm)
      if((length(lnorm)!= 0 )){
        
        lnorm.names <- as.data.frame(t(sapply(lnorm, 
                                              function(x) c(x$distname,x$method),simplify = T)),stringsAsFactors = F)
        
        
        ad_lnorm <- sapply(lnorm, 
                           function(x) ad.test(x = x$data,
                                               distr.fun =plnorm,
                                               meanlog = x$estimate[1],
                                               sdlog = x$estimate[2]),simplify = F)
        lnorm.ad.val <-  as.data.frame(t(sapply(ad_lnorm,function(x) c(x$p.value,x$statistic))))
        
        lnorm.ad.data <- cbind(lnorm.names,lnorm.ad.val)
        
        df_ad <- rbind(df_ad,lnorm.ad.data)
        
        
      }
    }
    
    # if(is.list(ln3)){
    #   ln3 <<- Filter(Negate(is.null), ln3)
    #   if((length(ln3)!= 0 )){
    # 
    #     ln3.names <- as.data.frame(t(sapply(ln3,
    #                                           function(x) c(x$distname,x$method),
    #                                         simplify = T)),stringsAsFactors = F)
    # 
    #     ad_ln3 <- sapply(ln3,
    #                        function(x) ad.test(x = x$data,
    #                                            distr.fun = plnorm3,
    #                                            shape = x$estimate[1],
    #                                            scale = x$estimate[2],
    #                                            thres = x$estimete[3]), simplify = F)
    # 
    #     ln3.ad.val <-  as.data.frame(t(sapply(ad_ln3,function(x) c(x$p.value,x$statistic))))
    # 
    #     ln3.ad.data <- cbind(lnorm.names,ln3.ad.val)
    # 
    #     df_ad <- rbind(df_ad,ln3.ad.data)
    # 
    # 
    #   }
    # }
    # 
    
    names(df_ad) <- c("name", "method", "pvalue", "stat")
    df_ad$stat <- signif(x = df_ad$stat, digits = 3)
    
    if (min(c(df_ks$pvalue,df_ad$pvalue)) > 0.25 ) {
      df_ks$colors <-  "#214A5B"
      df_ad$colors <-  "#214A5B"
      
    } else{
      df_ks$colors <-  PvalueColor(pval = df_ks$pvalue)
      df_ad$colors <-  PvalueColor(pval = df_ad$pvalue)
      
    }
    

    df_ad$long_name <- DistNameLP(name = df_ad$name)
    df_ks$long_name <- DistNameLP(name = df_ks$name)
    
    df_ad$name <- DistNameSP(name = df_ad$name)
    df_ks$name <- DistNameSP(name = df_ks$name)

    df_ad$long_met <- MethodNameLP(name = df_ad$method)
    df_ks$long_met <- MethodNameLP(name = df_ks$method)    
    
    df_ad$method <- MethodNameSP(name = df_ad$method)
    df_ks$method <- MethodNameSP(name = df_ks$method)
    
    hc <- highchart()  %>% 
      hc_xAxis(
        list(tickWidth = 0,
             lineColor = '#888888',
             lineWidth = 1,
             breaks = list(
               list(from =0.35,
                    to = 0.65,
                    breakSize = 0),
               list(from =1.35,
                    to = 1.65,
                    breakSize = 0),
               list(from =3.35,
                    to = 3.65,
                    breakSize = 0),
               list(from =4.35,
                    to = 4.65,
                    breakSize = 0),
               list(from =6.35,
                    to = 6.65,
                    breakSize = 0),
               list(from =7.35,
                    to = 7.65,
                    breakSize = 0),
               list(from =9.35,
                    to = 9.65,
                    breakSize = 0),
               list(from =10.35,
                    to = 10.65,
                    breakSize = 0),
               list(from =12.35,
                    to = 12.65,
                    breakSize = 0),
               list(from =13.35,
                    to = 13.65,
                    breakSize = 0)),
               
             categories = categories_grouped,
               labels = list(
                 rotation = 0,
                 style = list(color = '#ffffff',
                              fontSize = "13px"),
                 
                 groupedOptions = list(
                   list(rotation = 0,
                        align = "center",
                        style = list(color = '#444444',
                                     fontSize = "13px",
                                     fontWeight ='bold'))))),
        
        
    list(tickPositions = list(-0.5, 2.5, 5.5, 8.5, 11.5,14.5),
         min = -0.5,
         max = 14.5,
         offset = 0,
         labels = list(enabled = F),
         tickLength = 15,
         tickColor = '#999999',
         tickWidth = 1,
         lineWidth = 0,
         breaks = list(
           list(from =0.35,
                to = 0.65,
                breakSize = 0),
           list(from =1.35,
                to = 1.65,
                breakSize = 0),
           list(from =3.35,
                to = 3.65,
                breakSize = 0),
           list(from =4.35,
                to = 4.65,
                breakSize = 0),
           list(from =6.35,
                to = 6.65,
                breakSize = 0),
           list(from =7.35,
                to = 7.65,
                breakSize = 0),
           list(from =9.35,
                to = 9.65,
                breakSize = 0),
           list(from =10.35,
                to = 10.65,
                breakSize = 0),
           list(from =12.35,
                to = 12.65,
                breakSize = 0),
           list(from =13.35,
                to = 13.65,
                breakSize = 0)
           
           )),
    
    
    list(min = -0.5,
         max = 14.5,
         offset = 0,
         labels = list(enabled = T),
         tickLength = 5,
         tickColor = '#999999',
         tickWidth = 1,
         lineWidth = 0,
         tickPositions = list(0,1,2,3,
                              4,5,6,7,
                              8,9,10,11,
                              12,13,14),
         categories = list("MME","MLE","LMOM",
                           "MME","MLE","LMOM",
                           "MME","MLE","LMOM",
                           "MME","MLE","LMOM",
                           "MME","MLE","LMOM"),
         uniqueNames = F,
         tickmarkPlacement = 'on',
         breaks = list(
           list(from =0.35,
                to = 0.65,
                breakSize = 0),
           list(from =1.35,
                to = 1.65,
                breakSize = 0),
           list(from =3.35,
                to = 3.65,
                breakSize = 0),
           list(from =4.35,
                to = 4.65,
                breakSize = 0),
           list(from =6.35,
                to = 6.65,
                breakSize = 0),
           list(from =7.35,
                to = 7.65,
                breakSize = 0),
           list(from =9.35,
                to = 9.65,
                breakSize = 0),
           list(from =10.35,
                to = 10.65,
                breakSize = 0),
           list(from =12.35,
                to = 12.65,
                breakSize = 0),
           list(from =13.35,
                to = 13.65,
                breakSize = 0))
    )) %>%  
      
      
      hc_add_series(df_ks, "column", name = "Kolmogorov–Smirnov Test",  
                    color = '#214A5B',
                    hcaes(y = pvalue,
                          stat = stat,
                          met = method,
                          lname = long_name,
                          lmet = long_met,
                          color = colors,
                          x = name)) %>%
      
      hc_add_series(df_ad, "column", name = "Anderson–Darling Test", 
                    color = '#214A5B',
                    hcaes(y = pvalue,
                          stat = stat,
                          met = method,
                          lname = long_name,
                          lmet = long_met,
                          color = colors,
                          x = name)) %>%
      hc_plotOptions(series = list(pointPadding = 0.1,
                                   groupPadding = 0.22
                                   )) %>%
      
      hc_legend(enabled = T) %>%
      
      hc_chart(spacingBottom = 5)  %>% 
      
      hc_yAxis(title = list(text = "<b>P-Value</b>",
                            style = list(color = '#333333'),
                            margin=20),
               min=0,
               max = 1)  %>%
      
      hc_tooltip(valueDecimals = 3,
                 useHTML= T,
                 headerFormat = "",
                 pointFormat = 
                 "<span style='color:{point.color}'> 
                 ● <small>{point.name}: {point.met}</small></span>
                  <br><strong>{series.name}</strong> <br> 
                  P-value: <strong>{point.y}</strong>
                  <br>Statistic (K): {point.stat} <br>")
    hc
  

    
  })

  # Future improvement ------ reduce hcaes ->better tooltip method
  output$histogram <- renderHighchart({
    if (is.null(user_file()[['case_comp']])) {
      return(NULL)
    }
    arq <- user_file()[['case_comp']]
    
    bins <- BinData(arq$Variable, 
                     rule = input$histclass, 
                     type = input$histtype)
    
    hc <- highchart() %>%
      hc_add_series(bins,
                    "column",
                    hcaes(x        = x_,
                          y        = main,
                          dens     = dens_p,
                          min      = min_p,
                          max      = max_p,
                          count    = count_p,
                          cumct    = cumct_p,
                          cumfreq  = cumfreq_p),
                    color = "#444444",
                    name = "Histogram") %>%
      
      hc_yAxis(title = list(text = 
                              HistogramAxis(type = input$histtype),
                            style = list(color = '#333333'),
                            margin=15)) %>%
      
      hc_xAxis(gridLineWidth = 0,
               tickLength = 5,
               tickColor = '#888888',
               crosshair = T,
               tickWidth = 1,
               lineColor = '#888888',
               lineWidth = 1,
               title = list(text = AxisNameUnit(unidade =input$unidade),
                                                  style = list(color = '#333333'),
                                                  margin=15)) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(
        borderWidth = 1,
        headerFormat = "",
        useHTML= T,
        pointFormat = HistogramTooltip(type = input$histtype))  %>% 
      
      hc_plotOptions(column = list(
        pointPadding = 0,
        borderWidth = 0,
        groupPadding = 0,
        pointRange = max(bins$width)*1.01,
        shadow = F))
    
    hc
    
  })
  
  output$freqplot <-  renderHighchart({
    if (is.null(user_file()[['case_comp']])) {
      return(NULL)
    }
    
    hc <- highchart()
    
    dt <- FillData()[['fill.data']]
    unidade <- HtmlUnit(unidade = input$unidade)
    
    ######
    dt_sample <- dt[,c("rt","var")]
    dt_sample <- FreqTooltip(d_frame = dt_sample,
                                unidade = unidade,
                                selectizer = input$dist1,
                                opt = "sample")

    if (!is.null(input$dist1)){
      if (length(input$dist1) >= 1){
        dt1 <- DistributionSelector(data = dt,
                   selectizer = input$dist1[1])
        
        dt1 <- FreqTooltip(d_frame = dt1,
                             unidade = unidade,
                             selectizer =input$dist1,
                             opt = "dist1") # get a  dt$tooltip_p 

        hc <- hc %>% hc_add_series(dt1,
                                   "line",
                                   hcaes(y=var, 
                                         x=rt,
                                         tt_p = tooltip_p
                                         ), 
                                   color = "#d35400",
                                   name = DistNamesLongToShort(input$dist1[1]),
                                   marker = list(enabled = F,symbol = "circle",
                                                 states = list(hover = list(radius= 4))),
                                   states = list(hover = list(halo =list(size = 7))))
      }
      
      if (length(input$dist1) >= 2){
        dt2 <- DistributionSelector(data = dt,
                          selectizer = input$dist1[2])

        dt2 <- FreqTooltip(d_frame = dt2,
                             unidade = unidade,
                             selectizer =input$dist1,
                             opt = "dist2")

        hc <- hc %>% hc_add_series(dt2,
                                   "line",
                                   hcaes(y=var, 
                                         x=rt,
                                         tt_p = tooltip_p),
                                   color = "#2980b9",
                                   name = DistNamesLongToShort(input$dist1[2]),
                                   marker = list(enabled = F,symbol = "circle",
                                                 states = list(hover = list(radius= 4))),
                                   states = list(hover = list(halo =list(size = 7))))
        
      }
      
      if (length(input$dist1) == 3){
        
        dt3 <- DistributionSelector(data = dt,
                          selectizer= input$dist1[3])
        
        dt3 <- FreqTooltip(d_frame = dt3,
                              unidade = unidade,
                              selectizer =input$dist1,
                              opt = "dist3")

        
        hc <- hc %>% hc_add_series(dt3,
                                   "line",
                                   hcaes(y=var, 
                                         x=rt,
                                         tt_p = tooltip_p), 
                                   color = "#2ecc71",
                                   name = DistNamesLongToShort(input$dist1[3]),
                                   marker = list(enabled = F,symbol = "circle",
                                                 states = list(hover = list(radius= 4))),
                                   states = list(hover = list(halo = list(size = 7))))
        
      }
      
    }
    
    
    hc <- hc %>%
      hc_add_series(dt_sample,
                    "line",
                    hcaes(y = var,
                          x = rt,
                          tt_p = tooltip_p),
                    color = "#333333",
                    name = "Sample",
                    lineWidth = 0 ,
                    marker = list (enabled = T,
                                   radius = 4,
                                   symbol = "cross"),
                    
                    states = list(hover = list(enabled = T,
                                               lineWidthPlus = 0,
                                               halo =list(size = 7)))) %>%
      hc_add_theme(hc_thm = hc_theme_scatter())  %>% 
      hc_xAxis(
        title = list(text = "<b>Return Period (years)</b>",
                     style = list(color = '#333333'),
                     margin=15),
        type = "logarithmic",
        min = 1 )  %>%
      
      hc_yAxis(
        title = list(text = AxisNameUnit(unidade = input$unidade),
                     style = list(color = '#333333'),
                     margin = 15))  %>%
      hc_legend(align = "center") %>%  
      hc_tooltip(
        useHTML = T,
        crosshairs= T,
        shared = T,
        headerFormat = "",
        pointFormat= paste('{point.tt_p}'),
        footerFormat = '</table>')  %>%
      hc_chart(zoomType = 'xy')
    
    
    hc
    
    
    })
  

  # Modules -----------------------------------------------------------------
  param.module <- callModule(ShowParameters, 
             id = "parameter_box", param.values = FitData())
  
  
})

# Kite Note.... log pearson III - baixa assimetria
