# libs --------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(moments)
library(lmom)
library(DT)
library(highcharter)
library(trend)
library(classInt)
library(randtests)
library(FAdist)
library(fitdistrplus)
library(ADGofTest)
library(dplyr)
library(markdown)



# Modules 
source(file = "modules/Show_parameters.R", encoding = "UTF-8", local = T)

# Like Dictionaries functions ---------------------------------------------
## html units --> Convert unit to HTML label
### Linked to plots and descrptive stats table
### package used -- Shiny
HtmlUnit <- function(unidade) {
  if (unidade == 1) {
    html.unidade = paste("m", tags$sup(3), "/s", sep = "")
  } else if (unidade == 2) {
    html.unidade = "mm"
  } else if (unidade == 3) {
    html.unidade = "in"
  } else {
    html.unidade = "cfs"
  } 
  return(html.unidade)
}

## html axix 
AxisNameUnit <- function(unidade) {
  if (unidade == 1) {
    name = "<b>Discharge (m³/s)</b>"
  } else if (unidade == 2) {
    name = "<b>Rainfall Depth (mm)</b>"
  } else if (unidade == 3) {
    name = "<b>Rainfall Depth (in)</b>"
  } else {
    name = "<b>Discharge (cfs)</b>"
  } 
  return(name)
}

## type units for tooltips
TypeUnit <- function(unidade) {
  if (unidade == 1) {
    name = "Discharge:"
  } else if (unidade == 2) {
    name = "Rainfall Depth:"
  } else if (unidade == 3) {
    name = "Rainfall Depth:"
  } else {
    name = "Discharge:"
  } 
  return(name)
}

## Shorter names for distribution names
DistNamesLongToShort <- function(long_name){
  short <- switch(long_name,
                  "Gumbel - Method of Moments Estimators"           = "Gumbel (Moments)",
                  "Gumbel - Maximum Likelihood Estimators"          = "Gumbel (Max. Likelihood)",
                  "Gumbel - L-moments Estimators"                   = "Gumbel (L-Moments)",
                  "GEV - Method of Moments Estimators"              = "GEV (Moments)",
                  "GEV - Maximum Likelihood Estimators"             = "GEV (Max Likelihood)",
                  "GEV - L-moments Estimators"                      = "GEV (L-Moments)",
                  "Gamma - Method of Moments Estimators"            = "Gamma (Moments)",
                  "Gamma - Maximum Likelihood Estimators"           = "Gamma (Max. Likelihood)",
                  "Gamma - L-moments Estimators"                    = "Gamma - L-Moments",
                  "Log Normal (2P) - Method of Moments Estimators"  = "Log Normal 2P (Moments)" ,
                  "Log Normal (2P) - Maximum Likelihood Estimators" = "Log Normal 2P (Max. Likelihood)",
                  "Log Normal (2P) - L-moments Estimators"          = "Log Normal 2P (L-Moments)" ,
                  "Log Normal (3P) - Method of Moments Estimators"  = "Log Normal 3P (Moments)" ,
                  "Log Normal (3P) - Maximum Likelihood Estimators" = "Log Normal 3P (Max. Likelihood)",
                  "Log Normal (3P) - L-moments Estimators"          = "Log Normal 3P (L-Moments)",
                  "NEW")
  return(short)
  
}

# Even shorter names dist for plots labels
DistNameSP <- function(name){
  
  short <- recode(name,
                  "gumbel"  = "Gumbel",
                  "gamma"   = "Gamma",
                  "gev"     = "GEV",
                  "lnorm"   = "L-Normal (2P)",
                  "ln3"     = "L-Normal (3P)",
                  "gamma3"  = "Peason TIII",
                  "lgamma3" = "L-Pearson TIII",
                  .default = "NEW")
  return(short)
  
}

# long names dist for plots tooltips
DistNameLP <- function(name){
  
  long <- recode(name,
                 "gumbel"   = "Gumbel (EV Type I)",
                 "gamma"    = "Gamma",
                 "gev"      = "Generalized Extreme Value",
                 "lnorm"    = "Log-Normal (2 Parameters)",
                 "ln3"      = "Log-Normal (3 Parameters)",
                 "gamma3"   = "Peason Type III",
                 "lgamma3"  = "Log-Pearson Type III",
                 .default    = "NEW")
  return(long)
  
}

# short names method for plots labels
MethodNameLP <- function(name){
  
  long <- recode(name,
                 "mme" = "Method of Moments",
                 "mle" = "Maximum Likelihood",
                 "lmo" = "L-moments",
                 .default = "NEW")
  return(long)
}

# short names method for plots labels
MethodNameSP <- function(name){
  
  short <- recode(name,
                  "mme" = "MME",
                  "mle" = "MLE",
                  "lmo" = "LMOM",
                  .default = "NEW")
  return(short)
}



# Binning Data ------------------------------------------------------------

## Knuth binning
### linked to BinData -- Histogram -- Chi-square*(Future)
###   # https://arxiv.org/pdf/physics/0605197.pdf
KnuthOptbins <- function (data) {
  # Based   Knuth 2012 -- Optimal Data-Based Binning for Histograms
  # https://arxiv.org/pdf/physics/0605197.pdf
  
  n       <- length(data)
  maximum <- max(data)
  minimum <- min(data)
  range   <- maximum - minimum
  maxbins <- ceiling(n / 4)
  logp <- NULL
  
  for (i in 2:maxbins) {
    bin.width <- range / i
    N <- as.vector(table(cut(data, breaks = i, dig.lab = 5)))
    logp[i] = n * log(i) + lgamma(i / 2) - 
      lgamma(n + i / 2) - i * lgamma(1/2) + sum(lgamma(N + 0.5))
  }
  
  vec <- data.frame(bin.numbers = seq(1, maxbins, 1), logp)
  vec <- vec[order(vec$logp, decreasing = T), ]
  
  return(vec[1, 1]) #bin numbers
}

## shimazaki shinomoto binning
### linked to BinData -- Histogram -- Chi-square*(Future)
### http://176.32.89.45/~hideaki/res/histogram.html
ShiShiOptbins <- function(data) {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Based on 2006 Hideaki Shimazaki
  # Department of Physics, Kyoto University
  # shimazaki at ton.scphys.kyoto-u.ac.jp
  # http://176.32.89.45/~hideaki/res/histogram.html
  
  
  n       <- length(data)
  maximum <- max(data)
  minimum <- min(data)
  maxbins <- ceiling(n / 4)
  
  N <- 1:maxbins
  Cost <- NULL
  bin.width <- NULL
  
  for (i in 2:length(N)) {
    bin.width <- (maximum - minimum) / N[i]
    
    ki <- as.vector(table(cut(data, breaks = i, dig.lab = 5)))
    
    k <- mean(ki)
    v <- sum((ki - k) ^ 2) / (N[i])
    
    Cost[i] <- (2 * k - v) / (bin.width) ^ 2	#Cost Function
  }
  
  idx <- which.min(Cost)
  bin.numbers <- N[idx]
  
  return(bin.numbers)
}

## Rule based binning data
### linked to Histogram/Chi-square*(Future)
BinData <- function(data, rule, type) {
  
  n       <- length(data)
  maximum <- max(data)
  minimum <- min(data)
  
  range   <- maximum - minimum
  iqr     <- IQR(data)
  st.dev  <- sd(data)
  
  if (rule == "st") {
    bin.numbers   <- ceiling(log2(n) + 1)
    bin.width     <-  range / bin.numbers
    
  }
  
  else if (rule == "sc") {
    bin.width     <-  3.5 * st.dev / n ^ (1 / 3)
    bin.numbers   <-  ceiling(range / bin.width)
    new_range     <-  bin.width * bin.numbers
    minimum <- minimum - (new_range - range) / 2
    maximum <- maximum + (new_range - range) / 2
    #
    dummydata <- seq(minimum, maximum, length.out = bin.numbers * 2)
    
  }
  else if (rule == "fd") {
    bin.width     <- 2 * iqr / n ^ (1 / 3)
    bin.numbers   <-  ceiling(range / bin.width)
    new_range     <-  bin.width * bin.numbers
    minimum       <- minimum - (new_range - range) / 2
    maximum       <- maximum + (new_range - range) / 2
    #
    dummydata <- seq(minimum, maximum, length.out = bin.numbers * 2)
    
    
  }
  else if (rule == "kn") {
    bin.numbers <- KnuthOptbins(data = data)
    bin.width   <- range / bin.numbers
  }
  else {
    bin.numbers  <- ShiShiOptbins(data = data)
    bin.width   <- range / bin.numbers
    
  }
  if (exists('dummydata')) {
    c_int <- classIntervals(dummydata, n = bin.numbers, style = 'equal')
  } else {
    c_int <- classIntervals(data, n = bin.numbers, style = 'equal')
  }
  
  bins <- as.data.frame(table(cut(data, breaks = c_int$brks, include.lowest = T)))
  
  bins$xmin_    <- c_int$brks[-(length(c_int$brks))]
  bins$xmax_    <- c_int$brks[-1]
  bins$width_   <- bins$xmax_ - bins$xmin_
  bins$x_       <- bins$xmin_ + bins$width_ / 2
  bins$count_   <- bins$Freq
  bins$cumct_   <- cumsum(bins$Freq)
  bins$density_ <- bins$Freq / (sum(bins$Freq))
  bins$prob_    <- cumsum(bins$Freq) / (sum(bins$Freq))
  
  bins$Freq     <- NULL
  bins$Var1     <- NULL
  
  if (type == 'od') {
    bins$main_ <- bins$count_
  } else if (type == 'cf') {
    bins$main_ <- bins$cumct_
  } else if (type == 'rf') {
    bins$main_  <- bins$density_
  } else if (type == 'crf') {
    bins$main_ <- bins$prob_
  } else {
    bins$main_ <- bins$count_
  }
  
  binstoplot <- data_frame(min       = bins$xmin_,
                           x_        = bins$x_,
                           main      = bins$main_,
                           width     = bins$width_,
                           dens_p    = signif(x = bins$density_, digits = 4),
                           min_p     = signif(x = bins$xmin_, digits = 4),
                           max_p     = signif(x = bins$xmax_, digits = 4),
                           count_p   = signif(x = bins$count_, digits = 4),
                           cumct_p   = signif(x = bins$cumct_, digits = 4),
                           cumfreq_p = signif(x = bins$prob_, digits = 4))
                     
  
  return(binstoplot)
}

# Descriptive stats -------------------------------------------------------
# Should stay in global --- uses unit names in html (HtmlUnit)

## table conventional moments
### See HtmlUnit - general
ConvMoment <- function(variavel, unidade) {
  
  html.unidade <- HtmlUnit(unidade = unidade)
  
  nomes <-c("Mean:","CV:","Variance:","Skewness:","Kurtosis:")
  
  momentos <- c(mean(variavel, na.rm = T),
                (sd(variavel, na.rm = T)/mean(variavel, na.rm = T)),
                var(variavel, na.rm = T),
                skewness(variavel, na.rm = T),
                kurtosis(variavel, na.rm = T))
  
  unidades <- c(HTML(html.unidade),
                "-",
                HTML(paste("(", html.unidade, ")", tags$sup(2), sep = "")),
                "-",
                "-")
  
  momentos <- prettyNum(momentos, decimal.mark = ".", digits = 4)
  
  tabela <- cbind(nomes, momentos, unidades)
  
  return(tabela)
}


## l-moments
### See HtmlUnit - general
LMoment <- function(variavel, unidade) {
  
  html.unidade <- HtmlUnit(unidade = unidade)
  sample.lmoments <- samlmu(variavel)
  
  
  names <- c(HTML(paste("L", tags$sub(1), ":", sep = "")),
             "L-CV:",
             HTML(paste("L", tags$sub(2), ":", sep = "")),
             "L-Skew:",
             "L-Kurtosis:")
  
  l.moments <- c(sample.lmoments[1], 
                sample.lmoments[2]/sample.lmoments[1],
                sample.lmoments[2],
                sample.lmoments[3],
                sample.lmoments[4])
  

  
  unidades <- c(HTML(html.unidade),
                "-",
                "-",
                "-",
                "-")
  
  l.moments <- prettyNum(l.moments, decimal.mark = ".", digits = 4)
  tabela <- cbind(names, l.moments, unidades)
  row.names(tabela) <- NULL
  return(tabela)
}

## Others sample stats....
### See HtmlUnit - general
sumario <- function(variavel, unidade) {
  
  names <- c("Median:",
             "Range:",
             "Minimum:",
             "Maximum:",
             "IQ range:")
  
  result <- summary(variavel)
  
  stats.sum <- c(median(variavel, na.rm = T),
                 result[6] - result[1],
                 min(variavel, na.rm = T),
                 max(variavel, na.rm = T),
                 result[5] - result[2])
  
  stats.sum <- prettyNum(stats.sum , decimal.mark = ".", digits = 4)
  
  html.unidade <- HtmlUnit(unidade = unidade)
  
  unidades <- c(
    HTML(html.unidade),
    HTML(html.unidade),
    HTML(html.unidade),
    HTML(html.unidade),
    HTML(html.unidade)
  )
  tabela <- cbind(names, stats.sum, unidades)
  row.names(tabela) <- NULL
  return(tabela)
}

# h test ------------------------------------------------------------------
# Statiscal tests H-tests #
# Masks for test

  #### folder Htests


# Fit distributions -------------------------------------------------------
  #### folder parameters_estimators
# Plots -------------------------------------------------------------------

### coloring  P-values
PvalueColor <- function (pval) {
  color_gen <- c("#7F3235", "#7F3235", "#7F3235", "#7F3235", "#8F3533", "#9F3932", "#AF3C31", 
                 "#C04030", "#D0432F", "#D84F32", "#DB6038", "#DE703D", "#E18042", "#E49148", 
                 "#DA964C", "#B5874F", "#907852", "#6B6855", "#465958", "#214A5B", "#214A5B", 
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B",
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B",
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B",
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", 
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", 
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", 
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", 
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", 
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", 
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B",
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", 
                 "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B", "#214A5B")
  
  return(color_gen[round(pval * 100)])
}

## labs for tooltips
labfy <- function(x,sufix,digit,dec.mark){
  lab <- prettyNum(x,decimal.mark =dec.mark, digits = digit)
  lab <- as.character(lab)
  lab <- paste(lab,sufix,sep = " ")
  return(lab)
}

## Hist tooltips
HistogramTooltip <- function(type){
  ifelse(test = type == 'od'| type == 'cf',
         yes = "<small> [{point.min} , {point.max}) </small><br>Count: 
         <b>{point.count}</b><br>Cumulative: <b>{point.cumct}</b>",
         no  = "<small> [{point.min} , {point.max}) </small><br>Density: 
         <b>{point.dens}</b><br>Cumulative: <b>{point.cumfreq}</b>" )}

## Hist Y-axix
HistogramAxis <- function(type){
  if (type == "od") {
    name = "<b>Frequency</b>"
  } else if (type == "cf") {
    name = "<b>Cumulative Frequency</b>"
  } else if (type == "rf") {
    name = "<b> Density</b>"
  } else {
    name = "<b>Probability</b>"
  } 
  return(name)
}

# Choose a distribution by name
DistributionSelector <- function (data,
                        selectizer) {
  
  if(selectizer =="Gumbel - Method of Moments Estimators"){
    dt <- data[,c("rt","gum_mme")]
  }
  
  if(selectizer =="Gumbel - Maximum Likelihood Estimators"){
    dt <- data[,c("rt","gum_mle")]
  }
  
  
  if(selectizer =="Gumbel - L-moments Estimators"){
    dt <- data[,c("rt","gum_lmo")]
  }
  
  if(selectizer =="GEV - Method of Moments Estimators"){
    dt <- data[,c("rt","gev_mme")]
    
  }
  
  if(selectizer =="GEV - Maximum Likelihood Estimators"){
    dt <- data[,c("rt","gev_mle")]
    
  }
  
  if(selectizer == "GEV - L-moments Estimators"){
    dt <- data[,c("rt","gev_lmo")]
  }
  
  if(selectizer == "Gamma - Method of Moments Estimators"){
    dt <- data[,c("rt","gamma_mme")]
  }
  
  if(selectizer == "Gamma - Maximum Likelihood Estimators"){
    dt <- data[,c("rt","gamma_mle")]
  }  
  
  if(selectizer == "Gamma - L-moments Estimators"){
    dt <- data[,c("rt","gamma_lmo")]
  }
  
  
  if(selectizer ==  "Log Normal (2P) - Method of Moments Estimators"){
    dt <- data[,c("rt","lnorm_mme")]
  }
  
  if(selectizer == "Log Normal (2P) - Maximum Likelihood Estimators"){
    dt <- data[,c("rt","lnorm_mle")]
  }  
  
  if(selectizer == "Log Normal (2P) - L-moments Estimators"){
    dt <- data[,c("rt","lnorm_lmo")]
  }
  
  if(selectizer ==  "Log Normal (3P) - Method of Moments Estimators"){
    dt <- data[,c("rt","ln3_mme")]
  }
  
  if(selectizer == "Log Normal (3P) - Maximum Likelihood Estimators"){
    dt <- data[,c("rt","ln3_mle")]
  }  
  
  if(selectizer == "Log Normal (3P) - L-moments Estimators"){
    dt <- data[,c("rt","ln3_lmo")]
  }
  
  
  names(dt) <- c("rt","var")
  return(dt)
  
}

# Function for plots position
PlotPosition <- function(data,method = c("weibull",
                                     "bloom",
                                     "gringorten",
                                     "cunnane",
                                     "median",
                                     "hazen")){
  if (!is.vector(data)) {
    stop("Data is not a vector")
    }
  
  if (!is.numeric(data)){
    stop("Data is not a numeric")
  }
  if(missing(method)) {
    method <- "weibull" 
  }
  
  
  method <- match.arg(method)
  
  n <- length(data)
  
  data <- sort(data,decreasing = F)
  data <- data.frame(var = data)
  data$rank <- order(data$var) # Order faster than seq()...
  
  if(method == "weibull"){
    a <- 0 
  }

  if(method == "bloom")
    a <- 0.375 
  
  if(method == "cunnane")
    a <- 0.4 
  if(method == "gringorten")
    a <- 0.44 
  if(method == "median")
    a <- 0.3175 
  if(method == "hazen")
    a <- 0.5 
  
  data$probability <- (data$rank - a)/(n + 1 - 2 * a)
  data$rank <- NULL
  return(data)
}


# Tooltip for freq plot
## need </table> for closure
## Uses DistNamesLongToShort function
FreqTooltip <-  function (d_frame,
                            unidade,
                            selectizer,
                            opt) {
  
  
  if(length(selectizer) ==F ){
    name = "Sample"
    color = "#333333"
  } else {
    name <- switch(opt,
                   "dist1"  = DistNamesLongToShort(selectizer[1]),
                   "dist2"  = DistNamesLongToShort(selectizer[2]),
                   "dist3"  = DistNamesLongToShort(selectizer[3]),
                   "sample" = "Sample")
    
    color <- switch(opt,
                    "dist1"  = "#d35400",
                    "dist2"  = "#2980b9",
                    "dist3"  = "#2ecc71",
                    "sample" = "#333333")
  }

  
  header_p1 <- "<small> Return Period: <b>" # add rt_prettyfied
  header_p2 <- " years</b></small><table>"
  
  point_p1  <- paste0("<tr><td style = 'color:",
                      color,
                      "'> ● ",name,
                      ": </td>",
                      "<td style='text-align: right; padding: 3px'>") ## ad var
  
  point_p2 <- paste0(" ",unidade,"</td></tr>")
  

  
  if (opt== 'dist1'){
    d_frame$tooltip_p <- paste0(header_p1,
                                prettyNum(d_frame$rt, decimal.mark = ".", digits = 4),
                                header_p2,
                                point_p1,
                                prettyNum(d_frame$var, decimal.mark = ".", digits = 4),
                                
                                point_p2)
  } else if (opt =='dist2'| opt =='dist3') {
    
    d_frame$tooltip_p <- paste0(point_p1,
                                prettyNum(d_frame$var, decimal.mark = ".", digits = 4),
                                point_p2)
    
  } else if (opt == "sample"){
    if(length(selectizer) == F ){
      d_frame$tooltip_p <- paste0(header_p1,
                                  prettyNum(d_frame$rt, decimal.mark = ".", digits = 4),
                                  header_p2,
                                  point_p1,
                                  prettyNum(d_frame$var, decimal.mark = ".", digits = 4),
                                  point_p2)
    } else {
      d_frame$tooltip_p <- paste0(point_p1,
                                  prettyNum(d_frame$var, decimal.mark = ".", digits = 4),
                                  point_p2)
      }
  
  }
  
  
  return(d_frame)
  
}

hc_theme_scatter <- function(...){
  
  theme <-
    hc_theme(
      colors = c('#333333',"#d35400", "#2980b9", "#2ecc71",
                 "#f1c40f", "#2c3e50", "#7f8c8d"),
      chart = list(
        style = list(
          fontFamily = "Roboto",
          color = "#666666"
        ),
        spacingLeft   = 2,
        spacingRight  = 2,
        spacingBottom = 5
      ),
      title = list(
        align = "left",
        style = list(
          fontFamily = "Roboto Condensed",
          fontWeight = "bold"
        )
      ),
      subtitle = list(
        align = "left",
        style = list(
          fontFamily = "Roboto Condensed"
        )
      ),
      legend = list(
        align = "right",
        verticalAlign = "bottom"
      ),
      xAxis = list(
        labels = list(style = list(color = '#555555')),
        gridLineWidth = 1,
        lineWidth = 1,
        gridLineColor = "#eeeeee",
        lineColor = "#888888",
        minorGridLineColor = "#transparent",
        tickColor = "#888888",
        tickWidth = 1,
        tickLength = 5
      ),
      yAxis = list(
        labels = list(style = list(color = '#555555')),
        gridLineWidth = 1,
        lineWidth = 1,
        gridLineColor = "#eeeeee",
        lineColor = "#888888",
        minorGridLineColor = "#transparent",
        tickColor = "#888888",
        tickWidth = 1,
        tickLength = 5
      ),
      plotOptions = list(
        line = list(
          marker = list(enabled = FALSE)
        ),
        spline = list(
          marker = list(enabled = FALSE)
        ),
        area = list(
          marker = list(enabled = FALSE)
        ),
        areaspline = list(
          marker = list(enabled = FALSE)
        ),
        arearange = list(
          marker = list(enabled = FALSE)
        ),
        bubble = list(
          maxSize = "10%"
        )
      )
    )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  } 
  
  theme
  
}

  
