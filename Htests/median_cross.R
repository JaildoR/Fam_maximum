## Median Cross - full test
### Otput - h.test 
m.cross.test <-function (x,
                         alternative = c("two.sided",
                                         "less", 
                                         "greater")) {
  if (length(x) < 15)
    stop("Length less than 15 observations")
  if (any(!is.numeric(x)))
    stop("'x' must be a numeric vector")
  if (missing(alternative))
    alternative <- "two.sided"
  
  alternative <- match.arg(alternative)
  DNAME <- deparse(substitute(x))
  METHOD <- 'Median Crossing Test'
  
  med     <- median(x)
  med.vec <- ifelse(x > med, 1, 0)
  m       <- sum(abs(diff(med.vec, lag = 1)))
  
  n <- length(x)
  mean <- (n - 1) / 2
  vr   <- (n - 1) / 4
  
  STATISTIC <- (m - mean) / vr ^ 0.5
  
  
  PVAL <- switch(
    alternative,
    "two.sided" =  2 * pnorm(-abs(STATISTIC)),
    "greater"   = pnorm(STATISTIC, lower.tail = FALSE),
    "less"      = pnorm(STATISTIC)
  )
  
  
  
  names(STATISTIC) <- "Standard Normal"
  
  result <- list(
    statistic = STATISTIC,
    alternative = alternative,
    parameter = c(m = m, n = n,mean=mean,vr = vr),
    p.value = PVAL,
    method = METHOD,
    data.name = DNAME
  )
  
  class(result) <-  "htest"
  return(result)
}





## Median Cross - mask
### Otput - table
mc.test <- function(variavel) {
  nomes <- c("Null Hypothesis",
             "P-Value:",
             "Statistics (<i>Z</i>):",
             "Median Cr. (<i>C</i>):",
             "Estimate - E[<i>C</i>]:",
             "Estimate - Var[<i>C</i>]:",
             "Sample Size (<i>n</i>):")
  
  a <- m.cross.test(x = variavel)
  
  mc.sum <- c("Randomeness",
              prettyNum(a[[4]] , decimal.mark = ".", digits = 3),
              prettyNum(a[[1]] , decimal.mark = ".", digits = 3),
              
              prettyNum(a[[3]][1] , decimal.mark = ".", digits = 3),
              prettyNum(a[[3]][3] , decimal.mark = ".", digits = 3),
              prettyNum(a[[3]][4] , decimal.mark = ".", digits = 3),
              a[[3]][2])
  
  tabela <- list(cbind(nomes, mc.sum), a[[4]])
  return(tabela)
}