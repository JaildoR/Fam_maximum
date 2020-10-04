## waldwolf - based
### Package - trend
## Modify (estimates)
w_w.test <- function(x) {
  DNAME <- deparse(substitute(x))
  stopifnot(is.numeric(x))
  x <- na.omit(x)
  
  n <- length(x)
  if (n < 2) {
    stop("sample size must be greater than 1")
  }
  
  # The test statistic r
  r <- sum(x[1:(n-1)] * x[2:n]) + x[1] * x[n]
  
  s1 <- sum(x^1)
  s2 <- sum(x^2)
  s3 <- sum(x^3)
  s4 <- sum(x^4)
  
  # expected value of r
  er <- (s1^2 - s2) / (n - 1)
  
  # expected variance of r
  vr <- (s2^2 - s4) / (n - 1) - er^2 +
    (s1^4 - 4 * s1^2 * s2 + 4 * s1 * s3 + s2^2 - 2 * s4) /
    ((n - 1) * (n - 2))
  
  # Standardised quantile (approaches normal distribution)	  
  z <- (r - er) / sqrt(vr)
  
  # p-value, two-sided case
  pval <- 2 * min(0.5, pnorm(abs(z), lower.tail = FALSE))
  
  out <- list(statistic = c(z = z),
              p.value= pval,
              alternative = "The series is significantly different from \n independence and stationarity",
              method = "Wald-Wolfowitz test for independence and stationarity",
              data.name = DNAME,
              parameter = c(n = n, r = r,  er = er ,vr = vr))
  
  class(out) <- "htest"
  return(out)
}

waldwolf.test <- function(variavel) {
  nomes <- c("Null Hypothesis:",
             "P-Value:",
             "Statistics (<i>Z</i>):",
             "Statistics (<i>R</i>):",
             "Estimate - E[<i>R</i>]:",
             "Estimate - Var[<i>R</i>]:",
             "Sample size (<i>n</i>):")
  
  a <- w_w.test(x = variavel)
  
  ww.sum <- c("Independence",
              prettyNum(a[[2]] , decimal.mark = ".", digits = 3),
              prettyNum(a[[1]] , decimal.mark = ".", digits = 3),
              
              prettyNum(a[[6]][2] , decimal.mark = ".", digits = 3),
              prettyNum(a[[6]][3] , decimal.mark = ".", digits = 3),
              prettyNum(a[[6]][4] , decimal.mark = ".", digits = 3),
              prettyNum(a[[6]][1] , decimal.mark = ".", digits = 3))
  
  tabela <- list(cbind(nomes, ww.sum), a[[2]])
  return(tabela)
}
