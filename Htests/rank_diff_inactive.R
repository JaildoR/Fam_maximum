
## Rankdiff test - full test
### Otput - h.test
rankdiff.test <-
  function(x,
           alternative = c("two.sided", "less", "greater")) {
    if (length(x) < 15)
      stop("Length less than 15 observations")
    if (any(!is.numeric(x)))
      stop("'x' must be a numeric vector")
    if (missing(alternative))
      alternative <- "two.sided"
    
    alternative <- match.arg(alternative)
    DNAME <- deparse(substitute(x))
    METHOD <- 'Rank Difference Test'
    
    n <- length(x)
    mean <- (n + 1) * (n - 1) / 3
    vr   <- (n + 1) * (n - 2) * (4 * n - 7) / 90
    
    ranked.x <- rank(x)
    U <- sum(abs(diff(ranked.x)))
    
    STATISTIC  <- (U - mean) / vr ^ 0.5
    
    
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
      parameter = c(U = U, n = n),
      p.value = PVAL,
      method = METHOD,
      data.name = DNAME
    )
    
    class(result) <-  "htest"
    return(result)
    
  }

## Rankdiff test - mask
### Otput - table

rd.test <- function(variavel) {
  nomes <- c("Null Hypothesis:",
             "Sum Rank Diff:",
             "Statistics:")
  
  a <- rankdiff.test (x = variavel)
  
  rd.sum <- c("Randomeness",
              a[[3]][1],
              prettyNum(a[[1]] , decimal.mark = ".", digits = 3))
  
  tabela <- list(cbind(nomes, rd.sum), a[[4]])
  return(tabela)
}