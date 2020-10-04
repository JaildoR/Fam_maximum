## Mann Whit - full test
### Otput - h.test 


mannwhitney.test <-
  function (rank,
            subsample,
            alternative = c("two.sided", "less", "greater")) {
    if (length(rank) != length(subsample))
      stop("rank/subsample length are not equal")
    
    if (any(!is.numeric(rank)))
      stop("'rank' must be a numeric")
    
    if (missing(alternative))
      alternative <- "two.sided"
    
    dt <- data.frame(rank = rank, subsample = subsample)
    
    summ <- dt %>% group_by(subsample) %>%
      tally(sort = T)
    
    factor <- summ$subsample[2]
    
    if (length(summ$n) != 2)
      stop("More than 2 Groups")
    
    alternative <- match.arg(alternative)
    DNAME <- deparse(substitute(rank))
    METHOD <- 'Mann-Whitney Test'
    
    n <- min(summ$n)
    m <- max(summ$n)
    
    mu <- n * m / 2
    sd <- sqrt(n * m * (n + m + 1) / 12)
    
    V1 <- n * m + (n * (n + 1) / 2) - sum(dt[dt$subsample == factor, "rank"])
    V2 <- n * m  - V1
    V <- min(V1, V2)
    
    STATISTIC <- (V - mu) / sd
    
    
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
      parameter = c(
        V = V,
        mu = mu,
        sd = sd,
        n = n,
        m = m),
      p.value = PVAL,
      method = METHOD,
      data.name = DNAME
    )
    
    class(result) <-  "htest"
    return(result)
    
    
  }

## Mann Whit - mask
### Otput - table
mw.test <- function(dt) {
  nomes <- c("Null H.:",
             "P-Value:",
             "Stat. (<i>T</i>):",
             "Stat. (<i>V</i>):",
             "Est. - E[<i>V</i>]:",
             "Est. - sd[<i>V</i>]:",
             "Sample(A):",
             "Sample(B):")
  
  test <-
    mannwhitney.test(
      rank = dt$rank,
      subsample = dt$subsample,
      alternative = "two.sided"
    )
  mw.summary <- c(
    "Homogenity",
    prettyNum(test[[4]] , decimal.mark = ".", digits = 3),
    prettyNum(test[[1]] , decimal.mark = ".", digits = 3),
    prettyNum(test[[3]][1] , decimal.mark = ".", digits = 3),
    prettyNum(test[[3]][2] , decimal.mark = ".", digits = 3),
    prettyNum(test[[3]][3] , decimal.mark = ".", digits = 3),
    prettyNum(test[[3]][4] , decimal.mark = ".", digits = 3),
    prettyNum(test[[3]][5] , decimal.mark = ".", digits = 3))
  
  tabela <- list(cbind(nomes, mw.summary), test[[4]])
  return(tabela)
}