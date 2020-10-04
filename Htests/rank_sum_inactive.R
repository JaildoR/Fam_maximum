


## Ranksum test - full test - Inactive
### Otput - h.test 
ranksum.test <-
  function (rank,
            subsample,
            alternative = c("two.sided", "less", "greater")) {
    if (length(rank) != length(subsample))
      stop("rank/subsample length  are not equal")
    
    if (any(!is.numeric(rank)))
      stop("'' must be a numeric")
    
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
    METHOD <- 'Rank Sum Test'
    
    n <- min(summ$n)
    m <- max(summ$n)
    N <- max(dt$rank)
    
    mu <- n * (N + 1) / 2
    sd <- sqrt(n * m * (N + 1) / 12)
    
    S <- sum(dt[dt$subsample == factor, "rank"])
    
    if (S == mu) {
      STATISTIC = 0
    } else if (S > mu) {
      STATISTIC <- (S - 0.5 - mu) / sd
    } else {
      STATISTIC <- -abs((S + 0.5 - mu) / sd)
    }
    
    
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
        S = S,
        mu = mu,
        sd = sd,
        n = n
      ),
      p.value = PVAL,
      method = METHOD,
      data.name = DNAME
    )
    
    class(result) <-  "htest"
    return(result)
    
    
  }



## Ranksum test - mask - Inactive
### Otput - table

rcs.test <- function(dt) {
  nomes <- c("Null Hypothesis:",
             "Statistics (S):",
             "Smaller Group :")
  
  a <-
    ranksum.test(
      rank = dt$rank,
      subsample = dt$subsample,
      alternative = "two.sided"
    )
  rcs.sum <- c(
    "Homogenity",
    prettyNum(a[[3]][1] , decimal.mark = ".", digits = 3),
    prettyNum(a[[3]][4] , decimal.mark = ".", digits = 1)
  )
  
  tabela <- list(cbind(nomes, rcs.sum), a[[4]])
  return(tabela)
}