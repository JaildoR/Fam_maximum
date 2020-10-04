## Spearman - based -- cor.test
### Package - stats
spearman.test <- function(var, year) {
  n <- length(var)
  
  test_results <- cor.test(x = var, 
                           y = year, 
                           method = "spearman")
  
  names <- c("Null Hypothesis:",
             "P-Value:",
             "Statistic (<i>T</i>):",
             "Estimate (<i>r<sub>s</sub></i>):",
             "Sample Size (<i>n</i>):")
  
  statistic_t <- test_results[[4]][1]*sqrt((n-2)/(1-test_results[[4]][1]^2))
  
  sprho.sum <- c("Stationarity",
                 prettyNum(test_results[[3]][1], decimal.mark = ".", digits = 3),
                 prettyNum(  statistic_t       , decimal.mark = ".", digits = 3),
                 prettyNum(test_results[[4]][1], decimal.mark = ".", digits = 3),
                 prettyNum(n, format = "f", digits = 0)) 
  
  
  tabela <- list(cbind(names, sprho.sum), test_results[[3]])
  return(tabela)
}
