
## Mann Kendall- based
### Package - trend
mannkendall.test <- function(variavel) {
  nomes <- c("Null Hypothesis:",
             "P-Value:",
             "Statistics (<i>Z</i>):",
             "Statistics (<i>S</i>):",
             "Estimate - Var [<i>S</i>]:",
             "Sample Size (<i>n</i>):")
  
  a <- mk.test(x = variavel)
  
  mk.sum <- c(
    "Stationarity",
    prettyNum(a[[2]][1] , decimal.mark = ".", digits = 3),
    
    prettyNum(a[[3]][1] , decimal.mark = ".", digits = 3),
    prettyNum(a[[6]][1] , decimal.mark = ".", digits = 3),
    
    prettyNum(a[[6]][2] , decimal.mark = ".", digits = 3),
    a[[5]][1] 
  )
  
  tabela <- list(cbind(nomes, mk.sum), a[[2]])
  return(tabela)
}