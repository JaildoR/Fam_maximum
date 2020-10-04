## Turning points - based
### Otput - h.test to table
### Package -- Randtests
tp.test <- function(variavel) {
  nomes <- c("Null Hypothesis:",
             "P-Value:",
             "Statistics (<i>Z</i>):",
             "Turning points (<i>p</i>) :",
             "Sample size (<i>n</i>):")
  
  a <- turning.point.test(x = variavel)
  
  tp.sum <- c("Randomeness",
              prettyNum(a[[3]],decimal.mark = ".", digits = 3),
              prettyNum(a[[1]],decimal.mark = ".", digits = 3),
              a[[2]],
              a[[6]])
  
  tabela <- list(cbind(nomes, tp.sum), a[[3]])
  return(tabela)
}