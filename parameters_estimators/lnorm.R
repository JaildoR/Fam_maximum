
## lnorm lmo
lnormfit.lmo <- function(data){
  n <- length(data)
  if (!is.vector(data)){return(NULL)}
  if (!is.numeric(data)){return(NULL)}
  
  sample_lmom <- samlmu(data)
  
  F1 <- (1 + sample_lmom[2]/sample_lmom[1])/2
  u <- qnorm(p = F1)
  sdlog <- 2*u/sqrt(2)
  meanlog <- log(sample_lmom[1])- sdlog^2/2
  
  
  estimate <- c(meanlog = unname(meanlog) , sdlog = unname(sdlog))
  
  
  # loglik
  
  l_lik <- dlnorm(data,meanlog = estimate[1],sdlog = estimate[2])
  l_lik<- sum(log(l_lik))
  
  aic <- -2*l_lik + 2*length(estimate) 
  bic <- -2*l_lik + log(n)*length(estimate)
  
  fit <- list(estimate = estimate,
              method = 'lmo',
              loglik = l_lik,
              aic = aic,
              bic = bic,
              n = n,
              distname = 'lnorm',
              data = data)
  return(fit)
  
  
}

## lnorm max/mme - 
lnormfit <- function(data){
  
  if (!is.vector(data)){stop("Data is not a vector")}
  if (!is.numeric(data)){stop("Data is not a numeric")}
  
  lnorm.mme <- try(fitdist(data,dlnorm,method = "mme"),silent = T)
  lnorm.mle <- try(fitdist(data,dlnorm,method = "mle"),silent = T)
  
  lnorm.lmo <- try(lnormfit.lmo(data),silent = T)
  
  if(is(lnorm.mme,"try-error")){
    lnorm.mme <- NULL 
  } else {
    lnorm.mme <- lnorm.mme[c('estimate','method','loglik','aic','bic','n','distname','data')]}
  
  
  if (is(lnorm.lmo,"try-error"))
    lnorm.lmo <- NULL
  
  
  if (is(lnorm.mle,"try-error")){
    lnorm.mle <- NULL
  } else {
    lnorm.mle <- lnorm.mle[c('estimate','method','loglik','aic','bic','n','distname','data')]
  }
  
  all.est <- list(met1 = lnorm.mme,met2 = lnorm.mle, met3 = lnorm.lmo)
  return(all.est)
  
  
}
