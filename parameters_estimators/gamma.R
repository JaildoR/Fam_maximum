
## Gamma  l-moments
gammafit.lmo <- function(data){
  n <- length(data)
  if (!is.vector(data)){return(NULL)}
  if (!is.numeric(data)){return(NULL)}
  
  sample_lmom <- samlmu(data)
  gamma_est <- pelgam(lmom = sample_lmom)
  estimate <- c(shape = unname(gamma_est[1]), rate = 1/unname(gamma_est[2]))
  
  
  # loglik
  
  l_lik <- dgamma(data,shape = estimate[1],rate = estimate[2])
  l_lik<- sum(log(l_lik))
  
  aic <- -2*l_lik + 2*length(estimate) 
  bic <- -2*l_lik + log(n)*length(estimate)
  
  fit <- list(estimate = estimate,
              method = 'lmo',
              loglik = l_lik,
              aic = aic,
              bic = bic,
              n = n,
              distname = 'gamma',
              data = data)
  return(fit)
  
  
}

## Gamma max/mme
gammafit <- function(data){
  
  if (!is.vector(data)){stop("Data is not a vector")}
  if (!is.numeric(data)){stop("Data is not a numeric")}
  
  gamma.mme <- try(fitdist(data,dgamma,method = "mme"),silent = T)
  gamma.mle <- try(fitdist(data,dgamma,method = "mle",lower = c(0, 0)),silent = T)
  
  gamma.lmo <- try(gammafit.lmo(data),silent = T)
  
  if(is(gamma.mme,"try-error")){
    gamma.mme <- NULL 
  } else {
    gamma.mme <- gamma.mme[c('estimate','method','loglik','aic','bic','n','distname','data')]}
  
  
  if (is(gamma.lmo,"try-error"))
    gamma.lmo <- NULL
  
  
  if (is(gamma.mle,"try-error")){
    gamma.mle <- NULL
  } else {
    gamma.mle <- gamma.mle[c('estimate','method','loglik','aic','bic','n','distname','data')]
  }
  
  all.est <- list(met1 = gamma.mme,met2 = gamma.mle, met3 =gamma.lmo)
  return(all.est)
  
  
}
