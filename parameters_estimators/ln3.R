

ln3fit.mme <- function(data){
  if (!is.vector(data)){stop("Data is not a vector")}
  if (!is.numeric(data)){stop("Data is not a numeric")}
  
  # estimates
  n <- length(data)
  sample_cs <- skewness(data)
  sample_mean <- mean(data) 
  sample_sd <- sd(data)
  
  w <- (-sample_cs + sqrt(sample_cs^2 + 4))/2
  z2 <- (1-w^(2/3))/w^(1/3)
  
  sigma <- (log(z2^2 + 1))^(1/2)
  
  mu <- log(sample_sd/z2) - 0.5 * log(z2^2 + 1)
  
  zeta <- sample_mean - sample_sd/z2
  
  
  estimate <- c(shape = sigma, scale = mu, thres = zeta)
  
  # loglik
  
  l_lik <- dlnorm3(data , shape = estimate[1], 
                   scale = estimate[2],thres = estimate[3])
  l_lik<- sum(log(l_lik))
  
  aic <- -2*l_lik + 2*length(estimate) 
  bic <- -2*l_lik + log(n)*length(estimate)
  
  fit <- list(estimate = estimate,
              method = 'mme',
              loglik = l_lik,
              aic = aic,
              bic = bic,
              n = n,
              distname = 'ln3',
              data = data)
  return(fit)
}

ln3fit.lmo <- function(data){
  n <- length(data)
  if (!is.vector(data)) {
    return(NULL)
  }
  if (!is.numeric(data)) {
    return(NULL)
  }
  
  sample_lmom <- samlmu(data)
  ln3_est <- pelln3(lmom = sample_lmom)
  
  estimate <- c(
    shape = unname(ln3_est[3]),
    scale = unname(ln3_est[2]),
    thres = unname(ln3_est[1]))
  
  
  
  l_lik <- dlnorm3(data , shape = estimate[1], 
                   scale = estimate[2],thres = estimate[3])
  l_lik<- sum(log(l_lik))
  
  aic <- -2*l_lik + 2*length(estimate) 
  bic <- -2*l_lik + log(n)*length(estimate)
  
  fit <- list(estimate = estimate,
              method = 'mlo',
              loglik = l_lik,
              aic = aic,
              bic = bic,
              n = n,
              distname = 'ln3',
              data = data)
  
  return(fit)
  
  
  
  
}

ln3fit <- function(data){
  if (!is.vector(data)) {
    stop("Data is not a vector")
  }
  if (!is.numeric(data)) {
    stop("Data is not a numeric")
  }
  
  
  ln3.mme <- try(ln3fit.mme(data = data), silent = T)
  ln3.lmo <- try(ln3fit.lmo(data = data), silent = T)
  
  if (is(ln3.mme, "try-error"))
    ln3.mme <- NULL
  if (is(ln3.lmo, "try-error"))
    ln3.lmo <- NULL
  
  if (!is.null(ln3.mme)) {
    start = ln3.mme$estimate
  } else if (!is.null(ln3.lmo)) {
    start = ln3.lmo$estimate
  } else {
    start = c(
      shape = sd(log(data)),
      scale = mean(log(data)),
      thres = 0
    )
  }
  
  start = as.list(start)
  
  ln3.mle <- try(fitdist(data, dlnorm3, method = "mle", start = start,
                         control = list(maxit=1000,reltol = 1e-15)),silent = T)
  
  
  if (is(ln3.mle, "try-error")) {
    ln3.mle <- NULL
  } else {
    ln3.mle <-ln3.mle[c('estimate',
                'method',
                'loglik',
                'aic',
                'bic',
                'n',
                'distname',
                'data')]
    
    ln3.mle$distname <- 'ln3'
    
    }
  
  
  all.est <- list(met1 = ln3.mme,
                  met2 = ln3.mle,
                  met3 = ln3.lmo)
  
  
}
