
## Gev moments
gevfit.mme <- function(data) {
  # Fundamentals of Statistical Hydrology  Naghettinni (2017)
  if (!is.vector(data)) {
    stop("Data is not a vector")
  }
  if (!is.numeric(data)) {
    stop("Data is not a numeric")
  }
  
  n <- length(data)
  m <- mean(data)
  sd <- sd(data)
  skew <- skewness(data)
  
  if (skew == 1.1396) {
    stop("shape = 0")
  } else {
    if (skew > 1.1396) {
      domain <- c(-0.3333333, -0.00000001)
    } else {
      domain <- c(0.00001, 2)
    }
  }
  
  
  f <- function (shape)
    (skew - sign(shape) *
       (-gamma(1 + 3 * shape) + 3 * gamma(1 + shape) *
          gamma(1 + 2 * shape) - 2 * gamma(1 + shape) ^ 3) /
       (gamma(1 + 2 * shape) - gamma(1 + shape) ^ 2) ^
       (3 / 2))
  
  shape <- uniroot(f = f,interval = domain, tol = 5e-15)$root
  scale <- sd*abs(shape)/(sqrt(gamma(1+2*shape)-gamma(1+shape)^2))
  location <-  m -(scale/shape)*(1-gamma(1+shape))
  
  shape <- - shape
  
  estimate <-  c(location = location, scale = scale, shape = shape)
  
  
  l_lik <- dgev(data,location = estimate[1],scale = estimate[2],shape = estimate[3])
  l_lik<- sum(log(l_lik))
  
  aic <- -2*l_lik + 2*length(estimate)
  bic <- -2*l_lik + log(n)*length(estimate)
  
  fit <- list(estimate = estimate,
              method = 'mme',
              loglik = l_lik,
              aic = aic,
              bic = bic,
              n = n,
              distname = 'gev',
              data = data)
  return(fit)
  
  
}

## Gev l-moments
gevfit.lmo <- function(data) {
  n <- length(data)
  if (!is.vector(data)) {
    return(NULL)
  }
  if (!is.numeric(data)) {
    return(NULL)
  }
  
  sample_lmom <- samlmu(data)
  gev_est <- pelgev(lmom = sample_lmom)
  estimate <- c(
    location = unname(gev_est[1]),
    scale = unname(gev_est[2]),
    shape = unname(-gev_est[3])
  )
  
  
  # loglik
  
  l_lik <-
    dgev(data,
         location = estimate[1],
         scale = estimate[2],
         shape = estimate[3])
  l_lik <- sum(log(l_lik))
  
  aic <- -2 * l_lik + 2 * length(estimate)
  bic <- -2 * l_lik + log(n) * length(estimate)
  
  fit <- list(
    estimate = estimate,
    method = 'lmo',
    loglik = l_lik,
    aic = aic,
    bic = bic,
    n = n,
    distname = 'gev',
    data = data
  )
  return(fit)
  
  
}

## Gev max/all
gevfit <- function(data) {
  if (!is.vector(data)) {
    stop("Data is not a vector")
  }
  if (!is.numeric(data)) {
    stop("Data is not a numeric")
  }
  
  gev.mme <- try(gevfit.mme(data = data), silent = T)
  gev.lmo <- try(gevfit.lmo(data = data), silent = T)
  
  if (is(gev.mme, "try-error"))
    gev.mme <- NULL
  if (is(gev.lmo, "try-error"))
    gev.lmo <- NULL
  
  if (!is.null(gev.lmo)) {
    start = gev.lmo$estimate
  } else if (!is.null(gev.mme)) {
    start = gev.mme$estimate
  } else {
    start = c(
      location = mean(data),
      scale = sd(data),
      shape = skewness(data)
    )
  }
  
  start = as.list(start)
  
  gev.mle <- try(fitdist(data, dgev, method = "mle", start = start,
                         control = list(maxit=1000,reltol = 1e-15)))
  
  
  if (is(gev.mle, "try-error")) {
    gev.mle <- NULL
  } else {
    gev.mle <-
      gev.mle[c('estimate',
                'method',
                'loglik',
                'aic',
                'bic',
                'n',
                'distname',
                'data')]}
  
  all.est <- list(met1 = gev.mme,
                  met2 = gev.mle,
                  met3 = gev.lmo)
  return(all.est)
  
  
}