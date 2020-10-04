
## Gumble moments
gumfit.mme <- function(data) {
  if (!is.vector(data)) {
    stop("Data is not a vector")
  }
  if (!is.numeric(data)) {
    stop("Data is not a numeric")
  }
  # est
  n <- length(data)
  m <- mean(data, na.rm = T)
  Sx <- sd(data, na.rm = T)
  
  estimate <- c(scale = Sx / 1.283,
                location = m - 0.45 * Sx)
  
  # loglik
  
  l_lik <- dgumbel(data, scale = estimate[1], location = estimate[2])
  l_lik <- sum(log(l_lik))
  
  aic <- -2 * l_lik + 2 * length(estimate)
  bic <- -2 * l_lik + log(n) * length(estimate)
  
  fit <- list(
    estimate = estimate,
    method = 'mme',
    loglik = l_lik,
    aic = aic,
    bic = bic,
    n = n,
    distname = 'gumbel',
    data = data
  )
  return(fit)
}

## Gumble l-moments
gumfit.lmo <- function(data) {
  n <- length(data)
  if (!is.vector(data)) {
    return(NULL)
  }
  if (!is.numeric(data)) {
    return(NULL)
  }
  
  sample_lmom <- samlmu(data)
  gum_est <- pelgum(lmom = sample_lmom)
  estimate <-
    c(scale = unname(gum_est[2]), location = unname(gum_est[1]))
  
  
  # loglik
  
  l_lik <- dgumbel(data, scale = estimate[1], location = estimate[2])
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
    distname = 'gumbel',
    data = data
  )
  return(fit)
  
  
}

## Gumbel max / 
gumfit <- function(data) {
  if (!is.vector(data)) {
    stop("Data is not a vector")
  }
  if (!is.numeric(data)) {
    stop("Data is not a numeric")
  }
  
  gum.mme <- try(gumfit.mme(data = data), silent = T)
  gum.lmo <- try(gumfit.lmo(data = data), silent = T)
  
  if (is(gum.mme, "try-error"))
    gum.mme <- NULL
  if (is(gum.lmo, "try-error"))
    gum.lmo <- NULL
  
  if (!is.null(gum.lmo)) {
    start = gum.lmo$estimate
  } else if (!is.null(gum.mme)) {
    start = gum.mme$estimate
  } else {
    start = c(scale = sd(data), location = mean(data))
  }
  
  start = as.list(start)
  
  gum.mle <- try(fitdist(data, dgumbel, method = "mle", start = start,
                         control = list(maxit=1000,reltol = 1e-15)))
  if (is(gum.mle, "try-error")) {
    gum.mle <- NULL
  } else {
    gum.mle <-
      gum.mle[c('estimate',
                'method',
                'loglik',
                'aic',
                'bic',
                'n',
                'distname',
                'data')]
  }
  
  all.est <- list(met1 = gum.mme,
                  met2 = gum.mle,
                  met3 = gum.lmo)
  return(all.est)
  
  
}