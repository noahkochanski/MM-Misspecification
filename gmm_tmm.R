library(tidyverse)
library(mclust)
library(teigen)
library(doParallel)
library(transport)

sample_from_mixture <- function(d = 4, s = c(1, 2), df = 5, num_samples = 1000){
  k = length(s)
  means = seq(-(k-1)*d/2, (k-1)*d/2, by = d)
  u = purrr::rdunif(n = num_samples, a = 1, b = k)
  
  mm <- matrix(data = c(means[u], s[u]), ncol = 2)
  mm_samples <- mapply(FUN = function(mean, scale) rt(1, df = df)*scale + mean, mean = mm[,1], scale = mm[,2])
  return(mm_samples)
}





tmm_dens <- function(y, parameters){
  tryCatch(
    {
      d = parameters$pig[1]*dt((y - parameters$mean[1])/parameters$sigma[1], df = parameters$df[1]) + parameters$pig[2]*dt((y - parameters$mean[2])/parameters$sigma[2], df = parameters$df[2])
    },
    error=function(e) {
      message('An Error Occurred computing tmm density')
      print(e)
    },
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
  )
  return(d)
}

sample_from_fitted_tmm <- function(n = 10000, parameters){
  g <- rbernoulli(n = n, p = parameters$pig[1])
  tryCatch(
    {
      y <- g*(rt(n = length(g), df = parameters$df[1])*parameters$sigma[1] + parameters$mean[1]) + (1-g)*(rt(n = length(g), df = parameters$df[2])*parameters$sigma[2] + parameters$mean[2])
    },
    error=function(e) {
      message('An Error Occurred sampling from tmm')
      print(e)
    },
    #if a warning occurs, tell me the warning
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
  )
  return(y)
}  






gmm_dens <- function(y, parameters){
  tryCatch(
    {
      if(length(parameters$variance$sigmasq) == 1){
        d = parameters$pro[1]*dnorm(y, mean = parameters$mean[1], sd = parameters$variance$sigmasq) + parameters$pro[2]*dnorm(y, mean = parameters$mean[2], sd = parameters$variance$sigmasq)
      }else{
        d = parameters$pro[1]*dnorm(y, mean = parameters$mean[1], sd = parameters$variance$sigmasq[1]) + parameters$pro[2]*dnorm(y, mean = parameters$mean[2], sd = parameters$variance$sigmasq[2])
      }
    },
    error=function(e) {
      message('An Error Occurred computing gmm density')
      print(e)
    },
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
  )
  return(d)
}

sample_from_fitted_gmm <- function(n = 10000, parameters){
  tryCatch(
    {
      g <- rbernoulli(n = n, p = parameters$pro[1])
      if(length(parameters$variance$sigmasq) == 1){
        y <- g*rnorm(n = length(g), mean = parameters$mean[1], sd = parameters$variance$sigmasq) + (1-g)*rnorm(n = length(g), mean = parameters$mean[2], sd = parameters$variance$sigmasq)
      }else{
        y <- g*rnorm(n = length(g), mean = parameters$mean[1], sd = parameters$variance$sigmasq[1]) + (1-g)*rnorm(n = length(g), mean = parameters$mean[2], sd = parameters$variance$sigmasq[2])
      }
    },
    error=function(e) {
      message('An Error Occurred sampling from gmm')
      print(e)
    },
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
  )
  return(y)
}







calc_kl <- function(gmm, tmm){
  samp <- sample_from_fitted_gmm(parameters = gmm)
  kl <- c()
  for(i in samp){
    kl <- append(kl, log(gmm_dens(i, gmm) - log(tmm_dens(i, tmm))))
  }
  return(mean(kl))
}

calc_wasserstein <- function(gmm, tmm){
  a <- sample_from_fitted_gmm(parameters = gmm)
  b <- sample_from_fitted_tmm(parameters = tmm)
  d <- wasserstein1d(a = a, b = b)
  return(d)
}

calc_BIC <- function(dat, gmm, tmm){
  gmm_bic <- 5*length(dat) - 2*sum(log(gmm_dens(dat, parameters = gmm)))
  tmm_bic <- 5*length(dat) - 2*sum(log(tmm_dens(dat, parameters = tmm)))
  return(gmm_bic - tmm_bic)
}

