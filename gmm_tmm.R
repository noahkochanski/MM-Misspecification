library(tidyverse)
library(mclust)
library(teigen)
library(doParallel)
library(transport)

sample_from_mixture <- function(k = 2, d = 4, s = 1, df = 5, num_samples = 1000){
  samples <- NULL
  means = seq(-(k-1)*d/2, (k-1)*d/2, by = d)
  u = purrr::rdunif(n = num_samples, a = 1, b = k)
  mm_means <- means[u]
  mm_samples <- sapply(mm_means, FUN = function(x) rt(1, df = df)*s + x)
  return(mm_samples)
}

tmm_dens <- function(y, parameters){
  d = parameters$pig[1]*dt((y - parameters$mean[1])/parameters$sigma[1], df = parameters$df[1]) + parameters$pig[2]*dt((y - parameters$mean[2])/parameters$sigma[2], df = parameters$df[2])
  return(d)
}

gmm_dens <- function(y, parameters){
  d = parameters$pro[1]*dnorm(y, mean = parameters$mean[1], sd = parameters$variance$sigmasq) + parameters$pro[2]*dnorm(y, mean = parameters$mean[2], sd = parameters$variance$sigmasq)
  return(d)
}

sample_from_fitted_gmm <- function(n = 10000, parameters){
  g <- rbernoulli(n = n, p = parameters$pro[1])
  y <- g*rnorm(n = length(g), mean = parameters$mean[1], sd = parameters$variance$sigmasq) + (1-g)*rnorm(n = length(g), mean = parameters$mean[2], sd = parameters$variance$sigmasq)
  return(y)
}

sample_from_fitted_tmm <- function(n = 10000, parameters){
  g <- rbernoulli(n = n, p = parameters$pig[1])
  y <- g*(rt(n = length(g), df = parameters$df[1])*parameters$sigma[1] + parameters$mean[1]) + (1-g)*(rt(n = length(g), df = parameters$df[2])*parameters$sigma[2] + parameters$mean[2])
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
  tmm_bic <- 7*length(dat) - 2*sum(log(tmm_dens(dat, parameters = tmm)))
  return(gmm_bic - tmm_bic)
}





cluster <- makeCluster(16)
registerDoParallel(cluster)
alpha = 0.05
n=200


results <- foreach(i = seq(1, 4, by = 1),
        .packages=c('tidyverse', 'mclust', 'teigen', 'transport'),
        .combine = rbind) %dopar% {
  
  sim <- c()
  div <- c()
  was <- c()
  bic <- c()
  
  j = 0
  while(j < 5000){
    
    data <- sample_from_mixture(k = 2, d = i, df = 10, num_samples = n)
    sampled <- sample(seq(length(data)), size = n/2)
    
    ## split data into two groups
    D_0 <- data[sampled]
    D_1 <- data[-sampled]
    
    
    tmm_parameters <- teigen(D_1,
                             Gs=2,   # two components
                             scale=FALSE, dfupdate="numeric",
                             models=c("univUU"))$parameters[c('df', 'mean', 'sigma', 'pig')] 
    
    gmm_parameters <- densityMclust(D_1, G = 2, modelName = 'E')$parameters
    
    p1 <- log(tmm_dens(y = D_0, tmm_parameters))
    p0 <- log(gmm_dens(y = D_0, gmm_parameters))
    
    sim <- append(sim, sum(p1) - sum(p0) > log(1/alpha))
    div <- append(div, calc_kl(gmm = gmm_parameters, tmm = tmm_parameters))
    was <- append(was, calc_wasserstein(gmm = gmm_parameters, tmm = tmm_parameters))
    bic <- append(bic, calc_BIC(dat = D_1, gmm = gmm_parameters, tmm = tmm_parameters))
    j = j + 1
  }
  
  res <- matrix(c(i , mean(sim), mean(div), mean(was), mean(bic)), nrow = 1, ncol = 5)
  res
}


write.csv(results, 'sim_res_.csv')
