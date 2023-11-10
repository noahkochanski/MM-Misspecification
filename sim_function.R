

run_sim <- function(alpha = 0.05, 
                    n = 500,
                    num_of_sim = 50,
                    eq_var = T, 
                    scale = c(2,2),
                    diffs = seq(1, 4, by = 1)
){
  
  results <- foreach(i  = diffs,
          .packages=c('tidyverse', 'mclust', 'teigen', 'transport'),
          .combine = rbind) %dopar% {
            
            source('gmm_tmm.R')
            sim <- c()
            div <- c()
            bic <- c()
            
            j = 0
            while(j < num_of_sim){
              
              data <- sample_from_mixture(d = i, df = 10, s = scale, num_samples = n)
              sampled <- sample(seq(length(data)), size = n/2)
              
              ## split data into two groups
              D_0 <- data[sampled]
              D_1 <- data[-sampled]
              
              tryCatch(
                {tmm_parameters <- teigen(D_1,
                                          Gs=2,   # two components
                                          scale=FALSE, dfupdate="numeric",
                                          models=c("univUU"))$parameters[c('df', 'mean', 'sigma', 'pig')] 
                },
                error = function(e){
                  message('Could not fit TMM')
                  print(e)
                },
                warning = function(w){
                  message('warning occurred')
                          print(w)
                }
              )
              
              
              if(eq_var == T){
                tryCatch(
                  {gmm_parameters <- densityMclust(D_0, G = 2, modelName = 'E')$parameters
                  },
                  error = function(e){
                    message('Could not fit GMM')
                    print(e)
                  },
                  warning = function(w){
                    message('warning occurred')
                    print(w)
                  }
                )
              }else{
                tryCatch(
                  {gmm_parameters <- densityMclust(D_0, G = 2, modelName = 'V')$parameters
                  },
                  error = function(e){
                    message('Could not fit GMM')
                    print(e)
                  },
                  warning = function(w){
                    message('warning occurred')
                    print(w)
                  }
                )
              }
              
              p1 <- log(tmm_dens(y = D_0, tmm_parameters))
              p0 <- log(gmm_dens(y = D_0, gmm_parameters))
              
              sim <- append(sim, sum(p1) - sum(p0) > log(1/alpha))
              div <- append(div, calc_kl(gmm = gmm_parameters, tmm = tmm_parameters))
              bic <- append(bic, calc_BIC(dat = D_1, gmm = gmm_parameters, tmm = tmm_parameters))
              j = j + 1
            }
            
            res <- matrix(c(i , mean(sim), mean(div), mean(bic)), nrow = 1, ncol = 4)
            res
          }
  
  results = data.frame(results)
  colnames(results) = c('d', 'Power','Divergence', 'BIC')
  write.csv(results, file = paste(paste('sim', n, scale[1], scale[2], sep = '_'), '.csv', sep = ''))
  return(results)
}



