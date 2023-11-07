library(tidyverse)
library(mclust)
library(teigen)
library(doParallel)
library(transport)
source('sim_function.R')

cl <- makeCluster(32)
registerDoParallel(cl)

run_sim(alpha = 0.05, 
        n = 500,
        num_of_sim = 2,
        eq_var = T, 
        scale = c(1,1),
        diffs = seq(0.2, 4, by = 0.1))


run_sim(alpha = 0.05, 
        n = 1000,
        num_of_sim = 2,
        eq_var = T, 
        scale = c(1,1),
        diffs = seq(0.2, 4, by = 0.1))
