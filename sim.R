library(tidyverse)
library(mclust)
library(teigen)
library(doParallel)
library(transport)
source('sim_function.R')

cl <- makeCluster(2)
registerDoParallel(cl)

B = 1000

# equal scale
run_sim(alpha = 0.05, 
  n = 500,
  num_of_sim = B,
  eq_var = T, 
  scale = c(1,1),
  diffs = seq(0.2, 5, by = 0.1))

run_sim(alpha = 0.05, 
        n = 1000,
        num_of_sim = B,
        eq_var = T, 
        scale = c(1,1),
        diffs = seq(0.2, 5, by = 0.1))


run_sim(alpha = 0.05, 
        n = 2000,
        num_of_sim = B,
        eq_var = T, 
        scale = c(1,1),
        diffs = seq(0.2, 5, by = 0.1))

# 2-1 scale

run_sim(alpha = 0.05, 
        n = 500,
        num_of_sim = B,
        eq_var = F, 
        scale = c(2,1),
        diffs = seq(0.2, 5, by = 0.1))

run_sim(alpha = 0.05, 
        n = 1000,
        num_of_sim = B,
        eq_var = F, 
        scale = c(2,1),
        diffs = seq(0.2, 5, by = 0.1))


run_sim(alpha = 0.05, 
        n = 2000,
        num_of_sim = B,
        eq_var = F, 
        scale = c(2,1),
        diffs = seq(0.2, 5, by = 0.1))



# 3-1 scale

run_sim(alpha = 0.05, 
        n = 500,
        num_of_sim = B,
        eq_var = F, 
        scale = c(3,1),
        diffs = seq(0.2, 5, by = 0.1))

run_sim(alpha = 0.05, 
        n = 1000,
        num_of_sim = B,
        eq_var = F, 
        scale = c(3,1),
        diffs = seq(0.2, 5, by = 0.1))


run_sim(alpha = 0.05, 
        n = 2000,
        num_of_sim = B,
        eq_var = F, 
        scale = c(3,1),
        diffs = seq(0.2, 5, by = 0.1))






