#=========================================================================
# Simulating two-level data sets; for annotations about what the code does
# please see `simulation-motivation-for-data.R` script 
#========================================================================
N_sim <- 1000 # number of simulations for each cell in Figure 1 of the paper

#++++++++++++++++++++++++++
#N = 400 (ng = 20, n=20)
#++++++++++++++++++++++++

#==================================================
# 1 predictor
#==================================================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 for the fixed effects is ~ 0
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(123)
Rsq_0 <- list()

  for(i in 1:N_sim){
    nG <- 20
    n  <- 20 
    b1 <- 0  
    X1 <- rnorm(nG * n, 0, 0.8) 
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + u_0 + u_1*X1  + epsilon
    Rsq_0[[i]] <- data.frame(y, group, X1)

  }



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .02 (small)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_10 <- list()

  for(i in 1:N_sim){
    nG <- 20
    n  <- 20 
    b1 <- 0.12 
    X1 <- rnorm(nG * n, 0, 0.8) 
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + u_0 + u_1*X1  + epsilon
    Rsq_10[[i]] <- data.frame(y, group, X1)
  
  }


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .13 (medium)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_20 <- list()

  for(i in 1:N_sim){
    nG <- 20
    n  <- 20 
    b1 <- 0.33 
    X1 <- rnorm(nG * n, 0, 0.8) 
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + u_0 + u_1*X1  + epsilon
    Rsq_20[[i]] <- data.frame(y, group, X1)
    
  }


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .26 (large)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_30 <- list()

  for(i in 1:N_sim){
    nG <- 20
    n  <- 20 
    b1 <- 0.51
    X1 <- rnorm(nG * n, 0, 0.8) 
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + u_0 + u_1*X1  + epsilon
    Rsq_30[[i]] <- data.frame(y, group, X1)
    
  }


n_0.4k_1pred <- c(Rsq_0, Rsq_10, Rsq_20, Rsq_30)
rm(Rsq_0, Rsq_10, Rsq_20, Rsq_30)


#==================================================
# 2 predictors
#==================================================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ H0: FALSE pseudo R^2 for the fixed effects is ~ 0
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(123)
Rsq_0 <- list()

  for(i in 1:N_sim){
    nG <- 20   
    n  <- 20   
    b1 <- 0  
    b2 <- 0 
    X1 <- rnorm(nG * n, 0, 0.8) 
    X2 <- rnorm(nG * n, 0, 0.6)
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    slope_var_2 <- rnorm(nG, 0, 0.2)    
    u_2 <- rep(slope_var_2, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
    Rsq_0[[i]] <- data.frame(y, group, X1,  X2)
    
  }
  
  
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .02 (small)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_10 <- list()

  for(i in 1:N_sim){
    nG <- 20   
    n  <- 20   
    b1 <- 0.09 
    b2 <- 0.09 
    X1 <- rnorm(nG * n, 0, 0.8) 
    X2 <- rnorm(nG * n, 0, 0.6)
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    slope_var_2 <- rnorm(nG, 0, 0.2)    
    u_2 <- rep(slope_var_2, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon
    Rsq_10[[i]] <- data.frame(y, group, X1,  X2)
    
  }


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .13 (medium)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_20 <- list()

  for(i in 1:N_sim){
    nG <- 20   
    n  <- 20   
    b1 <- 0.26  
    b2 <- 0.26 
    X1 <- rnorm(nG * n, 0, 0.8) 
    X2 <- rnorm(nG * n, 0, 0.6)
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    slope_var_2 <- rnorm(nG, 0, 0.2)    
    u_2 <- rep(slope_var_2, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon
    Rsq_20[[i]] <- data.frame(y, group, X1,  X2)
    
  }


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .26 (large)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_30 <- list()

  for(i in 1:N_sim){
    nG <- 20   
    n  <- 20   
    b1 <- 0.4  
    b2 <- 0.4
    X1 <- rnorm(nG * n, 0, 0.8) 
    X2 <- rnorm(nG * n, 0, 0.6)
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    slope_var_2 <- rnorm(nG, 0, 0.2)    
    u_2 <- rep(slope_var_2, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2 + epsilon
    Rsq_30[[i]] <- data.frame(y, group, X1,  X2)
    
  }

  

n_0.4k_2pred <- c(Rsq_0, Rsq_10, Rsq_20, Rsq_30)
rm(Rsq_0, Rsq_10, Rsq_20, Rsq_30)



#++++++++++++++++++++++++++
#N = 3200 (ng = 80, n=40)
#++++++++++++++++++++++++

#==================================================
# 1 predictor
#==================================================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 for the fixed effects is ~ 0
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(123)
Rsq_0 <- list()

  for(i in 1:N_sim){
    nG <- 80   
    n  <- 40   
    b1 <- 0
    X1 <- rnorm(nG * n, 0, 0.8)
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
    Rsq_0[[i]] <- data.frame(y, group, X1)
    
  }



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .02 (small)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_10 <- list()

  for(i in 1:N_sim){
    nG <- 80   
    n  <- 40   
    b1 <- 0.11
    X1 <- rnorm(nG * n, 0, 0.8)
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
    Rsq_10[[i]] <- data.frame(y, group, X1)
    
  }
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .13 (medium)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_20 <- list()

  for(i in 1:N_sim){
    nG <- 80   
    n  <- 40   
    b1 <- 0.31
    X1 <- rnorm(nG * n, 0, 0.8)
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
    Rsq_20[[i]] <- data.frame(y, group, X1)
    
  }


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .26 (large)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_30 <- list()

  for(i in 1:N_sim){
    nG <- 80   
    n  <- 40   
    b1 <- 0.48
    X1 <- rnorm(nG * n, 0, 0.8)
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
    Rsq_30[[i]] <- data.frame(y, group, X1)
    
  }



n_3.2k_1pred <-c(Rsq_0, Rsq_10, Rsq_20, Rsq_30)
rm(Rsq_0, Rsq_10, Rsq_20, Rsq_30)


#==================================================
# 2 predictors
#==================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 for the fixed effects is ~ 0
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(123)
Rsq_0 <- list()

  for(i in 1:N_sim){
    nG <- 80   
    n  <- 40   
    b1 <- 0
    b2 <- 0
    X1 <- rnorm(nG * n, 0, 0.8) 
    X2 <- rnorm(nG * n, 0, 0.6) 
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    slope_var_2 <- rnorm(nG, 0, 0.2)    
    u_2 <- rep(slope_var_2, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
    Rsq_0[[i]] <- data.frame(y, group, X1,  X2)
    
  }



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .02 (small)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_10 <- list()

  for(i in 1:N_sim){
    nG <- 80   
    n  <- 40   
    b1 <- 0.1
    b2 <- 0.1
    X1 <- rnorm(nG * n, 0, 0.8) 
    X2 <- rnorm(nG * n, 0, 0.6) 
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    slope_var_2 <- rnorm(nG, 0, 0.2)    
    u_2 <- rep(slope_var_2, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
    Rsq_10 [[i]] <- data.frame(y, group, X1,  X2)
    
  }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .13 (medium)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_20 <- list()

  for(i in 1:N_sim){
    nG <- 80   
    n  <- 40   
    b1 <- 0.27
    b2 <- 0.27
    X1 <- rnorm(nG * n, 0, 0.8) 
    X2 <- rnorm(nG * n, 0, 0.6) 
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    slope_var_2 <- rnorm(nG, 0, 0.2)    
    u_2 <- rep(slope_var_2, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
    Rsq_20[[i]] <- data.frame(y, group, X1,  X2)
    
  }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ pseudo R^2 for the fixed effects is ~ .26  (large)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set.seed(123)
Rsq_30 <- list()

  for(i in 1:N_sim){
    nG <- 80   
    n  <- 40   
    b1 <- 0.42
    b2 <- 0.42
    X1 <- rnorm(nG * n, 0, 0.8) 
    X2 <- rnorm(nG * n, 0, 0.6) 
    group <- gl(nG, k = n) 
    intercept_var <- rnorm(nG, 0, 0.3)  
    u_0 <- rep(intercept_var, each = n) 
    slope_var_1 <- rnorm(nG, 0, 0.1)   
    u_1 <- rep(slope_var_1, each = n) 
    slope_var_2 <- rnorm(nG, 0, 0.2)    
    u_2 <- rep(slope_var_2, each = n) 
    epsilon <- rnorm(nG * n, 0, 0.6)
    y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
    Rsq_30[[i]] <- data.frame(y, group, X1,  X2)
    
  }

  

n_3.2k_2pred <- c(Rsq_0, Rsq_10, Rsq_20, Rsq_30)
rm(Rsq_0, Rsq_10, Rsq_20, Rsq_30)



rm(b1, b2, epsilon, group, i, intercept_var, n, nG, slope_var_1, slope_var_2, u_0, u_1, u_2, X1, X2, y)