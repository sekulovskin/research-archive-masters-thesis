#==================================
#Sensitivity Analysis 
#=================================
setwd("")  # set your working directory such that you can source the functions 
# you can also do this through RStudio by clicking Session -> Set Working Directory -> Choose Directory...
source("wrapper_function.R")
source("mi-Neff_functions(L1).R")
library(lme4)
library(jtools)

# Construct the data set corresponding to cell 13 of Figure 1

set.seed(1234)
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
data <- data.frame(y, group, X1, X2)

lmer_model <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data)
summ(lmer_model)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Sensitivity Analysis
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fraction <- c(1, 2, 3)  # i.e., 1*2, 2*J, 3*J

#--------------------------------------------------------------------------

# N = lvl1
BF.1 <- data.frame()
for(i in 1:3){
  BF.1[i, c(1,2,3)] <- bain_2lmer(lmer_model, "X1 = X2 = 0", 
                                  fraction = fraction[i] , standardize = FALSE,
                                  N = "level_1", seed = 123)[["fit"]][1, c(5,6,7)]
}

#--------------------------------------------------------------------------

# N = lvl2
BF.2 <- data.frame()
for(i in 1:3){
  BF.2[i, c(1,2,3)] <- bain_2lmer(lmer_model, "X1 = X2 = 0", 
                                  fraction = fraction[i] , standardize = FALSE,
                                  N = "level_2", seed = 123)[["fit"]][1, c(5,6,7)]
}

#--------------------------------------------------------------------------

# N = ICC
BF.3 <- data.frame()
for(i in 1:3){
  BF.3[i, c(1,2,3)] <- bain_2lmer(lmer_model, "X1 = X2 = 0", 
                                  fraction = fraction[i] , standardize = FALSE,
                                  N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
}


bain_2lmer(lmer_model, "X1 = X2 = 0", 
           fraction = fraction[i] , standardize = FALSE,
           N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]

# Check the calculated value for the ICC-based  effective sample size

bain_2lmer(lmer_model, "X1 = X2 = 0", 
           fraction = fraction[i] , standardize = FALSE,
           N = "ICC_effective", seed = 123)$n  # 413

#--------------------------------------------------------------------------

# N = MI-based effective sample size
model <- "model {   
  for (i in 1:3200){   # Likelihood
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] 
                             + beta_2[group[i]] * X2[i]
  }
  
  for(j in 1:80){   # level 2
    alpha[j] <- U[j,1]
    beta_1[j]  <-  U[j,2]
    beta_2[j]  <-  U[j,3]
    U[j,1:3] ~ dmnorm (MU[j,], invSigma[,])
    MU[j,1] <- mu.alpha
    MU[j,2] <- mu.beta_1
    MU[j,3] <- mu.beta_2
  }
  mu.alpha  ~ dnorm(0, 0.0001)  # (hyper)priors
  mu.beta_1 ~ dnorm(0,  0.0001)
  mu.beta_2 ~ dnorm(0,  0.0001)
  tau ~ dgamma (0.001, 0.001)  
  invSigma[1:3,1:3] ~ dwish(Tau, 3)  
  tau.alpha  ~ dgamma (0.001, 0.001)  
  tau.beta_1 ~ dgamma (0.001, 0.001)    
  tau.beta_2 ~ dgamma (0.001, 0.001)    
  Tau[1,1] <- pow(tau.alpha,  -1/2)  
  Tau[2,2] <- pow(tau.beta_1, -1/2)
  Tau[3,3] <- pow(tau.beta_2, -1/2)
  Tau[1,2] <- rho_1*tau.alpha*tau.beta_1 
  Tau[2,1] <- Tau[1,2]
  Tau[1,3] <- rho_2*tau.alpha*tau.beta_2 
  Tau[3,1] <- Tau[1,3]
  Tau[2,3] <- rho_3*tau.beta_1*tau.beta_2 
  Tau[3,2] <- Tau[2,3]
  rho_1 ~ dunif(-1, 1)  
  rho_2 ~ dunif(-1, 1)  
  rho_3 ~ dunif(-1, 1)  
}"


data2 <- data 
n_mi <- mi.Neff.2pred.nG80(data2) # 921

BF.4 <- data.frame()
for(i in 1:3){
  BF.4[i, c(1,2,3)] <- bain_2lmer(lmer_model, "X1 = X2 = 0", 
                                  fraction =fraction[i] , standardize = FALSE,
                                  N = n_mi, seed = 123)[["fit"]][1, c(5,6,7)]
}

#End of Sensitivity Analysis
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Combine the results
sensitivity <- rbind(BF.1, BF.2, BF.3, BF.4)
sensitivity$J <- rep(c(2,4,6), 4)
sensitivity$N <- as.factor(rep(c("Level-1", "Level-2", "ICC-effective", "MI-effective"), each = 3))

#save the results
write.csv(sensitivity, "sensitivity.csv")


