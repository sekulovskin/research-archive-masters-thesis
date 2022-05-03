#=======================================================
#Calculating the effective sample size based on the 
#`Multiple Imputation of missing data` approach
#====================================================

setwd("")  # set your working directory such that you can source the functions 
# you can also do this through RStudio by clicking Session -> Set Working Directory -> Choose Directory..
# or simply press Ctrl + Shift + H
source("mi-Neff_functions(L1).R") 

#=============================================
#One level-1 predictor
#============================================

# N = 400

#Specify the Jags  model 
model <- "model {   
  for (i in 1:400){   # Likelihood
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] 
  }
  
  for(j in 1:20){   # level 2
    alpha[j] <- U[j,1]
    beta_1[j]  <-  U[j,2]
    U[j,1:2] ~ dmnorm (MU[j,], invSigma[,])
    MU[j,1] <- mu.alpha
    MU[j,2] <- mu.beta_1
  }
  mu.alpha  ~ dnorm(0, 0.0001)  # (hyper)priors
  mu.beta_1 ~ dnorm(0,  0.0001)
  mu.beta_2 ~ dnorm(0,  0.0001)
  tau ~ dgamma (0.001, 0.001)  
  invSigma[1:2,1:2] ~ dwish(Tau, 2)  
  tau.alpha  ~ dgamma (0.001, 0.001)  
  tau.beta_1 ~ dgamma (0.001, 0.001)    
  Tau[1,1] <- pow(tau.alpha,  -1/2)  
  Tau[2,2] <- pow(tau.beta_1, -1/2)
  Tau[1,2] <- rho_1*tau.alpha*tau.beta_1 
  Tau[2,1] <- Tau[1,2]
  rho_1 ~ dunif(-1, 1)  
  rho_2 ~ dunif(-1, 1)  
}"


mi_nEff_0.4k_1pred <- lapply(n_0.4k_1pred, mi.Neff.1pred.nG20)

#---------------------------------------------------------------------

#N = 3200

#Specify the Jags  model 
model <- "model {   
  for (i in 1:3200){   # Likelihood
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] 
  }
  
  for(j in 1:80){   # level 2
    alpha[j] <- U[j,1]
    beta_1[j]  <-  U[j,2]
    U[j,1:2] ~ dmnorm (MU[j,], invSigma[,])
    MU[j,1] <- mu.alpha
    MU[j,2] <- mu.beta_1
  }
  mu.alpha  ~ dnorm(0, 0.0001)  # (hyper)priors
  mu.beta_1 ~ dnorm(0,  0.0001)
  mu.beta_2 ~ dnorm(0,  0.0001)
  tau ~ dgamma (0.001, 0.001)  
  invSigma[1:2,1:2] ~ dwish(Tau, 2)  
  tau.alpha  ~ dgamma (0.001, 0.001)  
  tau.beta_1 ~ dgamma (0.001, 0.001)    
  Tau[1,1] <- pow(tau.alpha,  -1/2)  
  Tau[2,2] <- pow(tau.beta_1, -1/2)
  Tau[1,2] <- rho_1*tau.alpha*tau.beta_1 
  Tau[2,1] <- Tau[1,2]
  rho_1 ~ dunif(-1, 1)  
  rho_2 ~ dunif(-1, 1)  
}"


#compute the effective sample size for each simulated data set 
mi_nEff_3.2k_1pred <- lapply(n_3.2k_1pred, mi.Neff.1pred.nG80)

#=============================================
#Two level-1 predictors 
#============================================

# N = 400

#Specify the Jags  model 
model <- "model {   
  for (i in 1:400){   # Likelihood
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] 
                             + beta_2[group[i]] * X2[i]
  }
  
  for(j in 1:20){   # level 2
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


mi_nEff_0.4k_2pred <- lapply(n_0.4k_2pred, mi.Neff.2pred.nG20)

#---------------------------------------------------------------------

#N = 3200

#Specify the Jags  model 
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



#compute the effective sample size for each simulated data set 
mi_nEff_3.2k_2pred <- lapply(n_3.2k_2pred, mi.Neff.2pred.nG80)




# Calculate the Average effective sample size for each population:


Mi_neff <- unlist(mi_nEff_0.4k_1pred)
mean_rsq0 <- mean(Mi_neff[1:1000])
mean_rsq1 <- mean(Mi_neff[1001:2000])
mean_rsq2 <- mean(Mi_neff[2001:3000])
mean_rsq3 <- mean(Mi_neff[3001:4000])
N_eff_0.4_1pred <- c(mean_rsq0, mean_rsq1, mean_rsq2, mean_rsq3)



Mi_neff <- unlist(mi_nEff_0.4k_2pred)
mean_rsq0 <- mean(Mi_neff[1:1000])
mean_rsq1 <- mean(Mi_neff[1001:2000])
mean_rsq2 <- mean(Mi_neff[2001:3000])
mean_rsq3 <- mean(Mi_neff[3001:4000])
N_eff_0.4_2pred <- c(mean_rsq0, mean_rsq1, mean_rsq2, mean_rsq3)


Mi_neff <- unlist(mi_nEff_3.2k_1pred)
mean_rsq0 <- mean(Mi_neff[1:1000])
mean_rsq1 <- mean(Mi_neff[1001:2000])
mean_rsq2 <- mean(Mi_neff[2001:3000])
mean_rsq3 <- mean(Mi_neff[3001:4000])
N_eff_3.2_1pred <- c(mean_rsq0, mean_rsq1, mean_rsq2, mean_rsq3)


Mi_neff <- unlist(mi_nEff_3.2k_2pred)
mean_rsq0 <- mean(Mi_neff[1:1000])
mean_rsq1 <- mean(Mi_neff[1001:2000])
mean_rsq2 <- mean(Mi_neff[2001:3000])
mean_rsq3 <- mean(Mi_neff[3001:4000])

N_eff_3.2_2pred <- c(mean_rsq0, mean_rsq1, mean_rsq2, mean_rsq3)

