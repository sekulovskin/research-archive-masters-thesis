#=======================================================
#Calculating the effective sample size based on the 
#`Multiple Imputation of missing data` approach
#====================================================

source("mi-Neff_functions(L1).R") 

#=============================================
#One level-1 predictor
#============================================

# N = 400

#Specify the Jags  model 
model <- "model {   
# Likelihood
for (i in 1:400){
  y[i] ~ dnorm(mu[i], tau)
  mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] 
}
# lvl 2
for(j in 1:20){  #we have 20 groups
alpha[j] <- B[j,1]
beta_1[j]  <-  B[j,2]
B[j,1:2] ~ dmnorm (B.hat[j,], invSigma.B[,])
B.hat[j,1] <- mu.alpha
B.hat[j,2] <- mu.beta_1
}
# Priors
mu.alpha ~ dnorm(0, 0.0001)
mu.beta_1 ~ dnorm(0,  0.0001)
tau ~ dgamma (0.001, 0.001)  #resiudal variance
invSigma.B[1:2,1:2] ~ dwish(Tau.B, 2)  # inverse of covariance matrix following a Wishard dist.
  tau.alpha ~ dgamma (0.001, 0.001)  #ntercept variance
  tau.beta_1 ~ dgamma (0.001, 0.001)    #slope variance
  Tau.B[1,1] <- pow(tau.alpha, -1/2)  #construct the cov matrix
  Tau.B[2,2] <- pow(tau.beta_1, -1/2)
  Tau.B[1,2] <- rho_1*tau.alpha*tau.beta_1 #covariance between slope of b1 and intercept
  Tau.B[2,1] <- Tau.B[1,2]
  rho_1 ~ dunif(-1, 1)  #cor is between -1 and 1
}
"


mi_nEff_0.4k_1pred <- lapply(n_0.4k_1pred, mi.Neff.1pred.nG20)

#---------------------------------------------------------------------

#N = 3200

#Specify the Jags  model 
model <- "model {  
# Likelihood
for (i in 1:3200){
  y[i] ~ dnorm(mu[i], tau)
  mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] 
}
# lvl 2
for(j in 1:80){  #we have 20 groups
alpha[j] <- B[j,1]
beta_1[j]  <-  B[j,2]
B[j,1:2] ~ dmnorm (B.hat[j,], invSigma.B[,])
B.hat[j,1] <- mu.alpha
B.hat[j,2] <- mu.beta_1
}
# Priors
mu.alpha ~ dnorm(0, 0.0001)
mu.beta_1 ~ dnorm(0,  0.0001)
tau ~ dgamma (0.001, 0.001)  #resiudal variance
invSigma.B[1:2,1:2] ~ dwish(Tau.B, 2)  # inverse of covariance matrix following a Wishard dist.
  tau.alpha ~ dgamma (0.001, 0.001)  #ntercept variance
  tau.beta_1 ~ dgamma (0.001, 0.001)    #slope variance
  Tau.B[1,1] <- pow(tau.alpha, -1/2)  #construct the cov matrix
  Tau.B[2,2] <- pow(tau.beta_1, -1/2)
  Tau.B[1,2] <- rho_1*tau.alpha*tau.beta_1 #covaroance between slope of b1 and intercept
  Tau.B[2,1] <- Tau.B[1,2]
  rho_1 ~ dunif(-1, 1)  #cor is between -1 and 1
}
"

#compute the effective sample size for each simulated data set 
mi_nEff_3.2k_1pred <- lapply(n_3.2k_1pred, mi.Neff.1pred.nG80)

#=============================================
#Two level-1 predictors 
#============================================

# N = 400

#Specify the Jags  model 
model <- "model {   
# Likelihood
for (i in 1:400){
  y[i] ~ dnorm(mu[i], tau)
  mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] + beta_2[group[i]] * X2[i]
}
# lvl 2
for(j in 1:20){  #we have 80 groups
alpha[j] <- B[j,1]
beta_1[j]  <-  B[j,2]
beta_2[j]  <-  B[j,3]
B[j,1:3] ~ dmnorm (B.hat[j,], invSigma.B[,])
B.hat[j,1] <- mu.alpha
B.hat[j,2] <- mu.beta_1
B.hat[j,3] <- mu.beta_2
}
  mu.alpha  ~ dnorm(0, 0.0001)  # (hyper)priors
  mu.beta_1 ~ dnorm(0,  0.0001)
  mu.beta_2 ~ dnorm(0,  0.0001)
  tau ~ dgamma (0.001, 0.001)  
  invSigma.B[1:3,1:3] ~ dwish(Tau.B, 3)  
  tau.alpha  ~ dgamma (0.001, 0.001)  
  tau.beta_1 ~ dgamma (0.001, 0.001)    
  tau.beta_2 ~ dgamma (0.001, 0.001)    
  Tau.B[1,1] <- pow(tau.alpha,  -1/2)  
  Tau.B[2,2] <- pow(tau.beta_1, -1/2)
  Tau.B[3,3] <- pow(tau.beta_2, -1/2)
  Tau.B[1,2] <- rho_1*tau.alpha*tau.beta_1 
  Tau.B[2,1] <- Tau.B[1,2]
  Tau.B[1,3] <- rho_2*tau.alpha*tau.beta_2 
  Tau.B[3,1] <- Tau.B[1,3]
  Tau.B[2,3] <- rho_3*tau.beta_1*tau.beta_2 
  Tau.B[3,2] <- Tau.B[2,3]
  rho_1 ~ dunif(-1, 1)  
  rho_2 ~ dunif(-1, 1)  
  rho_3 ~ dunif(-1, 1)  
}"



#Model that doesn't include a multivariate dist for the random effects
model <- "model {   #Model with random slopes and intercept and no correlation between random effects
# Likelihood
for (i in 1:400){
  y[i] ~ dnorm(mu[i], tau)
  mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] + beta_2[group[i]] * X2[i]
}

# lvl 2
for(j in 1:20){  #we have 80 groups
alpha[j] ~ dnorm(mu.alpha, tau.alpha) #intercept
beta_1[j] ~ dnorm(mu.beta_1, tau.beta_1) #reg coef
beta_2[j] ~ dnorm(mu.beta_2, tau.beta_2) #reg coef
}

# Priors
mu.alpha ~ dnorm(0, 0.0001)
mu.beta_1 ~ dnorm(0,  0.0001)
mu.beta_2 ~ dnorm(0,  0.0001)
tau ~ dgamma (0.001, 0.001)  #resiudal variance
tau.alpha ~ dgamma (0.001, 0.001)
tau.beta_1 ~ dgamma (0.001, 0.001)
tau.beta_2 ~ dgamma (0.001, 0.001)
}
" 

mi_nEff_0.4k_2pred <- lapply(n_0.4k_2pred, mi.Neff.2pred.nG20)

#---------------------------------------------------------------------

#N = 3200

#Specify the Jags  model 
model <- "model {   
# Likelihood
for (i in 1:3200){
  y[i] ~ dnorm(mu[i], tau)
  mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] + beta_2[group[i]] * X2[i]
}
# lvl 2
for(j in 1:80){  #we have 80 groups
alpha[j] <- B[j,1]
beta_1[j]  <-  B[j,2]
beta_2[j]  <-  B[j,3]
B[j,1:3] ~ dmnorm (B.hat[j,], invSigma.B[,])
B.hat[j,1] <- mu.alpha
B.hat[j,2] <- mu.beta_1
B.hat[j,3] <- mu.beta_2
}
  mu.alpha  ~ dnorm(0, 0.0001)  # (hyper)priors
  mu.beta_1 ~ dnorm(0,  0.0001)
  mu.beta_2 ~ dnorm(0,  0.0001)
  tau ~ dgamma (0.001, 0.001)  
  invSigma.B[1:3,1:3] ~ dwish(Tau.B, 3)  
  tau.alpha  ~ dgamma (0.001, 0.001)  
  tau.beta_1 ~ dgamma (0.001, 0.001)    
  tau.beta_2 ~ dgamma (0.001, 0.001)    
  Tau.B[1,1] <- pow(tau.alpha,  -1/2)  
  Tau.B[2,2] <- pow(tau.beta_1, -1/2)
  Tau.B[3,3] <- pow(tau.beta_2, -1/2)
  Tau.B[1,2] <- rho_1*tau.alpha*tau.beta_1 
  Tau.B[2,1] <- Tau.B[1,2]
  Tau.B[1,3] <- rho_2*tau.alpha*tau.beta_2 
  Tau.B[3,1] <- Tau.B[1,3]
  Tau.B[2,3] <- rho_3*tau.beta_1*tau.beta_2 
  Tau.B[3,2] <- Tau.B[2,3]
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

