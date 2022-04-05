#==================================
#Sensitivity analysis 
#=================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Master Thesis/Master's thesis repo/Sensitivity Analysis")
source("wrapper_function.R")
source("mi-Neff_functions(L1).R")
library(lme4)
library(jtools)

# Construct the data

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
#Sensitivity analysis
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fraction <- c(1, 2, 3) 
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

# Check the value ICC effective smaple size

bain_2lmer(lmer_model, "X1 = X2 = 0", 
           fraction = fraction[i] , standardize = FALSE,
           N = "ICC_effective", seed = 123)$n  #413.1038
#--------------------------------------------------------------------------

# N = MI effective
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
B[j,1:3] ~ dmnorm (B.hat[j,], Tau.B[,])
B.hat[j,1] <- mu.alpha
B.hat[j,2] <- mu.beta_1
B.hat[j,3] <- mu.beta_2
}

# Priors
mu.alpha ~ dnorm(0, 0.0001)
mu.beta_1 ~ dnorm(0,  0.0001)
mu.beta_2 ~ dnorm(0,  0.0001)
tau ~ dgamma (0.001, 0.001)  #resiudal variance
Tau.B[1:3,1:3] ~ dwish(Sigma.B, 3)  # inverse of covariance matrix following a Wishard dist.
  sigma.alpha ~ dgamma (0.001, 0.001)  #ntercept variance
  sigma.beta_1 ~ dgamma (0.001, 0.001)    #slope variance
  sigma.beta_2 ~ dgamma (0.001, 0.001)    #slope variance
  Sigma.B[1,1] <- pow(sigma.alpha, 2)  #construct the cov matrix
  Sigma.B[2,2] <- pow(sigma.beta_1, 2)
  Sigma.B[3,3] <- pow(sigma.beta_2, 2)
  Sigma.B[1,2] <- rho_1*sigma.alpha*sigma.beta_1 #covariance between slope of b1 and intercept
  Sigma.B[2,1] <- Sigma.B[1,2]
  Sigma.B[1,3] <- rho_2*sigma.alpha*sigma.beta_2 #covariance between slope of b2 and intercept
  Sigma.B[3,1] <- Sigma.B[1,3]
  Sigma.B[2,3] <- rho_3*sigma.beta_1*sigma.beta_2 #covariance between slope of b2 and slope of b1
  Sigma.B[3,2] <- Sigma.B[2,3]
  rho_1 ~ dunif(-1, 1)  #cor is between -1 and 1
  rho_2 ~ dunif(-1, 1)  #cor is between -1 and 1
  rho_3 ~ dunif(-1, 1)  #cor is between -1 and 1
}
" 
data2 <- data 
n_mi <- mi.Neff.2pred.nG80(data2) #1165.461

BF.4 <- data.frame()
for(i in 1:3){
  BF.4[i, c(1,2,3)] <- bain_2lmer(lmer_model, "X1 = X2 = 0", 
                                  fraction =fraction[i] , standardize = FALSE,
                                  N = n_mi, seed = 123)[["fit"]][1, c(5,6,7)]
}




sensitivity <- rbind(BF.1, BF.2, BF.3, BF.4)
sensitivity$J <- rep(c(2,4,6), 4)
sensitivity$N <- as.factor(rep(c("Level-1", "Level-2", "ICC-effective", "MI-effective"), each = 3))



#save the results
write.csv(sensitivity, "sensitivity.csv")


