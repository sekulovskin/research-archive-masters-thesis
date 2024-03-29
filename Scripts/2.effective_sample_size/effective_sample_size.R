#============================================================================================================
#A new Estimator of Effective Sample Size
#===========================================================================================================
setwd("")  # set your working directory such that you can source the functions 
# you can also do this through RStudio by clicking Session -> Set Working Directory -> Choose Directory...
# or simply press Ctrl + Shift + H
library(lme4)
library(jtools)
source("mi-Neff_functions(L1).R")  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Simulate a data set where a random intercept-only model (Added after revision) N_level-1 = 3200
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#=============================
# Data with ICC = 0.19
#============================
  # these are just placeholders such that the functions
                             # for the N_eff can work without the need to adjust them

set.seed(123)
nG <- 80   
n  <- 40 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6)
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <- u_0 + epsilon    #simulate data from a random intercept only model 
data1_int <- data.frame(y, group, X1, X2)  #The predictors are only included in the data set as placeholders 
                                           # in order for the Mi N_eff function to work

lmer_model1_int <- lmer(y ~ 1 + (1| group), REML = FALSE, data = data1_int)
summary(lmer_model1_int)

#=============================
# Data with ICC = 0.03
#============================

set.seed(123)
nG <- 80   
n  <- 40  
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.1)  
u_0 <- rep(intercept_var, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <- u_0 + epsilon  
data2_int <- data.frame(y, group, X1, X2)

lmer_model2_int <- lmer(y ~ 1 + (1| group), REML = FALSE, data = data2_int)
summary(lmer_model2_int)

#=============================
# Data with ICC = 0.3
#============================

set.seed(123)
nG <- 80   
n  <- 40  
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.41)  
u_0 <- rep(intercept_var, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <- u_0 + epsilon  
data3_int <- data.frame(y, group, X1, X2)

lmer_model3_int <- lmer(y ~ 1 + (1| group), REML = FALSE, data = data3_int)
summary(lmer_model3_int)

#=========================================================
# Calculate the effective sample size 
#=======================================================

#=========================================================
# JAGS specification for a random intercept-only model 
#=======================================================

model <- "model{   
  # Likelihood
  for (i in 1:3200){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] 
  }
  # level 2
  for(j in 1:80){  #we have 80 groups
    alpha[j] ~ dnorm(mu.alpha, tau.alpha) #intercept


  }
######### Priors

mu.alpha ~ dnorm(0, 0.0001)

tau ~ dgamma (0.001, 0.001)

tau.alpha ~ dgamma (0.001, 0.001)

}"

#using functions:

N_eff_0_1_int <- mi.Neff.2pred.nG80.0(data1_int)
N_eff_0_2_int <- mi.Neff.2pred.nG80.0(data2_int)
N_eff_0_3_int <- mi.Neff.2pred.nG80.0(data3_int)

#-------------------------------------------
#Calculate the ICC effective sample sizes
#------------------------------------------

N <- nrow(data1_int)

########data1
#get the ICC
ICC.1 <- .19
# calculate ICC N_eff
N_eff_ICC.1 <- N / (1 + (n - 1) * ICC.1)
N_eff_ICC.1 

#######data 2

#get the ICC
ICC.2 <- 0.03 
# calculate ICC N_eff
N_eff_ICC.2 <- N / (1 + (n - 1) * ICC.2)
N_eff_ICC.2  #1474

#########data 3
#get the ICC
ICC.3 <- 0.3

# calculate ICC N_eff
N_eff_ICC.3 <- N / (1 + (n - 1) * ICC.3)
N_eff_ICC.3  #252

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Simulate a data set from a random intercept-only model (Added after revision) N_level-1 = 400
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#=============================
# Data with ICC = 0.19
#============================

set.seed(12)
nG <- 20   
n  <- 20  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.34)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)   
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <- u_0 + epsilon  
data4_int <- data.frame(y, group, X1, X2)

lmer_model4_int <- lmer(y ~ 1 + (1| group), REML = FALSE, data = data4_int)
summary(lmer_model4_int)

#=============================
# Data with ICC = 0.03
#============================

set.seed(12)
nG <- 20   
n  <- 20  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.12)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <- u_0 + epsilon  
data5_int <- data.frame(y, group, X1, X2)

lmer_model.5_int <- lmer(y ~ 1 + (1| group), REML = FALSE, data = data5_int)
summary(lmer_model.5_int)

#=============================
# Data with ICC = 0.3
#============================

set.seed(12)
nG <- 20   
n  <- 20  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.48)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <- u_0 + epsilon 
data6_int <- data.frame(y, group, X1, X2)

lmer_model.6_int <- lmer(y ~ 1 + (1| group), REML = FALSE, data = data6_int)
summary(lmer_model.6_int)

#=====================================================
# Calculate MI effective sample size for the full model
#====================================================

#++++++++++++++++++++++++
#Jags model specification
#++++++++++++++++++++++++


# Jags model

model <- "model{   
  # Likelihood
  for (i in 1:400){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] 
  }
  # level 2
  for(j in 1:20){  #we have 80 groups
    alpha[j] ~ dnorm(mu.alpha, tau.alpha) 


  }
# Priors

mu.alpha ~ dnorm(0, 0.0001)

tau ~ dgamma (0.001, 0.001)

tau.alpha ~ dgamma (0.001, 0.001)

}"

#using functions:

N_eff_0_4_int <- mi.Neff.2pred.nG20.0(data4_int)
N_eff_0_5_int <- mi.Neff.2pred.nG20.0(data5_int)
N_eff_0_6_int <- mi.Neff.2pred.nG20.0(data6_int)


#-------------------------------------------
#Calculate the ICC effective sample sizes
#------------------------------------------
N <- nrow(data4_int)
########data1

#get the ICC
summary(lmer_model4_int)
ICC.4 <- .19
# calculate ICC N_eff
N_eff_ICC.4_int <- N / (1 + (n - 1) * ICC.4)
N_eff_ICC.4_int 

#######data 2

#get the ICC
summary(lmer_model.5_int)
ICC.5 <- 0.03 
# calculate ICC N_eff
N_eff_ICC.5_int <- N / (1 + (n - 1) * ICC.5)
N_eff_ICC.5_int  #1474

#########data 3

#get the ICC
summary(lmer_model.6_int)
ICC.6 <- 0.3

# calculate ICC N_eff
N_eff_ICC.6_int <- N / (1 + (n - 1) * ICC.6)
N_eff_ICC.6_int  #252


#######################################
#FULL MODELS - Including predictors
#######################################


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Simulate a data set where the marginal R^2 for the fixed effects is zero and N_level-1 = 3200
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#=============================
# Data with ICC = 0.19
#============================

set.seed(123)
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
data1 <- data.frame(y, group, X1, X2)

lmer_model1 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data1)
# random- intercept only
lmer_model1_0 <- lmer(y ~ 1+ (1| group), REML = FALSE, data = data1)
summary(lmer_model1)
summary(lmer_model1_0)

#=============================
# Data with ICC = 0.03
#============================

set.seed(123)
nG <- 80   
n  <- 40  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.1)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data2 <- data.frame(y, group, X1, X2)

lmer_model2 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data2)
# random- intercept only
lmer_model2_0 <- lmer(y ~ 1+ (1| group), REML = FALSE, data = data2)
summary(lmer_model2)
summary(lmer_model2_0)

#=============================
# Data with ICC = 0.3
#============================

set.seed(123)
nG <- 80   
n  <- 40  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.41)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data3 <- data.frame(y, group, X1, X2)

lmer_model3 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data3)
# random- intercept only
lmer_model3_0 <- lmer(y ~ 1+ (1| group), REML = FALSE, data = data3)
summary(lmer_model3)
summary(lmer_model3_0)

#=====================================================
# Calculate MI effective sample size for the full model
#====================================================

#++++++++++++++++++++++++
#Jags model specification
#++++++++++++++++++++++++

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



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Estimate the model and check the convergence and posterior estimates - Appendix A3
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model.def <- rjags::jags.model(file = textConnection(model), data = data1, n.chains = 2,
                               inits = list(.RNG.name="base::Wichmann-Hill",
                                            .RNG.seed=100))

update(object = model.def, n.iter = 1000)

#only the random effects (the ones we need for the aim of this study)
parameters <- c("mu.alpha", "mu.beta_1", "mu.beta_2", "tau", "invSigma")

results <- rjags::coda.samples(model = model.def, variable.names = parameters, n.iter =1000)


summary(results)
summary(lmer_model1) #compare with lmer

# Get the random effects in terms of variances:

1/11.32082  #intercept var
1/105.88326 #slope 1 var
1/39.79075  #slope 2 var
1/2.74561 #residual var

#+++++++++++++++++++++++++++++++++++++++++++++
#Calculate the effective sample size and B
#++++++++++++++++++++++++++++++++++++++++++++

N_eff_1 <- mi.Neff.2pred.nG80(data1)
N_eff_2 <- mi.Neff.2pred.nG80(data2)
N_eff_3 <- mi.Neff.2pred.nG80(data3)

#===============================================================================
# Multiple imputation vs ICC
#===============================================================================


#====================================================================
# Calculate MI effective sample size for the random-intercept models
#===================================================================

#=========================================================
# JAGS specification for a random intercept-only model 
#=======================================================

model <- "model{   
  # Likelihood
  for (i in 1:3200){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] 
  }
  # level 2
  for(j in 1:80){  #we have 80 groups
    alpha[j] ~ dnorm(mu.alpha, tau.alpha) #intercept


  }
######### Priors

mu.alpha ~ dnorm(0, 0.0001)

tau ~ dgamma (0.001, 0.001)

tau.alpha ~ dgamma (0.001, 0.001)

}"


#using functions:

N_eff_0_1 <- mi.Neff.2pred.nG80.0(data1)
N_eff_0_2 <- mi.Neff.2pred.nG80.0(data2)
N_eff_0_3 <- mi.Neff.2pred.nG80.0(data3)




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Simulate a data set where the marginal R^2 for the fixed effects is zero and N_level-1 = 400
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#=============================
# Data with ICC = 0.19
#============================

set.seed(12)
nG <- 20   
n  <- 20  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.34)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)   
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data4 <- data.frame(y, group, X1, X2)

lmer_model4 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data4)
# random- intercept only
lmer_model4_0 <- lmer(y ~ 1+ (1| group), REML = FALSE, data = data4)
summary(lmer_model4)
summary(lmer_model4_0)



#=============================
# Data with ICC = 0.03
#============================

set.seed(12)
nG <- 20   
n  <- 20  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.12)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data5 <- data.frame(y, group, X1, X2)

lmer_model.5 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data5)
summary(lmer_model.5)

#=============================
# Data with ICC = 0.3
#============================

set.seed(12)
nG <- 20   
n  <- 20  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.48)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data6 <- data.frame(y, group, X1, X2)

lmer_model.6 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data6)
summary(lmer_model.6)

#=====================================================
# Calculate MI effective sample size for the full model
#====================================================

#++++++++++++++++++++++++
#Jags model specification
#++++++++++++++++++++++++


# Jags model
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


N_eff_4 <- mi.Neff.2pred.nG20(data4)
N_eff_5 <- mi.Neff.2pred.nG20(data5)
N_eff_6 <- mi.Neff.2pred.nG20(data6)

#===============================================================================
# Multiple imputation vs ICC
#===============================================================================


#====================================================================
# Calculate MI effective sample size for the random-intercept models
#===================================================================

#=========================================================
# JAGS specification for a random intercept-only model 
#=======================================================

model <- "model{   
  # Likelihood
  for (i in 1:400){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] 
  }
  # level 2
  for(j in 1:20){  #we have 80 groups
    alpha[j] ~ dnorm(mu.alpha, tau.alpha) 


  }
# Priors

mu.alpha ~ dnorm(0, 0.0001)

tau ~ dgamma (0.001, 0.001)

tau.alpha ~ dgamma (0.001, 0.001)

}"

#using functions:

N_eff_0_4 <- mi.Neff.2pred.nG20.0(data4)
N_eff_0_5 <- mi.Neff.2pred.nG20.0(data5)
N_eff_0_6 <- mi.Neff.2pred.nG20.0(data6)


#-------------------------------------------
#Calculate the ICC effective sample sizes
#------------------------------------------
N <- nrow(data4)
########data1

#get the ICC
summary(lmer_model4)
ICC.4 <- .19
# calculate ICC N_eff
N_eff_ICC.4 <- N / (1 + (n - 1) * ICC.4)
N_eff_ICC.4 

#######data 2

#get the ICC
summary(lmer_model.5)
ICC.5 <- 0.03 
# calculate ICC N_eff
N_eff_ICC.5 <- N / (1 + (n - 1) * ICC.5)
N_eff_ICC.5  #1474

#########data 3

#get the ICC
summary(lmer_model.6)
ICC.6 <- 0.3

# calculate ICC N_eff
N_eff_ICC.6 <- N / (1 + (n - 1) * ICC.6)
N_eff_ICC.6  #252


# The ICC N_eff are the same

# Combine everything 

ICC <- rep(c(0.03, 0.19, 0.3), 2)

N_lvl1 <- rep(c(3200, 400), each = 3)

ICC_N <- c(N_eff_ICC.2, N_eff_ICC.1, N_eff_ICC.3, N_eff_ICC.5, 
           N_eff_ICC.4, N_eff_ICC.6)

MI_N0_int <- c(N_eff_0_2_int, N_eff_0_1_int, N_eff_0_3_int, N_eff_0_5_int, 
               N_eff_0_4_int, N_eff_0_6_int)

MI_N0 <- c(N_eff_0_2, N_eff_0_1, N_eff_0_3, N_eff_0_5, N_eff_0_4, N_eff_0_6)

MI_N <- c(N_eff_2, N_eff_1, N_eff_3, N_eff_5, N_eff_4, N_eff_6)

effective_sample_size <- data.frame(ICC, N_lvl1, ICC_N, MI_N0_int, 
                                    ICC_N, MI_N0, MI_N)

effective_sample_size[, c(3:7)] <- apply(effective_sample_size[, c(3:7)] , 
                                         2, round)

names(effective_sample_size) <- c("ICC","N = level-1 obs.", "N = ICC effective-int", 
                                  "MI-int","N = ICC effective-full", 
                                  "N = MI (intercept-only model)", 
                                  "N = MI (full model)")

write.csv(effective_sample_size, "effective_sample_size.csv")
