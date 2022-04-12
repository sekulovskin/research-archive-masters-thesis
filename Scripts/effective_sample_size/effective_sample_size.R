#============================================================================================================
#A new Estimator for effective sample size
# Note, only for the first data set the full code is used, for the remaining pre-written functions are used
# see: `mi-Neff_functions(L1).R` script 
#===========================================================================================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Master Thesis/Master's thesis repo/Effective_sample-size")
library(lme4)
library(jtools)
source("mi-Neff_functions(L1).R")  

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
summ(lmer_model1)

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

lmer_model.2 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data2)
summ(lmer_model.2)

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

lmer_model.3 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data3)
summ(lmer_model.3)

#=====================================================
# Calculate MI effective sample size for the full model
#====================================================

#++++++++++++++++++++++++
#Jags model specification
#++++++++++++++++++++++++

model <- "model {   
  # Likelihood
  for (i in 1:3200){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[group[i]] + beta_1[group[i]] * X1[i] + beta_2[group[i]] * X2[i]
  }
  # level 2
  for(j in 1:80){  #we have 80 groups
    alpha[j] <- B[j,1]
    beta_1[j]  <-  B[j,2]
    beta_2[j]  <-  B[j,3]
    B[j,1:3] ~ dmnorm (B.hat[j,], invSigma.B[,])
    B.hat[j,1] <- mu.alpha
    B.hat[j,2] <- mu.beta_1
    B.hat[j,3] <- mu.beta_2
  }
  # Priors
  mu.alpha ~ dnorm(0, 0.0001)
  mu.beta_1 ~ dnorm(0,  0.0001)
  mu.beta_2 ~ dnorm(0,  0.0001)
  tau ~ dgamma (0.001, 0.001)  #
  invSigma.B[1:3,1:3] ~ dwish(Sigma.B, 3)  
  sigma.alpha ~ dgamma (0.001, 0.001)  
  sigma.beta_1 ~ dgamma (0.001, 0.001)    
  sigma.beta_2 ~ dgamma (0.001, 0.001)   
  Sigma.B[1,1] <- pow(sigma.alpha, 2)  
  Sigma.B[2,2] <- pow(sigma.beta_1, 2)
  Sigma.B[3,3] <- pow(sigma.beta_2, 2)
  Sigma.B[1,2] <- rho_1*sigma.alpha*sigma.beta_1 
  Sigma.B[2,1] <- Sigma.B[1,2]
  Sigma.B[1,3] <- rho_2*sigma.alpha*sigma.beta_2 
  Sigma.B[3,1] <- Sigma.B[1,3]
  Sigma.B[2,3] <- rho_3*sigma.beta_1*sigma.beta_2 
  Sigma.B[3,2] <- Sigma.B[2,3]
  rho_1 ~ dunif(-1, 1)  #cor is between -1 and 1
  rho_2 ~ dunif(-1, 1)  #cor is between -1 and 1
  rho_3 ~ dunif(-1, 1)  #cor is between -1 and 1
}"



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Estimate the model and check the convergence and posterior estimates
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model.def <- rjags::jags.model(file = textConnection(model), data = data1, n.chains = 2,
                               inits = list(.RNG.name="base::Wichmann-Hill",
                                            .RNG.seed=100))

update(object = model.def, n.iter = 1000)

#only the random effects (the ones we need for the aim of this study)
parameters <- c("mu.alpha", "mu.beta_1", "mu.beta_2")
#parameters <- c("mu.alpha", "mu.beta_1", "mu.beta_2", "var.alpha", "var.beta_1", "var.beta_2" )
results <- rjags::coda.samples(model = model.def, variable.names = parameters, n.iter =1000)

#check convergence
#plot(results)
#coda::autocorr.plot(results)
#gelman.plot(results)

summary(results)
summary(lmer_model1) #compare with lmer

#+++++++++++++++++++++++++++++++++++++++++++++
#Calculate the effective sample size
#++++++++++++++++++++++++++++++++++++++++++++

#call rjags (again)
model.def <- rjags::jags.model(file = textConnection(model), data = data1, n.chains = 2,
                               inits = list(.RNG.name="base::Wichmann-Hill",
                                            .RNG.seed=100))

update(object = model.def, n.iter = 1000)

#only the random effects (the ones we need for the aim of this study)
parameters <- c("alpha", "beta_1", "beta_2","mu.alpha", "mu.beta_1", "mu.beta_2")

results <- rjags::coda.samples(model = model.def, variable.names = parameters, n.iter =1000)
#summary(results)

#Extract the samples from the posterior (from both chains), and separate the (random) intercepts and slopes:
chain1 <- as.data.frame(results[[1]])
chain2 <- as.data.frame(results[[2]])
samples <- rbind(chain1, chain2) #combine both chains
#Get the fixed effects
fixed_alphas <- samples[, 241]   
fixed_betas_1 <- samples[, 242]
fixed_betas_2 <- samples[, 243]
fixed_alphas <- as.data.frame(fixed_alphas)
fixed_betas_1 <- as.data.frame(fixed_betas_1)
fixed_betas_2 <- as.data.frame(fixed_betas_2)

# Repeat each fixed effect N times (in this case 4059) and store it in a separate 
#data frame which will be merged with the imputed data sets further below.
samp_fixed_alphas <- list()
for (i in 1:nrow(fixed_alphas)){
  samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(data1))  
}


samp_fixed_betas_1 <- list()
for (i in 1:nrow(fixed_betas_1)){
  samp_fixed_betas_1[[i]] <-rep.int(fixed_betas_1[i, 1], nrow(data1))   
}

samp_fixed_betas_2 <- list()
for (i in 1:nrow(fixed_betas_2 )){
  samp_fixed_betas_2[[i]] <-rep.int(fixed_betas_2[i, 1], nrow(data1))  
}

#Get sampled random effects
split <- split(data1, data1$group)
#size of each gr
n_gr <- sapply(split, nrow)
#exclude the fixed effects
samples <- samples[, -c(241,242,243)]

alphas <- samples [, 1:80] #extract the intercepts
betas_1 <- samples [, 81:160] #extract the slopes
betas_2 <- samples [, 161:240] #extract the slopes

#Use the size of each group to replicate, from each iteration, the respective alpha `n_gr` number of times.
#for alpha
samp_alphas <- list()
for (i in 1:nrow(alphas)){
  samp_alphas[[i]] <-rep.int(alphas[i, ], n_gr)  
}
#extract them in a big matrix with the samples from every iteration as a separate column.
samp_alphas_mat <- matrix(nrow = nrow(data1), ncol = nrow(alphas))
for(i in 1:nrow(alphas)){
  samp_alphas_mat[, i] <- unlist(samp_alphas[[i]])
}

#Repeat the same for the sloes
#for beta_1
samp_betas_1 <- list()
for (i in 1:nrow(betas_1)){
  samp_betas_1[[i]] <-rep.int(betas_1[i, ], n_gr)  
}

samp_betas_mat_1 <- matrix(nrow = nrow(data1), ncol = nrow(betas_1))

for(i in 1:nrow(betas_1)){
  samp_betas_mat_1[, i] <- unlist(samp_betas_1[[i]])
}
#for beta_2
samp_betas_2 <- list()
for (i in 1:nrow(betas_2)){
  samp_betas_2[[i]] <-rep.int(betas_2[i, ], n_gr)  
}

samp_betas_mat_2 <- matrix(nrow = nrow(data1), ncol = nrow(betas_2))

for(i in 1:nrow(betas_2)){
  samp_betas_mat_2[, i] <- unlist(samp_betas_2[[i]])
}

#Combine

imputed <- list()

for (i in 1:ncol(samp_alphas_mat)){
  imputed[[i]] <- cbind(data1, samp_alphas_mat[, i])
}

for (i in 1:ncol(samp_betas_mat_1)){
  imputed[[i]] <- cbind(imputed[[i]], samp_betas_mat_1[, i])
}

for (i in 1:ncol(samp_betas_mat_2)){
  imputed[[i]] <- cbind(imputed[[i]], samp_betas_mat_2[, i])
}

#add the fixed effects
for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_alphas[[i]])
}

for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_betas_1[[i]])
}
for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_betas_2[[i]])
}
####Define the new lm model
#tranform
transform_z <- function(df){
  df$z <- df[, 1] - df[, 5] - df[, 6] * df[, 3] - df[, 7] * df[, 4] + df[, 8] + df[, 9] * df[, 3] + df[, 10] * df[, 4]
  df
}
#apply to all imputed data sets
imputed <- lapply(imputed, transform_z)

#Fit the lm models
estimates <- list()
vCOV <- list()
for(i in seq_along(imputed)){
  estimates[[i]] <- coef(lm(imputed[[i]][,11] ~ imputed[[i]][,3] + imputed[[i]][,4]))
  vCOV [[i]] <- vcov(lm(imputed[[i]][,11] ~ imputed[[i]][,3] + imputed[[i]][,4]))
}
#Extract each type of coefficient
intercepts <- NULL
slopes1 <- NULL
slopes2 <- NULL
for(i in 1:length(estimates)){
  intercepts[i] <- estimates[[i]][1]
  slopes1[i] <- estimates[[i]][2]
  slopes2[i] <- estimates[[i]][3]
}
estimates <- data.frame(intercepts, slopes1, slopes2) #combine

# Apply the formulas by van Buuren (2012)
m <- nrow(estimates) #number of "imputations (iterations in this case)
Q_bar <- apply(estimates, 2, mean) 
Q_bar <- t(Q_bar)
Q_bar <- as.matrix(Q_bar)
#Q_bar

V <- matrix(nrow = nrow(estimates), ncol = 9)

for(i in seq_along(vCOV)){
  V[i, 1] <- vCOV[[i]][1,1]
  V[i, 2] <- vCOV[[i]][1,2]
  V[i, 3] <- vCOV[[i]][1,3]
  V[i, 4] <- vCOV[[i]][2,1]
  V[i, 5] <- vCOV[[i]][2,2]
  V[i, 6] <- vCOV[[i]][2,3]
  V[i, 7] <- vCOV[[i]][3,1]
  V[i, 8] <- vCOV[[i]][3,2]
  V[i, 9] <- vCOV[[i]][3,3]
  
}
U_bar <- apply(V, 2, mean)
U_bar <- matrix(U_bar, nrow = 3, ncol = 3)
#U_bar

B <- cov(estimates)
#B

Total_var <- U_bar + (1 + 1/m)*B
#Total_var

k <- ncol(estimates) #number of parameters
lambda_hat <- (1+ 1/m) * sum(diag(B %*% MASS::ginv(Total_var)))/k
#lambda_hat

N <- nrow(data1)
nu_old <- (m - 1)/lambda_hat^2
#nu_old 
nu_com <- N - k
#nu_com
nu_obs <- ((nu_com +1)/(nu_com + 3))*nu_com*(1 - lambda_hat)
#nu_obs
nu <- (nu_old*nu_obs)/(nu_old + nu_obs)
#nu
gamma <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))
#gamma
N_eff_1 <- N - gamma*N

# Calculate the MI effective sample size for the other two data sets 

N_eff_2 <- mi.Neff.2pred.nG80(data2)
N_eff_3 <- mi.Neff.2pred.nG80(data3)

#==================================================================================================
# Multiple imputation vs ICC
#=================================================================================================


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


#-------------------------------------------
#Calculate the ICC effective sample sizes
#------------------------------------------
N <- nrow(data1)
########data1

#get the ICC
summ(lmer_model1)
ICC.1 <- .19
# calculate ICC N_eff
N_eff_ICC.1 <- N / (1 + (n - 1) * ICC.1)
N_eff_ICC.1 

#######data 2

#get the ICC
summ(lmer_model.2)
ICC.2 <- 0.03 
# calculate ICC N_eff
N_eff_ICC.2 <- N / (1 + (n - 1) * ICC.2)
N_eff_ICC.2  #1474

#########data 3

#get the ICC
summ(lmer_model.3)
ICC.3 <- 0.3

# calculate ICC N_eff
N_eff_ICC.3 <- N / (1 + (n - 1) * ICC.3)
N_eff_ICC.3  #252



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Simulate a data set where the marginal R^2 for the fixed effects is zero and N_level-1 = 400
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#=============================
# Data with ICC = 0.19
#============================

set.seed(123)
nG <- 20   
n  <- 20  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.29)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)   
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data4 <- data.frame(y, group, X1, X2)

lmer_model4 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data4)
summ(lmer_model4)

#=============================
# Data with ICC = 0.03
#============================

set.seed(123)
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
summ(lmer_model.5)

#=============================
# Data with ICC = 0.3
#============================

set.seed(123)
nG <- 20   
n  <- 20  
b1 <- 0  
b2 <- 0 
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) 
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.39)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data6 <- data.frame(y, group, X1, X2)

lmer_model.6 <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data6)
summ(lmer_model.6)

#=====================================================
# Calculate MI effective sample size for the full model
#====================================================

#++++++++++++++++++++++++
#Jags model specification
#++++++++++++++++++++++++

#Model that doesn;t include a multivariate dist for the random effects

model <-  "model {   #Model with random slopes and intercept and no correlation between random effects
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

N_eff_4 <- mi.Neff.2pred.nG20(data4)
N_eff_5 <- mi.Neff.2pred.nG20(data5)
N_eff_6 <- mi.Neff.2pred.nG20(data6)

#==================================================================================================
# Multiple imputation vs ICC
#=================================================================================================


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
######### Priors

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
summ(lmer_model4)
ICC.4 <- .19
# calculate ICC N_eff
N_eff_ICC.4 <- N / (1 + (n - 1) * ICC.4)
N_eff_ICC.4 

#######data 2

#get the ICC
summ(lmer_model.5)
ICC.5 <- 0.03 
# calculate ICC N_eff
N_eff_ICC.5 <- N / (1 + (n - 1) * ICC.5)
N_eff_ICC.5  #1474

#########data 3

#get the ICC
summ(lmer_model.6)
ICC.6 <- 0.3

# calculate ICC N_eff
N_eff_ICC.6 <- N / (1 + (n - 1) * ICC.6)
N_eff_ICC.6  #252




# Combine everything 

ICC <- rep(c(0.03, 0.19, 0.3), 2)

N_lvl1 <- rep(c(3200, 400), each = 3)

MI_N0 <- c(N_eff_0_2, N_eff_0_1, N_eff_0_3, N_eff_0_5, N_eff_0_4, N_eff_0_6)

ICC_N <- c(N_eff_ICC.2, N_eff_ICC.1, N_eff_ICC.3, N_eff_ICC.5, N_eff_ICC.4, N_eff_ICC.6)

MI_N <- c(N_eff_2, N_eff_1, N_eff_3, N_eff_5, N_eff_4, N_eff_6)

effective_sample_size <- data.frame(ICC, N_lvl1, ICC_N, MI_N0, MI_N)

effective_sample_size[, c(3,4,5)] <- apply(effective_sample_size[, c(3,4,5)] , 2, round)

names(effective_sample_size) <- c("ICC","N = level-1 obs.", "N = ICC effective", "N = MI (intercept-only)", "N = MI (full model)")

write.csv(effective_sample_size, "effective_sample_size.csv")
