#===============================================================
# Example section
# these examples are also available at: https://www.nikolasekulovski.com/tutorials/
#==============================================================
setwd("")  # set your working directory such that you can source the functions 
# you can also do this through RStudio by clicking Session -> Set Working Directory -> Choose Directory...
# packages
library(R2MLwiN)
library(lme4)
library(MASS)
library(tidyverse)
library(jtools)
library(rjags)
source("wrapper_function.R")

# Data --------------------------------------------------------------------

# load
data("tutorial")

# inspect  
head(tutorial)
summary(tutorial)

# subset
tutorial <- tutorial[, c(1,2,3,5)]

# Model---------------------------------------------------------------------

# Fit lmer model 
model.1 <- lmer(normexam ~ standlrt + (standlrt | school), REML = FALSE, data = tutorial)

#inspect model fit
summary(model.1)

# Check for the R^2_m------------------------------------------------------

# 1st option using the `summ` function from `jtools`
summ(model.1)  # .32 (large effect size)

# 2nd option to calculate it by hand
fixef <- fixef(model.1)
y_hat <- fixef[1] + fixef[2]*tutorial$standlrt
sigma_f  <- var(y_hat)
sigma_u0 <- 0.09044
sigma_u1 <- 0.01454
sigma_e  <- 0.55366
Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed  


# Calculate the MI-based effective sample size -----------------------------

jags.model <-  "model {
# Likelihood
for (i in 1:4059){
  normexam[i] ~ dnorm(mu[i], tau)
  mu[i] <- alpha[school[i]] + beta[school[i]] * standlrt[i]
}

# Level-2
for(j in 1:65){
alpha[j] <- U[j,1]
beta[j]  <-  U[j,2]
U[j,1:2] ~ dmnorm (MU[j,], invSigma[,])
MU[j,1] <- mu.alpha
MU[j,2] <- mu.beta
}

# (hyper)Priors
mu.alpha ~ dnorm(0, 0.0001)
mu.beta  ~ dnorm(0,  0.0001)
tau ~ dgamma (0.001, 0.001)            # resiudal variance
invSigma[1:2,1:2] ~ dwish(Tau, 2)      # inverse of covariance matrix following a Wishard dist.
  tau.alpha ~ dgamma (0.001, 0.001)    # intercept variance
  tau.beta  ~ dgamma (0.001, 0.001)    # slope variance
  Tau[1,1] <- pow(tau.alpha, -1/2)     # construct the cov matrix
  Tau[2,2] <- pow(tau.beta, -1/2)
  Tau[1,2] <- rho_1*tau.alpha*tau.beta # covariance between the slope of standlrt and the intrcept
  Tau[2,1] <- Tau[1,2]
  rho_1 ~ dunif(-1, 1)                 # correlation (between -1 and 1)
}
"


# Check and inspect the model
model.def <- jags.model(file = textConnection(jags.model),
                        inits = list(.RNG.name="base::Wichmann-Hill",
                                     .RNG.seed=100),
                        data = tutorial, n.chains = 2)

update(object = model.def, n.iter = 1000)

# for checking whether the parameters are estimated correctly
parameters <- c("mu.alpha", "mu.beta", "InvSigma")


results <- coda.samples(model = model.def, variable.names = parameters, n.iter =1000)
summary(results)

#fit the model again but ask to monitor all the random effects
model.def <- jags.model(file = textConnection(jags.model),
                        inits = list(.RNG.name="base::Wichmann-Hill",
                                     .RNG.seed=100),
                        data = tutorial, n.chains = 2)

update(object = model.def, n.iter = 1000)

#only the random effects (the ones we need for the aim of this study)
parameters <- c("alpha", "beta", "mu.alpha", "mu.beta")

results <- coda.samples(model = model.def, variable.names = parameters, n.iter =1000)


chain1 <- as.data.frame(results[[1]])
chain2 <- as.data.frame(results[[2]])
samples <- rbind(chain1, chain2) #combine both chains

fixed_alphas <- samples[, 131]
fixed_betas <- samples[, 132]
fixed_alphas <- as.data.frame(fixed_alphas)
fixed_betas <- as.data.frame(fixed_betas)

samp_fixed_alphas <- list()
for (i in 1:nrow(fixed_alphas)){
  samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(tutorial))  
}

samp_fixed_betas <- list()
for (i in 1:nrow(fixed_betas)){
  samp_fixed_betas[[i]] <-rep.int(fixed_betas[i, 1], nrow(tutorial))  
}

split <- split(tutorial, tutorial$school)

n_gr <- matrix(nrow = length(split), ncol = 1)
for (i in seq_along(split)){
  n_gr[i, 1] <- nrow(split[[i]])
}

n_gr <- sapply(split, nrow) #same thing 

samples <- samples[, -c(131,132)]

alphas <- samples [, 1:65] #extract the intercepts
betas <- samples [, 66:130] #extract the slopes

samp_alphas <- list()

for (i in 1:nrow(alphas)){
  samp_alphas[[i]] <-rep.int(alphas[i, ], n_gr)  
}

samp_alphas_mat <- matrix(nrow = nrow(tutorial), ncol = nrow(alphas))

for(i in 1:nrow(alphas)){
  samp_alphas_mat[, i] <- unlist(samp_alphas[[i]])
}


samp_betas <- list()

for (i in 1:nrow(betas)){
  samp_betas[[i]] <-rep.int(betas[i, ], n_gr)  
}

samp_betas_mat <- matrix(nrow = nrow(tutorial), ncol = nrow(betas))

for(i in 1:nrow(betas)){
  samp_betas_mat[, i] <- unlist(samp_betas[[i]])
}

imputed <- list()

for (i in 1:ncol(samp_alphas_mat)){
  imputed[[i]] <- cbind(tutorial, samp_alphas_mat[, i])
}

for (i in 1:ncol(samp_betas_mat)){
  imputed[[i]] <- cbind(imputed[[i]], samp_betas_mat[, i])
}


for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_alphas[[i]])
}

for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_betas[[i]])
}


transform_z <- function(df){
  df$z <- df[, 3] - df[, 5] - df[, 6] * df[, 4] + df[, 7] + df[, 8] * df[, 4]
  df
}

imputed <- lapply(imputed, transform_z)

estimates <- list()
vCOV <- list()
for(i in seq_along(imputed)){
  estimates[[i]] <- coef(lm(imputed[[i]][,9] ~ imputed[[i]][,4]))
  vCOV [[i]] <- vcov(lm(imputed[[i]][,9] ~ imputed[[i]][,4]))
}

estimates <- unlist(estimates)
intercepts <- estimates[seq(1,length(estimates),2)] #select every other element
slopes <- estimates[seq(2,length(estimates),2)] #opposite
estimates <- data.frame(intercepts, slopes) #combine

m <- nrow(estimates) #number of "imputations (iterations in this case)

eta_bar <- apply(estimates, 2, mean) 
eta_bar <- t(eta_bar)
eta_bar <- as.matrix(eta_bar)

V <- matrix(nrow = nrow(estimates), ncol = 4)

for(i in seq_along(vCOV)){
  V[i, 1] <- vCOV[[i]][1,1]
  V[i, 2] <- vCOV[[i]][1,2]
  V[i, 3] <- vCOV[[i]][2,1]
  V[i, 4] <- vCOV[[i]][2,2]
}

U_bar <- apply(V, 2, mean)
U_bar <- matrix(U_bar, nrow = 2, ncol = 2)

B <- cov(estimates)

Total_var <- U_bar + (1 + 1/m)*B

k <- ncol(estimates) #number of parameters
lambda_hat <- (1+ 1/m) * sum(diag(B %*% ginv(Total_var)))/k

N <- nrow(tutorial) #sample size

nu_old <- (m - 1)/lambda_hat^2

nu_com <- N - k

nu_obs <- ((nu_com +1)/(nu_com + 3))*nu_com*(1 - lambda_hat)

nu <- (nu_old*nu_obs)/(nu_old + nu_obs)     

gamma <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))

MI_N_eff <- N - gamma*N  
MI_N_eff  #MI_N_eff

# Calculate the BF --------------------------------------------------------

hypotheses <- "standlrt =  0; 
               standlrt > 0"

BFs.1 <- bain_2lmer(model.1, hypotheses, standardize = F, N = MI_N_eff, seed = 123, jref = TRUE)
print(BFs.1)              

#take the inverse of BF_ou

BFu0 <- 1/BFs.1[["fit"]]$BF[1]
BFu0

# inspect b

BFs.1$b

# Get BF_i0
BF_iu <- BFs.1[["fit"]]$BF[2]/BFs.1[["fit"]]$BF[1]
BF_iu

#========================================================================================
# Repeat the analyses with everything the same but now with the null hypothesis being true 
#=======================================================================================

#Simulate the same data set with the null being true ----------------------------

split <- split(tutorial, tutorial$school)
n_gr <- sapply(split, nrow)  

set.seed(123)
nG <- 65 #schools
b1 <- 0  # new reg coefficient
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, times = n_gr) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, times = n_gr) 
epsilon <- rnorm(4059, 0, 0.7)
normexam <-  b1 *tutorial$standlrt +  u_0 + u_1*tutorial$standlrt + epsilon  #construct the new outcome 
tutorial.2 <- data.frame(tutorial$school, tutorial$student, normexam, tutorial$standlrt)
names(tutorial.2) <- c("school", "student", "normexam", "standlrt")

# Model-----------------------------------------------------------------------

# Fit lmer model 
model.2 <- lmer(normexam ~ standlrt + (standlrt | school), REML = FALSE, data = tutorial.2)

#inspect model fit
summary(model.2)

# Check for the R^2_m

# 1st option using the `summ` function from `jtools`
summ(model.2)  # 0

# Calculate the MI-based effective sample size --------------------------------


#fit the model again but ask to monitor all the random effects
model.def <- jags.model(file = textConnection(jags.model),
                        inits = list(.RNG.name="base::Wichmann-Hill",
                                     .RNG.seed=100),
                        data = tutorial.2, n.chains = 2)

update(object = model.def, n.iter = 1000)

#only the random effects (the ones we need for the aim of this study)
parameters <- c("alpha", "beta", "mu.alpha", "mu.beta")

results <- coda.samples(model = model.def, variable.names = parameters, n.iter =1000)


chain1 <- as.data.frame(results[[1]])
chain2 <- as.data.frame(results[[2]])
samples <- rbind(chain1, chain2) #combine both chains

fixed_alphas <- samples[, 131]
fixed_betas <- samples[, 132]
fixed_alphas <- as.data.frame(fixed_alphas)
fixed_betas <- as.data.frame(fixed_betas)

samp_fixed_alphas <- list()
for (i in 1:nrow(fixed_alphas)){
  samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(tutorial))  
}

samp_fixed_betas <- list()
for (i in 1:nrow(fixed_betas)){
  samp_fixed_betas[[i]] <-rep.int(fixed_betas[i, 1], nrow(tutorial))  
}

split <- split(tutorial, tutorial$school)

n_gr <- matrix(nrow = length(split), ncol = 1)
for (i in seq_along(split)){
  n_gr[i, 1] <- nrow(split[[i]])
}

n_gr <- sapply(split, nrow) #same thing 

samples <- samples[, -c(131,132)]

alphas <- samples [, 1:65] #extract the intercepts
betas <- samples [, 66:130] #extract the slopes

samp_alphas <- list()

for (i in 1:nrow(alphas)){
  samp_alphas[[i]] <-rep.int(alphas[i, ], n_gr)  
}

samp_alphas_mat <- matrix(nrow = nrow(tutorial), ncol = nrow(alphas))

for(i in 1:nrow(alphas)){
  samp_alphas_mat[, i] <- unlist(samp_alphas[[i]])
}


samp_betas <- list()

for (i in 1:nrow(betas)){
  samp_betas[[i]] <-rep.int(betas[i, ], n_gr)  
}

samp_betas_mat <- matrix(nrow = nrow(tutorial), ncol = nrow(betas))

for(i in 1:nrow(betas)){
  samp_betas_mat[, i] <- unlist(samp_betas[[i]])
}

imputed <- list()

for (i in 1:ncol(samp_alphas_mat)){
  imputed[[i]] <- cbind(tutorial, samp_alphas_mat[, i])
}

for (i in 1:ncol(samp_betas_mat)){
  imputed[[i]] <- cbind(imputed[[i]], samp_betas_mat[, i])
}


for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_alphas[[i]])
}

for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_betas[[i]])
}


transform_z <- function(df){
  df$z <- df[, 3] - df[, 5] - df[, 6] * df[, 4] + df[, 7] + df[, 8] * df[, 4]
  df
}

imputed <- lapply(imputed, transform_z)

estimates <- list()
vCOV <- list()
for(i in seq_along(imputed)){
  estimates[[i]] <- coef(lm(imputed[[i]][,9] ~ imputed[[i]][,4]))
  vCOV [[i]] <- vcov(lm(imputed[[i]][,9] ~ imputed[[i]][,4]))
}

estimates <- unlist(estimates)
intercepts <- estimates[seq(1,length(estimates),2)] #select every other element
slopes <- estimates[seq(2,length(estimates),2)] #opposite
estimates <- data.frame(intercepts, slopes) #combine

m <- nrow(estimates) #number of "imputations (iterations in this case)

eta_bar <- apply(estimates, 2, mean) 
eta_bar <- t(eta_bar)
eta_bar <- as.matrix(eta_bar)

V <- matrix(nrow = nrow(estimates), ncol = 4)

for(i in seq_along(vCOV)){
  V[i, 1] <- vCOV[[i]][1,1]
  V[i, 2] <- vCOV[[i]][1,2]
  V[i, 3] <- vCOV[[i]][2,1]
  V[i, 4] <- vCOV[[i]][2,2]
}

U_bar <- apply(V, 2, mean)
U_bar <- matrix(U_bar, nrow = 2, ncol = 2)

B <- cov(estimates)

Total_var <- U_bar + (1 + 1/m)*B

k <- ncol(estimates) #number of parameters
lambda_hat <- (1+ 1/m) * sum(diag(B %*% ginv(Total_var)))/k

N <- nrow(tutorial) #sample size

nu_old <- (m - 1)/lambda_hat^2

nu_com <- N - k

nu_obs <- ((nu_com +1)/(nu_com + 3))*nu_com*(1 - lambda_hat)

nu <- (nu_old*nu_obs)/(nu_old + nu_obs)     

gamma <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))

MI_N_eff.2 <- N - gamma*N  
MI_N_eff.2  #MI_N_eff

# Calculate the ICC-based effective sample size -----------------------------

model_0 <- lmer(normexam ~1 + (1|school), REML = FALSE, data = tutorial.2)
summ(model_0)
ICC <- 0.16
n_clus <- mean(sapply(split, nrow)) #the average group size (62.4 in this case)
ICC_N_eff.2 <- N / (1 + (n_clus - 1) * ICC)
ICC_N_eff.2 

# Calculate the BF -----------------------------------------------------------


BFs.2 <- bain_2lmer(model.2, hypotheses, standardize = F, N = MI_N_eff.2, seed = 123, jref = TRUE)
print(BFs.2)     

BF_0i.1 <- BFs.2[["fit"]]$BF[1]/BFs.2[["fit"]]$BF[2]
BF_0i.1

#incpect the fraction b
BFs.2$b

#=============================================================================
#Include a level-2 predictor
#============================================================================

# Fit lmer model 
model.3 <- lmer(normexam ~ standlrt + avslrt + (standlrt | school), REML = FALSE, data = tutorial)

summ(model.3)

#calculate the BF

hypotheses <- "standlrt = avslrt = 0;
               standlrt > 0 & avslrt > 0"


BFs.3 <- bain_2lmer(model.3, hypotheses, standardize = F, N = "ICC_effective", seed = 123, jref = TRUE)
print(BFs.3)
BFs.3$b

#Simulate the same data set with the null being true ----------------------------

split <- split(tutorial, tutorial$school)
n_gr <- sapply(split, nrow)  


set.seed(12)
nG <- 65 #schools
b1 <- 0  # new reg coefficient
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, times = n_gr) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, times = n_gr) 
epsilon <- rnorm(4059, 0, 0.7)
normexam <-  b1 *tutorial$standlrt + b1*tutorial$avslrt +  u_0 + u_1*tutorial$standlrt + epsilon  # construct the new outcome 
tutorial.3 <- data.frame(tutorial$school, tutorial$student, normexam, tutorial$standlrt, tutorial$avslrt)
names(tutorial.3) <- c("school", "student", "normexam", "standlrt", "avslrt")

# Fit lmer model 
model.4 <- lmer(normexam ~ standlrt + avslrt + (standlrt | school), REML = FALSE, data = tutorial.3)

summ(model.4)


#calculate the BF (using the ICC-based effective sample size)

BFs.4 <- bain_2lmer(model.4, hypotheses, standardize = F, N = "ICC_effective", seed = 123, jref = TRUE)
print(BFs.4)
BFs.4$b

#calculate BF_ui
BF_0i.2 <- BFs.4[["fit"]]$BF[1]/BFs.4[["fit"]]$BF[2]
BF_0i.2
