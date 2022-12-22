#========================================================
# Additional explanation regarding the MI-based N_eff
#========================================================
library(lme4)
options(scipen=999)


# JAGS specification for the full model 
model_full <- "model {   
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


# # JAGS specification for the random intercept-only model
model_int <- "model{   
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

#============================
# Data with ICC = 0.19
#===========================
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
data <- data.frame(y, group, X1, X2)




# Obtain the B and Gamma values from the calculation of the MI-based N_eff based on the full model

model.def <- rjags::jags.model(file = textConnection(model_full), data = data, n.chains = 2,
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
  samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(data))  
}


samp_fixed_betas_1 <- list()
for (i in 1:nrow(fixed_betas_1)){
  samp_fixed_betas_1[[i]] <-rep.int(fixed_betas_1[i, 1], nrow(data))   
}

samp_fixed_betas_2 <- list()
for (i in 1:nrow(fixed_betas_2 )){
  samp_fixed_betas_2[[i]] <-rep.int(fixed_betas_2[i, 1], nrow(data))  
}

#Get sampled random effects
split <- split(data, data$group)
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
samp_alphas_mat <- matrix(nrow = nrow(data), ncol = nrow(alphas))
for(i in 1:nrow(alphas)){
  samp_alphas_mat[, i] <- unlist(samp_alphas[[i]])
}

#Repeat the same for the sloes
#for beta_1
samp_betas_1 <- list()
for (i in 1:nrow(betas_1)){
  samp_betas_1[[i]] <-rep.int(betas_1[i, ], n_gr)  
}

samp_betas_mat_1 <- matrix(nrow = nrow(data), ncol = nrow(betas_1))

for(i in 1:nrow(betas_1)){
  samp_betas_mat_1[, i] <- unlist(samp_betas_1[[i]])
}
#for beta_2
samp_betas_2 <- list()
for (i in 1:nrow(betas_2)){
  samp_betas_2[[i]] <-rep.int(betas_2[i, ], n_gr)  
}

samp_betas_mat_2 <- matrix(nrow = nrow(data), ncol = nrow(betas_2))

for(i in 1:nrow(betas_2)){
  samp_betas_mat_2[, i] <- unlist(samp_betas_2[[i]])
}

#Combine

imputed <- list()

for (i in 1:ncol(samp_alphas_mat)){
  imputed[[i]] <- cbind(data, samp_alphas_mat[, i])
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

B_1 <- cov(estimates)
#B

Total_var_1 <- U_bar + (1 + 1/m)*B_1
#Total_var

k <- ncol(estimates) #number of parameters
lambda_hat <- (1+ 1/m) * sum(diag(B_1 %*% MASS::ginv(Total_var_1)))/k
#lambda_hat

N <- nrow(data)
nu_old <- (m - 1)/lambda_hat^2
#nu_old 
nu_com <- N - k
#nu_com
nu_obs <- ((nu_com +1)/(nu_com + 3))*nu_com*(1 - lambda_hat)
#nu_obs
nu <- (nu_old*nu_obs)/(nu_old + nu_obs)
#nu
gamma_1 <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))



# Obtain the B and Gamma values from the calculation of the MI-based N_eff based on thee random intercept-only model


model.def <- rjags::jags.model(file = textConnection(model_int), data = data, n.chains = 2,
                               inits = list(.RNG.name="base::Wichmann-Hill",
                                            .RNG.seed=100))

update(object = model.def, n.iter = 1000)

#only the random effects (the ones we need for the aim of this study)
parameters <- c("alpha", "mu.alpha")

results <- rjags::coda.samples(model = model.def, variable.names = parameters, n.iter =5000)

#summary(results)
#Extract the samples from the posterior (from both chains), and separate the (random) intercepts and slopes:
chain1 <- as.data.frame(results[[1]])
chain2 <- as.data.frame(results[[2]])
samples <- rbind(chain1, chain2) #combine both chains
#Get the fixed effects
fixed_alphas <- samples[, 81]   
fixed_alphas <- as.data.frame(fixed_alphas)


# Repeat each fixed effect N times (in this case 4059) and store it in a separate 
#data frame which will be merged with the imputed data sets further below.
samp_fixed_alphas <- list()
for (i in 1:nrow(fixed_alphas)){
  samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(data))  
}


#Get sampled random effects
split <- split(data, data$group)
#size of each gr
n_gr <- sapply(split, nrow)
#exclude the fixed effects
samples <- samples[, -81]

alphas <- samples [, 1:80] #extract the intercepts


#Use the size of each group to replicate, from each iteration, the respective alpha `n_gr` number of times.
#for alpha
samp_alphas <- list()
for (i in 1:nrow(alphas)){
  samp_alphas[[i]] <-rep.int(alphas[i, ], n_gr)  
}

#extract them in a big matrix with the samples from every iteration as a separate column.
samp_alphas_mat <- matrix(nrow = nrow(data), ncol = nrow(alphas))
for(i in 1:nrow(alphas)){
  samp_alphas_mat[, i] <- unlist(samp_alphas[[i]])
}


#Combine

imputed <- list()

for (i in 1:ncol(samp_alphas_mat)){
  imputed[[i]] <- cbind(data, samp_alphas_mat[, i])
}


#add the fixed effects
for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_alphas[[i]])
}


####Define the new lm model
#tranform
transform_z <- function(df){
  df$z <- df[, 1] - df[, 5]  + df[, 6]
  df
}
#apply to all imputed data sets
imputed <- lapply(imputed, transform_z)

#Fit the lm models
estimates <- list()
vCOV <- list()
for(i in seq_along(imputed)){
  estimates[[i]] <- coef(lm(imputed[[i]][,7] ~ 1))
  vCOV [[i]] <- vcov(lm(imputed[[i]][,7] ~ 1))
}
#Extract each type of coefficient
estimates <- unlist(estimates)
estimates <- data.frame(estimates)
m <- nrow(estimates) #number of "imputations (iterations in this case)
Q_bar <- apply(estimates, 2, mean) 
Q_bar <- t(Q_bar)
Q_bar <- as.matrix(Q_bar)
#Q_bar

SE <- unlist(vCOV)
SE <- data.frame(SE)
U_bar <- mean(SE$SE)
U_bar
#U_bar

B_2 <- cov(estimates)
#B

Total_var_2 <- U_bar + (1 + 1/m)*B_2
#Total_var

k <- ncol(estimates) #number of parameters
lambda_hat <- (1+ 1/m) * sum(diag(B_2 %*% MASS::ginv(Total_var_2)))/k
#lambda_hat

N <- nrow(data)
nu_old <- (m - 1)/lambda_hat^2
#nu_old 
nu_com <- N - k
#nu_com
nu_obs <- ((nu_com +1)/(nu_com + 3))*nu_com*(1 - lambda_hat)
#nu_obs
nu <- (nu_old*nu_obs)/(nu_old + nu_obs)
#nu
gamma_2 <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))





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
data <- data.frame(y, group, X1, X2)



# Obtain the B and Gamma values from the calculation of the MI-based N_eff based on the full model

model.def <- rjags::jags.model(file = textConnection(model_full), data = data, n.chains = 2,
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
  samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(data))  
}


samp_fixed_betas_1 <- list()
for (i in 1:nrow(fixed_betas_1)){
  samp_fixed_betas_1[[i]] <-rep.int(fixed_betas_1[i, 1], nrow(data))   
}

samp_fixed_betas_2 <- list()
for (i in 1:nrow(fixed_betas_2 )){
  samp_fixed_betas_2[[i]] <-rep.int(fixed_betas_2[i, 1], nrow(data))  
}

#Get sampled random effects
split <- split(data, data$group)
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
samp_alphas_mat <- matrix(nrow = nrow(data), ncol = nrow(alphas))
for(i in 1:nrow(alphas)){
  samp_alphas_mat[, i] <- unlist(samp_alphas[[i]])
}

#Repeat the same for the sloes
#for beta_1
samp_betas_1 <- list()
for (i in 1:nrow(betas_1)){
  samp_betas_1[[i]] <-rep.int(betas_1[i, ], n_gr)  
}

samp_betas_mat_1 <- matrix(nrow = nrow(data), ncol = nrow(betas_1))

for(i in 1:nrow(betas_1)){
  samp_betas_mat_1[, i] <- unlist(samp_betas_1[[i]])
}
#for beta_2
samp_betas_2 <- list()
for (i in 1:nrow(betas_2)){
  samp_betas_2[[i]] <-rep.int(betas_2[i, ], n_gr)  
}

samp_betas_mat_2 <- matrix(nrow = nrow(data), ncol = nrow(betas_2))

for(i in 1:nrow(betas_2)){
  samp_betas_mat_2[, i] <- unlist(samp_betas_2[[i]])
}

#Combine

imputed <- list()

for (i in 1:ncol(samp_alphas_mat)){
  imputed[[i]] <- cbind(data, samp_alphas_mat[, i])
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

B_3 <- cov(estimates)
#B

Total_var_3 <- U_bar + (1 + 1/m)*B_3
#Total_var

k <- ncol(estimates) #number of parameters
lambda_hat <- (1+ 1/m) * sum(diag(B_3 %*% MASS::ginv(Total_var_3)))/k
#lambda_hat

N <- nrow(data)
nu_old <- (m - 1)/lambda_hat^2
#nu_old 
nu_com <- N - k
#nu_com
nu_obs <- ((nu_com +1)/(nu_com + 3))*nu_com*(1 - lambda_hat)
#nu_obs
nu <- (nu_old*nu_obs)/(nu_old + nu_obs)
#nu
gamma_3 <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))



# Obtain the B and Gamma values from the calculation of the MI-based N_eff based on thee random intercept-only model


model.def <- rjags::jags.model(file = textConnection(model_int), data = data, n.chains = 2,
                               inits = list(.RNG.name="base::Wichmann-Hill",
                                            .RNG.seed=100))

update(object = model.def, n.iter = 1000)

#only the random effects (the ones we need for the aim of this study)
parameters <- c("alpha", "mu.alpha")

results <- rjags::coda.samples(model = model.def, variable.names = parameters, n.iter =5000)

#summary(results)
#Extract the samples from the posterior (from both chains), and separate the (random) intercepts and slopes:
chain1 <- as.data.frame(results[[1]])
chain2 <- as.data.frame(results[[2]])
samples <- rbind(chain1, chain2) #combine both chains
#Get the fixed effects
fixed_alphas <- samples[, 81]   
fixed_alphas <- as.data.frame(fixed_alphas)


# Repeat each fixed effect N times (in this case 4059) and store it in a separate 
#data frame which will be merged with the imputed data sets further below.
samp_fixed_alphas <- list()
for (i in 1:nrow(fixed_alphas)){
  samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(data))  
}


#Get sampled random effects
split <- split(data, data$group)
#size of each gr
n_gr <- sapply(split, nrow)
#exclude the fixed effects
samples <- samples[, -81]

alphas <- samples [, 1:80] #extract the intercepts


#Use the size of each group to replicate, from each iteration, the respective alpha `n_gr` number of times.
#for alpha
samp_alphas <- list()
for (i in 1:nrow(alphas)){
  samp_alphas[[i]] <-rep.int(alphas[i, ], n_gr)  
}

#extract them in a big matrix with the samples from every iteration as a separate column.
samp_alphas_mat <- matrix(nrow = nrow(data), ncol = nrow(alphas))
for(i in 1:nrow(alphas)){
  samp_alphas_mat[, i] <- unlist(samp_alphas[[i]])
}


#Combine

imputed <- list()

for (i in 1:ncol(samp_alphas_mat)){
  imputed[[i]] <- cbind(data, samp_alphas_mat[, i])
}


#add the fixed effects
for (i in 1:length(imputed)){
  imputed[[i]] <- cbind(imputed[[i]], samp_fixed_alphas[[i]])
}


####Define the new lm model
#tranform
transform_z <- function(df){
  df$z <- df[, 1] - df[, 5]  + df[, 6]
  df
}
#apply to all imputed data sets
imputed <- lapply(imputed, transform_z)

#Fit the lm models
estimates <- list()
vCOV <- list()
for(i in seq_along(imputed)){
  estimates[[i]] <- coef(lm(imputed[[i]][,7] ~ 1))
  vCOV [[i]] <- vcov(lm(imputed[[i]][,7] ~ 1))
}
#Extract each type of coefficient
estimates <- unlist(estimates)
estimates <- data.frame(estimates)
m <- nrow(estimates) #number of "imputations (iterations in this case)
Q_bar <- apply(estimates, 2, mean) 
Q_bar <- t(Q_bar)
Q_bar <- as.matrix(Q_bar)
#Q_bar

SE <- unlist(vCOV)
SE <- data.frame(SE)
U_bar <- mean(SE$SE)
U_bar
#U_bar

B_4 <- cov(estimates)
#B

Total_var_4 <- U_bar + (1 + 1/m)*B_4
#Total_var

k <- ncol(estimates) #number of parameters
lambda_hat <- (1+ 1/m) * sum(diag(B_4 %*% MASS::ginv(Total_var_4)))/k
#lambda_hat

N <- nrow(data)
nu_old <- (m - 1)/lambda_hat^2
#nu_old 
nu_com <- N - k
#nu_com
nu_obs <- ((nu_com +1)/(nu_com + 3))*nu_com*(1 - lambda_hat)
#nu_obs
nu <- (nu_old*nu_obs)/(nu_old + nu_obs)
#nu
gamma_4 <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))



# Combine everything: 
N_eff <- 1537
rand_int_03 <- data.frame(B_4, gamma_4, N_eff)
names(rand_int_03) <- c("B matrix", "Gamma", "N-eff")


N_eff <- 1231
full_03 <- data.frame(B_3, gamma_3, N_eff)
names(full_03) <- c("B matrix", "B matrix","B matrix", "Gamma", "N-eff")


N_eff <- 1009
full_19 <- data.frame(B_1, gamma_1, N_eff)
names(full_19) <- c("B matrix", "B matrix","B matrix", "Gamma", "N-eff")



N_eff <- 316
rand_int_19 <- data.frame(B_2, gamma_2, N_eff)
names(rand_int_19) <- c("B matrix", "Gamma", "N-eff")


# save 

Table_2 <- list("Random intercept(.03)" = rand_int_03, "Full (.03)" = full_03,
                "Random intercept(.19)" = rand_int_19, "Full (.19)" = full_19 )

sink("Table_2.txt")
print(Table_2)
sink()
