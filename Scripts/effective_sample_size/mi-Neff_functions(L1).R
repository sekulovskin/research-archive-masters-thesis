#Function that calculates the Effective sample size based on the multiple imputation approach
#tailored for the specific data sets for this simulation!!!

# This function, at its current state, is not very user-friendly.
#If you wish to use it you have to name everything in the exact same way as it is done here

#-------------------
# 1 predictor
#-----------------

#data <- n_0.4k_1pred[[1]]
#NG = 20

mi.Neff.1pred.nG20 <- function(data){
  
  model.def <- rjags::jags.model(file = textConnection(model), data = data, n.chains = 2, 
                                 inits = list(.RNG.name="base::Wichmann-Hill",
                                 .RNG.seed=100))
  update(object = model.def, n.iter = 1000)
  
  #only the random effects (the ones we need for the aim of this study)
  parameters <- c("alpha", "beta_1","mu.alpha", "mu.beta_1")
  
  results <- rjags::coda.samples(model = model.def, variable.names = parameters, n.iter =1000)
  #summary(results)
  
  #Extract the samples from the posterior (from both chains), and separate the (random) intercepts and slopes:
  chain1 <- as.data.frame(results[[1]])
  chain2 <- as.data.frame(results[[2]])
  samples <- rbind(chain1, chain2) #combine both chains
  #Get the fixed effects
  fixed_alphas <- samples[, 41]   
  fixed_betas_1 <- samples[, 42]
  fixed_alphas <- as.data.frame(fixed_alphas)
  fixed_betas_1 <- as.data.frame(fixed_betas_1)
  
  samp_fixed_alphas <- list()
  for (i in 1:nrow(fixed_alphas)){
    samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(data))  
  }
  
  
  samp_fixed_betas_1 <- list()
  for (i in 1:nrow(fixed_betas_1)){
    samp_fixed_betas_1[[i]] <-rep.int(fixed_betas_1[i, 1], nrow(data))   
  }
  
  
  #Get sampled random effects
  split <- split(data, data$group)
  #size of each gr
  n_gr <- sapply(split, nrow)
  #exclude the fixed effects
  samples <- samples[, -c(41,42)]
  
  alphas <- samples [, 1:20] #extract the intercepts
  betas_1 <- samples [, 21:40] #extract the slopes
  
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
  
  
  #Combine
  
  imputed <- list()
  
  for (i in 1:ncol(samp_alphas_mat)){
    imputed[[i]] <- cbind(data, samp_alphas_mat[, i])
  }
  
  for (i in 1:ncol(samp_betas_mat_1)){
    imputed[[i]] <- cbind(imputed[[i]], samp_betas_mat_1[, i])
  }
  
  
  #add the fixed effects
  for (i in 1:length(imputed)){
    imputed[[i]] <- cbind(imputed[[i]], samp_fixed_alphas[[i]])
  }
  
  for (i in 1:length(imputed)){
    imputed[[i]] <- cbind(imputed[[i]], samp_fixed_betas_1[[i]])
  }
  
  ####Define the new lm model
  #transform
  transform_z <- function(df){
    df$z <- df[, 1] - df[, 4] - df[, 5] * df[, 3] + df[, 6] + df[, 7] * df[, 3] 
    df
  }
  #apply to all imputed data sets
  imputed <- lapply(imputed, transform_z)
  
  #Fit the lm models
  estimates <- list()
  vCOV <- list()
  for(i in seq_along(imputed)){
    estimates[[i]] <- coef(lm(imputed[[i]][,8] ~ imputed[[i]][,3]))
    vCOV [[i]] <- vcov(lm(imputed[[i]][,8] ~ imputed[[i]][,3]))
  }
  #Extract each type of coefficient
  intercepts <- NULL
  slopes1 <- NULL
  for(i in 1:length(estimates)){
    intercepts[i] <- estimates[[i]][1]
    slopes1[i] <- estimates[[i]][2]
  }
  estimates <- data.frame(intercepts, slopes1) #combine
  
  # Apply the formulas by van Buuren (2012)
  m <- nrow(estimates) #number of "imputations (iterations in this case)
  Q_bar <- apply(estimates, 2, mean) 
  Q_bar <- t(Q_bar)
  Q_bar <- as.matrix(Q_bar)
  #Q_bar
  
  V <- matrix(nrow = nrow(estimates), ncol = 4)
  
  for(i in seq_along(vCOV)){
    V[i, 1] <- vCOV[[i]][1,1]
    V[i, 2] <- vCOV[[i]][1,2]
    V[i, 3] <- vCOV[[i]][2,1]
    V[i, 4] <- vCOV[[i]][2,2]
  }
  
  U_bar <- apply(V, 2, mean)
  U_bar <- matrix(U_bar, nrow = 2, ncol = 2)
  #U_bar
  
  B <- cov(estimates)
  #B
  
  Total_var <- U_bar + (1 + 1/m)*B
  #Total_var
  
  k <- ncol(estimates) #number of parameters
  lambda_hat <- (1+ 1/m) * sum(diag(B %*% MASS::ginv(Total_var)))/k
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
  gamma <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))
  #gamma
  N_eff <- N - gamma*N
  
  return(N_eff)
}


#NG = 80
#data <- n_3.2k_1pred[[1]]

mi.Neff.1pred.nG80 <- function(data){
  
  model.def <- rjags::jags.model(file = textConnection(model), data = data, n.chains = 2,
                                 inits = list(.RNG.name="base::Wichmann-Hill",
                                              .RNG.seed=100))
  
  update(object = model.def, n.iter = 1000)
  
  #only the random effects (the ones we need for the aim of this study)
  parameters <- c("alpha", "beta_1","mu.alpha", "mu.beta_1")
  
  results <- rjags::coda.samples(model = model.def, variable.names = parameters, n.iter =1000)
  #summary(results)
  
  #Extract the samples from the posterior (from both chains), and separate the (random) intercepts and slopes:
  chain1 <- as.data.frame(results[[1]])
  chain2 <- as.data.frame(results[[2]])
  samples <- rbind(chain1, chain2) #combine both chains
  #Get the fixed effects
  fixed_alphas <- samples[, 161]   
  fixed_betas_1 <- samples[, 162]
  fixed_alphas <- as.data.frame(fixed_alphas)
  fixed_betas_1 <- as.data.frame(fixed_betas_1)

  samp_fixed_alphas <- list()
  for (i in 1:nrow(fixed_alphas)){
    samp_fixed_alphas[[i]] <-rep.int(fixed_alphas[i, 1], nrow(data))  
  }
  
  
  samp_fixed_betas_1 <- list()
  for (i in 1:nrow(fixed_betas_1)){
    samp_fixed_betas_1[[i]] <-rep.int(fixed_betas_1[i, 1], nrow(data))   
  }
  
  
  #Get sampled random effects
  split <- split(data, data$group)
  #size of each gr
  n_gr <- sapply(split, nrow)
  #exclude the fixed effects
  samples <- samples[, -c(161,162)]
  
  alphas <- samples [, 1:80] #extract the intercepts
  betas_1 <- samples [, 81:160] #extract the slopes
  
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
 
  
  #Combine
  
  imputed <- list()
  
  for (i in 1:ncol(samp_alphas_mat)){
    imputed[[i]] <- cbind(data, samp_alphas_mat[, i])
  }
  
  for (i in 1:ncol(samp_betas_mat_1)){
    imputed[[i]] <- cbind(imputed[[i]], samp_betas_mat_1[, i])
  }
  
  
  #add the fixed effects
  for (i in 1:length(imputed)){
    imputed[[i]] <- cbind(imputed[[i]], samp_fixed_alphas[[i]])
  }
  
  for (i in 1:length(imputed)){
    imputed[[i]] <- cbind(imputed[[i]], samp_fixed_betas_1[[i]])
  }
  
  ####Define the new lm model
  #transform
  transform_z <- function(df){
    df$z <- df[, 1] - df[, 4] - df[, 5] * df[, 3] + df[, 6] + df[, 7] * df[, 3] 
    df
  }
  #apply to all imputed data sets
  imputed <- lapply(imputed, transform_z)
  
  #Fit the lm models
  estimates <- list()
  vCOV <- list()
  for(i in seq_along(imputed)){
    estimates[[i]] <- coef(lm(imputed[[i]][,8] ~ imputed[[i]][,3]))
    vCOV [[i]] <- vcov(lm(imputed[[i]][,8] ~ imputed[[i]][,3]))
  }
  #Extract each type of coefficient
  intercepts <- NULL
  slopes1 <- NULL
  for(i in 1:length(estimates)){
    intercepts[i] <- estimates[[i]][1]
    slopes1[i] <- estimates[[i]][2]
  }
  estimates <- data.frame(intercepts, slopes1) #combine
  
  # Apply the formulas by van Buuren (2012)
  m <- nrow(estimates) #number of "imputations (iterations in this case)
  Q_bar <- apply(estimates, 2, mean) 
  Q_bar <- t(Q_bar)
  Q_bar <- as.matrix(Q_bar)
  #Q_bar
  
  V <- matrix(nrow = nrow(estimates), ncol = 4)
  
  for(i in seq_along(vCOV)){
    V[i, 1] <- vCOV[[i]][1,1]
    V[i, 2] <- vCOV[[i]][1,2]
    V[i, 3] <- vCOV[[i]][2,1]
    V[i, 4] <- vCOV[[i]][2,2]
  }
  
  U_bar <- apply(V, 2, mean)
  U_bar <- matrix(U_bar, nrow = 2, ncol = 2)
  #U_bar
  
  B <- cov(estimates)
  #B
  
  Total_var <- U_bar + (1 + 1/m)*B
  #Total_var
  
  k <- ncol(estimates) #number of parameters
  lambda_hat <- (1+ 1/m) * sum(diag(B %*% MASS::ginv(Total_var)))/k
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
  gamma <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))
  #gamma
  N_eff <- N - gamma*N
  
  return(N_eff)
}


#-------------------
# 2 predictors
#-----------------

#Ng = 20

mi.Neff.2pred.nG20 <- function(data){
  
  model.def <- rjags::jags.model(file = textConnection(model), data = data, n.chains = 2,
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
  fixed_alphas <- samples[, 61]   
  fixed_betas_1 <- samples[, 62]
  fixed_betas_2 <- samples[, 63]
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
  samples <- samples[, -c(61,62,63)]
  
  alphas <- samples [, 1:20] #extract the intercepts
  betas_1 <- samples [, 21:40] #extract the slopes
  betas_2 <- samples [, 41:60] #extract the slopes
  
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
  
  B <- cov(estimates)
  #B
  
  Total_var <- U_bar + (1 + 1/m)*B
  #Total_var
  
  k <- ncol(estimates) #number of parameters
  lambda_hat <- (1+ 1/m) * sum(diag(B %*% MASS::ginv(Total_var)))/k
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
  gamma <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))
  #gamma
  N_eff <- N - gamma*N
  
  return(N_eff)
}



# nG = 80
#data <- n_3.2k_2pred[[1]]

mi.Neff.2pred.nG80 <- function(data){
  
  model.def <- rjags::jags.model(file = textConnection(model), data = data, n.chains = 2,
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
  
  B <- cov(estimates)
  #B
  
  Total_var <- U_bar + (1 + 1/m)*B
  #Total_var
  
  k <- ncol(estimates) #number of parameters
  lambda_hat <- (1+ 1/m) * sum(diag(B %*% MASS::ginv(Total_var)))/k
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
  gamma <- ((nu + 1)/(nu + 3)) * lambda_hat + (2/(nu + 3))
  #gamma
  N_eff <- N - gamma*N
  
  return(N_eff)
}


