#================================================================================================
# Calculate the BF's for hypotheses stating that the fixed parameters are equal to zero
# for models estimated using Full Maximum likelihood Estimation 
#================================================================================================

setwd("")  # set your working directory such that you can source the functions 
# you can also do this through RStudio by clicking Session -> Set Working Directory -> Choose Directory...
# or simply press Ctrl + Shift + H
library(lme4)
library(bain)
library(jtools)
source("wrapper_function.R")
memory.limit(size=40000)

#==============================================================================
#1 predictor
#=============================================================================
hypothesis <- "X1 = 0"

# N= 400

mod_n0.4k_1pred <- list()
options(warn = -1)
  for (i in 1:length(n_0.4k_1pred)){
    mod_n0.4k_1pred[[i]] <- lmer(y ~ X1 + (X1 | group),
                                 REML = FALSE, data = n_0.4k_1pred[[i]])
  }


# Compute BF
BF.n0.4k_1pred <- data.frame()

  for(i in 1:length(mod_n0.4k_1pred)){
    BF.n0.4k_1pred[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_1pred[[i]], hypothesis, 
                                              jref = TRUE, standardize = FALSE,
                                              N = mi_nEff_0.4k_1pred[[i]], seed = 123)[["fit"]][1, c(5,6,7)]
  } 


#Add indicator for the partial R^2
BF.n0.4k_1pred$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#------------------------------------------------------------------------------

# N= 3200

mod_n3.2k_1pred <- list()
options(warn = -1)
  for (i in 1:length(n_3.2k_1pred)){
    mod_n3.2k_1pred[[i]] <- lmer(y ~ X1 + (X1 | group),
                                 REML = FALSE, data = n_3.2k_1pred[[i]])
  }


# Compute BF
BF.n3.2k_1pred <- data.frame()

  for(i in 1:length(mod_n3.2k_1pred)){
    BF.n3.2k_1pred[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_1pred[[i]], hypothesis, 
                                              jref = TRUE, standardize = FALSE,
                                              N = mi_nEff_3.2k_1pred[[i]], seed = 123)[["fit"]][1, c(5,6,7)]
  } 


#Add indicator for the partial R^2
BF.n3.2k_1pred$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#==============================================================================
#2 predictors
#=============================================================================

hypothesis <- "X1 = X2 = 0"

# N= 400
mod_n0.4k_2pred <- list()
options(warn = -1)
  for (i in 1:length(n_0.4k_2pred)){
    mod_n0.4k_2pred[[i]] <- lmer(y ~ X1 + X2 + (X1 + X2 | group),
                                 REML = FALSE, data = n_0.4k_2pred[[i]])
  }


# Compute BF
BF.n0.4k_2pred <- data.frame()

  for(i in 1:length(mod_n0.4k_2pred)){
    BF.n0.4k_2pred[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_2pred[[i]], hypothesis, 
                                              jref = TRUE, standardize = FALSE,
                                              N = mi_nEff_0.4k_2pred[[i]], seed = 123)[["fit"]][1, c(5,6,7)]
  } 

#Add indicator for the partial R^2
BF.n0.4k_2pred$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#-----------------------------------------------------------------------------

# N= 3200

mod_n3.2k_2pred <- list()
options(warn = -1)
  for (i in 1:length(n_3.2k_2pred)){
    mod_n3.2k_2pred[[i]] <- lmer(y ~ X1 + X2 + (X1 + X2 | group),
                                 REML = FALSE, data = n_3.2k_2pred[[i]])
  }


# Compute BF
BF.n3.2k_2pred <- data.frame()

  for(i in 1:length(mod_n3.2k_2pred)){
    BF.n3.2k_2pred[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_2pred[[i]], hypothesis, 
                                              jref = TRUE, standardize = FALSE,
                                              N = mi_nEff_3.2k_2pred[[i]], seed = 123)[["fit"]][1, c(5,6,7)]
  } 


#Add indicator for the partial R^2
BF.n3.2k_2pred$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

