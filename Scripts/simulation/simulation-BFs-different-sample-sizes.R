#================================================================================================
# Calculate the BF's for hypotheses stating that the fixed parameters are equal to zero
# for models estimated with Full Maximum likelihood Estimation 
# using N = level-1 obs; level-2 obs; ICC-base N_eff
#================================================================================================

setwd("")  # set your working directory such that you can source the functions 
# you can also do this through RStudio by clicking Session -> Set Working Directory -> Choose Directory...
# or simply press Ctrl + Shift + H
library(lme4)
library(bain)
library(jtools)
source("wrapper_function.R")

#+++++++++++++++++++
# N = level-1 obs
#+++++++++++++++++


#==============================================================================
#1 predictor
#=============================================================================
hypothesis <- "X1 = 0"

# N= 400


BF.n0.4k_1pred.nlvl1 <- data.frame()

for(i in 1:length(mod_n0.4k_1pred)){
  BF.n0.4k_1pred.nlvl1[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_1pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "level_1", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n0.4k_1pred.nlvl1$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#------------------------------------------------------------------------------

# N= 3200


BF.n3.2k_1pred.nlvl1 <- data.frame()

for(i in 1:length(mod_n3.2k_1pred)){
  BF.n3.2k_1pred.nlvl1[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_1pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "level_1", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n3.2k_1pred.nlvl1$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#==============================================================================
#2 predictors
#=============================================================================

hypothesis <- "X1 = X2 = 0"


BF.n0.4k_2pred.nlvl1 <- data.frame()

for(i in 1:length(mod_n0.4k_2pred)){
  BF.n0.4k_2pred.nlvl1[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_2pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "level_1", seed = 123)[["fit"]][1, c(5,6,7)]
} 

#Add indicator for the partial R^2
BF.n0.4k_2pred.nlvl1$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#-----------------------------------------------------------------------------

# N= 3200

BF.n3.2k_2pred.nlvl1 <- data.frame()

for(i in 1:length(mod_n3.2k_2pred)){
  BF.n3.2k_2pred.nlvl1[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_2pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "level_1", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n3.2k_2pred.nlvl1$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#+++++++++++++++++++
# N = level-2 obs
#+++++++++++++++++


#==============================================================================
#1 predictor
#=============================================================================
hypothesis <- "X1 = 0"

# N= 400


BF.n0.4k_1pred.nlvl2 <- data.frame()

for(i in 1:length(mod_n0.4k_1pred.REML)){
  BF.n0.4k_1pred.nlvl2[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_1pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "level_2", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n0.4k_1pred.nlvl2$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#------------------------------------------------------------------------------

# N= 3200


BF.n3.2k_1pred.nlvl2 <- data.frame()

for(i in 1:length(mod_n3.2k_1pred)){
  BF.n3.2k_1pred.nlvl2[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_1pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "level_2", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n3.2k_1pred.nlvl2$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#==============================================================================
#2 predictors
#=============================================================================

hypothesis <- "X1 = X2 = 0"


BF.n0.4k_2pred.nlvl2 <- data.frame()

for(i in 1:length(mod_n0.4k_2pred)){
  BF.n0.4k_2pred.nlvl2[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_2pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "level_2", seed = 123)[["fit"]][1, c(5,6,7)]
} 

#Add indicator for the partial R^2
BF.n0.4k_2pred.nlvl2$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#-----------------------------------------------------------------------------

# N= 3200

BF.n3.2k_2pred.nlvl2 <- data.frame()

for(i in 1:length(mod_n3.2k_2pred)){
  BF.n3.2k_2pred.nlvl2[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_2pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "level_2", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n3.2k_2pred.nlvl2$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))


#++++++++++++++++++++++
# N = ICC-based N_eff
#++++++++++++++++++++


#==============================================================================
#1 predictor
#=============================================================================
hypothesis <- "X1 = 0"

# N= 400


BF.n0.4k_1pred.n.icc <- data.frame()

for(i in 1:length(mod_n0.4k_1pred)){
  BF.n0.4k_1pred.n.icc[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_1pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n0.4k_1pred.n.icc$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#------------------------------------------------------------------------------

# N= 3200


BF.n3.2k_1pred.n.icc <- data.frame()

for(i in 1:length(mod_n3.2k_1pred)){
  BF.n3.2k_1pred.n.icc[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_1pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n3.2k_1pred.n.icc$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#==============================================================================
#2 predictors
#=============================================================================

hypothesis <- "X1 = X2 = 0"


BF.n0.4k_2pred.n.icc <- data.frame()

for(i in 1:length(mod_n0.4k_2pred)){
  BF.n0.4k_2pred.n.icc[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_2pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
} 

#Add indicator for the partial R^2
BF.n0.4k_2pred.n.icc$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#-----------------------------------------------------------------------------

# N= 3200

BF.n3.2k_2pred.n.icc <- data.frame()

for(i in 1:length(mod_n3.2k_2pred)){
  BF.n3.2k_2pred.n.icc[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_2pred[[i]], hypothesis, 
                                                       jref = TRUE, standardize = FALSE,
                                                       N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n3.2k_2pred.n.icc$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))




