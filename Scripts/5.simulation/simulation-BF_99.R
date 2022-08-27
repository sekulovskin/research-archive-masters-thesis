#================================================================================================
# Calculate the BF's for hypotheses stating that the fixed parameters are equal to zero
# for models estimated with Full Maximum likelihood Estimation 
# FOr BF = 99, using ICC-based N_eff and FML
#================================================================================================

setwd("")  # set your working directory such that you can source the functions 
# you can also do this through RStudio by clicking Session -> Set Working Directory -> Choose Directory...
# or simply press Ctrl + Shift + H
library(lme4)
library(bain)
library(jtools)
source("wrapper_function_BF_99.R")

#==============================================================================
#1 predictor
#=============================================================================
hypothesis <- "X1 = 0"

# N= 400


BF.n0.4k_1pred.n.icc_99<- data.frame()

for(i in 1:length(mod_n0.4k_1pred)){
  BF.n0.4k_1pred.n.icc_99[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_1pred[[i]], hypothesis, 
                                                  jref = TRUE, standardize = FALSE,
                                                  N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n0.4k_1pred.n.icc_99$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#------------------------------------------------------------------------------

# N= 3200


BF.n3.2k_1pred.n.icc_99 <- data.frame()

for(i in 1:length(mod_n3.2k_1pred)){
  BF.n3.2k_1pred.n.icc_99[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_1pred[[i]], hypothesis, 
                                                  jref = TRUE, standardize = FALSE,
                                                  N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n3.2k_1pred.n.icc_99$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#==============================================================================
#2 predictors
#=============================================================================

hypothesis <- "X1 = X2 = 0"


BF.n0.4k_2pred.n.icc_99 <- data.frame()

for(i in 1:length(mod_n0.4k_2pred)){
  BF.n0.4k_2pred.n.icc_99[i, c(1,2,3)] <- bain_2lmer(mod_n0.4k_2pred[[i]], hypothesis, 
                                                  jref = TRUE, standardize = FALSE,
                                                  N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
} 

#Add indicator for the partial R^2
BF.n0.4k_2pred.n.icc_99$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))

#-----------------------------------------------------------------------------

# N= 3200

BF.n3.2k_2pred.n.icc_99 <- data.frame()

for(i in 1:length(mod_n3.2k_2pred)){
  BF.n3.2k_2pred.n.icc_99[i, c(1,2,3)] <- bain_2lmer(mod_n3.2k_2pred[[i]], hypothesis, 
                                                  jref = TRUE, standardize = FALSE,
                                                  N = "ICC_effective", seed = 123)[["fit"]][1, c(5,6,7)]
} 


#Add indicator for the partial R^2
BF.n3.2k_2pred.n.icc_99$rsq <- factor(rep(c(0, 0.02, 0.13, 0.26), each=N_sim))




