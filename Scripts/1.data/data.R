#===========================================================
# Motivation for the values of the simulated data sets
#==========================================================
library(lme4)
library(jtools)
library(rsq)

# Note: The estimated marginal R� values are APPROXIMATELY equal to the desired values based on Cohen (1992);
# For more information on the marginal R� for the fixed effects see:
# Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
# obtaining R� from generalized linear mixed-effects models
# In this script the marginal R� values are calculated by hand AND they are also shown
# by the output of the `summ` function, and denoted as `Pseudo-R� (fixed effects)`
# Pseudo-R� (total) denoted the marginal R�, such that:
# Pseudo-R� (total) -  Pseudo-R�  = R� random effects


# The values for the variances of the random effects (and residual variance) are inspired
# by fitting two-level models to openly-available two-level data sets such as the
# `tutorial` data set from the `R` package `R2MLwiN` (Zhang et al., 2016) and the popularity data set 
# from Hox et al.(2017) https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/tree/master/chapter%202/popularity

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#N = 400 (ng = 20, n=20)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#==================================================
# 1 predictor
#==================================================

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ 0 
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 20   #number of groups (level-2 obs)
n  <- 20   #group size (within group N)

#fixed effects
b1 <- 0  #regression coefficient

X1 <- rnorm(nG * n, 0, 0.8) #predictor

group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  #random effect for the intercept
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   #random effect for the slope
u_1 <- rep(slope_var_1, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  #construct the outcome
data <- data.frame(y, group, X1)

model <- lmer(y ~ X1 + (X1 | group), REML = FALSE, data = data) #fit lmer model
summ(model)
#Calculate the pseudo R^2 

# variances of the random effects (which remain the same throughout)
sigma_u0 <- 0.3^2
sigma_u1 <- 0.1^2
sigma_e  <- 0.6^2

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed

#The rest of the code follows the same logic (thus, no annotations are provided)

#++++++++++++++++++++++++++++++
#+ Small effect pseudo R^2 ~ .02
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 20   
n  <- 20   
b1 <- 0.12
X1 <- rnorm(nG * n, 0, 0.8)
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
data <- data.frame(y, group, X1)

model <- lmer(y ~ X1 + (X1 | group), REML = FALSE, data = data) 
summ(model)

fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .13
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 20   
n  <- 20   
b1 <- 0.33
X1 <- rnorm(nG * n, 0, 0.8)
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
data <- data.frame(y, group, X1)

model <- lmer(y ~ X1 + (X1 | group), REML = FALSE, data = data) 
summ(model)


fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .26
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 20   
n  <- 20   
b1 <- 0.51
X1 <- rnorm(nG * n, 0, 0.8)
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
data <- data.frame(y, group, X1)

model <- lmer(y ~ X1 + (X1 | group), REML = FALSE, data = data) 
summ(model)



fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)
 
Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed

#==================================================
# 2 predictors
#==================================================

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ 0
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 20   
n  <- 20   
b1 <- 0  
b2 <- 0 #add second regression coefficient
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6) #add second predictor
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)   #add random effects for the second predictor 
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data <- data.frame(y, group, X1, X2)

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 
summ(model)
#Calculate the pseudo R^2 

#add the variance
sigma_u2 <- 0.2^2
#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed

#The rest of the code follows the same logic (thus, no annotations are provided)

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .02
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 20   
n  <- 20   
b1 <- 0.09
b2 <- 0.09
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

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 
summ(model)



fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .13
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 20   
n  <- 20   
b1 <- 0.26
b2 <- 0.26
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

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 
summ(model)


fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .26
#+++++++++++++++++++++++++++++
set.seed(123)
nG <- 20   
n  <- 20   
b1 <- 0.4
b2 <- 0.4
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

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 
summ(model)



fixef <- fixef(model)
 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#N = 3200 (ng = 80, n=40)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+
#==================================================
# 1 predictor
#==================================================
#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ 0
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 80   # Now we have 80 groups 
n  <- 40   # with a within group size of 40 each

#fixed effects
b1 <- 0  #regression coefficient

X1 <- rnorm(nG * n, 0, 0.8) #predictor

group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  #random effect for the intercept
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   #random effect for the slope
u_1 <- rep(slope_var_1, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  #construct the outcome
data <- data.frame(y, group, X1)

model <- lmer(y ~ X1 + (X1 | group), REML = FALSE, data = data) #fit lmer model
summ(model)
#Calculate the pseudo R^2 

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed

#The rest of the code follows the same logic (thus, no annotations are provided)

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .02
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 80   
n  <- 40   
b1 <- 0.11
X1 <- rnorm(nG * n, 0, 0.8)
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
data <- data.frame(y, group, X1)

model <- lmer(y ~ X1 + (X1 | group), REML = FALSE, data = data) 
summ(model)



fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .13
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 80   
n  <- 40   
b1 <- 0.31
X1 <- rnorm(nG * n, 0, 0.8)
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
data <- data.frame(y, group, X1)

model <- lmer(y ~ X1 + (X1 | group), REML = FALSE, data = data) 
summ(model)



fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .26
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 80   
n  <- 40 
b1 <- 0.48
X1 <- rnorm(nG * n, 0, 0.8)
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + u_0 + u_1*X1  + epsilon  
data <- data.frame(y, group, X1)

model <- lmer(y ~ X1 + (X1 | group), REML = FALSE, data = data) 
summ(model)


fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed


#==================================================
# 2 predictors
#==================================================
#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ 0
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 80   
n  <- 40  
b1 <- 0  
b2 <- 0  # add the second coefficient
X1 <- rnorm(nG * n, 0, 0.8) 
X2 <- rnorm(nG * n, 0, 0.6)  # add the second predictor
group <- gl(nG, k = n) 
intercept_var <- rnorm(nG, 0, 0.3)  
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)   
u_2 <- rep(slope_var_2, each = n) #add the second slope variance
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data <- data.frame(y, group, X1, X2)

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 
summ(model)
#Calculate the pseudo R^2

fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed

#The rest of the code follows the same logic (thus, no annotations are provided)

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .02
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 80   
n  <- 40   
b1 <- 0.1
b2 <- 0.1
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

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 
summ(model)



fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .13
#+++++++++++++++++++++++++++++

set.seed(123)
nG <- 80   
n  <- 40  
b1 <- 0.27
b2 <- 0.27
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

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 
summ(model)



fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed

#++++++++++++++++++++++++++++++
#+ H0: TRUE pseudo R^2 ~ .26
#+++++++++++++++++++++++++++++
set.seed(123)
nG <- 80   
n  <- 40  
b1 <- 0.42
b2 <- 0.42
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

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 
summ(model)



fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed



#===============================================================================
#Varying ICC values: used *only* for the effective sample size section 
#===============================================================================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#N = 400 (ng = 20, n=20)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
data <- data.frame(y, group, X1, X2)

model <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data)
summ(model)

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
intercept_var <- rnorm(nG, 0, 0.12)   # decrease the intercept variance
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data <- data.frame(y, group, X1, X2)

model <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data)
summ(model)

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
intercept_var <- rnorm(nG, 0, 0.48)   # increase the intercept variance
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data <- data.frame(y, group, X1, X2)

model <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data)
summ(model)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#N = 3200 (ng = 80, n=40)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
data <- data.frame(y, group, X1, X2)

model <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data)
summ(model)

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
intercept_var <- rnorm(nG, 0, 0.1)    # decrease the intercept variance
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data <- data.frame(y, group, X1, X2)

model <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data)
summ(model)

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
intercept_var <- rnorm(nG, 0, 0.41)   # increase the intercept variance
u_0 <- rep(intercept_var, each = n) 
slope_var_1 <- rnorm(nG, 0, 0.1)   
u_1 <- rep(slope_var_1, each = n) 
slope_var_2 <- rnorm(nG, 0, 0.2)    
u_2 <- rep(slope_var_2, each = n) 
epsilon <- rnorm(nG * n, 0, 0.6)
y <-  b1 * X1 + b2 * X2 + u_0 + u_1*X1 + u_2*X2  + epsilon  
data <- data.frame(y, group, X1, X2)

model <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data)
summ(model)


