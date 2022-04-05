#===========================================================
# Motivation for the values of the simulated data sets
#==========================================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Master Thesis/Master's thesis repo/Simulation")
library(lme4)
library(jtools)
library(rsq)
source("partial_R-sq_function.R")

#Note: The estimated pseudo R^2 values are APPROXIMATELY equal to the desired values based on Cohen (1992);
#For more information behind the motivation for the pesudo R^2 for the fixed effects see:
#Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
#obtaining $R^2$ from generalized linear mixed-effects models
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#N = 400 (ng = 20, n=20)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+
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

#Calculate the pseudo R^2

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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

#Calculate the pseudo R^2

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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

#Calculate the pseudo R^2

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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

#Calculate the pseudo R^2 

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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

#Calculate the pseudo R^2 

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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

#Calculate the pseudo R^2 

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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
nG <- 80   #number of groups (level-2 obs)
n  <- 40   #group size (within group N)

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

#Calculate the pseudo R^2

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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

#Calculate the pseudo R^2

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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




fixef <- fixef(model)
s 
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

model <- lmer(y ~ X1 + X2 +(X1 + X2 | group), REML = FALSE, data = data) 



fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed


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

#Calculate the pseudo R^2 

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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

#Calculate the pseudo R^2 

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
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

#Calculate the pseudo R^2 

#extract the estimated fixed effects
fixef <- fixef(model)
#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)
#calculate the marginal R^2 
Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed

