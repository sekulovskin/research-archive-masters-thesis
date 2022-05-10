#====================================
# How to calculate J_ref example
#===================================
setwd("")  # set your working directory such that you can source the functions 
# you can also do this through RStudio by clicking Session -> Set Working Directory -> Choose Directory...
# or simply press Ctrl + Shift + H
library(lme4)
library(jtools)
source("wrapper_function.R")

#The data set used in the second section
set.seed(1234)
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

lmer_model <- lmer(y ~ X1 + X2 + (X1 + X2 | group), REML = FALSE, data = data)
summ(lmer_model)

# Check the marginal R^2
sigma_u0 <- 0.3^2
sigma_u1 <- 0.1^2
sigma_u2 <- 0.2^2
sigma_e  <- 0.6^2

#extract the estimated fixed effects
fixef <- fixef(lmer_model)

#obtain the variance attributable to the fixed effects components 
y_hat <- fixef[1] + fixef[2]*data$X1 + fixef[3]*data$X2
sigma_f  <- var(y_hat)

#calculate the marginal R^2 
Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_u2 + sigma_e)
Pseudo_Rsq_fixed

# sample size (equal to the calculated effective sample size, see `sensitivity-analysis.R`)
N_eff <- 921
M <- 2

#Calculate J_ref and fracref 
J_ref <- N_eff / 19^(2/M)
J_ref

fracref <- J_ref/M
fracref

#Calculate the BF
res <- bain_2lmer(lmer_model, "X1 = X2 = 0", 
           fraction = fracref, standardize = FALSE,
           N = N_eff, seed = 123)
print(res)

# We can also inspect the calculated value for the fraction by
res$b

# which corresponds to
J_ref/N_eff

# We can do the same automatically within the wrapper function 
res <- bain_2lmer(lmer_model, "X1 = X2 = 0", 
                  jref = TRUE, standardize = FALSE, #I change fraction to jref = TRUE
                  N = N_eff, seed = 123)
print(res)

