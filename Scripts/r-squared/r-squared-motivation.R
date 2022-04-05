#=======================================================
# Motivation for the Pseudo R^2 for the fixed effects (based on Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
#'  obtaining $R^2$ from generalized linear mixed-effects models.)
#======================================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Master Thesis/Master's thesis repo/r-squared")
library(lme4)
library(foreign)

# Read in a multilevel data set available from Hox, J. J., Moerbeek, M., & Van de Schoot, R. (2017). Multilevel analysis: Techniques and applications.

data <- read.spss("popular.sav", to.data.frame = TRUE)


# Fit a two level model containing a random slope and random intercept

model <- lmer(popular ~  extrav + (extrav| class), 
     data = data, REML = F)

summary(model)


#Obtain the partial R^2 for the fixed effects

# 1. Obtain sigma_f

fixef <- fixef(model)

y_hat <- fixef[1] + fixef[2]*data$popular 
sigma_f  <- var(y_hat)
sigma_u0 <- 2.95487
sigma_u1 <- 0.02523
sigma_e  <- 0.89504


Pseudo_Rsq_fixed <- (sigma_f)/(sigma_f + sigma_u0 + sigma_u1 + sigma_e)
Pseudo_Rsq_fixed  #similar 


# The value for R^2_m is roughly equivalent to the R^2 given by fitting a linear regression model 

lm_model <- lm(popular ~  extrav, data = data)

summary(lm_model)


