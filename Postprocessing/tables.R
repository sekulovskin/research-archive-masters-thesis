#=========================================
#Tables presented in the paper
#=======================================

library(kableExtra)
library(dplyr)
#==============================
#Effective sample size table
#==============================
effective_sample_size <- read.csv("effective_sample_size.csv")
effective_sample_size <- effective_sample_size[, -1]
kable(effective_sample_size, format = 'latex', booktabs = TRUE, caption = "Effective sample size")

#=================================
#Appenix B - sensitivity analysis
#================================

sensitivity$BF <- round(sensitivity$BF, 0)
sensitivity %>%
  select(-X) %>%
  kable(format = 'latex', booktabs = TRUE, caption = "fit, complexity and BF's for diffferent values of J and N")  %>%
  kable_styling(latex_options = "hold_position")



#=================================
#Appenix C - Simulation
#================================

#C1
FMLE <- read.csv("simulation-FMLE.csv")
FMLE$rsq <- rep(c("0", ".02", "13", ".26"), 4)
FMLE <- FMLE[, -1]
kable(FMLE, format = 'latex', booktabs = TRUE)


#C2
RMLE <- read.csv("simulation-RMLE.csv")
RMLE$rsq <- rep(c("0", ".02", "13", ".26"), 4)
RMLE <- RMLE[, -1]
kable(RMLE, format = 'latex', booktabs = TRUE)








