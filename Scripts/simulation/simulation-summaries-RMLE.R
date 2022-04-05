#=============================
#Simulation summaries RMLE#
#============================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Master Thesis/Master's thesis repo/Simulation")
library(dplyr)

#=======================================================
#1 predictor
#=======================================================
#=======================================================
#N = 400
#=======================================================

BF <- split(BF.n0.4k_1pred.RMLE, BF.n0.4k_1pred.RMLE$rsq)
BF.0  <- BF[["0"]]
BF.1 <- BF[["0.02"]]
BF.2 <- BF[["0.13"]]
BF.3 <- BF[["0.26"]]


#------------------------
#Summary tables
#-----------------------

means.0 <- BF.0 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))


means.1 <- BF.1 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))

means.2 <- BF.2 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))

means.3 <- BF.3 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))


#Obtain 95% intervals
ci <- data.frame()

ci[1, c(1,2)] <- quantile(BF.0$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.0 <- cbind(means.0, ci)

ci[1, c(1,2)] <- quantile(BF.1$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.1 <- cbind(means.1, ci)

ci[1, c(1,2)] <- quantile(BF.2$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.2 <- cbind(means.2, ci)

ci[1, c(1,2)] <- quantile(BF.3$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.3 <- cbind(means.3, ci)



#combine

means.n0.4k_1pred <- rbind(means.0, means.1, means.2, means.3)
means.n0.4k_1pred$rsq <- factor(c(0, 0.02, 0.13, 0.26))



rm(BF, BF.0, BF.1, BF.2, BF.3, ci, means.0,means.1, means.2, means.3)


#======================================================
#N = 3200
#=======================================================

BF <- split(BF.n3.2k_1pred.RMLE, BF.n3.2k_1pred.RMLE$rsq)
BF.0  <- BF[["0"]]
BF.1 <- BF[["0.02"]]
BF.2 <- BF[["0.13"]]
BF.3 <- BF[["0.26"]]


#------------------------
#Summary tables
#-----------------------

means.0 <- BF.0 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))


means.1 <- BF.1 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))

means.2 <- BF.2 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))

means.3 <- BF.3 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))


#Obtain 95% intervals
ci <- data.frame()

ci[1, c(1,2)] <- quantile(BF.0$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.0 <- cbind(means.0, ci)

ci[1, c(1,2)] <- quantile(BF.1$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.1 <- cbind(means.1, ci)

ci[1, c(1,2)] <- quantile(BF.2$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.2 <- cbind(means.2, ci)

ci[1, c(1,2)] <- quantile(BF.3$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.3 <- cbind(means.3, ci)




#combine

means.n3.2k_1pred <- rbind(means.0, means.1, means.2, means.3)
means.n3.2k_1pred$rsq <- factor(c(0, 0.02, 0.13, 0.26))



rm(BF, BF.0, BF.1, BF.20, BF.3, ci, means.0,means.1, means.2, means.3)


#=======================================================
#2 predictors 
#=======================================================
#=======================================================
#N = 400
#=======================================================

BF <- split(BF.n0.4k_2pred.RMLE, BF.n0.4k_2pred.RMLE$rsq)
BF.0  <- BF[["0"]]
BF.1 <- BF[["0.02"]]
BF.2 <- BF[["0.13"]]
BF.3 <- BF[["0.26"]]


#------------------------
#Summary tables
#-----------------------

means.0 <- BF.0 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))


means.1 <- BF.1 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))

means.2 <- BF.2 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))

means.3 <- BF.3 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))


#Obtain 95% intervals
ci <- data.frame()

ci[1, c(1,2)] <- quantile(BF.0$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.0 <- cbind(means.0, ci)

ci[1, c(1,2)] <- quantile(BF.1$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.1 <- cbind(means.1, ci)

ci[1, c(1,2)] <- quantile(BF.2$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.2 <- cbind(means.2, ci)

ci[1, c(1,2)] <- quantile(BF.3$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.3 <- cbind(means.3, ci)

#combine

means.n0.4k_2pred <- rbind(means.0, means.1, means.2, means.3)
means.n0.4k_2pred$rsq <- factor(c(0, 0.02, 0.13, 0.26))



rm(BF, BF.0, BF.1, BF.2, BF.3, ci, means.0,means.1, means.2, means.3)

#
#=======================================================
#N = 3200
#=======================================================

BF <- split(BF.n3.2k_2pred.RMLE, BF.n3.2k_2pred.RMLE$rsq)
BF.0  <- BF[["0"]]
BF.1 <- BF[["0.02"]]
BF.2 <- BF[["0.13"]]
BF.3 <- BF[["0.26"]]


#------------------------
#Summary tables
#-----------------------

means.0 <- BF.0 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))


means.1 <- BF.1 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))

means.2 <- BF.2 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))

means.3 <- BF.3 %>% 
  select(Fit,Com, BF) %>%
  summarise_all(funs(mean))


#Obtain 95% intervals
ci <- data.frame()

ci[1, c(1,2)] <- quantile(BF.0$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.0 <- cbind(means.0, ci)

ci[1, c(1,2)] <- quantile(BF.1$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.1 <- cbind(means.1, ci)

ci[1, c(1,2)] <- quantile(BF.2$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.2 <- cbind(means.2, ci)

ci[1, c(1,2)] <- quantile(BF.3$BF, probs = c(0.025, 0.975))

names(ci) <- c("2.5%", "97.5%")
means.3 <- cbind(means.3, ci)


#combine

means.n3.2k_2pred <- rbind(means.0, means.1, means.2, means.3)
means.n3.2k_2pred$rsq <- factor(c(0, 0.02, 0.13, 0.26))



rm(BF, BF.0, BF.1, BF.2, BF.3, ci, means.0, means.1, means.2, means.3)


# Combine all in one DF

means_BF.RMLE <- rbind(means.n0.4k_1pred, means.n0.4k_2pred, means.n3.2k_1pred, means.n3.2k_2pred)

# add indicator for the population

means_BF.RMLE$setting <- rep(c("N = 400, 1 predictor", "N = 400, 2 predictors", "N = 3200, 1 predictor", "N = 3200, 2 predictors"), each = 4)

rm(means.n0.4k_1pred, means.n0.4k_2pred, means.n3.2k_1pred, means.n3.2k_2pred)

#save

write.csv(means_BF.RMLE, "simulation-RMLE.csv")
