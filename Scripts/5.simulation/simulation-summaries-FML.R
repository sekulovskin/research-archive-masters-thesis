#=============================
#Simulation summaries FML#
#============================
library(dplyr)

#=======================================================
#1 predictor
#=======================================================
#=======================================================
#N = 400
#=======================================================

BF <- split(BF.n0.4k_1pred, BF.n0.4k_1pred$rsq)
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

BF <- split(BF.n3.2k_1pred, BF.n3.2k_1pred$rsq)
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

BF <- split(BF.n0.4k_2pred, BF.n0.4k_2pred$rsq)
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

BF <- split(BF.n3.2k_2pred, BF.n3.2k_2pred$rsq)
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

means_BF.FML <- rbind(means.n0.4k_1pred, means.n0.4k_2pred, means.n3.2k_1pred, means.n3.2k_2pred)

# add indicator for the population

means_BF.FML$setting <- rep(c("N = 400, 1 predictor", "N = 400, 2 predictors", "N = 3200, 1 predictor", "N = 3200, 2 predictors"), each = 4)

rm(means.n0.4k_1pred, means.n0.4k_2pred, means.n3.2k_1pred, means.n3.2k_2pred)

# Save 
write.csv(means_BF.FML, "simulation-FML.csv")


#Add indicators to which setting to the BF belong to
BF.n0.4k_1pred$setting <- rep(as.factor("N = 400, 1 predictor"),  200) 
BF.n0.4k_2pred$setting <- rep(as.factor("N = 400, 2 predictors"), 200) 
BF.n3.2k_1pred$setting <- rep(as.factor("N = 3200, 1 predictor"), 200) 
BF.n3.2k_2pred$setting <- rep(as.factor("N = 3200, 2 predictors"),200) 



combined_BF <- rbind(BF.n0.4k_1pred, BF.n0.4k_2pred, BF.n3.2k_1pred, BF.n3.2k_2pred)
combined_BF$BFlog <- log(combined_BF$BF)

write.csv(combined_BF, "combined_BF.csv")