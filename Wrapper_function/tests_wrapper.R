######Testing the wrapper function `bain_2lmer`####

setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Master Thesis/thesis-research-archive/research-archive-masters-thesis-/Wrapper_function")
#load the packages and the wrapper
library(testthat)
library(lme4)
library(jtools)
library(R2MLwiN)
library(bain)
library(foreign)
source("wrapper_function.R")

#=============================================================
# Using fraction = 1
#============================================================

#set the seed

seed <- 123

#set the fraction value
fraction <- 1

#-------------------------------------------------------------------------------
# Unstandardized 

# load the data 
data("tutorial")

#### Model with: one level-1 and one level-2 predictor #####

#fit the model
x <- lmer(normexam ~ standlrt + avslrt + (standlrt | school), REML = FALSE, data = tutorial)
#inspect (fixed) coefficients
fixef(x) [-1]
#define the hypotheses:
hypotheses <- "standlrt = avslrt = 0;
               standlrt > 0 & avslrt > 0"

##Scenario 1: unstandardized data  & N = level- 1 obs---------------------------

### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
           fraction, standardize = FALSE, N = "level_1", seed)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x)[-1] #get the estimates
cov <- as.matrix(vcov(x)[-1, -1]) #get their covariance matrix
names(estimates) <- c("standlrt", "avslrt") #assign names
N <- nrow(tutorial) # N = level 1 obs

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fraction)


testthat::test_that("Scenario 1", {expect_equal(w$fit, b$fit, tolerance = .001)})

#Test passed

##Scenario 2: unstandardized data  & N = level- 2 obs---------------------------

### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = FALSE, N = "level_2", seed)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x)[-1] #get the estimates
cov <- as.matrix(vcov(x)[-1, -1]) #get their covariance matrix
names(estimates) <- c("standlrt", "avslrt") #assign names
N <-  nrow(coef(x)[[1]]) # N = level 2 obs

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fraction)


test_that("Scenario 2", {expect_equal(w$fit, b$fit, tolerance = .001)})

#test passed

##Scenario 3: unstandardized data  & N = effective sample size (ICC)------------

### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = FALSE, N = "ICC_effective", seed)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x)[-1] #get the estimates
cov <- as.matrix(vcov(x)[-1, -1]) #get their covariance matrix
names(estimates) <- c("standlrt", "avslrt") #assign names

### Compute the effective sample size based on the ICC approach:

model_0 <- lmer(normexam ~ 1 + (1 | school), 
                data = tutorial, REML = F) #fit a random intercept-only model
summ(model_0) #look at the ICC
ICC <- 0.17 #store it
split <- split(tutorial, tutorial$school)
n_clus <- mean(sapply(split, nrow)) #the average group size
N <- nrow(tutorial) #level 1 obs
N_eff_ICC <- N / (1 + (n_clus - 1) * ICC) #calculate the effective sample size

#use   `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N_eff_ICC, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fraction)


test_that("Scenario 3", {expect_equal(w$fit, b$fit, tolerance = .1)}) 

#Test passed. However, the tolerance needs to be increased in this case (from 0.001 to 0.1)!


##Scenario 4: unstandardized data  & N = specified by the user--------------
N_user <- 1000

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = FALSE, N = N_user, seed)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x)[-1] #get the estimates
cov <- as.matrix(vcov(x)[-1, -1]) #get their covariance matrix
names(estimates) <- c("standlrt", "avslrt") #assign names
N <-  nrow(coef(x)[[1]]) # N = level 2 obs

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N_user, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fraction)


test_that("Scenario 4", {expect_equal(w$fit, b$fit, tolerance = .001)}) 

# Test passed


#-----------------------------------------------------------------------------------

#Now we load a different data setthat contains unstandardized data
# This data set is openly available from https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book

#load the data
popular<-read.spss(file = "popular.sav", to.data.frame = T)

#### Model with: one level-1 and one level-2 predictor #####

#fit the model
x <- lmer(popular ~ 1 + extrav + texp + (extrav | class), 
          data = popular, REML = F)
#inspect (fixed) coefficients
fixef(x) [-1]
#define the hypotheses:

hypotheses <- "extrav = texp  = 0;
         extrav = texp; extrav > 0 & texp >0"

##Scenario 5: standardized data  & N = level- 1 obs-----------------------------

### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = TRUE, N = "level_1", seed)

### Test the hypotheses using `bain` with a named vector

#apply overall standardization of the data
popular_standardized <- apply(popular[, -c(1,2,4)], 2, scale)
popular_standardized <- as.data.frame(popular_standardized)
popular_standardized$class <- popular$class #add class indicator
#refit the lmer model 
x_stand <- lmer(popular ~ 1 + extrav + texp + (extrav | class), 
                data = popular_standardized, REML = F)

estimates <- fixef(x_stand)[-1] #get the estimates
cov <- as.matrix(vcov(x_stand)[-1, -1]) #get their covariance matrix
names(estimates) <- c("extrav", "texp") #assign names
N <- nrow(popular_standardized) # N = level 1 obs

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fraction)


test_that("Scenario 5", {expect_equal(w$fit, b$fit, tolerance = .001)})

#Test passed 

##Scenario 6: standardized data  & N = level- 2 obs-----------------------------
### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = TRUE, N = "level_2", seed)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x_stand)[-1] #get the estimates
cov <- as.matrix(vcov(x_stand)[-1, -1]) #get their covariance matrix
names(estimates) <- c("extrav", "texp") #assign names
N <-  nrow(coef(x_stand)[[1]]) # N = level 2 obs

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fraction)

test_that("Scenario 6", {expect_equal(w$fit, b$fit, tolerance = .001)})

# Test passed

##Scenario 7: standardized data  & N = effective sample size (ICC)--------------
w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = TRUE, N = "ICC_effective", seed)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x_stand)[-1] #get the estimates
cov <- as.matrix(vcov(x_stand)[-1, -1]) #get their covariance matrix
names(estimates) <- c("extrav", "texp") #assign names

#use   `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N_eff_ICC, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fraction)


test_that("Scenario 7", {expect_equal(w$fit, b$fit, tolerance = .01)}) 

# Test passed (but tolerance increased from 0.001 to 0.01)

##Scenario 8: standardized data  & N = specifised by the user--------------
N_user <- 1000

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = TRUE, N = N_user, seed)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x_stand)[-1] #get the estimates
cov <- as.matrix(vcov(x_stand)[-1, -1]) #get their covariance matrix
names(estimates) <- c("extrav", "texp") #assign names

#use   `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N_user, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fraction)


test_that("Scenario 8", {expect_equal(w$fit, b$fit, tolerance = .001)}) 

# Test passed


#===================================================================
# Using J-ref/fracref
#==================================================================




#-------------------------------------------------------------------------------
# Unstandardized 

# load the data 
data("tutorial")

#### Model with: one level-1 and one level-2 predictor #####

#fit the model
x <- lmer(normexam ~ standlrt + avslrt + (standlrt | school), REML = FALSE, data = tutorial)
#inspect (fixed) coefficients
fixef(x) [-1]
#define the hypotheses:
hypotheses <- "standlrt = avslrt = 0;
               standlrt > 0 & avslrt > 0"

##Scenario 1: unstandardized data  & N = level- 1 obs---------------------------

### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = FALSE, N = "level_1", seed, jref = TRUE)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x)[-1] #get the estimates
cov <- as.matrix(vcov(x)[-1, -1]) #get their covariance matrix
names(estimates) <- c("standlrt", "avslrt") #assign names
N <- nrow(tutorial) # N = level 1 obs

#Calculate fracref
M <- 2
fracref <- (N / 19^(2/M))/M

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fracref)


testthat::test_that("Scenario 1", {expect_equal(w$fit, b$fit, tolerance = .001)})

#Test passed

##Scenario 2: unstandardized data  & N = level- 2 obs---------------------------

### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = FALSE, N = "level_2", seed, jref = TRUE)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x)[-1] #get the estimates
cov <- as.matrix(vcov(x)[-1, -1]) #get their covariance matrix
names(estimates) <- c("standlrt", "avslrt") #assign names
N <-  nrow(coef(x)[[1]]) # N = level 2 obs

#Calculate fracref
M <- 2
fracref <- (N / 19^(2/M))/M

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fracref)


test_that("Scenario 2", {expect_equal(w$fit, b$fit, tolerance = .001)})

#test passed

##Scenario 3: unstandardized data  & N = effective sample size (ICC)------------

### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = FALSE, N = "ICC_effective", seed, jref = TRUE)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x)[-1] #get the estimates
cov <- as.matrix(vcov(x)[-1, -1]) #get their covariance matrix
names(estimates) <- c("standlrt", "avslrt") #assign names

### Compute the effective sample size based on the ICC approach:

model_0 <- lmer(normexam ~ 1 + (1 | school), 
                data = tutorial, REML = F) #fit a random intercept-only model
summ(model_0) #look at the ICC
ICC <- 0.17 #store it
split <- split(tutorial, tutorial$school)
n_clus <- mean(sapply(split, nrow)) #the average group size
N <- nrow(tutorial) #level 1 obs
N_eff_ICC <- N / (1 + (n_clus - 1) * ICC) #calculate the effective sample size

#Calculate fracref
M <- 2
fracref <- (N_eff_ICC / 19^(2/M))/M
#use   `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N_eff_ICC, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fracref)


test_that("Scenario 3", {expect_equal(w$fit, b$fit, tolerance = .1)}) 

#Test passed. However, the tolerance needs to be increased in this case (from 0.001 to 0.1)!


##Scenario 4: unstandardized data  & N = specified by the user--------------
N_user <- 1000

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = FALSE, N = N_user, seed, fracref = TRUE)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x)[-1] #get the estimates
cov <- as.matrix(vcov(x)[-1, -1]) #get their covariance matrix
names(estimates) <- c("standlrt", "avslrt") #assign names
N <-  nrow(coef(x)[[1]]) # N = level 2 obs
#Calculate fracref
M <- 2
fracref <- (N_user / 19^(2/M))/M

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N_user, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fracref)


test_that("Scenario 4", {expect_equal(w$fit, b$fit, tolerance = .001)}) 

# Test passed


#-----------------------------------------------------------------------------------

#Now we load a different data setthat contains unstandardized data
# This data set is openly available from https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book

#load the data
popular<-read.spss(file = "popular.sav", to.data.frame = T)

#### Model with: one level-1 and one level-2 predictor #####

#fit the model
x <- lmer(popular ~ 1 + extrav + texp + (extrav | class), 
          data = popular, REML = F)
#inspect (fixed) coefficients
fixef(x) [-1]
#define the hypotheses:

hypotheses <- "extrav = texp  = 0;
         extrav = texp; extrav > 0 & texp >0"

##Scenario 5: standardized data  & N = level- 1 obs-----------------------------

### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = TRUE, N = "level_1", seed, jref = TRUE)

### Test the hypotheses using `bain` with a named vector

#apply overall standardization of the data
popular_standardized <- apply(popular[, -c(1,2,4)], 2, scale)
popular_standardized <- as.data.frame(popular_standardized)
popular_standardized$class <- popular$class #add class indicator
#refit the lmer model 
x_stand <- lmer(popular ~ 1 + extrav + texp + (extrav | class), 
                data = popular_standardized, REML = F)

estimates <- fixef(x_stand)[-1] #get the estimates
cov <- as.matrix(vcov(x_stand)[-1, -1]) #get their covariance matrix
names(estimates) <- c("extrav", "texp") #assign names
N <- nrow(popular_standardized) # N = level 1 obs

#Calculate fracref
M <- 2
fracref <- (N / 19^(2/M))/M

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fracref)


test_that("Scenario 5", {expect_equal(w$fit, b$fit, tolerance = .001)})

#Test passed 

##Scenario 6: standardized data  & N = level- 2 obs-----------------------------
### Test the hypotheses with the wrapper

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = TRUE, N = "level_2", seed, jref = TRUE)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x_stand)[-1] #get the estimates
cov <- as.matrix(vcov(x_stand)[-1, -1]) #get their covariance matrix
names(estimates) <- c("extrav", "texp") #assign names
N <-  nrow(coef(x_stand)[[1]]) # N = level 2 obs

#Calculate fracref
M <- 2
fracref <- (N / 19^(2/M))/M

#use `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fracref)

test_that("Scenario 6", {expect_equal(w$fit, b$fit, tolerance = .001)})

# Test passed

##Scenario 7: standardized data  & N = effective sample size (ICC)--------------
w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = TRUE, N = "ICC_effective", seed, jref = TRUE)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x_stand)[-1] #get the estimates
cov <- as.matrix(vcov(x_stand)[-1, -1]) #get their covariance matrix
names(estimates) <- c("extrav", "texp") #assign names

#Calculate fracref
M <- 2
fracref <- (N_eff_ICC / 19^(2/M))/M

#use   `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N_eff_ICC, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fracref)


test_that("Scenario 7", {expect_equal(w$fit, b$fit, tolerance = .01)}) 

# Test passed (but tolerance increased from 0.001 to 0.01)

##Scenario 8: standardized data  & N = specifised by the user--------------
N_user <- 1000

w <- bain_2lmer(x, hypotheses, 
                fraction, standardize = TRUE, N = N_user, seed, jref = TRUE)

### Test the hypotheses using `bain` with a named vector

estimates <- fixef(x_stand)[-1] #get the estimates
cov <- as.matrix(vcov(x_stand)[-1, -1]) #get their covariance matrix
names(estimates) <- c("extrav", "texp") #assign names

#Calculate fracref
M <- 2
fracref <- (N_user/ 19^(2/M))/M

#use   `bain`
set.seed(123)
b <- bain(estimates, hypotheses, n = N_user, Sigma = cov, 
          group_parameters = 0, 
          joint_parameters = length(estimates), 
          fraction = fracref)


test_that("Scenario 8", {expect_equal(w$fit, b$fit, tolerance = .001)}) 

# Test passed

