
# Testing the wrapper for J_ref

# Same data from the sensitivity analysis

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



hypothesis <- "X1 = X2 = 0"

x <- lmer(y ~ X1 + X2 +  (X1 + X2| group),
          REML = FALSE, data = data)


bain_2lmer(x, hypothesis, standardize = FALSE, fraction = 1,
           N = 1165, seed = 123, jref = TRUE)