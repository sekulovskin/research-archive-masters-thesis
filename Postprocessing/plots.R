#=========================================
#Plots presented in the paper
#=======================================
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(dplyr)
library(grid)

#Set font
font_add(family = "CM", regular = "CENTURY.ttf")
showtext_auto()

#======================================
#Sensitivity Analysis
#=====================================

sensitivity <- read.csv("sensitivity.csv")


A  <- sensitivity %>%
  filter(N == "Level-1") %>%
  ggplot(aes(x= c(2, 4, 6), y = BF)) +
  geom_point() +
  geom_line() +
  theme_classic() + 
  theme(text = element_text(family = "CM")) +
  scale_x_discrete(limits=c(2, 4, 6)) +
  theme(legend.position="none") +
  labs(y = TeX('$BF_{0u}$'), x= TeX('$J$'), title =  TeX("N = $N_{level-1}$"))

B  <- sensitivity %>%
  filter(N == "Level-2") %>%
  ggplot(aes(x= c(2, 4, 6), y = BF)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  theme(text = element_text(family = "CM")) +
  scale_x_discrete(limits=c(2, 4, 6)) +
  theme(legend.position="none") +
  labs(y = TeX('$BF_{0u}$'), x= TeX('$J$'), title = TeX("N = $N_{level-2}$"))

C  <- sensitivity %>%
  filter(N == "ICC-effective") %>%
  ggplot(aes(x= c(2, 4, 6), y = BF)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  theme(text = element_text(family = "CM")) +
  scale_x_discrete(limits=c(2, 4, 6)) +
  theme(legend.position="none") +
  labs(y = TeX('$BF_{0u}$'), x= TeX('$J$'), title = TeX("N = ICC based $N_{eff}$"))

D  <-sensitivity %>%
  filter(N == "MI-effective") %>%
  ggplot(aes(x= c(2, 4, 6), y = BF)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  theme(text = element_text(family = "CM")) +
  scale_x_discrete(limits=c(2, 4, 6)) +
  theme(legend.position="none") +
  labs(y = TeX('$BF_{0u}$'), x= TeX('$J$'), title = TeX("N = MI based $N_{eff}$"))


ggarrange(A, B, C, D,
          labels = c("", "", "", ""),
          ncol = 2, nrow = 2)




#======================================
#Simulation study
#=====================================

# Using FML
combined_BF <- read.csv("combined_BF.csv")
combined_BF$rsq <- as.factor(combined_BF$rsq)
combined_BF$setting <- as.factor(combined_BF$setting)

# Using REML
combined_BF.REML <- read.csv("combined_BF_REML.csv")
combined_BF.REML$rsq <- as.factor(combined_BF.REML$rsq)
combined_BF.REML$setting <- as.factor(combined_BF.REML$setting)

#----------------------------
#Figure 4

A <- combined_BF %>%
  filter(setting == "N = 400, 1 predictor") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) + 
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 1 predictor')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 99"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 98"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 99"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 98"), size =3)

B <- combined_BF %>%
  filter(setting == "N = 400, 2 predictors") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) + 
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 2 predictors')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 194"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 194"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 195"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 194"), size =3) 


C <- combined_BF %>%
  filter(setting == "N = 3200, 1 predictor") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_hline(yintercept = 1, linetype = 2) + 
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) + 
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 1 predictor')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 967"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 963"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 968"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 961"), size =3) 


D <- combined_BF %>%
  filter(setting == "N = 3200, 2 predictors") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) + 
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 2 predictors')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 895"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 902"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 904"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 895"), size =3) 

ggarrange(A, B, C, D,
          labels = c("", "", "", ""),
          ncol = 2, nrow = 2, common.legend = TRUE)

#----------------------------
# Figure 5

A <- combined_BF %>%
  filter(setting == "N = 400, 1 predictor") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 1 predictor')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 99"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 98"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 99"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 98"), size =3)

B <- combined_BF %>%
  filter(setting == "N = 400, 2 predictors") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 2 predictors')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 194"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 194"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 195"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 194"), size =3)


C <- combined_BF %>%
  filter(setting == "N = 3200, 1 predictor") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 1 predictor')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 967"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 963"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 968"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 961"), size =3)


D <- combined_BF %>%
  filter(setting == "N = 3200, 2 predictors") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 2 predictors')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 895"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 902"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 904"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 895"), size =3) 

ggarrange(A, B, C, D,
          labels = c("", "", "", ""),
          ncol = 2, nrow = 2, common.legend = TRUE)


#----------------------------
# Figure 6

A <- combined_BF.REML %>%
  filter(setting == "N = 400, 1 predictor") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) + 
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 1 predictor')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 99"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 98"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 99"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 98"), size =3)

B <- combined_BF.REML %>%
  filter(setting == "N = 400, 2 predictors") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) + 
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 2 predictors')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 194"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 194"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 195"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 194"), size =3)


C <- combined_BF.REML %>%
  filter(setting == "N = 3200, 1 predictor") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_hline(yintercept = 1, linetype = 2) + 
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) + 
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 1 predictor')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 967"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 963"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 968"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 961"), size =3)


D <- combined_BF.REML %>%
  filter(setting == "N = 3200, 2 predictors") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = 2) + 
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 2 predictors')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 895"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 902"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 904"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 895"), size =3)  

ggarrange(A, B, C, D,
          labels = c("", "", "", ""),
          ncol = 2, nrow = 2, common.legend = TRUE)

#-------------------
# Figure 7

A <- combined_BF.REML %>%
  filter(setting == "N = 400, 1 predictor") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 1 predictor')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 99"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 98"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 99"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 98"), size =3)

B <- combined_BF.REML %>%
  filter(setting == "N = 400, 2 predictors") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 2 predictors')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 194"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 194"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 195"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 194"), size =3)

C <- combined_BF.REML %>%
  filter(setting == "N = 3200, 1 predictor") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 1 predictor')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 967"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 963"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 968"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 961"), size =3) 


D <- combined_BF.REML %>%
  filter(setting == "N = 3200, 2 predictors") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none", text = element_text(family = "CM")) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 2 predictors')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 895"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 902"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 904"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 895"), size =3)  

ggarrange(A, B, C, D,
          labels = c("", "", "", ""),
          ncol = 2, nrow = 2, common.legend = TRUE)


