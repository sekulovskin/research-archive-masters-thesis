#=========================================
#Plots presented in the paper
#=======================================
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(dplyr)
library(grid)
#======================================
#Sensitivity plots
#=====================================

sensitivity <- read.csv("sensitivity.csv")
#Set font
font_add(family = "CM", regular = "CENTURY.ttf")
showtext_auto()

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
#Simulation plots
#=====================================

combined_BF <- read.csv("combined_BF.csv")
combined_BF$rsq <- as.factor(combined_BF$rsq)
combined_BF$setting <- as.factor(combined_BF$setting)

# using the BF


A <- combined_BF %>%
  filter(setting == "N = 400, 1 predictor") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 1 predictor')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 207"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 211"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 211"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 214"), size =3)

B <- combined_BF %>%
  filter(setting == "N = 400, 2 predictors") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 2 predictors')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 182"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 182"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 182"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 182"), size =3) 


C <- combined_BF %>%
  filter(setting == "N = 3200, 1 predictor") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 1 predictor')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 1231"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 1217"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 1225"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 1238"), size =3) 


D <- combined_BF %>%
  filter(setting == "N = 3200, 2 predictors") %>%
  ggplot(aes(y = BF, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-5, 19) +
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(y = TeX('$BF_{0u}$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 2 predictors')) +
  annotate("text", x=1, y= -4, label= TeX("$N_{eff}$ = 1239"), size =3) +
  annotate("text", x=2, y= -4, label= TeX("$N_{eff}$ = 1232"), size =3) +
  annotate("text", x=3, y= -4, label= TeX("$N_{eff}$ = 1242"), size =3) +
  annotate("text", x=4, y= -4, label= TeX("$N_{eff}$ = 1232"), size =3) 

ggarrange(A, B, C, D,
          labels = c("", "", "", ""),
          ncol = 2, nrow = 2, common.legend = TRUE)


# using log(BF)

A <- combined_BF %>%
  filter(setting == "N = 400, 1 predictor") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 1 predictor')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 207"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 211"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 211"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 214"), size =3)

B <- combined_BF %>%
  filter(setting == "N = 400, 2 predictors") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 400, 2 predictors')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 182"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 182"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 182"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 182"), size =3) 


C <- combined_BF %>%
  filter(setting == "N = 3200, 1 predictor") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 1 predictor')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 1231"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 1217"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 1225"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 1238"), size =3) 


D <- combined_BF %>%
  filter(setting == "N = 3200, 2 predictors") %>%
  ggplot(aes(y = BFlog, x = rsq, color = rsq)) +
  geom_boxplot() +
  theme_classic() +
  ylim(-800, 200) +
  geom_hline(yintercept = 2.9, linetype = 2) + 
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(y = TeX('$log(BF_{0u})$'), x = TeX('$R^2_m$'), color = TeX('$R^2_m$'),
       title = TeX('$N_{level-1}$ = 3200, 2 predictors')) +
  annotate("text", x=1, y= -700, label= TeX("$N_{eff}$ = 1239"), size =3) +
  annotate("text", x=2, y= -700, label= TeX("$N_{eff}$ = 1232"), size =3) +
  annotate("text", x=3, y= -700, label= TeX("$N_{eff}$ = 1242"), size =3) +
  annotate("text", x=4, y= -700, label= TeX("$N_{eff}$ = 1232"), size =3) 

ggarrange(A, B, C, D,
          labels = c("", "", "", ""),
          ncol = 2, nrow = 2, common.legend = TRUE)



