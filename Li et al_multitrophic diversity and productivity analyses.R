# AIM------
# Analyses for the paper Li et al. 2022 Multitrophic arthropod diversity mediates tree diversity effects on primary productivity

# Script author: Dr. Yi Li

# Input Data:
# plot_level_multitrophic_diversity_and_stand_volume.csv (or sheet one in the excel file)


# Reproducibility-----

# Libraries-----
library(lmerTest)
library(lme4)
library(ggplot2)
library(gridExtra) #combine different figs together
library(piecewiseSEM)



## HYPOTHESES I
# Analysis of relationships between tree species richness and arthropod diversity----------

# Data----------
multitrophic <- read.csv("plot_level_multitrophic_diversity_and_stand_volume.csv", header = T)
names(multitrophic)

# GLMM
glmer.TR.herbivore <- glmer(herbivore_diversity ~ scale(year)*log2(tree_diversity)+(1|site/year),
                                  data = multitrophic,family = "poisson")
summary(glmer.TR.herbivore) 


# Plot(Fig2,FigS1)----------
p1 <- 
  ggplot(data = multitrophic,aes(x = log2(tree_diversity),y = log(herbivore_diversity+1),color=as.factor(year)))+
  (geom_point(shape=20,size=2,alpha=.8))+
  geom_smooth(aes(linetype=as.factor(her_div_sig)),method="lm",formula= y ~ x,se=F)+
  geom_smooth(method="lm",formula= y ~ x,color="black")+
  scale_color_manual(values=c("#E0EFDB","#C6E0B5","#AACF8D","#71AD49","#548235","#375622"))+
  (scale_x_continuous(breaks=c(0,log2(2),log2(4),log2(8),log2(16),log2(24)),labels= c("1","2","4","8","16","24")))+
  (scale_y_continuous(breaks=c(log(2),log(10),log(50)),labels= c("2","10","50")))+
  theme_classic()+
  xlab("Tree species richness")+
  ylab("Herbivore species richness")

p1
# Note: (1) species richness could be replaced by abundance;
# (2) herbivore could be replaced by predator, parasitoid, and overall trophic groups
grid.arrange(p1, p2, p3, p4, ncol =2,nrow = 2)



#### HYPOTHESES II
# Analysis of relationships between arthropod diversity and tree productivity----------
names(multitrophic)

# LMM
lmer.herbivore.productivity <- lmer(sqrt(stand_volume) ~ log2(tree_diversity)*log(herbivore_diversity+1)*scale(year)+(1|site/year),
                 data = multitrophic)
summary(lmer.herbivore.productivity) 

# Plot(Fig3,FigS2-S4)----------
p1 <-
ggplot(data =multitrophic,aes(x = log(herbivore_diversity+1),y = stand_volume,color = as.factor(year),linetype=as.factor(her_div_vol_sig)))+  #,linetype=as.factor(para_div_vol_sig)  
  (geom_point()+
  geom_smooth(method="lm",formula= y ~ x,se=F)+
  geom_smooth(method="lm",formula= y ~ x,color="black")+
  scale_color_manual(values=c("#E0EFDB","#C6E0B5","#AACF8D","#71AD49","#548235","#375622"))+
  (scale_y_continuous(breaks=c(sqrt(4),sqrt(25),sqrt(81),sqrt(169),sqrt(289)),labels= c("4","25","81","169","289")))+
  theme_classic()+
  xlab("herbivore richness")+ 
  ylab(bquote(Primary~ productivity~ (m^3~ha^-1))))
  
p1
# Note: (1) stand_voluume could be replaced by stannd_volume increment;
# (2) herbivore could be replaced by predator, parasitoid, and overall trophic groups
grid.arrange(p1, p2, p3, p4, ncol =2,nrow = 2)




#### HYPOTHESES III
# Analysis of relationships among tree diversity, funtional traits, arthropod diversity 
# and tree productivity----------

names(multitrophic)

SEM.all.arthropods <-
  psem(lme(log(all_trophic_diversity)~log2(tree_diversity)+scale(FDis)+scale(Traits_PC1),
                              random = ~1|site/year,data=multitrophic),
               lme(sqrt(stand_volume)~log(all_trophic_diversity)+scale(FDis)+scale(Traits_PC1),
                              random = ~1|site/year,data=multitrophic),
               data = multitrophic)

summary(SEM.all.arthropods)
# Note: all arthropod group could be replaced by herbivore, predator or parasitoid


###### The end ######
