#### Yield Analysis ####

# DBCA Analysis #

#1. Charge data #

setwd("C:/Users/pc/Desktop/Join")

library(readxl)
library(tidyverse)

data <- read_excel("yield_data.xlsx")

#2. Explore data #

str(data)

data <- data %>% 
  mutate_at(c("bloque", "tratamiento","ntub"), as.factor)

str(data)

#3. Descriptive statistics #

library(skimr)

skim(data)

attach(data)

library(FSA)

sts <- Summarize(peso ~ tratamiento, data = data, digits = 2, na.rm = TRUE)

sts$var <- sts$sd*sts$sd
sts

#4. Graphs #

library(ggthemes)

#4.1 Histogram

ggplot(data, aes(x = peso))+
  geom_histogram(aes(y =..density..), color = "black", fill = "white", bins = 8) +
  geom_density(alpha = 0.01, fill = "orange") +
  labs(title = "Weight distribution",
       x = "DM",
       y = "Frequency")+
  theme_minimal()

#4.2 Boxplot

ggplot(data, aes(x = tratamiento, y = peso, color = tratamiento)) +
  stat_boxplot(geom = "errorbar", # Error bars
               width = 0.25)+
  geom_boxplot() +
  guides(color = "none")+
  labs(title = "Weight Mean Comparision",
       x = "Tratamiento",
       y = "Weght") +
  theme_minimal()

#5. Inferential statistics #

#5.1 Model

mod <- lm(peso ~ bloque + tratamiento, data = data)

#5.2 Assumptions #

# Linealidad

mean(mod$residuals)

# Residuales Estandarizados

ri <- rstandard(mod)
sort(ri)

# Normalidad

shapiro.test(ri)

# Homocedasticidad

# Non - constant Variance Score Test

library(car)
ncvTest(mod)

# Independence

durbinWatsonTest(mod)

# Graphic

library(gvlma)
plot(gvlma(mod))

par(mfrow=c(2,2))
plot(mod)

##6. ANOVA ##

anova(mod)

#7. PostHoc Tukey

library(multcompView)
library(lsmeans)
library(multcomp)
library(agricolae)

mod1 <- aov(peso ~ bloque + tratamiento, data = data)
TukeyHSD(mod1) 

marginal <- lsmeans(mod,~ tratamiento)
marginal
pairs(marginal, adjust="tukey")
CLD <- cld(marginal, alpha   = 0.05, Letters = letters,  adjust  = "tukey")  
CLD

#8. Comparacion de Fisher

gl <- df.residual(mod1)          
cm <- deviance(mod1)/gl      
compara <- LSD.test(peso,tratamiento,gl,cm)
compara

#Grafico

library(effects)
plot(allEffects(mod1))


#8. Plot

par(mfrow=c(1,1))

ggplot(CLD, aes(x = tratamiento, y = lsmean, label = .group)) +
  ylab("Weigth") +
  xlab("Tratamientos")+
  geom_point(shape  = 15, size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax  =  upper.CL), width = 0.2, size  =  0.7)  +
  theme_minimal()       +  
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  geom_text(nudge_x = c(0, 0, 0, 0),
            nudge_y = c(400, 400, 400, 400),  
            color   = "black") +  
  scale_y_continuous(limits = c(300,1500))+
  ggtitle("Medias de Peso con Intervalos de Confianza al 95%")
  

## Factorial Analysis ##

#1. Descriptive statistics #

sts1 <- Summarize(peso ~ ntub, data = data, digits = 2, na.rm = TRUE)

sts1$var <- sts1$sd*sts1$sd
sts1

#1.2 Boxplot

ggplot(data, aes(x = ntub, y = peso, color = ntub)) +
  stat_boxplot(geom = "errorbar", # Error bars
               width = 0.25)+
  geom_boxplot() +
  guides(color = "none")+
  labs(title = "Numero de tuberculos Comparision",
       x = "Numero de tuberculos",
       y = "Weght")+
  theme_minimal()

#2. Inferential statistics #

factorA <- tratamiento
factorB <- ntub

mod2 <- lm(peso ~ bloque + factorA + factorB + factorA:factorB)
anova(mod2)

#2.2 Assumptions #

# Normalidad

ri1 <- rstandard(mod2)
shapiro.test(ri1) 

# Shapiro - Francia

library(nortest) 
sf.test(ri1)

#Homogeneidad de Varianzas

ncvTest(mod2) 

# Plot del modelo

par(mfrow=c(2,2))
plot(mod2)

#3. LSD de Fisher para el Factor que presenta diferencias

gl1 <- df.residual(mod2)           
cm1 <- deviance(mod2)/gl      

# Comparacion del factor que si difiere, en este caso FactorB

compara1 <- LSD.test(peso, factorB, gl1, cm1)   # FactorB
compara1


#4. Comparacion de Tukey del factorB

mod3 <- lm(peso ~ factorA + factorB)

marginal1 <- lsmeans(mod3,~ factorB)   # Por no haber interaccion, colocamos el que difiere
marginal1 

CLD1 <- cld(marginal1, alpha   = 0.05,  Letters = letters, adjust  = "tukey")
CLD1

#Graficas  

library(effects, pos=23)
plot(allEffects(mod3))

# 5. Plot del efector principal del factor B

par(mfrow=c(1,1))

ggplot(CLD1, aes(x = factorB, y = lsmean, label = .group)) +
  ylab("Weigth") +
  xlab("Numero de tuberculos")+
  geom_point(shape  = 15, size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax  =  upper.CL), width = 0.2, size  =  0.7)  +
  theme_minimal()       +  
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  geom_text(nudge_x = c(0, 0, 0, 0),
            nudge_y = c(300, 600, 500, 500, 600, 600, 500, 500, 500),  
            color   = "black") +  
  scale_y_continuous(limits = c(300,1800))+
  ggtitle("Medias de Peso con Intervalos de Confianza al 95%")

# Calculo del tamaño de muestra

library(pwr)

# Cálculo Tamaño de efecto Anova

efecto_anova <- cohen.ES(test="anov",size="large")  
efecto_anova

# Tamaño de muestra

pwr.anova.test(f=0.4, k=4, sig.level = 0.05, power = 0.80)

# Poder del estudio 

pwr.anova.test(n=9,k=4,sig.level = 0.05,f=0.25)

# Efecto de Diseño

pwr.anova.test(n=9,k=4,sig.level = 0.05,power=.80)
