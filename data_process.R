#### Data Processing ####

#1. Charge data #

setwd("C:/Users/pc/Desktop/Join")

library(readxl)
library(tidyverse)

data <- read_excel("data.xlsx")

#2. Explore data #

str(data)
data$tratamiento <- factor(data$tratamiento)
str(data)

#3. Descriptive statistics #

library(skimr)

skim(data)

attach(data)

library(FSA)

sts <- Summarize(DM ~ tratamiento, data = data, digits = 2, na.rm = TRUE)

sts$var <- sts$sd*sts$sd
sts

#4. Graphs #

library(ggthemes)

#4.1 Histogram

ggplot(data, aes(x = DM))+
geom_histogram(aes(y =..density..), color = "black", fill = "white", bins = 8) +
  geom_density(alpha = 0.01, fill = "orange") +
  labs(title = "DM distribution",
       x = "DM",
       y = "Frequency")+
  theme_stata()

#4.2 Boxplot

ggplot(data, aes(x = tratamiento, y = DM, color = tratamiento)) +
  stat_boxplot(geom = "errorbar", # Error bars
               width = 0.25)+
  geom_boxplot() +
  guides(color = "none")+
  labs(title = "DM Mean Comparision",
       x = "Tratamiento",
       y = "DM")+
  theme_stata()

#5. Inferential statistics #

#5.1 Model

mod <- lm(DM ~ tratamiento, data = data)

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

# Breusch - Pagan Test

library(lmtest)
bptest(mod)

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

TukeyHSD(aov(DM~tratamiento))
marginal <- lsmeans(mod,~ tratamiento)
marginal
CLD <- cld(marginal, alpha   = 0.05, Letters = letters,  adjust  = "tukey")  
CLD

#8. Plot

par(mfrow=c(1,1))
plot(TukeyHSD(aov(DM~tratamiento)))

ggplot(CLD, aes(x = tratamiento, y = lsmean, label = .group)) +
  geom_point(shape  = 15, size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax  =  upper.CL), width = 0.2, size  =  0.7)  +
  ylab("DM") +
  xlab("Tratamientos")+
  theme_stata()       +  
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(5, 5, 5),  
            color   = "black") +  
  ggtitle("Medias de porcentaje de Masa seca con Intervalos de Confianza al 95%")

