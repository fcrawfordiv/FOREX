library(ggplot2)
library(tidyverse)
library(dplyr)

Data2 <- na.omit(ST.Project)
sd(Data2$X2.2, na.rm = TRUE)

## Mutate character to numeric
str(Data2$X2.2)
Data2$Pips <- as.numeric(Data2$X2.2)
str(Data2$X2.3)
Data2$predicted <- as.numeric(Data2$X2.3)

mean_predicted <- mean(Data2$predicted, na.rm = TRUE)
mean_predicted

mean_pips <- mean(Data2$Pips, na.rm = TRUE)
mean_pips
#----------------------------------------------- 


#Rename
Data2$Country <- (Data2$United.Kingdom..................)
Data2$Time <- (Data2$X4.00.00)
Data2$Date <- (Data2$X2011.01.01)
Data2$Volatility <- (Data2$Low.Volatility.Expected.........)
#--------------------------------------------------------------


#Time subset 
Subset1 <- subset(Data2, X4.00.00 == "4:00:00")
#-----------------------------------------------


#Plots
#Box and whisker plot
ggplot(Data2, aes(x = Country , y = predicted)) +
  geom_boxplot() +
  xlab("Country") +
  ylab("Pips") +
  coord_flip()

ggplot(Data2, aes(x = Volatility , y = Pips)) +
  geom_boxplot() +
  xlab("Volatility Level") +
  ylab("Pips") +
  coord_flip()


#Residual Plot/Fit Model
mod1 <- lm(Pips ~ Volatility + predicted + Volatility:predicted, data = Data2)
summary(mod1)

plot(y = resid(mod1), x = fitted(mod1), xlab = "Fitted", ylab = "Residuals") 
abline(h = 0)


#QQ Plot
qqnorm(resid(mod1))
qqline(resid(mod1))
#-----------------------------------------------------


#Confidence Interval
confint(mod1)
anova(mod1)


#Cooks Distance
plot(mod1, which = 4)

#Sum stat
summary(Data2$Pips)





