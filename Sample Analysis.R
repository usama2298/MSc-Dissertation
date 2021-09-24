library(tidyverse)
library(MASS)
library(faraway)
library(nnet)

# Converting Variables into factor
Obesity$AgeGroup <- as.factor(Obesity$AgeGroup)
Obesity$Sex <- as.factor(Obesity$Sex)
Obesity$Employment <- as.factor(Obesity$Employment)
Obesity$Veg <- as.factor(Obesity$Veg)
Obesity$Fruit <- as.factor(Obesity$Fruit)
Obesity$BMIgroup <- as.factor(Obesity$BMIgroup)
Obesity$Obese <- as.factor(if_else(Obesity$BMIgroup == "Obese", "Yes", "No"))

# Ordering BMIgroup variable levels
Obesity$BMIgroup <- factor(x = Obesity$BMIgroup,
                           levels = c("Underweight", "Normal", "Overweight", "Obese"))
levels(Obesity$BMIgroup)

# Proportion graphs
Prop_Emp <- group_by(Obesity, Employment, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Employment) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Emp, aes(x=Employment, y=proportion, group=BMIgroup,
                     linetype=BMIgroup, color=BMIgroup)) + geom_line()

Prop_Sex <- group_by(Obesity, Sex, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Sex) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Sex, aes(x=Sex, y=proportion, group=BMIgroup, linetype=BMIgroup,
                     color=BMIgroup)) + geom_line()

Prop_Veg <- group_by(Obesity, Veg, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Veg) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Veg, aes(x=Veg, y=proportion, group=BMIgroup, linetype=BMIgroup,
                     color=BMIgroup)) + geom_line()

Prop_Fruit <- group_by(Obesity, Fruit, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Fruit) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Fruit, aes(x=Fruit, y=proportion, group=BMIgroup, linetype=BMIgroup,
                       color=BMIgroup)) + geom_line()

Prop_AgeGroup <- group_by(Obesity, AgeGroup, BMIgroup) %>%
  summarise(count=n()) %>% group_by(AgeGroup) %>%
  mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_AgeGroup, aes(x=AgeGroup, y=proportion, group=BMIgroup,
                          linetype=BMIgroup, color=BMIgroup)) + geom_line()

Prop_Year <- group_by(Obesity, Year, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Year) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Year, aes(x=Year, y=proportion, group=BMIgroup, linetype=BMIgroup,
                      color=BMIgroup)) + geom_line()


# Multinomial Model
multi_mod1 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg, data = Obesity)
mod <- step(multi_mod3)
multi_mod2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit, data = Obesity)
multi_mod3 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex, data = Obesity)
multi_mod4 <- multinom(BMIgroup ~ AgeGroup + Sex, data = Obesity)
multi_mod5 <- multinom(BMIgroup ~ AgeGroup, data = Obesity)

# Predictions by Different Models
Predictions <- data.frame(Orignal = Obesity$BMIgroup, Full_Model = predict(multi_mod1),
                          Mod2 = predict(multi_mod2), Mod3 = predict(multi_mod3),
                          Mod4 = predict(multi_mod4), Mod5 = predict(multi_mod5))

# Correct Classification rate
CP <- xtabs( ~ Predictions$Orignal + Predictions$Full_Model)
(CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity)
