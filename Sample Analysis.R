library(tidyverse)
library(MASS)
library(faraway)
library(nnet)
library(randomForest)
library(ggthemes)
library(sjPlot)

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
                           levels = c("Underweight", "Normal", "Overweight",
                                      "Obese"))
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
multi_mod1 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                       data = Obesity)
## Model Selection
null_mod <- multinom(BMIgroup ~ 1, data=Obesity)
setequal((null_mod$deviance - multi_mod1$deviance),
         (qchisq(p = 0.95,df = (multi_mod1$edf-null_mod$edf))))
## This shows that the null model is insignificant
mod <- step(multi_mod1)
multi_mod2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                       data = Obesity) # Best Model
multi_mod3 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex, data = Obesity)
multi_mod4 <- multinom(BMIgroup ~ AgeGroup + Sex, data = Obesity)
multi_mod5 <- multinom(BMIgroup ~ AgeGroup, data = Obesity)


# Random_forest Model
rf_mod1 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                        data = Obesity) # Best Model
rf_mod2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                        data = Obesity)
rf_mod3 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex, data = Obesity)
plot(rf_mod1)
legend('topright', colnames(rf_mod1$err.rate), col=1:5, fill=1:5)
## The Plot shows that the prediction error rate of underweight is 100%,
## Overweight has lowest prediction error rate. overall predictions error rate
## is near 55%


# Get importance
importance <- importance(rf_mod1) ; importance
varImportance <- data.frame(Variables = row.names(importance), 
                        Importance = round(importance[ ,'MeanDecreaseGini'],2))
## Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
## Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') + coord_flip() + theme_excel_new()


# Predictions by Different Models
Predictions <- data.frame(True_Values = Obesity$BMIgroup,
                          Multi_Mod1 = predict(multi_mod1),
                          Multi_Mod2 = predict(multi_mod2),
                          Multi_Mod3 = predict(multi_mod3),
                          Multi_Mod4 = predict(multi_mod4),
                          Multi_Mod5 = predict(multi_mod5),
                          Rf_Multi_Mod1 = predict(rf_mod1),
                          Rf_Multi_Mod2 = predict(rf_mod2),
                          Rf_Multi_Mod3 = predict(rf_mod3))


# Correct Classification rate
CP <- xtabs( ~ Predictions$True_Values + Predictions$Multi_Mod2)
(CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity)
## So according to multinomial model, the Model 2 is best.
## According to Random_Forest, the Model 1 is best, will all variables.


# Checking the prediction values based on solely on intercept, which will give
## prediction values of base categories:
predict(multi_mod2, data.frame(AgeGroup = "16-24",
                               Employment = "Doing something else",
                               Sex = "Female", Fruit = "No"), type="probs")
Alpha_Pred <- c(0, 3.388426, 2.220982, 1.971101) 
## It can be done by following way:
exp(Alpha_Pred)/sum(exp(Alpha_Pred))


# Slope term gives the log odds of moving from base category to next one which
## can be seen using following command:
Beta.Pred <- predict(multi_mod2, data.frame(AgeGroup = c("16-24", "25-34"),
                      Employment = c("Doing something else",
                                      "Doing something else"), 
                      Sex = c("Female", "Female"), Fruit = c("No", "No")),
                     type="probs") ; Beta.Pred
exp(log(Beta.Pred[1,1]*Beta.Pred[2,2]/(Beta.Pred[1,2]*Beta.Pred[2,1])))
## or we can just take the coefficient of AgeGroup "25-34" and take it's exp
exp(0.6011892)
## The odds of being Normal weight (versus "Underweight") for AgeGroup "25-34" &
## considering all other covariates baseline is exp(0.6011) = 1.8241 times the
## odds for "16-24" year old (multi_mod2). As the odds multiplier is greater
## than "1", it means that younger people more tend to be "Underweight".
### Confidence interval of odds multiplier: Coef +- 1.96 * SE(Coef)
exp(c((0.6011892 - 1.96 * 0.2569298), (0.6011892 + 1.96 * 0.2569298)))
### Plot showing variable significance and Confidence interval ranges:
plot_model(multi_mod2, show.values = TRUE, title = "Odds", show.p = F)


















