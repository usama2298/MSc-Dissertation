library(tidyverse)
library(MASS)
library(faraway)
library(nnet)
library(randomForest)
library(ggthemes)
library(sjPlot)


# Reading Data
Obesity <- read_csv("Obesity.csv")


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


# Spiting data into "Train" and "Test" set
Split = sample(x = c(TRUE, FALSE), size = nrow(Obesity), replace = TRUE,
               prob = c(0.75, 0.25))
Obesity_train = Obesity[Split, ]
Obesity_test = Obesity[!Split, ]
write_csv(Obesity_train, "D:/OneDrive - University of Glasgow/Study/Courses/Dissertation/R Analysis/MSc-Dissertation/Obesity_train.csv")
write_csv(Obesity_test, "D:/OneDrive - University of Glasgow/Study/Courses/Dissertation/R Analysis/MSc-Dissertation/Obesity_test.csv")
write_rds(Split, "D:/OneDrive - University of Glasgow/Study/Courses/Dissertation/R Analysis/MSc-Dissertation/Split.rds")
rm(Obesity, Obesity_train, Obesity_test, Split) # Removing main data as not needed anymore


# Reading Train and Test data
# Split <- read_rds("Split.rds") # Train and Test indices, read if necessary
Obesity_train <- read_csv("Obesity_train.csv")
Obesity_test <- read_csv("Obesity_test.csv")


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
#Removing prop dataframes as there is no further use
rm(Prop_Sex, Prop_AgeGroup, Prop_Emp, Prop_Fruit, Prop_Veg, Prop_Year)


# Multinomial Model
multi_mod1 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                       data = Obesity_train)
## Model Selection
null_mod <- multinom(BMIgroup ~ 1, data=Obesity_train)
setequal((null_mod$deviance - multi_mod1$deviance),
         (qchisq(p = 0.95,df = (multi_mod1$edf-null_mod$edf))))
## This shows that the null model is insignificant
mod <- step(multi_mod1) ; rm(mod)
multi_mod2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                       data = Obesity_train) # Best Model
multi_mod3 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex, data = Obesity_train)
multi_mod4 <- multinom(BMIgroup ~ AgeGroup + Sex, data = Obesity_train)
multi_mod5 <- multinom(BMIgroup ~ AgeGroup, data = Obesity_train)




# Checking the prediction values based on solely on intercept, which will give
## prediction values of base categories:
predict(multi_mod2, data.frame(AgeGroup = "16-24",
                               Employment = "Doing something else",
                               Sex = "Female", Fruit = "No"), type="probs")
summary(multi_mod2)
Alpha <- c(0, 2.966386, 1.809317, 1.524672) 
## It can be done by following way:
exp(Alpha)/sum(exp(Alpha)) ; rm(Alpha)


# Slope term gives the log odds of moving from base category to next one which
## can be seen using following command:
Beta <- predict(multi_mod2, data.frame(AgeGroup = c("16-24", "25-34"),
                                       Employment = c("Doing something else",
                                                      "Doing something else"), 
                          Sex = c("Female", "Female"), Fruit = c("No", "No")),
                          type="probs") ; Beta.Pred
exp(log(Beta[1,1]*Beta[2,2]/(Beta[1,2]*Beta[2,1]))) ; rm(Beta)
## or we can just take the coefficient of AgeGroup "25-34" and take it's exp
exp(0.7468219)
## The odds of being Normal weight (versus "Underweight") for AgeGroup "25-34" &
## considering all other covariates baseline is exp(0.74682) = 2.1102 times the
## odds for "16-24" year old (multi_mod2). As the odds multiplier is greater
## than "1", it means that younger people more tend to be "Underweight".
### Confidence interval of odds multiplier: Coef +- 1.96 * SE(Coef)
exp(c((0.7468219 - 1.96 * 0.3051748), (0.7468219 + 1.96 * 0.3051748))) # OR
exp(confint(multi_mod2))
### Plot showing variable significance and Confidence interval ranges:
plot_model(multi_mod2, show.values = TRUE, title = "Odds", show.p = F)



# Linear Discriminant Analysis
lda_mod1 <- lda(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                data = Obesity_train) # Best Model
lda_mod2 <- lda(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                data = Obesity_train)
lda_mod3 <- lda(BMIgroup ~ AgeGroup + Employment + Sex, data = Obesity_train)

## Prior probabilities are the default observed proportion in the data.
## Here in Proportion of trace we can see that the first component is the most
## dominating but the variation is not satisfactory so we take LD2 also for
## classification. Age variable is dominated in LD1 and Employment variable is
## dominated in LD2 and as seen before the classification will be done on the
## basis of mainly first two LD1 & LD2.


# Random_forest Model
rf_mod1 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                        data = Obesity_train)
rf_mod2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                        data = Obesity_train)
rf_mod3 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex,
                        data = Obesity_train) # Best Model
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
# Removing importance as there is no further use
rm(importance, varImportance, rankImportance)














# Predictions by Different Models
Predictions <- tibble(True_Values = Obesity_test$BMIgroup,
                      Multi_Mod2 = predict(multi_mod2, Obesity_test),
                      Multi_Mod3 = predict(multi_mod3, Obesity_test),
                      Multi_Mod4 = predict(multi_mod4, Obesity_test),
                      Multi_Mod5 = predict(multi_mod5, Obesity_test),
                      LDA_Mod1 = predict(lda_mod1, Obesity_test)$class,
                      LDA_Mod2 = predict(lda_mod2, Obesity_test)$class,
                      LDA_Mod3 = predict(lda_mod3, Obesity_test)$class,
                      Rf_Multi_Mod1 = predict(rf_mod1, Obesity_test),
                      Rf_Multi_Mod2 = predict(rf_mod2, Obesity_test),
                      Rf_Multi_Mod3 = predict(rf_mod3, Obesity_test),)

# Correct Classification rate
CP <- xtabs( ~ Predictions$True_Values + Predictions$Rf_Multi_Mod3) ; CP
(CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity_test) ; rm(CP)

# Removing bad models and unnecessary predictions to save memory
rm(null_mod, multi_mod1, multi_mod3, multi_mod4, multi_mod5, lda_mod2, lda_mod3, 
   rf_mod1, rf_mod2, Predictions)


