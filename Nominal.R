# Libraries ----
library(MASS); library(nnet); library(randomForest); library(caret)
library(forcats); library(vip)

library(recipes); library(tidymodels); library(rsample)
library(parsnip); library(tune); library(workflows)
library(doParallel); library(ranger) ; library(themis)



# Multinomial Model ----
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
summary(multi_mod1)
Alpha <- c(0, 3.710500, 2.546042, 2.368471) 
## It can be done by following way:
exp(Alpha)/sum(exp(Alpha)) ; rm(Alpha)


# Slope term gives the log odds of moving from base category to next one which
## can be seen using following command:
Beta <- predict(multi_mod2, data.frame(AgeGroup = c("16-24", "25-34"),
                                       Employment = c("Doing something else",
                                                      "Doing something else"), 
                                       Sex = c("Female", "Female"), Fruit = c("No", "No")),
                type="probs") ; Beta
exp(log(Beta[1,1]*Beta[2,2]/(Beta[1,2]*Beta[2,1]))) ; rm(Beta)
## or we can just take the coefficient of AgeGroup "25-34" and take it's exp
exp(0.8045142)
## The odds of being Normal weight (versus "Underweight") for AgeGroup "25-34" &
## considering all other covariates baseline is exp(0.804514) = 2.2356 times the
## odds for "16-24" year old (multi_mod2). As the odds multiplier is greater
## than "1", it means that younger people more tend to be "Underweight".
### Confidence interval of odds multiplier: Coef +- 1.96 * SE(Coef)
exp(c((0.8045142 - 1.96 * 0.3000024), (0.8045142 + 1.96 * 0.3000024))) # OR
exp(confint(multi_mod2))
### Plot showing variable significance and Confidence interval ranges:
plot_model(multi_mod2, show.values = TRUE, title = "Odds", show.p = F)


# Predictions by Different Models
Predictions <- tibble(True_Values = Obesity_test$BMIgroup,
                      Multi_Mod1 = predict(multi_mod1, Obesity_test),
                      Multi_Mod2 = predict(multi_mod2, Obesity_test),
                      Multi_Mod3 = predict(multi_mod3, Obesity_test),
                      Multi_Mod4 = predict(multi_mod4, Obesity_test),
                      Multi_Mod5 = predict(multi_mod5, Obesity_test))

# Correct Classification rate
CP <- table(Predictions$True_Values, Predictions$Multi_Mod2) ; CP
# Proportion Table
prop.table(CP, margin=1)
(CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity_test) ; rm(CP)
# Error rate
1- mean(Predictions$True_Values == Predictions$Multi_Mod1)
# Confusion Matrix
confusionMatrix(Predictions$True_Values, Predictions$Multi_Mod1)


# Random_forest Model ----
rf_mod1 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                        data = Obesity_train, ntree = 300)
rf_bg_mod1 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                           data = Obesity_train, mtry = 5, ntree = 300)
## Bagging is not suitable for this data as the AgeGroup is a most influential variable
rf_mod2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                        data = Obesity_train)

attributes(rf_mod1)
rf_mod1 %>% vip(geom = "col") # Imp plot
importance(rf_mod1) # Importance of variables
varUsed(rf_mod1)

# After removing Fruit and Veg as they have very low importance
rf_mod3 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex,
                        data = Obesity_train, ntree = 300) # Best Model
plot(rf_mod3)
legend('topright', colnames(rf_mod1$err.rate), col=1:5, fill=1:5)
## The Plot shows that the prediction error rate of underweight is 100%,
## Overweight has lowest prediction error rate. overall predictions error rate
## is near 60%

# Random forest with dummy variables
Obesity_train_dum <- recipe(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg, 
                            data = Obesity_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% prep() %>% juice()
Obesity_test_dum <- recipe(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg, 
                           data = Obesity_test) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% prep() %>% juice()

rf_mod1_dum <- randomForest(BMIgroup ~ . , data = Obesity_train_dum, ntree = 300)
rf_mod1_dum %>% vip(geom = "col")

Predictions <- tibble(True_Values = Obesity_test$BMIgroup,
                      Rf_Multi_Mod1 = predict(rf_mod1, Obesity_test),
                      Rf_Multi_Mod1_dum = predict(rf_mod1_dum, Obesity_test_dum),
                      # Rf_Multi_Mod2 = predict(rf_mod2, Obesity_test),
                      Rf_Multi_Mod3 = predict(rf_mod3, Obesity_test),)

# Correct Classification rate
CP <- table(Predictions$True_Values, Predictions$Rf_Multi_Mod3) ; CP
# Proportion Table
prop.table(CP, margin=1)
(CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity_test) ; rm(CP)
# Error rate
1- mean(Predictions$True_Values == Predictions$Rf_Multi_Mod1)

# Confusion Matrix

confusionMatrix(Predictions$True_Values, Predictions$Rf_Multi_Mod3)



# Multinomial without Underweight ----

Obesity_train_2 <- Obesity_train %>% subset(BMIgroup != "Underweight")
Obesity_test_2 <- Obesity_test %>% subset(BMIgroup != "Underweight")

Obesity_train_2$BMIgroup <- factor(x = Obesity_train_2$BMIgroup,
                                   levels = c("Normal", "Overweight",
                                              "Obese"))
Obesity_test_2$BMIgroup <- factor(x = Obesity_test_2$BMIgroup,
                                  levels = c("Normal", "Overweight",
                                             "Obese"))


multi_mod1.2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                         data = Obesity_train_2)


summary(multi_mod1.2)
mod <- step(multi_mod1.2); rm(mod)

multi_mod2.2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Veg,
                         data = Obesity_train_2)
multi_mod3.2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex,
                         data = Obesity_train_2)



# Predictions by Different Models
Predictions <- tibble(True_Values = Obesity_test_2$BMIgroup,
                      Multi_Mod1.2 = predict(multi_mod1.2, Obesity_test_2),
                      Multi_Mod2.2 = predict(multi_mod2.2, Obesity_test_2),
                      Multi_Mod3.2 = predict(multi_mod3.2, Obesity_test_2))

confusionMatrix(Predictions$True_Values, Predictions$Multi_Mod2.2)


# Random_forest without Underweight ----
rf_mod1.2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                          data = Obesity_train_2, ntree = 300)

rf_mod1.2 %>% vip(geom = "col") # Imp plot
importance(rf_mod1.2) # Importance of variables
varUsed(rf_mod1.2)

rf_mod2.2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Veg,
                          data = Obesity_train_2, ntree = 300)

# After removing Fruit and Veg as they have very low importance
rf_mod3.2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex,
                          data = Obesity_train_2, ntree = 300)
plot(rf_mod1.2)
legend('topright', colnames(rf_mod1.2$err.rate), col=1:4, fill=1:4)

Predictions <- tibble(True_Values = Obesity_test_2$BMIgroup,
                      Rf_Multi_Mod1.2 = predict(rf_mod1.2, Obesity_test_2),
                      Rf_Multi_Mod2.2 = predict(rf_mod2.2, Obesity_test_2),
                      Rf_Multi_Mod3.2 = predict(rf_mod3.2, Obesity_test_2),)

confusionMatrix(Predictions$True_Values, Predictions$Rf_Multi_Mod2.2)

# rm(multi_mod1, multi_mod2, multi_mod3, multi_mod4, multi_mod5, null_mod, rf_mod1, rf_mod3)

