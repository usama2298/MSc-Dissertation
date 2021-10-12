library(MASS); library(nnet); library(randomForest); library(caret)
library(forcats)


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
                          type="probs") ; Beta
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


# Predictions by Different Models
Predictions <- tibble(True_Values = Obesity_test$BMIgroup,
                      Multi_Mod1 = predict(multi_mod1, Obesity_test),
                      Multi_Mod2 = predict(multi_mod2, Obesity_test),
                      Multi_Mod3 = predict(multi_mod3, Obesity_test),
                      Multi_Mod4 = predict(multi_mod4, Obesity_test),
                      Multi_Mod5 = predict(multi_mod5, Obesity_test))

# Correct Classification rate
CP <- table(Predictions$True_Values, Predictions$Multi_Mod1) ; CP
# Proportion Table
prop.table(CP, margin=1)
(CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity_test) ; rm(CP)
# Error rate
1- mean(Predictions$True_Values == Predictions$Multi_Mod1)



# # Linear Discriminant Analysis
# lda_mod1 <- lda(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
#                 data = Obesity_train) ; lda_mod1
# lda_mod2 <- lda(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
#                 data = Obesity_train) ; lda_mod2
# lda_mod3 <- lda(BMIgroup ~ AgeGroup + Employment + Sex, data = Obesity_train)
# lda_mod3
# 
# Lda.Pred <- predict(lda_mod2)
# ldahist(data = Lda.Pred$x[,c(2)], g=Obesity_train$BMIgroup, cex=1.2)
# 
# # Predictions by Different Models
# Predictions <- tibble(True_Values = Obesity_train$BMIgroup,
#                       LDA_Mod1 = predict(lda_mod1)$class,
#                       LDA_Mod2 = predict(lda_mod2)$class,
#                       LDA_Mod3 = predict(lda_mod3)$class)
# 
# # Correct Classification rate
# CP <- table(Predictions$True_Values, Predictions$LDA_Mod3) ; CP
# # Proportion Table
# prop.table(CP, margin=1)
# (CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity_train) ; rm(CP)
# # Error rate
# 1- mean(Predictions$True_Values == Predictions$LDA_Mod3)
## Prior probabilities are the default observed proportion in the data.
## Here in Proportion of trace we can see that the first component is the most
## dominating but the variation is not satisfactory so we take LD2 also for
## classification. Age variable is dominated in LD1 and Employment variable is
## dominated in LD2 and as seen before the classification will be done on the
## basis of mainly first two LD1 & LD2.


# Random_forest Model
rf_mod1 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                        data = Obesity_train, ntree = 300)
rf_bg_mod1 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                        data = Obesity_train, mtry = 5, ntree = 300)
rf_mod2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                        data = Obesity_train)

hist(treesize(rf_mod1))
importance(rf_mod1) # Importance of variables
varImpPlot(rf_mod1) # Imp plot
varUsed(rf_mod1)

# After removing Fruit and Veg as they have very low importance
rf_mod3 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex,
                        data = Obesity_train, ntree = 300) # Best Model
plot(rf_mod3)
legend('topright', colnames(rf_mod1$err.rate), col=1:5, fill=1:5)
## The Plot shows that the prediction error rate of underweight is 100%,
## Overweight has lowest prediction error rate. overall predictions error rate
## is near 55%


Predictions <- tibble(True_Values = Obesity_test$BMIgroup,
                      Rf_Multi_Mod1 = predict(rf_mod1, Obesity_test),
                      # Rf_Multi_Bg_Mod1 = predict(rf_bg_mod1, Obesity_test),
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












# Removing bad models and unnecessary predictions to save memory
# rm(null_mod, multi_mod1, multi_mod3, multi_mod4, multi_mod5, lda_mod2, lda_mod3,
#    rf_mod1, rf_mod2, rf_mod3, Predictions)

