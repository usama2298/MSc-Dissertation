# Libraries ----
library(MASS); library(nnet); library(randomForest); library(caret)
library(forcats); library(vip)

library(recipes); library(tidymodels); library(rsample)
library(parsnip); library(tune); library(workflows)
library(doParallel); library(ranger) ; library(themis)



# Multinomial Model ----
multi_mod1 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                       data = Obesity_train) ; summary(multi_mod1)
## Model Selection
null_mod <- multinom(BMIgroup ~ 1, data=Obesity_train) ;  summary(null_mod)
setequal((null_mod$deviance - multi_mod1$deviance),
         (qchisq(p = 0.95,df = (multi_mod1$edf-null_mod$edf))))
## This shows that the null model is insignificant
mod <- stepAIC(multi_mod2) ; rm(mod)
multi_mod2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                       data = Obesity_train) ; summary(multi_mod2)
multi_mod3 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex,
                       data = Obesity_train) ; summary(multi_mod3) # Best Model
multi_mod4 <- multinom(BMIgroup ~ AgeGroup + Sex, data = Obesity_train)
multi_mod5 <- multinom(BMIgroup ~ AgeGroup, data = Obesity_train)

anova(null_mod, multi_mod1)
# Checking the prediction values based solely on intercept, which will give
## prediction values of base categories:
predict(multi_mod2, data.frame(AgeGroup = "16-24",
                               Employment = "Doing something else",
                               Sex = "Female", Fruit = "No"), type="probs")
summary(multi_mod2)
Alpha <- c(0, 3.710500, 2.546042, 2.368471) 
## It can be done by following way:
exp(Alpha)/sum(exp(Alpha)) ; rm(Alpha)


# Slope term gives the log odds of moving from base category to next one which
## can be seen using following command:
Beta <- predict(multi_mod2, data.frame(AgeGroup = c("16-24", "25-34"),
                                       Employment = c("Else", "Else"), 
                                       Sex = c("Female", "Female"),
                                       Fruit = c("No", "No")), type="probs") ; Beta

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
CP <- table(Predictions$True_Values, Predictions$Multi_Mod3) ; CP
# Proportion Table
prop.table(CP, margin=1)
(CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity_test) ; rm(CP)
# Error rate
1- mean(Predictions$True_Values == Predictions$Multi_Mod1)
# Confusion Matrix
confusionMatrix(Predictions$True_Values, Predictions$Multi_Mod3)

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
randomForest::importance(rf_mod1) # Importance of variables
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
                      # Rf_Multi_Mod1_dum = predict(rf_mod1_dum, Obesity_test_dum),
                      Rf_Multi_Mod2 = predict(rf_mod2, Obesity_test),
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
Obesity_train$BMIgroup[Obesity_train$BMIgroup =="Underweight"] <- "Normal"
Obesity_test$BMIgroup[Obesity_test$BMIgroup =="Underweight"] <- "Normal"

Obesity_train_2 <- Obesity_train
Obesity_test_2 <- Obesity_test

Obesity_train_2$BMIgroup <- factor(x = Obesity_train_2$BMIgroup,
                                   levels = c("Normal", "Overweight", "Obese"))
Obesity_test_2$BMIgroup <- factor(x = Obesity_test_2$BMIgroup,
                                  levels = c("Normal", "Overweight", "Obese"))

multi_mod1.2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                         data = Obesity_train_2)

summary(multi_mod1.2)
mod <- stepAIC(multi_mod1.2); rm(mod)

multi_mod2.2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Veg,
                         data = Obesity_train_2)
multi_mod3.2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex,
                         data = Obesity_train_2)


# Predictions by Different Models
Predictions <- tibble(True_Values = Obesity_test_2$BMIgroup,
                      Multi_Mod1.2 = predict(multi_mod1.2, Obesity_test_2),
                      Multi_Mod2.2 = predict(multi_mod2.2, Obesity_test_2),
                      Multi_Mod3.2 = predict(multi_mod3.2, Obesity_test_2))


# Random_forest without Underweight ----
rf_mod1.2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                          data = Obesity_train_2, ntree = 300, mtry = 5)

rf_mod1.2 %>% vip(geom = "col") # Imp plot
randomForest::importance(rf_mod1.2) # Importance of variables
varUsed(rf_mod1.2)

rf_mod2.2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex + Veg,
                          data = Obesity_train_2, ntree = 300, mtry = 4)

# After removing Fruit and Veg as they have very low importance
rf_mod3.2 <- randomForest(BMIgroup ~ AgeGroup + Employment + Sex,
                          data = Obesity_train_2, ntree = 300, mtry = 3)
summary(rf_mod3.2)
plot(rf_mod1.2)
legend('topright', colnames(rf_mod1.2$err.rate), col=1:4, fill=1:4)

Predictions <- tibble(True_Values = Obesity_test_2$BMIgroup,
                      Rf_Multi_Mod1.2 = predict(rf_mod1.2, Obesity_test_2),
                      Rf_Multi_Mod2.2 = predict(rf_mod2.2, Obesity_test_2),
                      Rf_Multi_Mod3.2 = predict(rf_mod3.2, Obesity_test_2),)

confusionMatrix(Predictions$True_Values, Predictions$Rf_Multi_Mod3.2)
# Correct Classification rate
CP <- table(Predictions$True_Values, Predictions$Rf_Multi_Mod3.2) ; CP
# Proportion Table
prop.table(CP, margin=1)
confusionMatrix(Predictions$True_Values, Predictions$Multi_Mod2.2)

rm(multi_mod1, multi_mod2, multi_mod3, multi_mod4, multi_mod5, null_mod, rf_mod1, rf_mod2, rf_mod3)




Obesity$BMIgroup[Obesity$BMIgroup =="Underweight"] <- "Normal"
Obesity3 <- Obesity
Obesity3 <- Obesity %>% subset(BMIgroup != "Underweight")
Obesity3$BMIgroup <- factor(x = Obesity3$BMIgroup,
                                   levels = c("Normal", "Overweight", "Obese"))
table(Obesity3$BMIgroup)

set.seed(42)
Obesity2 <- Obesity[,-c(6,7,9)]
Obesity2 <- Obesity[,-c(4:7,9)]
Obesity2 <- Obesity3[,-c(6,7,9)]
Obesity2 <- Obesity[,-c(4:7,9)]

scaleFUN <- function(x) sprintf("%.f", x)
random_data <- dplyr::slice(Obesity2,sample(1:n()))
test_idx <- round(seq(1,nrow(Obesity2),by=nrow(Obesity2)/11))
accuracy_df <- data.frame(iteration= as.numeric(),accruacy_score= as.numeric())
for(i in 1:10){
  
  test_data <- slice(random_data,test_idx[i]:test_idx[i+1])
  train_data <- slice(random_data,-test_idx[i]:-test_idx[i+1])
  model_cv <- multinom(BMIgroup~.,data=train_data)
  predict_cv <- as.character(predict(model_cv,newdata=test_data %>% dplyr::select(-BMIgroup)))
  accuracy_score <- sum(as.character(predict_cv)==as.character(test_data$BMIgroup))/nrow(test_data)
  accuracy_df <- rbind(accuracy_df,data.frame(iteration=i,accuracy_score=accuracy_score))
}
accuracy_df %>%
  ggplot(aes(x=iteration,y=accuracy_score))+
  geom_line()+labs(x='Iteration Number',y='Accuracy Score',title='Accuracy Scores for each Iteration')+
  scale_x_continuous(labels = scaleFUN)+
  theme(plot.title = element_text(hjust=0.5))+
  geom_point()

accuracy_df %>% write.csv("C:/Users/chusa/Desktop/file.csv")
table(Obesity_train$BMIgroup)




coeff_dt <- data.frame(summary(multi_mod3.2)$coeff, row.names = NULL)
classes <- c("Overweight", "Obese")
coeff_dt_1 <- coeff_dt %>% mutate(classes = classes) %>%
  gather(variable,increase_in_log_odds,2:ncol(coeff_dt))

coeff_dt_1$variable[coeff_dt_1$variable == "AgeGroup25.34"] <- "24-35"
coeff_dt_1$variable[coeff_dt_1$variable == "AgeGroup35.44"] <- "34-45"
coeff_dt_1$variable[coeff_dt_1$variable == "AgeGroup45.54"] <- "44-55"
coeff_dt_1$variable[coeff_dt_1$variable == "AgeGroup55.64"] <- "54-65"
coeff_dt_1$variable[coeff_dt_1$variable == "AgeGroup65.74"] <- "64-75"
coeff_dt_1$variable[coeff_dt_1$variable == "AgeGroup75."] <- "75+"
coeff_dt_1$variable[coeff_dt_1$variable == "EmploymentFT.Edu"] <- "FT Edu"
coeff_dt_1$variable[coeff_dt_1$variable == "EmploymentEmployed"] <- "Employed"
coeff_dt_1$variable[coeff_dt_1$variable == "EmploymentHomemaking"] <- "Homemaking"
coeff_dt_1$variable[coeff_dt_1$variable == "EmploymentJob.Seeking"] <- "Job Seeking"
coeff_dt_1$variable[coeff_dt_1$variable == "EmploymentUnemployable"] <- "Unemployable"
coeff_dt_1$variable[coeff_dt_1$variable == "EmploymentRetd"] <- "Retd"
coeff_dt_1$variable[coeff_dt_1$variable == "SexMale"] <- "Male"

coeff_dt_1 <- coeff_dt_1 %>% mutate(increase_in_odds = exp(coeff_dt_1$increase_in_log_odds))

coeff_dt_1 %>%
  dplyr::select(classes,variable,increase_in_log_odds) %>%
  mutate(pos = ifelse(increase_in_log_odds>0,'Positive','Negative')) %>%
  ggplot(aes(x=variable,y=increase_in_log_odds,fill=pos))+
  geom_bar(stat='identity',width=0.6,position = position_dodge(width=0.5))+
  facet_wrap(~classes,nrow=5)+
  theme(axis.text.x=element_text(size=12,angle = 0),axis.text.y = element_blank(),panel.background = element_blank(),axis.ticks.y = element_blank(),
        legend.title = element_blank())+
  labs(x=element_blank(),y='Increase in Log Odds', )





















