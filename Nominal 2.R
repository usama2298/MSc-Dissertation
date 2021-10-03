
Obesity_num <- Obesity

# Multinomial Analysis by putting Underweight as Normal
Obesity_num$BMIgroup <- ifelse(Obesity_num$BMIgroup == "Underweight", "Normal", Obesity_num$BMIgroup)
Obesity_num$AgeGroup <- as.factor(Obesity_num$AgeGroup)
Obesity_num$Sex <- as.factor(Obesity_num$Sex)
Obesity_num$Employment <- as.factor(Obesity_num$Employment)
Obesity_num$Veg <- as.factor(Obesity_num$Veg)
Obesity_num$Fruit <- as.factor(Obesity_num$Fruit)
Obesity_num$BMIgroup <- as.factor(Obesity_num$BMIgroup)
Obesity_num$Obese <- as.factor(if_else(Obesity_num$BMIgroup == "Obese", "Yes", "No"))

multi_mod1_1 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg, data = Obesity_num)
mod <- step(multi_mod1_1) ; rm(mod)
multi_mod2_2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit, data = Obesity_num)
multi_mod3_3 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex, data = Obesity_num)
multi_mod4_4 <- multinom(BMIgroup ~ AgeGroup + Sex, data = Obesity_num)
multi_mod5_5 <- multinom(BMIgroup ~ AgeGroup, data = Obesity_num)

# Predictions by Different Models
Predictions2 <- data.frame(True_Values = Obesity_num$BMIgroup, Multi_Mod1_1 = predict(multi_mod3_3),
                           Multi_Mod2_2 = predict(multi_mod2_2), Multi_Mod3_3 = predict(multi_mod3_3),
                           Multi_Mod4_4 = predict(multi_mod4_4), Multi_Mod5_5 = predict(multi_mod5_5))

# Correct Classification rate
CP2 <- xtabs( ~ Predictions2$True_Values + Predictions2$Multi_Mod2_2) ; CP2
(CP2[1,1]+CP2[2,2]+CP2[3,3]+CP2[4,4])/nrow(Obesity_num) ; rm(CP2)

rm(Obesity_num, multi_mod1_1, mod, multi_mod2_2, multi_mod3_3, multi_mod4_4, multi_mod5_5, Predictions2)





train.data <- data.frame(Y = as.character(Obesity_train$BMIgroup))
train.data$X1 <- as.character(Obesity_train$AgeGroup)
train.data$X1 <- ifelse(train.data$X1 == "16-24", 1, train.data$X1)
train.data$X1 <- ifelse(train.data$X1 == "25-34", 2, train.data$X1)
train.data$X1 <- ifelse(train.data$X1 == "35-44", 3, train.data$X1)
train.data$X1 <- ifelse(train.data$X1 == "45-54", 4, train.data$X1)
train.data$X1 <- ifelse(train.data$X1 == "55-64", 5, train.data$X1)
train.data$X1 <- ifelse(train.data$X1 == "65-74", 6, train.data$X1)
train.data$X1 <- ifelse(train.data$X1 == "75+", 7, train.data$X1)

train.data$X2 <- as.character(Obesity_train$Employment)
train.data$X2 <- ifelse(train.data$X2 == "Doing something else", 11, train.data$X2)
train.data$X2 <- ifelse(train.data$X2 == "In full-time education",12, train.data$X2)
train.data$X2 <- ifelse(train.data$X2 == "In paid employment, self-employed or on gov't training", 13, train.data$X2)
train.data$X2 <- ifelse(train.data$X2 == "Looking after home/family", 14, train.data$X2)
train.data$X2 <- ifelse(train.data$X2 == "Looking for/intending to look for paid work", 15, train.data$X2)
train.data$X2 <- ifelse(train.data$X2 == "Perm unable to work", 16, train.data$X2)
train.data$X2 <- ifelse(train.data$X2 == "Retired", 17, train.data$X2)

str(train.data)


test.data <- data.frame(Y = as.character(Obesity_test$BMIgroup))
test.data$X1 <- as.character(Obesity_test$AgeGroup)
test.data$X1 <- ifelse(test.data$X1 == "16-24", 1, test.data$X1)
test.data$X1 <- ifelse(test.data$X1 == "25-34", 2, test.data$X1)
test.data$X1 <- ifelse(test.data$X1 == "35-44", 3, test.data$X1)
test.data$X1 <- ifelse(test.data$X1 == "45-54", 4, test.data$X1)
test.data$X1 <- ifelse(test.data$X1 == "55-64", 5, test.data$X1)
test.data$X1 <- ifelse(test.data$X1 == "65-74", 6, test.data$X1)
test.data$X1 <- ifelse(test.data$X1 == "75+", 7, test.data$X1)

test.data$X2 <- as.character(Obesity_test$Employment)
test.data$X2 <- ifelse(test.data$X2 == "Doing something else", 11, test.data$X2)
test.data$X2 <- ifelse(test.data$X2 == "In full-time education", 12, test.data$X2)
test.data$X2 <- ifelse(test.data$X2 == "In paid employment, self-employed or on gov't training", 13, test.data$X2)
test.data$X2 <- ifelse(test.data$X2 == "Looking after home/family", 14, test.data$X2)
test.data$X2 <- ifelse(test.data$X2 == "Looking for/intending to look for paid work", 15, test.data$X2)
test.data$X2 <- ifelse(test.data$X2 == "Perm unable to work", 16, test.data$X2)
test.data$X2 <- ifelse(test.data$X2 == "Retired", 17, test.data$X2)

train.data$Y <- as.factor(train.data$Y)
test.data$Y <- as.factor(test.data$Y)
train.data$X1 <- as.numeric(train.data$X1)
train.data$X2 <- as.numeric(train.data$X2)
test.data$X1 <- as.numeric(test.data$X1)
test.data$X2 <- as.numeric(test.data$X2)

rm(train.data, test.data)








