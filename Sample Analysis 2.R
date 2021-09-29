
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














