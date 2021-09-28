#Multinomial Analysis by putting Underweight as Normal.
Obesity2$BMIgroup <- ifelse(Obesity2$BMIgroup == "Underweight", "Normal",Obesity2$BMIgroup)
Obesity2$AgeGroup <- as.factor(Obesity2$AgeGroup)
Obesity2$Sex <- as.factor(Obesity2$Sex)
Obesity2$Employment <- as.factor(Obesity2$Employment)
Obesity2$Veg <- as.factor(Obesity2$Veg)
Obesity2$Fruit <- as.factor(Obesity2$Fruit)
Obesity2$BMIgroup <- as.factor(Obesity2$BMIgroup)
Obesity2$Obese <- as.factor(if_else(Obesity2$BMIgroup == "Obese", "Yes", "No"))

multi_mod1_1 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg, data = Obesity2)
mod <- step(multi_mod1_1)
multi_mod2_2 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit, data = Obesity2)
multi_mod3_3 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex, data = Obesity2)
multi_mod4_4 <- multinom(BMIgroup ~ AgeGroup + Sex, data = Obesity2)
multi_mod5_5 <- multinom(BMIgroup ~ AgeGroup, data = Obesity2)

# Predictions by Different Models
Predictions2 <- data.frame(True_Values = Obesity2$BMIgroup, Multi_Mod1_1 = predict(multi_mod3_3),
                           Multi_Mod2_2 = predict(multi_mod2_2), Multi_Mod3_3 = predict(multi_mod3_3),
                           Multi_Mod4_4 = predict(multi_mod4_4), Multi_Mod5_5 = predict(multi_mod5_5))

# Correct Classification rate
CP <- xtabs( ~ Predictions2$True_Values + Predictions2$Multi_Mod2_2)
(CP[1,1]+CP[2,2]+CP[3,3]+CP[4,4])/nrow(Obesity2)

rm(Obesity2, multi_mod1_1, mod, multi_mod2_2, multi_mod3_3, multi_mod4_4, multi_mod5_5, Predictions2)




Obesity2 <- Obesity
levels(Obesity$AgeGroup)
Obesity2$agelin <- 0
Obesity2$agelin[Obesity2$AgeGroup=="25-34"] <- 1
Obesity2$agelin[Obesity2$AgeGroup=="35-44"] <- 2
Obesity2$agelin[Obesity2$AgeGroup=="45-54"] <- 3
Obesity2$agelin[Obesity2$AgeGroup=="55-64"] <- 4
Obesity2$agelin[Obesity2$AgeGroup=="75+"] <- 5

multi_mod1_1 <- multinom(BMIgroup ~ agelin + Employment + Sex + Fruit + Veg, data = Obesity2)
mod <- step(multi_mod1_1)
multi_mod2_2 <- multinom(BMIgroup ~ agelin + Employment + Sex + Fruit, data = Obesity2)
multi_mod3_3 <- multinom(BMIgroup ~ agelin + Employment + Sex, data = Obesity2)
multi_mod4_4 <- multinom(BMIgroup ~ agelin + Sex, data = Obesity2)
multi_mod5_5 <- multinom(BMIgroup ~ agelin, data = Obesity2)

# Predictions by Different Models
Predictions2 <- data.frame(True_Values = Obesity2$BMIgroup, Multi_Mod1_1 = predict(multi_mod3_3),
                           Multi_Mod2_2 = predict(multi_mod2_2), Multi_Mod3_3 = predict(multi_mod3_3),
                           Multi_Mod4_4 = predict(multi_mod4_4), Multi_Mod5_5 = predict(multi_mod5_5))













