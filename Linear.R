# Libraries ----
library(olsrr)

# Linear Model ----

lm_mod1 <- lm(BMI ~ AgeGroup + Employment + Sex + Fruit + Veg,
                       data = Obesity_train); summary(lm_mod1)
plot(lm_mod1.1)

## Model Selection

Best_subset <- ols_step_best_subset(lm_mod1)

lm_mod1.1 <- lm(log(BMI) ~ AgeGroup + Employment + Sex + Fruit + Veg,
                       data = Obesity_train) ; summary(lm_mod1.1)


Best_subset_log <- ols_step_best_subset(lm_mod1)
stepAIC(lm_mod1)

lm_mod1.2 <- lm(log(BMI) ~ AgeGroup + Employment + Sex,
                data = Obesity_train) ; summary(lm_mod1.1)
