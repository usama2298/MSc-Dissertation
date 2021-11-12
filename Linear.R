# Libraries ----
library(olsrr) ; library(faraway) ; library(ggfortify)
library(dummies)
library(caTools); library(plotly); library(lattice); 

# Linear Model ----

ggpairs(Obesity_train)

plot(BMI ~ Employment, Obesity)
t.test(BMI ~ Fruit, Obesity, var.equal=T)
t.test(BMI ~ Veg, Obesity, var.equal=T)
t.test(BMI ~ Sex, Obesity, var.equal=T)

Anova_test <- aov(BMI ~ AgeGroup, data = Obesity)
Anova_test <- aov(BMI ~ Employment, data = Obesity)
summary(Anova_test) ; rm(Anova_test)


lm_mod1 <- lm(BMI ~ AgeGroup + Employment + Sex + Fruit + Veg,
                       data = Obesity_train); summary(lm_mod1)
plot(lm_mod1.1)

## Model Selection ----

Best_subset <- ols_step_best_subset(lm_mod1)

# Full Model
lm_mod1.1 <- lm(log(BMI) ~ AgeGroup + Employment + Sex + Fruit + Veg,
                       data = Obesity_train) ; summary(lm_mod1.1)

# Checking the significance of variables by doping variables one by one
drop1(lm_mod1.1,test="F")

# Finding Best Subset
Best_subset_log <- ols_step_best_subset(lm_mod1)
stepAIC(lm_mod1)

# Best Subset
null_mod <- lm(log(BMI) ~ 1, data = Obesity_train) ; summary(null_mod)
lm_mod1.2 <- lm(log(BMI) ~ AgeGroup + Employment + Sex,
                data = Obesity_train) ; summary(lm_mod1.2)
drop1(lm_mod1.2,test="F")

# Residual Plots
## Because the data are integers and the fitted values happen to be integers also, some
## discreteness is obvious in the Q–Q plot. Of course, discrete data cannot be normally
## distributed. However, here the residuals are approximately normal and so we can go
## ahead with the inference without much concern
autoplot(lm_mod1.2, which = 1:6)

# Anova Analysis
lm_mod1.2 <- lm(log(BMI) ~ AgeGroup + Employment + Sex,
                data = Obesity_train) ; summary(lm_mod1.2)

anova(lm_mod1.2)
anova(null_mod, lm_mod1.2)

Obesity_train %>% group_by(AgeGroup) %>% summarise(var = var(BMI))

# rm(null_mod, lm_mod1, lm_mod1.1, lm_mod1.2, lm_mod1.3)
autoplot(lm_mod1.2, which = 1:6)

plot(residuals(lm_mod1.2) ~ AgeGroup, Obesity_train, ylab="Residuals")

qqnorm(residuals(lm_mod1.2),main="")
qqline(residuals(lm_mod1.2))
plot(fitted(lm_mod1.2),residuals(lm_mod1.2),xlab="Fitted",ylab="Residuals")
abline(h=0)

TukeyHSD(aov(log(BMI) ~ AgeGroup + Employment + Sex, data = Obesity_train))


step(lm_mod1.1)

quantile(Obesity$BMI, 0.9)
Obesity_High_BMI <- subset(Obesity, BMI > quantile(Obesity$BMI, 0.9))

summary(Obesity$BMI)

# dummy variables

























