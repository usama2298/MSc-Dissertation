# Exploratory Analysis ----

ggpairs(Obesity_train)

Obesity_train %>% group_by(AgeGroup) %>% 
  summarise(Var = var(BMI), Mean = mean(BMI), Min = min(BMI), Max = max(BMI))
Obesity_train %>% group_by(Employment) %>% 
  summarise(Var = var(BMI), Mean = mean(BMI), Min = min(BMI), Max = max(BMI))
Obesity_train %>% group_by(Sex) %>% 
  summarise(Var = var(BMI), Mean = mean(BMI), Min = min(BMI), Max = max(BMI))
## It can be seen in these three table that female class has much higher difference in
## variance as compared to males, it is because the more high values for BMI goes into
## this class. same for the Employment variable where the variances are high for classes
## Homemaking, unemployable and job-seeking

plot(BMI ~ Employment, Obesity)
t.test(BMI ~ Fruit, Obesity, var.equal=T)
t.test(BMI ~ Veg, Obesity, var.equal=T)
t.test(BMI ~ Sex, Obesity, var.equal=T)

ggplot(Obesity, aes(x = BMI)) +
  geom_histogram(binwidth = 1.5, mapping = aes(y=..density..),colour="black", fill="white") +
  geom_density(alpha=.15, fill="#657f14") + 
  geom_vline(aes(xintercept=mean(Obesity$BMI)), color = "red", linetype="dashed") +
  annotate(geom="text", x=28.35, y=0.04, label="mean = 27.9",
           color="black", angle='90', size = 4) + facet_wrap( ~ AgeGroup)  #log transformation required

my_skim <- skim_with(base=sfl(Mean = mean, Median = median, Var = var, Min = min, Max = max),
                     factor=sfl(ordered=NULL),
                     numeric=sfl(hist = NULL, sd = NULL, p0 = NULL, p100 = NULL,
                                 p50 = NULL, mean = NULL, skim_variable = NULL))

Obesity %>% group_by(Year) %>% my_skim(BMI) %>% 
  select(-skim_variable,-skim_type) %>% 
  rename(`P-25` = numeric.p25, `P-75` = numeric.p75)

Var1 <- Obesity %>% group_by(Year) %>% my_skim(BMI) %>% 
  select(-skim_variable,-skim_type) %>%
  rename(`P-25` = numeric.p25, `P-75` = numeric.p75) %>%
  gather(key = Stats, value = Value, - c(Year))

ggplot(Var1, aes(x = Year, y = Value, group = Stats)) + geom_line()

# Full Linear Model ----
lm_mod1 <- lm(BMI ~ AgeGroup + Employment + Sex + Fruit + Veg,
                       data = Obesity_train); summary(lm_mod1)
autoplot(lm_mod1); rm(lm_mod1)
## qq-plot seems deviation from the tails, further the density plot of BMI shows that,
## log transformation is required

lm_mod1.1 <- lm(log(BMI) ~ AgeGroup + Employment + Sex + Fruit + Veg + Year,
                       data = Obesity_train) ; summary(lm_mod1.1)
get_regression_table(lm_mod1.1)  %>%  write.csv("C:/Users/chusa/Desktop/file4.csv")
#R square increased but there are few insignificant variables which need to be doped

## Model Selection ----

# Comparison with null model
null_mod <- lm(log(BMI) ~ 1, data = Obesity_train) ; summary(null_mod)
anova(null_mod, lm_mod1.1) ; rm(null_mod)


# Checking the significance of variables by drooping variables one by one
drop1(lm_mod1.1,test="F")
drop1(stats::update(lm_mod1.1, ~ . -Year -Veg - Fruit), test = "F")
stats::step(lm_mod1.1, direction = "backward")
summary(stats::step(lm_mod1.1, direction = "both"))

ols_step_both_aic(lm_mod1.1)
stats::step(lm_mod1.1)
ols_step_best_subset(lm_mod1.1)
## According to the analysis of F test and analysis of variance and AIC, Veg and Fruit
## come out to be insignificant so we will proceed without these variables.

# Selected Model
lm_mod1.2 <- lm(log(BMI) ~ AgeGroup + Employment + Sex, data = Obesity_train)
summary(lm_mod1.2)
plot(lm_mod1.2$residuals ~ Obesity_train$Sex)

get_regression_table(lm_mod1.2)

# Residual Plots
ggplot(mapping = aes(x = lm_mod1.2$residuals)) +
  geom_histogram(bins = 20, mapping = aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.15, fill="#657f14") +
  xlab(label = "Residuals") + labs(title = "Residual Distribution") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))
plot(lm_mod1.2)

autoplot(lm_mod1.2, smooth.colour = "#ed7d31", colour = "#446ed8",
         which = 6, nrow = 1, alpha = 0.5) +
  #labs(title = NULL)+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))


## Because the data are integers and the fitted values happen to be integers also, some
## discreteness is obvious in the Q–Q plot. Of course, discrete data cannot be normally
## distributed. However, here the residuals are approximately normal and so we can go
## ahead with the inference without much concern.

## Diagnostics ----

TukeyHSD(aov(BMI ~ AgeGroup + Employment + Sex, data = Obesity))

rmse(actual = Obesity_test$BMI, predicted = exp(predict(lm_mod1.2, Obesity_test)))
mae(actual = Obesity_test$BMI, predicted = exp(predict(lm_mod1.2, Obesity_test)))
mse(actual = Obesity_test$BMI, predicted = exp(predict(lm_mod1.2, Obesity_test)))
#MSE penalizes less heavily as compared to MAE, so MSE is a better option

## Min_Max_Accuracy ----
Prediction <- as_tibble(exp(predict(lm_mod1.2, Obesity_test, interval = "confidence"))) %>%
  mutate(Observed = Obesity_test$BMI) %>% rename(Predicted = fit) %>% relocate(Observed)

my_skim <- skim_with(base=sfl(Var = var, Min = min, Mean = mean, Median = median, Max = max),
                     numeric=sfl(hist = NULL, sd = NULL, p0 = NULL, p100 = NULL,
                                 p50 = NULL, mean = NULL, skim_variable = NULL))
Prediction %>% select(Observed, Predicted) %>% my_skim() %>% 
  write.csv("C:/Users/chusa/Desktop/file4.csv")

c(quantile(Prediction$Observed, 0.1), quantile(Prediction$Observed, 0.9))
c(quantile(Prediction$Predicted, 0.1), quantile(Prediction$Predicted, 0.9))

p1 <- ggplot(Prediction, aes(x = Observed)) +
  geom_histogram(bins = 19, mapping = aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.15, fill="#657f14") + 
  geom_vline(aes(xintercept=quantile(Observed, 0.25)), color = "#ed7d31", linetype="dashed") +
  annotate(geom="text", x=25, y=0.05, label="P25 = 23.99",
           color="black", angle='90', size = 3.5) + 
  geom_vline(aes(xintercept=mean(Observed)), color = "#ed7d31", linetype="dashed") +
  annotate(geom="text", x=28.35, y=0.035, label="mean = 27.9",
           color="black", angle='90', size = 3.5) + 
  geom_vline(aes(xintercept=quantile(Observed, 0.75)), color = "#ed7d31", linetype="dashed") +
  annotate(geom="text", x=31.36, y=0.02, label="P75 = 30.91",
           color="black", angle='90', size = 3.5) +
   annotate(geom="text", x=49, y=0.065, label="Min = 14.06", color="#ed7d31", size = 4) + 
   annotate(geom="text", x=49, y=0.059, label="Max = 55.45", color="#ed7d31", size = 4) + 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))
p2 <- ggplot(Prediction, mapping = aes(x = Predicted)) +
  geom_histogram(bins = 20, mapping = aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.15, fill="#657f14") + 
  geom_vline(aes(xintercept=quantile(Predicted, 0.25)), color = "#ed7d31", linetype="dashed") +
  annotate(geom="text", x=26.5, y=0.15, label="P25 = 26.99",
           color="black", angle='90', size = 3.5) + 
  geom_vline(aes(xintercept=mean(Predicted)), color = "#ed7d31", linetype="dashed") +
  annotate(geom="text", x=27.35, y=0.15, label="mean = 27.41",
           color="black", angle='90', size = 3.5) + 
  geom_vline(aes(xintercept=quantile(Predicted, 0.75)), color = "#ed7d31", linetype="dashed") +
  annotate(geom="text", x=28.19, y=0.15, label="P75 = 28.13",
           color="black", angle='90', size = 3.5) +
   annotate(geom="text", x=25, y=0.6, label="Min = 23.84", color="#ed7d31", size = 4) + 
   annotate(geom="text", x=25, y=0.55, label="Max = 29.49", color="#ed7d31", size = 4) + 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        axis.title.y = element_blank())
p3 <- ggplot(Prediction, aes(y = Observed, x = Predicted)) +
  geom_jitter(colour="#446ed8", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#ed7d31") + 
  annotate(geom="text", x=25.5, y=48, label="Corr = 0.18", color="#ed7d31", size = 5) +
  labs(title = "Observed vs Predicted BMI") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))
p3
grid.arrange(p1, p2, p3, nrow = 1, top = "Distribution of Observed and Predicted Values")
cor(Prediction$Observed, Prediction$Predicted)

coeff_dt <- data.frame(summary(lm_mod1.2)$coeff) %>% 
  rownames_to_column(var = "Variable") %>% select(Variable, Estimate) %>%
  slice(-1) %>% mutate(Estimate = exp(Estimate))
coeff_dt$Variable[coeff_dt$Variable == "AgeGroup25-34"] <- "25-34"
coeff_dt$Variable[coeff_dt$Variable == "AgeGroup35-44"] <- "35-44"
coeff_dt$Variable[coeff_dt$Variable == "AgeGroup45-54"] <- "45-54"
coeff_dt$Variable[coeff_dt$Variable == "AgeGroup55-64"] <- "55-64"
coeff_dt$Variable[coeff_dt$Variable == "AgeGroup65-74"] <- "65-74"
coeff_dt$Variable[coeff_dt$Variable == "AgeGroup75+"] <- "75+"
coeff_dt$Variable[coeff_dt$Variable == "EmploymentFT Edu"] <- "FT Edu"
coeff_dt$Variable[coeff_dt$Variable == "EmploymentHomemaking"] <- "Homemaking"
coeff_dt$Variable[coeff_dt$Variable == "EmploymentJob Seeking"] <- "Job Seeking"
coeff_dt$Variable[coeff_dt$Variable == "EmploymentUnemployable"] <- "Unemployable"
coeff_dt$Variable[coeff_dt$Variable == "EmploymentRetd"] <- "Retd"
coeff_dt$Variable[coeff_dt$Variable == "SexMale"] <- "Male"

coeff_dt %>% ggplot(aes(x=Variable,y=Estimate))+
  geom_bar(stat='identity',width=0.6,position = position_dodge(width=0.5)) +
             theme(axis.text.x=element_text(size=12,angle = 90))



# dummy variables
Obesity_train_d <- Obesity_train %>% dplyr::select(-c(BMIgroup, Obese, Year)) %>%
  recipe(BMI ~ .) %>% step_dummy(all_nominal()) %>% prep() %>% bake(Obesity_train)


ggplot(mapping = aes(x = lm_mod1.2$residuals))+ geom_density()

