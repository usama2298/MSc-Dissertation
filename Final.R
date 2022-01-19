# Libraries ----
library(tidyverse); library(skimr); library(gmodels); library(ggfortify)
library(moderndive); library(Metrics); library(gridExtra); library(nnet)
library(caret); library(recipes); library(themis)

# Preparing Data ----
Obesity <- read_csv("Obesity.csv")

## Converting Variables into factor
Obesity$AgeGroup <- as.factor(Obesity$AgeGroup)
Obesity$Sex <- as.factor(Obesity$Sex)
Obesity$Employment <- as.factor(Obesity$Employment)
Obesity$Veg <- as.factor(Obesity$Veg)
Obesity$Fruit <- as.factor(Obesity$Fruit)
Obesity$BMIgroup <- as.factor(Obesity$BMIgroup)
Obesity$Year <- as.factor(Obesity$Year)

## Generating new variable Obese as indicated in Project Guideline
Obesity$Obese <- as.factor(if_else(Obesity$BMIgroup == "Obese", "Yes", "No"))

## Renaming Employment levels with short forms

### Else         = "Doing something else"                                  
### FT Edu       = "In full-time education"                                
### Employed     = "In paid employment, self-employed or on govt training"
### Homemaking   = "Looking after home/family"                             
### Job Seeking  = "Looking for/intending to look for paid work"           
### Unemployable = "Perm unable to work"                                   
### Retd         = "Retired"
levels(Obesity$Employment)
levels(Obesity$Employment) <- c("Else", "FT Edu", "Employed", "Homemaking",
                                "Job Seeking", "Unemployable", "Retd")

## Setting Full Time Education as reference category
Obesity$Employment <- relevel(Obesity$Employment, ref = "FT Edu")

## Ordering BMIgroup variable levels
levels(Obesity$BMIgroup)
Obesity$BMIgroup <- factor(x = Obesity$BMIgroup,
                           levels = c("Underweight", "Normal", "Overweight",
                                      "Obese"))

## Spiting data into "Train" and "Test" set to avoid over fitting
Split <- sample(x = c(TRUE, FALSE), size = nrow(Obesity), replace = TRUE,
                prob = c(0.75, 0.25))
Obesity_train <- Obesity[Split, ]
Obesity_test <- Obesity[!Split, ]

## Checking the counts of the variable classes in every data set
apply(Obesity[ ,-c(6,7)], 2, table)
apply(Obesity_train[ ,-c(6,7)], 2, table)
apply(Obesity_test[ ,-c(6,7)], 2, table)

## Writing files for future use ----
write_rds(Obesity, "Obesity.RData")
write_rds(Obesity_train, "Obesity_train.RData")
write_rds(Obesity_test, "Obesity_test.RData")
write_rds(Split, "Split.RData")


# Reading Train and Test data ----
Obesity <- read_rds("Obesity.RData")
Obesity_train <- read_rds("Obesity_train.RData")
Obesity_test <- read_rds("Obesity_test.RData")


# Exploratory Analysis ----
skim(Obesity) # Descriptive Statistics of Data

# BMI continuous Response
## Descriptive plot of BMI distribution
ggplot(Obesity, aes(x = BMI)) +
  geom_histogram(binwidth = 1.5, mapping = aes(y=..density..), colour="black",
                 fill="white") + geom_density(alpha=.15, fill="grey") + 
  geom_vline(aes(xintercept=mean(Obesity$BMI)), color = "#3F6EC3",
             linetype="dashed") + ylab("Density") +
  annotate(geom="text", x=28.35, y=0.04, label="mean = 27.9",
           color="black", angle='90', size = 4) +
  geom_vline(aes(xintercept=quantile(Obesity$BMI, 0.75)),
             color = "#657F14", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(Obesity$BMI, 0.25)),
             color = "#EC792B", linetype="dashed") + ylab("Density") +
  theme(panel.background = element_rect(fill = "transparent"))
## plot shows skewness in the data, log transformation possibly needed

# BMIgroup Nominal Response
table(Obesity$BMIgroup)
prop.table(table(Obesity$BMIgroup))
## There is issue of class balance, as the class Underweight is just 1.2%

# Obese Binary response (generated from BMIgroup)
table(Obesity$Obese)
prop.table(table(Obesity$Obese))
## class balance can be seen here are also, as 70% are "No"


## BMI vs All Predictors ----
my_skim <- skim_with(base=sfl(Mean = mean, Median = median, Var = var,
                              Min = min, Max = max), factor=sfl(ordered=NULL),
                     numeric=sfl(hist = NULL, sd = NULL, p0 = NULL, p100 = NULL,
                                 p50 = NULL, mean = NULL, skim_variable = NULL))

### AgeGroup vs BMI ----
age_stats <- Obesity %>% group_by(AgeGroup) %>% my_skim(BMI) ; age_stats
# line plot of Age Group by BMI Stats 
age_stats %>% select(-c(skim_variable,skim_type, Var, Min, Max)) %>%
  rename(`P-25` = numeric.p25, `P-75` = numeric.p75) %>% 
  gather(key = Stats, value = Value, - c(AgeGroup)) %>%
  ggplot(aes(x = AgeGroup, y = Value, group = Stats, color=Stats)) + geom_line()
# Plot shows increasing trend as we move from AgeGroup 16-24 to 65-74
# it decreases for 75+ throughout all the quantiles and means


### Employment vs BMI ----
emp_stats <- Obesity %>% group_by(Employment) %>% my_skim(BMI) ; emp_stats
# line plot of Employment by BMI Stats
emp_stats %>% dplyr::select(-c(skim_variable,skim_type, Var, Min, Max)) %>%
  rename(`P-25` = numeric.p25, `P-75` = numeric.p75) %>% 
  gather(key = Stats, value = Value, - c(Employment)) %>%
  ggplot(aes(x = Employment, y = Value, group = Stats, color=Stats))+geom_line()
# The relationship between BMI and Employment shows different trends in first
# and third quantiles

### Sex vs BMI ----
sex_stats <- Obesity %>% group_by(Sex) %>% my_skim(BMI) ; sex_stats
# line plot of Sex by BMI Stats
sex_stats %>% dplyr::select(-c(skim_variable,skim_type, Var, Min, Max)) %>%
  rename(`P-25` = numeric.p25, `P-75` = numeric.p75) %>% 
  gather(key = Stats, value = Value, - c(Sex)) %>%
  ggplot(aes(x = Sex, y = Value, group = Stats, color = Stats)) + geom_line()
# the spread of BMI is high in males because of high variance
# Mean almost same in both groups
t.test(BMI ~ Sex, Obesity, var.equal=T) # t-test of mean comparison
# accepting H0 stating means of both groups are same

### Fruit vs BMI ----
frt_stats <- Obesity %>% group_by(Fruit) %>% my_skim(BMI) ; frt_stats
# line plot of Fruit by BMI Stats
frt_stats %>% dplyr::select(-c(skim_variable,skim_type, Var, Min, Max)) %>%
  rename(`P-25` = numeric.p25, `P-75` = numeric.p75) %>% 
  gather(key = Stats, value = Value, - c(Fruit)) %>%
  ggplot(aes(x = Fruit, y = Value, group = Stats, color = Stats)) + geom_line()
# There is very small difference in both groups
t.test(BMI ~ Fruit, Obesity, var.equal=T) # t-test of mean comparison
# rejecting H0 stating that means are different

### Veg vs BMI ----
veg_stats <- Obesity %>% group_by(Veg) %>% my_skim(BMI) ; veg_stats
# line plot of Veg by BMI Stats
veg_stats %>% dplyr::select(-c(skim_variable,skim_type, Var, Min, Max)) %>%
  rename(`P-25` = numeric.p25, `P-75` = numeric.p75) %>% 
  gather(key = Stats, value = Value, - c(Veg)) %>%
  ggplot(aes(x = Veg, y = Value, group = Stats, color = Stats)) + geom_line()
# There is very small difference in both groups
t.test(BMI ~ Veg, Obesity, var.equal=T) # t-test of mean comparison
# accepting H0 stating means of both groups are same

### Year vs BMI ----
year_stats <- Obesity %>% group_by(Year) %>% my_skim(BMI) ; year_stats
# line plot of Year by BMI Stats
year_stats %>% dplyr::select(-c(skim_variable,skim_type, Var, Min, Max)) %>%
  rename(`P-25` = numeric.p25, `P-75` = numeric.p75) %>% 
  gather(key = Stats, value = Value, - c(Year)) %>%
  ggplot(aes(x = Year, y = Value, group = Stats, color = Stats)) + geom_line()
# Trend of year seems to be same across the quantiles, means and medians

## removing unnecessary functions and data frames to same memory
rm(age_stats, emp_stats, frt_stats, sex_stats, veg_stats, year_stats, my_skim)




## BMIgroup vs All predictors ----

### BMIgroup vs AgeGroup ----

# Prop table of BMIgroup vs AgeGroup
CrossTable(Obesity$AgeGroup, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
## Stacked bar plot
Obesity %>% group_by(AgeGroup, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(AgeGroup) %>% mutate(total=sum(count), proportion=count/total) %>%
  ggplot(aes(fill=BMIgroup, x=proportion, y=AgeGroup)) + 
  geom_bar(position="fill", stat="identity", width = 0.5) + 
  scale_fill_manual(values = c("#4472c4","#ed7d31", "#ffc000", "#657f14"))+
  theme(panel.background = element_blank())
## the percentage of Obese and Overweight people increases as we move from the
## age category 16-24 to 65-74

### BMIgroup vs Employment ----
# Prop table of BMIgroup vs Employment
CrossTable(Obesity$Employment, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
## Stacked bar plot
Obesity %>% group_by(Employment, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Employment) %>% mutate(total=sum(count), proportion=count/total) %>%
  ggplot(aes(fill=BMIgroup, x=proportion, y=Employment)) + 
  geom_bar(position="fill", stat="identity", width = 0.5) + 
  scale_fill_manual(values = c("#4472c4","#ed7d31", "#ffc000", "#657f14"))+
  theme(panel.background = element_blank())
## plot shows proportion of BMIgroup varies for different BMIgroups

### BMIgroup vs Sex ----
# Prop table of BMIgroup vs Sex
CrossTable(Obesity$Sex, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
## Stacked bar plot
Obesity %>% group_by(Sex, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Sex) %>% mutate(total=sum(count), proportion=count/total) %>%
  ggplot(aes(fill=Sex, y=proportion, x=BMIgroup)) + 
  geom_bar(position="fill", stat="identity", width = 0.4) + 
  scale_fill_manual(values = c("#4472c4","#ed7d31"))+
  theme(panel.background = element_blank())
## More females are Underweight and Normal, but the proportion of overweight
## males is high while in class obese both stand at the same position

### BMIgroup vs Fruit and Veg ----
# Prop table of BMIgroup vs Fruit
CrossTable(Obesity$Fruit, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
## Stacked bar plot
Obesity %>% group_by(Fruit, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Fruit) %>% mutate(total=sum(count), proportion=count/total) %>%
  ggplot(aes(fill=Fruit, y=proportion, x=BMIgroup)) + 
  geom_bar(position="fill", stat="identity", width = 0.4) + 
  scale_fill_manual(values = c("#4472c4","#ed7d31"))+
  theme(panel.background = element_blank())

# Prop table of BMIgroup vs Veg
CrossTable(Obesity$Veg, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
## Stacked bar plot
Obesity %>% group_by(Veg, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Veg) %>% mutate(total=sum(count), proportion=count/total) %>%
  ggplot(aes(fill=Veg, y=proportion, x=BMIgroup)) + 
  geom_bar(position="fill", stat="identity", width = 0.4) + 
  scale_fill_manual(values = c("#4472c4","#ed7d31"))+
  theme(panel.background = element_blank())
## The proportion of people consuming vegetables and fruit are approximately
## equal to those who don't eat except class Underweight. In class underweight
## the proportion of people who don't consume fruits and vegetables are high

### BMIgroup vs Year ----
# Prop table of BMIgroup vs Year
CrossTable(Obesity$Year, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
## Stacked bar plot
Obesity %>% group_by(Year, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Year) %>% mutate(total=sum(count), proportion=count/total) %>%
  ggplot(aes(fill=BMIgroup, x=proportion, y=Year)) + 
  geom_bar(position="fill", stat="identity", width = 0.5) + 
  scale_fill_manual(values = c("#4472c4","#ed7d31", "#ffc000", "#657f14"))+
  theme(panel.background = element_blank())
## The trend of all BMI categories is almost same across all years


# Trend of Obesity Over time ----
Prop_Year <- Obesity %>% group_by(Year, Obese) %>% summarise(count=n()) %>% 
  group_by(Year) %>% mutate(total=sum(count), proportion=count/total) %>%
  subset(Obese == "Yes")
# Line plot of year vs Obese (Yes)
Prop_Year %>%
  ggplot(mapping = aes(x=Year, y=proportion, group=Obese, color=Obese)) +
  geom_line(size = 0.5)
# Changes in proportion of Obese people is very small for all years

## Chi Square Test of Proportion Comparison ----
prop.test(Prop_Year$count, Prop_Year$total) ; rm(Prop_Year)
# A the p value comes out to be 0.89, we accept H0 stating that proportion of 
# Obese is same for all years


# Full Linear Model ----
lm_mod1 <- lm(BMI ~ AgeGroup + Employment + Sex + Fruit + Veg + Year,
              data = Obesity_train); summary(lm_mod1)
autoplot(lm_mod1); rm(lm_mod1)
## qq-plot shows deviation on upper tail, further the density plot of BMI 
## shows that, log transformation is required

lm_mod1.1 <- lm(log(BMI) ~ AgeGroup + Employment + Sex + Fruit + Veg + Year,
                data = Obesity_train) ; summary(lm_mod1.1)
get_regression_table(lm_mod1.1)
# R squared increased but there are few insignificant variables which need to be 
# dropped as the confidence intervals of Year, Veg and Fruit contains zero.

## Model Selection ----

# Comparison with null model
null_mod <- lm(log(BMI) ~ 1, data = Obesity_train) ; summary(null_mod)
anova(null_mod, lm_mod1.1) ; rm(null_mod)
## Good to proceed with full model for now as P < 0.05

# Step wise both AIC
stats::step(lm_mod1.1, direction = "both")
## step wise AIC resulted in dropping Year and Fruit

# Verifying results by step wise F test
drop1(lm_mod1.1,test="F")
## Year dropped as it came out to be most insignificant
drop1(stats::update(lm_mod1.1, ~ . -Year), test = "F")
## Veg came out to be most insignificant which was retained by AIC
drop1(stats::update(lm_mod1.1, ~ . -Year -Veg), test = "F")
## Fruit is also insignificant with p 0.89 this time
drop1(stats::update(lm_mod1.1, ~ . -Year -Veg - Fruit), test = "F")
## According to AIC, Year and Fruit came out to be insignificant
## Further Veg was dropped based on F test and p value

rmse(actual = Obesity_test$BMI, predicted = exp(predict(lm_mod1.1, Obesity_test)))
# RMSE comes out to be 5.36

rm(lm_mod1.1) # removing model, as no further use

## Selected Model ----
lm_mod1.2 <- lm(log(BMI) ~ AgeGroup + Employment + Sex, data = Obesity_train)
summary(lm_mod1.2)
get_regression_table(lm_mod1.2)
# Summary of the model shows improved adjusted R2 value and all the variables 
# are significant

plot(lm_mod1.2$residuals ~ Obesity_train$AgeGroup)
## plot between predictor and residuals shows the normality of residuals

rmse(actual = Obesity_test$BMI, predicted = exp(predict(lm_mod1.2, Obesity_test)))
# RMSE stays same for the selected Model

## Residual Plots ----
ggplot(mapping = aes(x = lm_mod1.2$residuals)) +
  geom_histogram(bins = 20, mapping = aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.15, fill="#657f14") +
  xlab(label = "Residuals") + labs(title = "Residual Distribution") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))
#Residual Histogram shows Normality of residuals


autoplot(lm_mod1.2, smooth.colour = "#ed7d31", colour = "#446ed8",
         which = 1:4, nrow = 2, ncol = 2, alpha = 0.5) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))
# Residuals have zero mean and constant variance.
# Because the data are integers and the fitted values happen to be integers also, some
# discreteness is obvious in the QQ plot. Of course, discrete data cannot be normally
# distributed. However, here the residuals are approximately normal and so we can go
# ahead with the inference without much concern.
# There is no point out of the Cook's Distance range


# Confidence Intervals of the predicted values
pred <- as_tibble(exp(predict(lm_mod1.2, Obesity_test,
                              interval="confidence", level=0.95)))

## Min_Max_Accuracy ----
Prediction <- 
  Obesity_test %>% mutate(Predicted = exp(predict(lm_mod1.2, Obesity_test))) %>%
  rename(Actual = BMI) %>% dplyr::select(Actual, Predicted)
Prediction %>% summary()
# The prediction range of the model is very small


## Plots of Predicted and Observed values ----
p1 <- ggplot(Prediction, aes(x = Actual)) +
  geom_histogram(bins = 19, mapping = aes(y=..density..), colour="black", 
                 fill="white") + geom_density(alpha=.15, fill="#657f14") + 
  geom_vline(aes(xintercept=quantile(Actual, 0.25)), color = "#ed7d31", 
             linetype="dashed") +
  annotate(geom="text", x=25, y=0.05, label="P25 = 23.99",
           color="black", angle='90', size = 3.5) +
  geom_vline(aes(xintercept=mean(Actual)), color = "#ed7d31", linetype="dashed") +
  annotate(geom="text", x=28.35, y=0.035, label="mean = 27.9",
           color="black", angle='90', size = 3.5) +
  geom_vline(aes(xintercept=quantile(Actual, 0.75)), color = "#ed7d31",
             linetype="dashed") +
  annotate(geom="text", x=31.36, y=0.02, label="P75 = 30.91",
           color="black", angle='90', size = 3.5) +
  annotate(geom="text", x=49, y=0.065, label="Min = 14.06", 
           color="#ed7d31", size = 4) +
  annotate(geom="text", x=49, y=0.059, label="Max = 55.45",
           color="#ed7d31", size = 4) +
  theme(panel.background = element_rect(fill = "transparent"))
p2 <- ggplot(Prediction, mapping = aes(x = Predicted)) +
  geom_histogram(bins = 20, mapping = aes(y=..density..), colour="black",
                 fill="white") +
  geom_density(alpha=.15, fill="#657f14") + 
  geom_vline(aes(xintercept=quantile(Predicted, 0.25)), color = "#ed7d31",
             linetype="dashed") +
  annotate(geom="text", x=26.5, y=0.15, label="P25 = 26.99",
           color="black", angle='90', size = 3.5) +
  geom_vline(aes(xintercept=mean(Predicted)), color = "#ed7d31",
             linetype="dashed") +
  annotate(geom="text", x=27.35, y=0.15, label="mean = 27.41",
           color="black", angle='90', size = 3.5) +
  geom_vline(aes(xintercept=quantile(Predicted, 0.75)), color = "#ed7d31",
             linetype="dashed") +
  annotate(geom="text", x=28.19, y=0.15, label="P75 = 28.13",
           color="black", angle='90', size = 3.5) +
  annotate(geom="text", x=25, y=0.6, label="Min = 23.84", color="#ed7d31",
           size = 4) +
  annotate(geom="text", x=25, y=0.55, label="Max = 29.49", color="#ed7d31",
           size = 4) +
  theme(panel.background = element_rect(fill = "transparent"))

gridExtra::grid.arrange(p1, p2, nrow = 1,
                        top = "Distribution of Actual and Predicted Values"); rm(p1,p2)
# Plots show that the distribution of both predicted and observed values a very
# different and range of predicted values are quite small and stays around the
# median of the BMI

## Scatter plot between predicted and observed values
ggplot(Prediction, aes(y = Actual, x = Predicted)) +
  geom_jitter(colour="#657f14", alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "#ed7d31") + 
  annotate(geom="text", x=25.5, y=53, label="Corr = 0.18", color="#ed7d31",
           size=4)+theme(panel.background = element_rect(fill = "transparent"))
cor(Prediction$Actual, Prediction$Predicted)
# correlation and the plot shows very weak positive relationship between. This 
# conclude the variation in response variable is not defined well. However, it
# can be concluded that AgeGroup, Employment and Sex have a significant effect
# on BMI.

rm(Prediction)


# Multinomial Regression ----

# Full Model Containing all predictors
multi_full_mod <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg 
                           + Year, data = Obesity_train)
summary(multi_full_mod)

## Model Selection ----
null_mod <- multinom(BMIgroup ~ 1, data=Obesity_train) ;  summary(null_mod)
anova(multi_full_mod, null_mod)
setequal((null_mod$deviance - multi_full_mod$deviance),
         (qchisq(p = 0.95,df = (multi_full_mod$edf-null_mod$edf))))
# This shows that the null model is insignificant, Hence proceed with full mod

stats::step(multi_full_mod)
## step wise AIC removes Year, Let's proceed without year
multi_selected <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg,
                           data = Obesity_train)
# Likelihood Ratio test of Full model vs Selected Model
anova(multi_full_mod, multi_selected)
## P value 0.93 indicates that selected model is significant

# AIC has a drawback in selecting a model with a higher number of predictors 
# where the sample size is large; therefore, we proceed with Likelihood Ratio 
# Statistic for model selection. We perform a step wise forward model comparison
# by adding variables one by one manually to find the best fit.
multi_mod2 <- multinom(BMIgroup ~ AgeGroup, data = Obesity_train)
multi_mod3 <- multinom(BMIgroup ~ AgeGroup + Sex, data = Obesity_train)
multi_mod4 <- multinom(BMIgroup ~ AgeGroup + Sex + Employment,
                       data = Obesity_train)
multi_mod5 <- multinom(BMIgroup ~ AgeGroup + Employment + Sex + Fruit,
                       data = Obesity_train)

# Comparison of all models
anova(multi_full_mod, multi_mod2, multi_mod3, multi_mod4, multi_mod5,
      multi_selected, null_mod)
## Result show that our selected Model is significant

# removing unnecessary models
rm(multi_full_mod, multi_mod2, multi_mod3, multi_mod4, multi_mod5, null_mod)



## Predictions ----
summary(multi_selected)

# Checking the prediction values based solely on intercept, which will give
# prediction values of base categories:
levels(Obesity$Employment)
predict(multi_selected, data.frame(AgeGroup = "16-24",
                                   Employment = "FT Edu",Sex = "Female", 
                                   Fruit = "No", Veg = "No"), type="probs")


# Slope term gives the log odds of moving from base category to next one which
# can be seen using following command: age 25-34 vs base category, keeping all
# other variables in their base category
predict(multi_selected, data.frame(AgeGroup = c("16-24", "25-34"),
                                   Employment = c("FT Edu", "FT Edu"), 
                                   Sex = c("Female", "Female"),
                                   Fruit = c("No", "No"),
                                   Veg = c("No", "No")), type="probs")

exp(confint(multi_selected))
## Confidence intervals are observed to see the significance of variables also

## Confusion Metrics ----

# Prediction on test set
Predictions <- tibble(True_Values = Obesity_test$BMIgroup,
                      Predicted = predict(multi_selected, Obesity_test))
confusionMatrix(Predictions$True_Values, Predictions$Predicted)
## This shows that the model has 43% accuracy while predicting on test set.
## Further no value is classified in Underweight class, which is possibly 
## because of imbalanced data 

## SMOTE Sampling ----

# Dummy variables are created while smote sampling
Obesity_train_samp <- recipe(BMIgroup ~ AgeGroup + Employment + Sex + Fruit +
                               Veg, data = Obesity_train) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_smote(BMIgroup, over_ratio = 0.5, neighbors = 2) %>% prep() %>% juice()
##sampling is done using knn parameter default 

# Dummy variables for test set for prediction
Obesity_test_dm <- recipe(BMIgroup ~ AgeGroup + Employment + Sex + Fruit + Veg, 
                          data = Obesity_test) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% prep() %>% juice()

# Selected Model was applied on sampled training data
multi_selected <- multinom(BMIgroup ~ . ,data = Obesity_train_samp)

# updated Confusion Matrix
confusionMatrix(Obesity_test$BMIgroup, predict(multi_selected, Obesity_test_dm))

# Conclusion ----

## The prediction accuracy is a little decreased but now underweight class
## values can be seen classified with correct classification rate of 31%.
## Overweight has the highest accuracy rate of 64%, followed by Normal and 
## Obese with an accuracy rate of 27% and 21%, respectively.
## We determine a significant difference in BMIgroup classification by age,
## gender, socioeconomic status. But the year variable is insignificant


