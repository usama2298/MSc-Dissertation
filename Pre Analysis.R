library(tidyverse); library(faraway); library(GGally); library(skimr)
library(ggthemes); library(sjPlot); library(class); library(gmodels)
library(ggridges)


# Reading Data
Obesity <- read_csv("Obesity.csv")


# Converting Variables into factor
Obesity$AgeGroup <- as.factor(Obesity$AgeGroup)
Obesity$Sex <- as.factor(Obesity$Sex)
Obesity$Employment <- as.factor(Obesity$Employment)
Obesity$Veg <- as.factor(Obesity$Veg)
Obesity$Fruit <- as.factor(Obesity$Fruit)
Obesity$BMIgroup <- as.factor(Obesity$BMIgroup)
Obesity$Obese <- as.factor(if_else(Obesity$BMIgroup == "Obese", "Yes", "No"))


# Ordering BMIgroup variable levels
Obesity$BMIgroup <- factor(x = Obesity$BMIgroup,
                           levels = c("Underweight", "Normal", "Overweight",
                                      "Obese"))
levels(Obesity$BMIgroup)


# Spiting data into "Train" and "Test" set to avoid over fitting
Split = sample(x = c(TRUE, FALSE), size = nrow(Obesity), replace = TRUE,
               prob = c(0.75, 0.25))
Obesity_train = Obesity[Split, ]
Obesity_test = Obesity[!Split, ]
write_rds(Obesity, "Obesity.RData")
write_rds(Obesity_train, "Obesity_train.RData")
write_rds(Obesity_test, "Obesity_test.RData")
write_rds(Split, "Split.RData")
rm(Obesity, Obesity_train, Obesity_test, Split) # Removing main data as not needed anymore


# Reading Train and Test data
# Split <- read_rds("Split.RData") # Train and Test indices, read if necessary
Obesity <- read_rds("Obesity.RData")
Obesity_train <- read_rds("Obesity_train.RData")
Obesity_test <- read_rds("Obesity_test.RData")


# Exploratory Analysis
my_skim <- skim_with(base=sfl(n=length,n_missing=n_missing),factor=sfl(ordered=NULL),
                     numeric=sfl(p0=NULL,p100=NULL,hist = NULL))
my_skim(Obesity_train); rm(my_skim)


CrossTable(Obesity_train$BMIgroup, Obesity_train$AgeGroup,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity_train$BMIgroup, Obesity_train$Sex,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity_train$Employment, Obesity_train$BMIgroup,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity_train$BMIgroup, Obesity_train$Veg,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity_train$BMIgroup, Obesity_train$Fruit,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity_train$BMIgroup, Obesity_train$Year,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)

table(Obesity$BMIgroup)
ggplot(Obesity_test, aes(x = AgeGroup, fill = BMIgroup)) + geom_bar()

aggregate(Obesity$BMI, by=list(Obesity$BMIgroup), FUN=var)
aggregate(Obesity_train$BMI, by=list(Obesity_train$BMIgroup), FUN=var)
aggregate(Obesity_test$BMI, by=list(Obesity_test$BMIgroup), FUN=var)

apply(Obesity[ ,-c(6,7)], 2, table)
apply(Obesity_train[ ,-c(6,7)], 2, table)
apply(Obesity_test[ ,-c(6,7)], 2, table)

ggpairs(Obesity_train, columns=2, ggplot2::aes(colour=BMIgroup), legend = T)

ggplot(Obesity_train, aes(x=BMI, y=Sex, fill=BMIgroup)) +
  geom_density_ridges(alpha=0.5) +
  theme_ridges()


# Proportion graphs
Prop_Emp <- group_by(Obesity, Employment, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Employment) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Emp, aes(x=Employment, y=proportion, group=BMIgroup,
                     linetype=BMIgroup, color=BMIgroup)) + geom_line()

Prop_Sex <- group_by(Obesity, Sex, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Sex) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Sex, aes(x=Sex, y=proportion, group=BMIgroup, linetype=BMIgroup,
                     color=BMIgroup)) + geom_line()

Prop_Veg <- group_by(Obesity, Veg, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Veg) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Veg, aes(x=Veg, y=proportion, group=BMIgroup, linetype=BMIgroup,
                     color=BMIgroup)) + geom_line()

Prop_Fruit <- group_by(Obesity, Fruit, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Fruit) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Fruit, aes(x=Fruit, y=proportion, group=BMIgroup, linetype=BMIgroup,
                       color=BMIgroup)) + geom_line()

Prop_AgeGroup <- group_by(Obesity, AgeGroup, BMIgroup) %>%
  summarise(count=n()) %>% group_by(AgeGroup) %>%
  mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_AgeGroup, aes(x=AgeGroup, y=proportion, group=BMIgroup,
                          linetype=BMIgroup, color=BMIgroup)) + geom_line()

Prop_Year <- group_by(Obesity, Year, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Year) %>% mutate(etotal=sum(count), proportion=count/etotal)
ggplot(Prop_Year, aes(x=Year, y=proportion, group=BMIgroup, linetype=BMIgroup,
                      color=BMIgroup)) + geom_line()
#Removing prop dataframes as there is no further use
rm(Prop_Sex, Prop_AgeGroup, Prop_Emp, Prop_Fruit, Prop_Veg, Prop_Year)