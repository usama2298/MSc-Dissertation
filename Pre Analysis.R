library(tidyverse); library(faraway); library(GGally); library(skimr)
library(ggthemes); library(sjPlot); library(class); library(gmodels)
library(ggridges); library(gridExtra)

# Reading Data ----
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

# Checking if the data is equally divided
apply(Obesity[ ,-c(6,7)], 2, table)
apply(Obesity_train[ ,-c(6,7)], 2, table)
apply(Obesity_test[ ,-c(6,7)], 2, table)

# Writing files for future use
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

# Summary ----
my_skim <- skim_with(base=sfl(n=length,n_missing=n_missing),factor=sfl(ordered=NULL),
                     numeric=sfl(p0=NULL,p100=NULL,hist = NULL))
my_skim(Obesity); rm(my_skim)

## Cross Tables ----
CrossTable(Obesity$BMIgroup, Obesity$AgeGroup,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$BMIgroup, Obesity$Sex,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$Employment, Obesity$BMIgroup,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$BMIgroup, Obesity$Veg,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$BMIgroup, Obesity$Fruit,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$BMIgroup, Obesity$Year,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$Obese, Obesity$Year,digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)

table(Obesity$BMIgroup)
ggplot(Obesity, aes(x = Year, fill = BMIgroup)) + geom_bar(position = "dodge")
ggplot(Obesity, aes(x = Year, fill = Obese)) + geom_bar() + facet_wrap(~ Obese)

aggregate(Obesity$BMI, by=list(Obesity$BMIgroup), FUN=var)
aggregate(Obesity$BMI, by=list(Obesity$Year), FUN=var)
aggregate(Obesity$BMI, by=list(Obesity$BMIgroup), FUN=var)

ggpairs(Obesity, columns=2, ggplot2::aes(colour=BMIgroup), legend = T)

ggplot(Obesity, aes(x = BMI, y = as.factor(Year), fill = BMIgroup)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges()


# Proportion graphs ----
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


## Prop plots for Year and Obese ----
Prop_Year <- group_by(Obesity, Year, Obese) %>% summarise(count=n()) %>% 
  group_by(Year) %>% mutate(etotal=sum(count), proportion=count/etotal)

p1 <- Prop_Year %>% subset(Obese == "Yes") %>%
  ggplot(mapping = aes(x=Year, y=proportion, group=Obese, color=Obese)) +
  geom_line(size = 0.5)
p2 <- Prop_Year %>% subset(Obese == "No") %>%
  ggplot(mapping = aes(x=Year, y=proportion, group=Obese, color=Obese)) +
  geom_line(size = 0.5)

grid.arrange(p1, p2)

## Prop plots for Year and BMIgroup ----
Prop_Year <- group_by(Obesity, Year, BMIgroup) %>% summarise(count=n()) %>%
  group_by(Year) %>% mutate(etotal=sum(count), proportion=count/etotal)

p1 <- Prop_Year %>% subset(BMIgroup == "Underweight") %>%
ggplot(mapping = aes(x=Year, y=proportion, group=BMIgroup, color=BMIgroup)) +
  geom_line(size = 0.5)
p2 <- Prop_Year %>% subset(BMIgroup == "Normal") %>%
ggplot(mapping = aes(x=Year, y=proportion, group=BMIgroup, color=BMIgroup)) +
  geom_line(size = 0.5)
p3 <- Prop_Year %>% subset(BMIgroup == "Overweight") %>%
ggplot(mapping = aes(x=Year, y=proportion, group=BMIgroup, color=BMIgroup)) +
  geom_line(size = 0.5)
p4 <- Prop_Year %>% subset(BMIgroup == "Obese") %>%
ggplot(mapping = aes(x=Year, y=proportion, group=BMIgroup, color=BMIgroup)) +
  geom_line(size = 0.5)

grid.arrange(p1, p2, p3, p4)



#Removing prop data frames as there is no further use
rm(Prop_Sex, Prop_AgeGroup, Prop_Emp, Prop_Fruit, Prop_Veg, Prop_Year)









