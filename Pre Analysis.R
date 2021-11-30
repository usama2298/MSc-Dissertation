# Libraries ----
library(easypackages)
libraries("olsrr", "faraway", "ggfortify", "caTools", "plotly", "lattice", "Metrics",
          "jtools", "moderndive", "MASS", "broom", "infer", "janitor", "recipes",
          "tidyverse", "GGally", "skimr", "ggthemes", "sjPlot", "class", "gmodels",
          "gridExtra", "AICcmodavg", "recipes", "randomForest", "caret", "vip")

# Reading Data ----
Obesity <- read_csv("Obesity.csv")

# Converting Variables into factor
Obesity$AgeGroup <- as.factor(Obesity$AgeGroup)
Obesity$Sex <- as.factor(Obesity$Sex)
Obesity$Employment <- as.factor(Obesity$Employment)
Obesity$Veg <- as.factor(Obesity$Veg)
Obesity$Fruit <- as.factor(Obesity$Fruit)
Obesity$BMIgroup <- as.factor(Obesity$BMIgroup)
Obesity$Year <- as.factor(Obesity$Year)
Obesity$Obese <- as.factor(if_else(Obesity$BMIgroup == "Obese", "Yes", "No"))

table(Obesity$BMIgroup)

# Renaming Employment levels with short forms
## Else         = "Doing something else"                                  
## FT Edu       = "In full-time education"                                
## Employed     = "In paid employment, self-employed or on govt training"
## Homemaking   = "Looking after home/family"                             
## Job Seeking  = "Looking for/intending to look for paid work"           
## Unemployable = "Perm unable to work"                                   
## Retd         = "Retired" 
levels(Obesity$Employment) <- c("Else", "FT Edu", "Employed", "Homemaking",
                                "Job Seeking", "Unemployable", "Retd")
Obesity$Employment <- relevel(Obesity$Employment, ref = "FT Edu")

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
my_skim <- skim_with(base=sfl(n=length), factor=sfl(ordered=NULL),
                     numeric=sfl(hist = NULL, p0 = NULL, p100 = NULL))
my_skim(Obesity); rm(my_skim)

Obesity %>% group_by(Fruit) %>% my_skim(BMI) %>% 
  dplyr::select(Fruit, Mean = numeric.mean, SD = numeric.sd, P25 = numeric.p25,
                P50 = numeric.p50, P75 = numeric.p75) %>%
  write.csv("C:/Users/chusa/Desktop/file.csv")



ggplot(Obesity, aes(x = BMI)) +
  geom_histogram(binwidth = 1.5, mapping = aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.15, fill="grey") + 
  geom_vline(aes(xintercept=mean(Obesity$BMI)), color = "#3F6EC3", linetype="dashed") + 
  ylab("Density") +
  annotate(geom="text", x=28.35, y=0.04, label="mean = 27.9",
           color="black", angle='90', size = 4) +
  geom_vline(aes(xintercept=quantile(Obesity$BMI, 0.75)), color = "#657F14", linetype="dashed") + 
  ylab("Density") +
  geom_vline(aes(xintercept=quantile(Obesity$BMI, 0.25)), color = "#EC792B", linetype="dashed") + 
  ylab("Density") +
  theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg
                
ggpairs(Obesity[,-c(6:7, 9)], lower = NULL, diag = NULL)
table(Obesity$Obese)
## Cross Tables ----
CrossTable(Obesity$AgeGroup, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$Employment, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$Sex, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$Fruit, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$Veg, Obesity$BMIgroup,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(Obesity$AgeGroup, Obesity$Obese,digits=2,
           prop.c=F, prop.t=F, prop.chisq=F)



ggplot(Obesity, aes(x = Year, fill = BMIgroup)) + geom_bar(position = "dodge")
ggplot(Obesity, aes(x = Year, fill = Obese)) + geom_bar()# + facet_wrap(~ Obese)

aggregate(Obesity$BMI, by=list(Obesity$BMIgroup), FUN=var)
aggregate(Obesity$BMI, by=list(Obesity$Year), FUN=mean)
aggregate(Obesity$BMI, by=list(Obesity$BMIgroup), FUN=var)

ggpairs(Obesity, columns=2, ggplot2::aes(colour=BMIgroup), legend = T)

ggplot(Obesity, aes(x = BMI, y = as.factor(Year), fill = BMIgroup)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges()

levels(Obesity$Employment)
# Proportion graphs ----
Prop_Emp <- group_by(Obesity, Employment, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Employment) %>% mutate(total=sum(count), proportion=count/total)
ggplot(Prop_Emp, aes(x=Employment, y=proportion, group=BMIgroup,
                     linetype=BMIgroup, color=BMIgroup)) + geom_line() + theme_bw()
Prop_Emp <- Prop_Emp %>% subset(BMIgroup == "Obese")
prop.test(Prop_Emp$count, Prop_Emp$total)

Prop_Sex <- group_by(Obesity, Sex, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Sex) %>% mutate(total=sum(count), proportion=count/total)
ggplot(Prop_Sex, aes(x=Sex, y=proportion, group=BMIgroup, linetype=BMIgroup,
                     color=BMIgroup)) + geom_line() + theme_bw()

Prop_Veg <- group_by(Obesity, Veg, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Veg) %>% mutate(total=sum(count), proportion=count/total)
ggplot(Prop_Veg, aes(x=Veg, y=proportion, group=BMIgroup, linetype=BMIgroup,
                     color=BMIgroup)) + geom_line()

Prop_Fruit <- group_by(Obesity, Fruit, BMIgroup) %>% summarise(count=n()) %>% 
  group_by(Fruit) %>% mutate(total=sum(count), proportion=count/total)
ggplot(Prop_Fruit, aes(x=Fruit, y=proportion, group=BMIgroup, linetype=BMIgroup,
                       color=BMIgroup)) + geom_line()

Prop_AgeGroup <- group_by(Obesity, AgeGroup, BMIgroup) %>%
  summarise(count=n()) %>% group_by(AgeGroup) %>%
  mutate(total=sum(count), proportion=count/total)
ggplot(Prop_AgeGroup, aes(x=AgeGroup, y=proportion, group=BMIgroup,
                          linetype=BMIgroup, color=BMIgroup)) + geom_line()
group_by(Obesity, AgeGroup) %>% summarise(Count=n()) %>%
  ggplot(aes(x = AgeGroup, y = Count)) + geom_col(aes(fill = AgeGroup))


## Prop plots for Year and BMIgroup ----
Prop_Year <- group_by(Obesity, Year, BMIgroup) %>% summarise(count=n()) %>%
  group_by(Year) %>% mutate(total=sum(count), proportion=count/total)


### Prop plot for Year and Obese
Prop_Year %>% subset(BMIgroup == "Obese") %>%
  ggplot(mapping = aes(x=Year, y=proportion, group=BMIgroup, color=BMIgroup)) +
  geom_line(size = 0.5) + labs(title = "Proportion Plot: Obese vs Year")

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

# Extracting data for Excel
Prop_Fruit %>% dplyr::select(-c(3,4)) %>%
  pivot_wider(names_from = BMIgroup, values_from = proportion) %>%
  write.csv("C:/Users/chusa/Desktop/file.csv")

# Removing prop data frames as there is no further use
rm(Prop_Sex, Prop_AgeGroup, Prop_Emp, Prop_Fruit, Prop_Veg, p1, p2, p3, p4)

# Proportion Comparison ----
Prop_Year <- Prop_Year %>% subset(BMIgroup == "Obese")
prop.test(Prop_Year$count, Prop_Year$total) ; rm(Prop_Year)
stats::chisq.test(Obesity$Obese, Obesity$Year)





