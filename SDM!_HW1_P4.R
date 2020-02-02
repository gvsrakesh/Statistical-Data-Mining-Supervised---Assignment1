library(ggplot2)
library(DataExplorer)
library(lattice)
library(data.table)
library(mltools)
library(tidyverse)
library(caret)
library(ElemStatLearn)
library(gmodels)
library(DMwR)
require(class)
library(GGally)
library(MASS)


boston = Boston
quartz()
ggpairs(boston)
quartz()
plot_correlation(boston)

?Boston


ggplot(boston, aes(x=boston$indus, y=boston$tax)) + geom_point()
median(boston$indus)
median(boston$tax)

#Indus-Tax: strong positive correlation between high density of industries and higher tax shows that 
#industrial suburbs are high the tax paying entities. Infact apart from one of the five Boston employment
#centres all others pay tax greater than the median tax of Boston.

ggplot(boston, aes(x=boston$indus, y=boston$nox)) + geom_point()
median(boston$indus)
median(boston$nox)

#Indus-Nox: strong positive correlation between high density of industries and higher nox shows that 
#industrial suburbs are producing emissions of nitrogen oxides. The highest emissions are produced by
#suburbs with highest density of industries and only one suburb with industrial density greater than .
#15 has emissions less than 0.55

ggplot(boston, aes(x=boston$indus, y=boston$dis)) + geom_point()
median(boston$indus)
median(boston$dis)

#Indus-Dis: Strong negative correlation between high density of industries and distance shows that
# most of the suburbs are further away from the industrial centres. We can also see that there are a
#cluster of suburbs which are very close by to the five employment centres

ggplot(boston, aes(x=boston$nox, y=boston$age)) + geom_point()
median(boston$nox)
median(boston$age)

#Nox-Age: Strong positive correlation between nitrous oxide emissions and age shows that most
#of the suburbs with high nitrous oxide emissions have been occupied for a long time and these people
#have the risk of health problems

ggplot(boston, aes(x=boston$nox, y=boston$dis)) + geom_point()
median(boston$nox)
median(boston$dis)

#Nox-Dis: Strong negavtive correlation suggests that people who live further away from employment centres
#have less exposure to nitogen oxide emissions and can lead a healthier life

ggplot(boston, aes(x=boston$age, y=boston$dis)) + geom_point()
median(boston$age)
median(boston$dis)

#Age-Dis: Strong negative correlation suggests that people who live in old houses are the closest to
# employment centres. The high median values of age and low median value of dis indicate high
# concentration of suburbs in that quadrant

summary(boston$crim)
quartz()
ggplot(boston, aes(boston$crim)) + stat_ecdf(geom = "step")

#relation of crime rate with tax
hist(boston$tax)
summary(boston$tax)
ggplot(boston, aes(boston$tax)) + stat_ecdf(geom = "step")
boston = boston %>% mutate(tax_quantile = ntile(tax, 5))
tapply(boston$tax, boston$tax_quantile, max)
boston$tax_quantile = as.factor(boston$tax_quantile)
ggplot(boston, aes(x=(boston$tax_quantile), y=boston$crim)) + geom_boxplot()

#relation of crime with the radial highwayss
ggplot(boston, aes(boston$rad)) + stat_ecdf(geom = "step")
boston = boston %>% mutate(rad_quantile = ntile(tax, 5))
tapply(boston$rad, boston$rad_quantile, min)
boston$rad_quantile = as.factor(boston$rad_quantile)
ggplot(boston, aes(x=(boston$rad_quantile), y=boston$crim)) + geom_boxplot()

#relation of the lower status of the population w.r.t. crime rate
ggplot(boston, aes(x = boston$lstat, y = boston$crim)) + geom_point() + stat_smooth()

#crime rate analysis of suburbs
boston = boston %>% mutate(crime_decile = ntile(crim, 10))
boston$crime_decile = as.factor(boston$crime_decile)
tapply(boston$crim, boston$crime_decile, min)
ggplot(boston, aes(x=(boston$crime_decile), y=boston$crim)) + geom_boxplot()
ggplot(boston, aes(boston$crim)) + stat_ecdf(geom = "step")

hist(boston$ptratio)
summary(boston$ptratio)
ggplot(boston, aes(boston$ptratio)) + stat_ecdf(geom = "step")

hist(boston$tax)
summary(boston$tax)
ggplot(boston, aes(boston$tax)) + stat_ecdf(geom = "step")


nrow(boston[boston$rm>7,])
nrow(boston[boston$rm>8,])

boston_rm_8 = boston[boston$rm>8,]       
summary(boston_rm_8)
summary(boston)
quartz()
plot_correlation(boston_rm_8[,c(1:14)])
