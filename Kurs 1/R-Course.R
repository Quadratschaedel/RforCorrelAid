###############################
#    CorrelAid Mannheim       #
#  A basic introduction to R  #
#                             #
# 23.10.2017 - Jochen Schäfer #
###############################

# R as a calculator

6+4

10/2


# Introducing: Objects and Variables

a <- 5
b <- 6
a+b
c <- a+b


# Introducing: Lists

MultOfFive <- c(a*1, a*2, a*3, a*4, a*5, a*6, a*7, a*8, a*9, a*10)

MultOfSix <- c(b*1, b*2, b*3, b*4, b*5, b*6, b*7, b*8, b*9, b*10)

# Intoducing: Functions

print("Hello World")

summary(MultOfFive)
var(MultOfFive)
sd(MultOfFive)
sum(MultOfFive)


# Introducing: Data Frames

df <- data.frame(MultOfFive, MultOfSix)

# Let's get started

setwd('/home/jochen/Dokumente/CorrelAid/R-Kurs')

# Indroducing: Packages

#install.packages("foreign")

library(foreign)

titanic <- read.csv("titanic_original.csv")

# Accessing values in a data frame
View(titanic)

titanic[1]

titanic$fare
titanic$fare[5]

# Introducing: Tables
# Simple frequency tables
x <- table(titanic$sex)

# Cross tables

survival_by_class <- table(titanic$pclass, titanic$survived)
survival_by_class

# Introducing: Graphics

# When using the "plot()" comamnd, R tries to guess the most appropriate type of
# graph (Based on the type of object you're trying to plot)

# This might or might not work out well:

plot(titanic$sex)

# looks reasonable, but

plot(titanic$age)

# is pretty useless. In this case specify the type of graphics directly:

plot(titanic$age, type="h")

# use
?plot()
# to see which options are available.

# However, the plot above is still rather uninformative. In most cases, we are not
# interested in the age of each individual person, but in the distribution of age
# over all passengers. This is what density and frequency plots are for:

hist(titanic$age)
hist(titanic$age, freq = FALSE)

# Adjust the labels
hist(titanic$age, xlab="Agegroups", main="Distribution of Age among Titanic-Passengers")

# If you are interested in interactions between variables, mosaic-plots can be
# very useful. Use the table() function to plot them

plot(survival_by_class, xlab = "Class", ylab="Survived", main="Survival by class")




####-------------------Excercises---------------------------------###



# Some first descriptive Analysis

# Load the Titanic dataset in your R-Environment. The dataset has been provided as a
# .csv-file.

# How many passengers survived the sinking of Titanic?

# Hint: The Data Frame contains a binary variable indicating whether a certain passenger
# survived (1) or not (0).

sum(titanic$survived)


# What age were the passengers of Titanic? Find the age of the youngest and the oldest
# passenger and calculate mean and median of the variable.




summary(titanic$age)

# Was it more likely for Men or for Women to survive the sinking? After that, 
# visualize your results in a plot and assign appropriate labels to the axis
# and the main title.

# Hint: Use the observed frequencies of deaths and survivals.

table(titanic$sex, titanic$survived)

plot(table(titanic$sex, titanic$survived), main="Survival by Gender", xlab="Gender", ylab="Survival")


###-----------------------BACKUP----------------------###

# Calculating correlations

cor(titanic$age, titanic$fare, use="pairwise.complete.obs")

# Calculating OLS-Regressions for numeric variables

reg1 <- lm(fare~age, data=titanic)
summary(reg1)

# Calculating OLS-Regressions für categrical attributes

reg2 <- lm(fare~age + sex, data=titanic)
summary(reg2)

# But what about the passenger class?

reg3 <- lm(fare~age + pclass, data=titanic)
summary(reg3)


reg4 <- lm(fare~age + sex + as.factor(pclass), data=titanic)
summary(reg4)

