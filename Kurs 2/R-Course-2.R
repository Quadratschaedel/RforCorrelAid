###############################
#    CorrelAid Mannheim       #
#  A basic introduction to R  #
#                             #
#          Part II            #
#                             #
# 27.11.2017 - Jochen Schäfer #
###############################

# In today's workshop you will learn
#     - to estimate basic predictive models (OLS-Regression)
#     - to interprete and to visualise the parameters of these models
#     - to avoid typical pitfalls
#     - basic principles of programming in general

# Load Data
setwd('/home/jochen/Dokumente/CorrelAid/R-Kurs')
titanic <- read.csv("titanic_original.csv")

# As a first step, you ususally have a look at the data
View(titanic)

head(titanic)

tail(titanic)

# Does the data and the encoding look reasonable?

# Introducing: Correlation

cor(titanic$age, titanic$fare)

# [1] NA

# Yikes, what happened?

# Strategies to deal with missing values
# - Ignore/delete them
# - Replace them with predefined values
# - Replace them with mean/median/modus
# - Run an imputation-algorithm

# Calculating correlations

cor(titanic$age, titanic$fare, use="pairwise.complete.obs")


# Calculating OLS-Regressions for numeric variables

reg <- lm(fare~1, data=titanic)
summary(reg)
mean(titanic$fare, na.rm = T)

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
# extracting estimation results

coef(reg4)       # the model coefficients (as a named vector)
vcov(reg4)       # the variance covariance matrix of the coefficients (as a named matrix)
confint(reg4)    # 95% confidence intervals of the coefficients 
residuals(reg4)  # the model residuals

# calculating effect sizes and predicting values

int <- as.vector(reg4$coefficients[1])   # the intercept (without its name)
age <- as.vector(reg4$coefficients[2])   # the coef of age (without its name)
male <- as.vector(reg4$coefficients[3])   # the coef of age (without its name)
class2 <- as.vector(reg4$coefficients[4])   # the coef of age (without its name)
class3 <- as.vector(reg4$coefficients[5])   # the coef of age (without its name)

# Predicting values

int+20*age+1*male+0*class2+1*class3   # predicted income for a young man, travelling third class
int+20*age+1*male+0*class2+0*class3   # predicted income for the same young man, now travelling first class

int+80*age+0*male+0*class2+0*class3   # predicted income for an old woman, travelling second class

minyhat <- min(titanic$age,na.rm=T)*age  # predicted effect of age for youngest person in sample (3rd class)
maxyhat <- max(titanic$age,na.rm=T)*age # predicted effect of age for oldest person in sample (3rd class)

maxyhat-minyhat    # effect of increasing income from observed min to observed max


# Visualising your results

plot(titanic$age, titanic$fare)
abline(reg4, col="red", lwd=2)


# Does the passenger class affect the fare differently for men and women?

reg5 <- lm(fare~age + sex + as.factor(pclass) + as.factor(pclass)*sex , data=titanic)
summary(reg6)



# What happenes if you include the port of embarkment into the model?

reg6 <- lm(fare ~ sex + as.factor(pclass) + age + embarked, data=titanic)
summary(reg5)


# Can we estimate the influece of gender, passenger class and age onto survival?
# Let's try:

reg7 <- lm(survived ~ sex + as.factor(pclass) + age, data=titanic)
summary(reg7)

# Obviously, this model predicts invalid values (i.e. values, that are neither 0 nor 1)

# Use a logistic regression instead

logit <- glm(survived ~ sex + as.factor(pclass) + age ,family=binomial(link='logit'),data=titanic)
summary(logit)

# Correct results, but logit-coefficients are really hard to interpret


############################# Excercises ###############################

# Calculate the predicted ticket price of a 85 year old man travelling in third class.
# Use the coefficients of model reg4. Does the result look reasonable?


int+85*age+1*male+0*class2+1*class3   # predicted fare for an old man, travelling third class

# The regression predicted a negative price of the ticket. This is obviously not possible, but a
# side-effect of our model specification.

# Build a model that predicts the age of a passenger based on the Number of parents/childen (parch),
# the number of siblings and spouses (sibsp). Visualise your results in an appropriate graph.

summary(lm(age ~ parch + sibsp, data = titanic))
summary(reg8)
plot(titanic$parch, titanic$age)
abline(reg8, col="red", lwd=2)


#-------------------------- BACKUP: Loops --------------------------#