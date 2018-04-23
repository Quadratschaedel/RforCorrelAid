################################################
#             CorrelAid Mannheim               #
#  A slightly more advanced introduction to R  #
#                                              #
#                  Part III                    #
#                                              #
#         19.03.2018 - Jochen Schäfer          #
################################################

# In today's workshop you will learn how to
#     - handle proprietary data formats into R (STATA, SPSS)
#     - recode and subset data
#     - use control flow in R (for, while, ifelse)
#     - write your own functions


                              #################
                              ## Preparing R ##
                              #################
      
# Initial Setup
setwd("/home/jochen/Dokumente/Privat/CorrelAid/R-Kurs/")
install.packages("foreign")
install.packages("car")

# Unlike R, most other popular statictics tools are Closed-Source-Software. This means, that their data
# formats are not supported by R. To load them, we have to use the Package "foreign".

library(foreign)
library(car)


                        #############################
                        ## Reading foreign formats ##
                        ##      Data Management    ##
                        #############################

# To read files in STATA's .dta-Format, use read.dta()

soep <- read.dta("soep_lebensz.dta")

# Foreign also provides functionality for several other data formats, including SPSS's .sav and .arff
# Let's have a look at the Data

View(soep)

# R imports the variable names and uses them as column names in the data frame. But what do they mean?

soep.label <- attr(soep,"var.labels")
soep.label  <- data.frame(soep.name=names(soep),soep.label)


                                  #####################
                                  ## Recoding values ##
                                  #####################

# Let's assume, we want to create a new dummy variable named "health_dummy", which is 0 for respondents
# with a poor perception of their health (Schlecht, Weniger gut) and 1 for respondents who regard
# themselves as healthy. 

# Let's find the numeric values behind the labels:
levels(soep$gesund_org)
table(soep$gesund_org, as.numeric(soep$gesund_org))

soep$health_dummy <- recode(as.numeric(soep$gesund_org), "c(4,5) = 0; 6:8 = 1; else=NA")

# Check if recoding worked
table(soep$gesund_org, soep$health_dummy)

# Looks good. Now, for the sake of completeness, let's assume we want to assign value labels to our new
# variable. Therefore, we have to turn it into a factor again:

soep$health_dummy_lab <- factor(soep$health_dummy,
                    levels = c(0,1),
                    labels = c("not satisfied", "satisfied")) 

# NOTE: Turning a Dummy into a factor is something we usually do NOT want to do. Why?

table(soep$health_dummy_lab, as.numeric(soep$health_dummy_lab))

# => By assigning value labels, R somehow screws up the original values.
# Let's delete the column beore we get too confused

soep$health_dummy_lab <- NULL
                        
                        ##########################
                        ## Substeting by column ##
                        ##########################

# We want to split our data into two datasets: One containing the health-relatded variables and
# the other one containing variables linked to life satisfaction.

# Way 1: Use column names
soep.health <- soep[c("persnr", "jahr","gesund_org", "gesund_std", "health_dummy")]

# Way 2: Use column positions aka "Index notation"
soep.life <- soep[,c(1,2,7,9)]

# This also works the ther way around. The code
soep.anonymous <- soep[,-1]

# will create a copy of "soep" with all but the first column.
rm(soep.anonymous)


                        ##########################
                        ## Substeting by values ##
                        ##########################

# Excursus: Boolean algebra
# Comparisons: ==, !=, <, >, >=, <=
# Operators: &, |, xor (and also &&, || which will rarely be used)
# Brackets can be used to define a hierarchy of operators

1 == 1
1 == 2
1 != 2

(1==1) & (1==2)
(1==1) | (1==2)
(1==1) | (2==2)

xor((1==1), (1==2))
xor((1==1), (2==2))

((1==1) | (1==2)) & (1==2)
(1==1) | ((1==2) & (1==2))

# Often, we want to select data based on the values of one or more variables. Let's restrict
# our dataset to observations made in the year 2002: This can also be done using the index-notation. 

soep.2002 <- soep[soep$jahr==2002,]

# Behind the scenes
head(soep$jahr==2002)

# Using index notation can be quite cumbersome for complex conditions and badly formatted data.
# In such cases, we can use R's subset() function.

soep.0203 <- subset(soep, (jahr==2002 | jahr==2003) & health_dummy==1)

rm(soep.0203, soep.2002)
                    
                    ######################
                    ## Replacing values ##
                    ######################

# Sometimes, we have to replace certain values in our dataset. For example, we might want to replace all
# values for "bildung" which are greater than 15 with 15.
# We can use a notation similar to subsetting

soep$bildung[soep$bildung>15] <- 15


################################### Excercises ###################################
# Use the current version soep-dataset

# Create a new ordinal variable for education, using the following encoding
# 0 to 8 years: 1
# 9 to 13 years: 2
# 14 or more years: 3







soep$educ_groups <- recode(soep$bildung, "0:8=1; 9:13=2; 14:Inf=3; else=NA")


# Assign value labels to your newly created variable, using the following codebook:
# 1: low
# 2: intermediate
# 3: high








soep$educ_groups <- factor(soep$educ_groups,
                                levels = c(1,2,3),
                                labels = c("low", "intermediate","high")) 

# Restrict your dataset to highly educated people with two or more children






soep <- subset(soep, educ_groups=="high" & anz_kind>=2)

# Write the soep.life dataset into a STATA-File named "soep_life.dta"

write.dta(soep.life,"soep_life.dta")

##################################################################################


                    ##################
                    ## Control Flow ##
                    ##################

# Programming languages usually support three basic control flow structures
# 1) Loops
# 2) Branches

# For-Loops execute certain commands a specified number of times.
# The value of the variable in the loop definition can be accessed dynamically from
# within the loop.

for(x in (1:5)){
  print("Hello, I am a for-loop")
}

for(x in c("Hello,", "I", "am", "a", "for-loop")){
  print("Hello", quote = F)
}

# While-loops execute certain commands as long as a specific condition is true
x <- 5
while(x > 0){
  print(x)
  x <- x-1
}

# CAUTION: Beware of Infinite loops!
x <- 5
while(x > 0){
  x <- 5
  print(x)
  x <- x-1
}

# Branches execute different commands depending on a condition:

if(x>0){
  print("x is positive")
} else if(x<0){
  print("x is negative")
}else{
  print("x is zero")
}


################################### Excercises ###################################

# Reload the soep-dataset and restrict it to oberservations that have no missing value
# in the anz_kind-columm. Use a for loop to find out, for how many respondents the number
# of children changed over the years.
# Hint: unique(soep$persnr) gives you a list of the IDs of all respondents in the dataset


soep <- read.dta("soep_lebensz.dta")

counter <- 0
soep <- soep[!is.na(soep$anz_kind),]

for(x in unique(soep$persnr)){
  if(min(soep$anz_kind[soep$persnr==x])!=max(soep$anz_kind[soep$persnr==x])){
    counter <- counter+1
  }
}


##############################################################################


                              ############################
                              ## User-defined functions ##
                              ############################

# If you have to perform the same computations (i.e. execute the same lines of code)
# multiple times for different data, it might be useful to write a function.

# Functions define a set of operations to be performed. The data (and more) is handed over
# to the function as a "Parameter". After computations are completed, the function gives
# back a "Return value". Everything in between is a black box and cannot be seen from outside
# the function.

hello <- function(){
  print("Hello World!", quote = F)
}

posneg <- function(x){
  if(x>0){
    print("Your number is positive")
  } else if(x<0){
    print("Your number is negative")
  }else{
    print("Your number is zero")
  }
}

add <- function(a=1, b=2){
  return(a+b)
}


################################### Excercises ###################################

# Write a funtion, that implements the children's game "Fizz-Buzz" in R by printing
# the appropriate line of text for X numbers, starting with 1.

# Rules: - If a number is divisible by 3, print the word "Fizz"
#        - If a number is divisible by 5, print the word "Buzz"
#        - If a number is divisible by 3 AND 5, print "Fizz Buzz"
#        - Else, print the number

# Hint: The operator %% gives you the remainder of an integer division


fizzbuzz <- function(x){
	for (i in 1:x) {
 if (i %% 3 == 0 & i %% 5 == 0) {print("FizzBuzz")}
 else if (i %% 3 == 0) {print("Fizz")}
 else if (i %% 5 == 0) {print("Buzz")}
 else print(i)
}
}