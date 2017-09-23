#
###################################################
# Exercise 3.1                                    #
###################################################
# Show/argue that the odds-ratio in eq.(7)        #
# can be written as the inequality (eq.(8))       #
# between the ratio of the prior probabilities    #
# and the posterior probabilities.                #
###################################################
#  ℙ(∏1 | x)
#             >  1
#  ℙ(∏2 | X)
#
# <=>
# 
#      f1(x) π1
#  __________________             
#  f1(x)π1 + f2(x)π2
# ______________________  > 1                             
#      f2(x) π2
#  _________________
#  f1(x)π1 + f2(x)π2
#
# <=>
#
#    f1(x)π1      > 1
# _____________               Cross multiply 
#    f2(x)π2      > 1
#
# <=>
# 
# f1(x)π1 > f2(x)π2
#
# <=>
#
#  f1(x)      π2
# _______  > ____
#  f2(x)      π1
#
#                  □

#################################################
# Exercise 3.2a                                 #
#################################################
# Source:                                       #############################################################
# https://eight2late.wordpress.com/2015/11/06/a-gentle-introduction-to-naive-bayes-classification-using-r/  #
#############################################################################################################
setwd("~/Documents/rprojects/dataScience")

# install.packages("mlbench")
# Load the mlbench library, which contains the HouseVote84
library(mlbench)
# Load the dataset
data("HouseVotes84")

# Barplots for specific elements
## Plot for all Yes and No
plot(as.factor(HouseVotes84[, 2]))
title(main = "Amount of YES and NO votes for V2 (Handicapped infants)", xlab = "Votes", ylab = "Amount")

## By Party
### Republicans
plot(as.factor(HouseVotes84[HouseVotes84$Class == "republican", 2]))
title(main = "Republican votes for V2", xlab = "Votes", ylab = "Amount")

## Democrats
plot(as.factor(HouseVotes84[HouseVotes84$Class == "democrat", 2]))
title(main = "Democrat votes for V2", xlab = "Votes", ylab = "Amount")


#################################################
# Exercise 3.2b                                 #
#################################################

# Taking care of missing values before we actually use Naive Bayes
## Function to return number of NA's by vote and class (democrat or republican)
na_by_col_class <- 
  function(col, cls) {
    return(sum(
      is.na(
        HouseVotes84[, col]
      ) &
        HouseVotes84$Class == cls
      )
    )
  }

## Function for the conditional probability that a member of a party will cast y 
## for a topic, based on all members of te party who cast a vote (without NA)
p_y_col_class <- 
  function(col, cls){
    sum_y <- sum(
      HouseVotes84[, col] == "y" & 
        HouseVotes84$Class == cls, 
      na.rm = TRUE
    )
    sum_n <- sum(
      HouseVotes84[, col] == "n" &
        HouseVotes84$Class == cls,
      na.rm = TRUE
    )
    return(sum_y / (sum_y + sum_n))
  }

## Checking if the functions actually worked
p_y_col_class(2, "democrat")
p_y_col_class(2, "republican")
na_by_col_class(2, "democrat")
na_by_col_class(2, "republican")

## Replace missing values by randomly assigning y or n to NA's
### Uniform distribution is used used to set the NA value to yes if the random numer returned
### is less than the probability of a yes vote and to no otherwise.
for (i in 2:ncol(HouseVotes84)) {
  if (sum(is.na(HouseVotes84[, i]) > 0)) {
    c1 <- which(is.na(HouseVotes84[, i]) & HouseVotes84$Class == "democrat", arr.ind = TRUE)
    c2 <- which(is.na(HouseVotes84[, i]) & HouseVotes84$Class == "republican", arr.ind = TRUE)
    HouseVotes84[c1, i] <-
      ifelse(runif(na_by_col_class(i, "democrat")) < p_y_col_class(i, "democrat"), "y", "n")
    HouseVotes84[c2, i] <-
      ifelse(runif(na_by_col_class(i, "republican")) < p_y_col_class(i, "republican"), "y", "n")
  }
}

# View the new dataset
View(HouseVotes84)

# Now we are ready to actually work with the dataset and do some Naive Bayes

# Divide into test and training sets
# Create new col "train" and assign 1 or 0 in 80/20 proportion via random uniform dist
HouseVotes84[, "train"] <- 
  ifelse(runif(nrow(HouseVotes84)) < 0.80, 1, 0)

# Get col number of train / test indicator column (needed a bit later)
trainColNum <- grep("train", names(HouseVotes84))

# Seperate training and test sets and remove training column before modeling
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train == 1, -trainColNum]
testHouseeVotes84 <- HouseVotes84[HouseVotes84$train == 0, -trainColNum]

# Finally we can now build the freaking Naive Bayes model

#install.packages("e1071")

# We need this library! As it contains the algorithm made nicely available for us!
library(e1071) 

# the Class~. means, the dependent variable (to be predicted) appears on the left hand side
# of the ~ and the independent variables (predictors or features) are on the right hand side.
# The dot is simply shorthand for "all variable other than the dependent one". And then we 
# have the dataframe (trainHouseVotes84)
nb_model <- naiveBayes(Class~. , data = trainHouseVotes84)

# Lets look at the model. Three differents ways to do that.
nb_model
summary(nb_model)
str(nb_model)

#################################################
# Exercise 3.2c                                 #
#################################################
# Time to do some predicting
# This is done by feeding test data into our model (nb_model) and then comparing with known
# values. As always a confusion matrix will be used.

# Dun dun dun!
nb_test_predict <- predict(nb_model, testHouseeVotes84[, -1])
# The confusion matrix
table(pred = nb_test_predict, true = testHouseeVotes84$Class)
# As seen in the confusion matrix, the algorithm has correctly classified 
# 43 out of 48 (43+5) democrats and 26 out of 30 republicans. Not bad!
# The values might vary on your NON-Linux computers.

#################################################
# Exercise 3.2d                                 #
#################################################
# Fractions of correct predictions
mean(nb_test_predict == testHouseeVotes84$Class)

#################################################
# Exercise 3.2e                                 #
#################################################
# One is the loneliest number, therefore we do things multiple times!

# Function to create, run and record model results
nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for (i in 1:n){
    HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<train_fraction,1,0)
    trainColNum <- grep("train",names(HouseVotes84))
    trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-trainColNum]
    testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-trainColNum]
    nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
    nb_test_predict <- predict(nb_model,testHouseVotes84[,-1])
    fraction_correct[i] <- mean(nb_test_predict==testHouseVotes84$Class)
  }
  return(fraction_correct)
}

# 20 runs, 80% of data randomly selected for training set in each run
fraction_correct_predictions <- nb_multiple_runs(0.8, 20)
fraction_correct_predictions

# Summary of results
summary(fraction_correct_predictions)

# Standard Deviation
sd(fraction_correct_predictions)

# That is it for exercise 3.2

#################################################
# Exercise 3.3a                                 #
#################################################
# Source:                                       #####################################
# https://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/iris_plots/  #
#####################################################################################

# Lets do some fancy colourful plotting
plot(iris$Petal.Length, iris$Petal.Width, pch = 21, bg = c("red", "green3", "blue")
     [unclass(iris$Species)], main = "Iris Data", xlab = "Petal Length", ylab = "Petal Width")

# As illustrated in the scatterplot the different species are different in length and width
# Now, how do the other variables behave?
# For this we can use "pairs" command, which pairs scatterplots into a matrix
pairs(iris[1:4], main = "Iris Data", pch = 21, bg = c("red", "green3", "blue")
      [unclass(iris$Species)])

# Can probably be done even fancier!

#################################################
# Exercise 3.3b                                 #
#################################################
# Source:                                       #
# http://rischanlab.github.io/RandomForest.html #
#################################################

# Lets make a forest out of the flowers

# Divide the data into 0.7 / 0.3 training and test data


