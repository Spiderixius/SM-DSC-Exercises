setwd("~/Documents/rprojects/dataScience")


# Defintion of Linear Discriminant Analysis:
#     Which feaure (attribute) can best help determine the classification of a data row(x)
#     The decisive attributes for classification
#     This means, we are filtering the features and weighing them to see which features
#     makes sense to use for the classification.


#####################################
# Exercise 1a                       #
#####################################
# Source:                           ####################################################
# http://maths-people.anu.edu.au/~johnm/courses/dm/math3346/2008/pdf/r-exercisesVI.pdf #
########################################################################################

#install.packages("MASS")
library(MASS)
data("Pima.tr")
View(Pima.tr)
help(Pima.tr)

#####################################
# Exercise 1b                      #
#####################################

# Linear Discriminant analysis.
# We firstly take all columns of Pima.tr, except the type.
# CV is set to TRUE, to get predictions of class memberships.
# This is to get the cross-validation while leaving type out.
PimaCV.lda <- lda(type ~ . , data = Pima.tr, CV = TRUE)

# A table which is our confusion matrix
tab <- table(Pima.tr$type, PimaCV.lda$class)
tab

# Convert confusion matrix intro percentage.
conCV1 <- rbind(tab[1,] / sum(tab[1,]), tab[2,] / sum(tab[2,]))

# Dimension names
dimnames(conCV1)


dimnames(conCV1) <-
  list(Actual = c("No", "Yes"),
       "Predicted (cv)" = c("No",
                            "Yes"))
print(round(conCV1, 3))

#####################################
# Exercise 1c                       #
#####################################
# This time we don't make use of CV
Pima.lda <- lda(type ~ . , data = Pima.tr)
Pima.hat <- predict(Pima.lda)
tabtrain <- table(Pima.tr$type, Pima.hat$class)

# Convert confusion matrix intro percentage.
conTrain <-
  rbind(tab[1,] / sum(tab[1,]), tab[2,] / sum(tab[2,]))

# Dimension names
dimnames(conTrain) <-
  list(Actual = c("No", "Yes"),
       "Predicted (cv)" = c("No",
                            "Yes"))
print(round(conTrain, 3))

#####################################
# Exercise 1d                       #
#####################################
library(lattice)

# The plot shows the distribution of discriminants
# x = Linear discriminant
# y = Density (percentage)
densityplot( ~ Pima.hat$x, groups = Pima.tr$type)


#####################################
# Exercise 1e                       #
#####################################

# 
confusion <-
  function(actual,
           predicted,
           names = NULL,
           printit = TRUE,
           prior = NULL) {
    # if names variable is null, then set default value with actual
    if (is.null(names))
      names <- levels(actual)
    
    # Put actual and predicted into a matrix (Confusion matrix)
    tab <- table(actual, predicted)
    # Convert confusion matrix to percentages
    acctab <- t(apply(tab, 1, function(x) x / sum(x)))
    
    # Set dimmension names for acctab
    dimnames(acctab) <- list(Actual = names, "Predicted (cv)" = names)
    
    # If there is no prior probability, then a probability is calculated from the actual
    # acc = accuracy is also calculated from the new prior
    if (is.null(prior)) {
      relnum <- table(actual)
      prior <- relnum / sum(relnum)
      acc <- sum(tab[row(tab) == col(tab)]) / sum(tab)
    }
    # If prior has a value, then accuracy is calculated from the existing prior
    else {
      acc <- sum(prior * diag(acctab))
      names(prior) <- names
    }
    # Print the accuracy and prior frequency
    if (printit)
      print(round(c(
        "Overall accuracy" = acc, "Prior frequency" = prior
      ),
      4))
    # Print the confusion matrix
    if (printit) {
      cat("\nConfusion matrix", "\n")
      print(round(acctab, 4))
    }
    invisible(acctab)
  }


#####################################
# Exercise 2a                       #
#####################################
# Source:                           ########################################################
# https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html #
############################################################################################
#install.packages("rattle")
#install.packages("car")
library(car)
setwd("~/Documents/rprojects/dataScience/DataFiles")

# Could not find wine in the "rattle" library, so wine has been brought down manually
# Source: https://artax.karlin.mff.cuni.cz/r-help/library/rattle/html/wine.html
wine <- read.csv("wine.data")
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')
wine$Type <- as.factor(wine$Type)

#####################################
# Exercise 2b                       #
#####################################
# Scat time
scatterplotMatrix(wine[2:6])

#####################################
# Exercise 2c                       #
#####################################
library(MASS)
wine.lda <- lda(Type ~ . , data = wine)
wine.lda
# Wine.lda finds the two best discriminant functions

# The function expresses the LDA of features.

#####################################
# Exercise 2d                       #
#####################################
wine.lda.values <- predict(wine.lda)
ldahist(data = wine.lda.values$x[, 1], g= wine$Type)
ldahist(data = wine.lda.values$x[, 2], g= wine$Type)

# In the plot the two best discriminant functions are plotted on 
# each axis to determine which function seperates the fgroups best.

#####################################
# Exercise 2e                       #
#####################################
plot(wine.lda.values$x[, 1], wine.lda.values$x[, 2])
text(wine.lda.values$x[, 1], wine.lda.values$x[, 2], wine$Type, cex=0.7, pos=4, col="red")

# Scatterplot of the two best discriminant functions functions, with
# the data points labelled by cultivar.