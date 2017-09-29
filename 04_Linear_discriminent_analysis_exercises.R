setwd("~/Documents/rprojects/dataScience")


# Defintion of Linear Discriminant Analysis:
#     Which feaure (attribute) can best help determine the classification of a data row(x)
#     The decisive attributes for classification


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
