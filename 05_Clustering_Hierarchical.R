#################################################
# Exercise 1a to 1c is uploaded to Google drive #
#################################################

#####################################
# Exercise 2a                       #
#####################################
# Get familiar with 'cluster' and 'dendextend' libraries.
# What can they be used for. (Check out the links.)

#install.packages("cluster")
#install.packages("dendextend")
library(cluster)
# https://cran.r-project.org/web/packages/cluster/cluster.pdf
library(dendextend)
# https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html

#####################################
# Exercise 2b                       #
#####################################
# What are the observations and what are the variables?

# Observations are the 50 state of Unitied States of America
# The four observations are:
#       - [,1] 	Murder 	numeric 	Murder arrests (per 100,000)
#       - [,2] 	Assault 	numeric 	Assault arrests (per 100,000)
#       - [,3] 	UrbanPop 	numeric 	Percent urban population
#       - [,4] 	Rape 	numeric 	Rape arrests (per 100,000)

#####################################
# Exercise 2c                       #
#####################################
# Do some descriptive statistics such as SD maybe a scatterplot too.

# Set working directory to where the dataset is
setwd("~/Documents/rprojects/dataScience/DataFiles")

# Import the dataset
data("USArrests")
head(USArrests)
summary(USArrests)

# As we can see we need to normalize the data
df <- scale(USArrests)
df <- na.omit(USArrests)
# And now if we look at the head
head(df) # Boom scaled! Also Scaled is not the same as normalize, normalize is between 0 and 1.

# Paired scatterplot
require(graphics)
pairs(df, panel = panel.smooth, main = "USArrests data")

#####################################
# Exercise 2d                       #
##############################################################
# Source: http://www.sthda.com/english/wiki/print.php?id=237 #
##############################################################
# Dissimilarity matrix
# Found a kickass library for that but lets do it as stated in the paper as the extra library does
# not contain ward.D2

# install.packages("factoextra")
# library(factoextra)
# ward.D2 distance method
hc.D2 <- hclust(dist(df), "ward.D2")
plot(hc.D2)

# Just some extra linkages.
# Complete linkage
hc.complete <- hclust(dist(df), "complete")
plot(hc.complete)

# Single linkage
hc.single <- hclust(dist(df), "single")
plot(hc.single)

# Avg linkage
hc.avg <- hclust(dist(df), "average")
plot(hc.avg)


#####################################
# Exercise 2e                       #
#####################################







