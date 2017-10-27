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
# The four variables are:
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

# As we can see we need to standardize the data
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
# Dissimilarity matrix using euclidean distance formula
d <- dist(df, method = "euclidean")

# Ward.D2 method
hc.D2 <- hclust(d, "ward.D2")
plot(hc.D2)

# Just some extra linkages.
# Complete linkage
hc.complete <- hclust(d, "complete")
plot(hc.complete)

# Single linkage
hc.single <- hclust(d, "single")
plot(hc.single)

# Avg linkage
hc.avg <- hclust(d, "average")
plot(hc.avg)

#####################################
# Exercise 2e                       #
#####################################
# AGNES and DIANA in action

# AGNES
res.agnes <- agnes(df, method = "ward")
pltree(res.agnes, main = "Dendogram for AGNES")

# DIANA
res.diana <- diana(df)
pltree(res.diana, main = "Dendogram for DIANA")

#####################################
# Exercise 2f                       #
#####################################
# Time to cut the tree using cutree, 
# this will truncate the tree and give us some amount of clusters
grp <- cutree(hc.D2, k = 4)
table(grp)

# Plot the new cut dendogram
plot(hc.D2, cex = 0.6)
# Draw rectangles around the clusters
rect.hclust(hc.D2, k = 4, border = 2:5)

#####################################
# Exercise 2g                       #
#####################################
# Compare dendograms

# Create two dendrograms
dend1 <- as.dendrogram (res.agnes)
dend2 <- as.dendrogram (res.diana)

# Put them in a list
dend_list <- dendlist(dend1, dend2)
plot(dend_list)
tanglegram(dend1, dend2)

#####################################
# Exercise 2h                       #
#####################################
# Explore the silhouette function, plot it.
silhouette(grp,d)
