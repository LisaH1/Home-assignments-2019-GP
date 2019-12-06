
######################### Geoffrey Patching - assignment 2 #########################


### author: Lisa Holzer
### date:   3rd December 2019


#Task: 18 students rated global similarity of different pairs of nations on a 9-point rating scale ranging from `1=very different' to `9=very similar'. Mean rating is shown in data set.
#Use multidimensional scaling to examine the students' perceived dissimilarities between the nations.


#load packages
require(readxl)
require(psych)
require(lsr)
require(tidyverse)
require(car)
require(olsrr)
require(ggpubr)
require(factoextra)
require(ggpubr)
require(ggplot2)
require(smacof)


##set working directory
setwd("C:/Users/holze/Desktop/Lisa/Uni/Schweden/PSYP13/home assignments")
source("GraphPlot.R")



#################### data diagnostics ####################



##load data and look at it
Nations <- read.delim("C:/Users/holze/Desktop/Lisa/Uni/Schweden/PSYP13/Data R/Nations.txt")
View(Nations)
summary(Nations)
describe(Nations)
str(Nations)


##adjust row-names
row.names(Nations) <- c("Brazil", "Congo", "Cuba", "Egypt", "France", "India", "Israel", "Japan", "China", "UdSSR", "USA", "Yugoslavia")


## convert similarities to dissimilarities
Nations_new <- sim2diss(Nations, method = 9) 
Nations_new
view(Nations_new)



#################### multidimensional scaling (MDS) ####################



##execute MDS, non-metrical
Nations_MDS <- mds(Nations_new, type = "ordinal")
Nations_MDS 

names(Nations_MDS) #Output Objects


##access matrix of the distances among the points of the MDS solution
Nations_MDS$confdist 
round(Nations_MDS$confdist, 2) 


##plots
windows()
plot(Nations_MDS)
windows()
plot(Nations_MDS, plot.type = "Shepard")
windows()
plot(Nations_MDS, plot.type = "bubbleplot")
windows()
plot(Nations_MDS, plot.type = "stressplot")
