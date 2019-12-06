
######################### Geoffrey Patching - assignment 1 #########################


### author: Lisa Holzer
### date:   6th December 2019



#load packages
require(psych)
require(reshape2)
require(ggplot2)
require(stats)
require(FactoMineR)
require(stats)


##set working directory
setwd("C:/Users/holze/Desktop/Lisa/Uni/Schweden/PSYP13/home assignments")
source("GraphPlot.R")



#################### data diagnostics ####################



##load data and look at it
PAQ_Lisa <- read.csv("C:/Users/holze/Desktop/Lisa/Uni/Schweden/PSYP13/Data R/PAQ_Lisa.txt", sep="")
View(PAQ_Lisa)
summary(PAQ_Lisa)
describe(PAQ_Lisa)
str(PAQ_Lisa)


##clean and organize data
data_organized<-dcast(PAQ_Lisa, id~var) #reorder rows and columns
View(data_organized)
summary(data_organized)


#replace NA's with mean
x1<-round(mean(data_organized$Q4_freeze, na.rm=TRUE), digits=0)
mean(data_organized$Q4_freeze, na.rm=TRUE)
data_organized$Q4_freeze[is.na(data_organized$Q4_freeze)]=3

x2<-round(mean(data_organized$Q5_alien, na.rm=TRUE), digits=0)
mean(data_organized$Q5_alien, na.rm=TRUE)
data_organized$Q5_alien[is.na(data_organized$Q5_alien)]=3

x3<-round(mean(data_organized$Q7_weep, na.rm=TRUE), digits=0)
mean(data_organized$Q7_weep, na.rm=TRUE)
data_organized$Q7_weep[is.na(data_organized$Q7_weep)]=3

summary(data_organized)

#check distribution of data with histograms
hist(data_organized$Q1_cry, breaks= c(0:5))
hist(data_organized$Q2_help, breaks= c(0:5))
hist(data_organized$Q3_breathe, breaks= c(0:5))
hist(data_organized$Q4_freeze, breaks= c(0:5))
hist(data_organized$Q5_alien, breaks= c(0:5))
hist(data_organized$Q6_inferior, breaks= c(0:5))
hist(data_organized$Q7_weep, breaks= c(0:5))
hist(data_organized$Q8_Support, breaks= c(0:5)) #high "strongly disagree"
hist(data_organized$Q9_Nerd, breaks= c(0:5))

#exclude ID, age, sex
data_pca<- data_organized[, c(3:11)]



#################### principal component analysis (PCA) ####################



##PCA covariance 
data_pcacov <- princomp(data_pca, cor = FALSE)
summary(data_pcacov, loadings=TRUE)

plot(data_pcacov)

#or

##PCA correlation 
data_pcacor <- princomp(data_pca, cor = TRUE)
summary(data_pcacor, loadings=TRUE)

windows()
plot(data_pcacor)



#################### create graphs ####################



#biplot
windows()
biplot(data_pcacor, col = c("light grey", "dark blue"))
windows()
biplot(data_pcacor, choices = c(1,3), scale = 0, col = c("light grey", "dark blue"))
windows()
biplot(data_pcacor, choices = c(2,3), scale = 0, col = c("light grey", "dark blue"))


#scree-plot
windows()
plot(data_pcacor$sdev^2, xlab = "Component number",ylab = "Component variance", type = "l", main = "Scree diagram", xaxt = 'n')
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9"))


#log-eigenvalue diagram
#cut-off 0
windows()
plot(log(data_pcacor$sdev^2), xlab = "Component number",ylab = "Component variance", type = "l", main = "log(Eigenvector) diagram", xaxt = 'n')
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) #gives you the labels of the components


windows()
data_pca_pca<-PCA(data_pca)
windows()
data_pca_pca$eig
