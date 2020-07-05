#setting the working directory as the current folder
setwd("C:/Users/Nivedita/Desktop/google-play-store-apps/30_PlayStoreApps/")

#checking the working directory
getwd()

#reading the csv file into the variable data
data <- read.csv(file = '30_PlayStoreApps.csv')

#does any field have NaN value or missing value in the dataset
any(is.na(data))

#printing the number of rows and colmns of data set
dim(data)

#sum of all the unavailable values
sum(is.na(data))

#removing all the missing values from the entire dataset
data<-na.omit(data)

#number of unavailable values
sum(is.na(data))

#checking for NaN values again
any(is.na(data))

#All the value in the datatset are above 0 that is data is clean
#Now Performing EDA(Extratory Data Analysis) on the dataset
#To perform EDA we rquire some packages to be installed, Now installing the require package
#install.packages("dplyr")
#install.packages("moments")
#install.packages("Hmisc")
#install.packages("DataExplorer")#not availabe for r version 3.4.4
# Summary Analysis
# An approach to unearth, summarize
# and visualize the important characteristics of a dataset.
#dimensions of data after removing all missing values
dim(data)

#viewing column names of the dataset
names(data)

#viewing the first six rows of the csv file
head(data)

#viewing the last six rows of the csv file
tail(data)

#printing the class of the dataset
class(data)

#summary of the dataset
summary(data)

#mean of all the ratings
mean(data$Rating)

#median of all ratings
median(data$Rating)

#checking data type of the column Reviews
class(data$Reviews)

#converting Reviews to numeric type
data$Reviews<-as.numeric(data$Reviews)
class(data$Reviews)

#checking the range minimum and maximum of a column object
range(data$Reviews)
range(data$Rating)
range(data$Min_Installs)
range(data$Price_dollars)

#finding mean number of reviews on every app
mean(data$Reviews)

#finding mean number of installs on google play store
mean(data$Min_Installs)

#finding mean number of price of applications
mean(data$Price_dollars)


#finding median of number of rows
median(data$Reviews)

#finding median number of mininum installs
median(data$Min_Installs)

#finding median number of price of all apps
median(data$Price_dollars)

#standard deviation of price
sd(data$Price_dollars)

#Descriptive Analysis
#Extension of Summary Analylsis.
#Skewness is a measure of the asymmetry of the probability distribution of a real-valued random variable about its mean.
#The skewness value can be positive or negative, or undefined.
a<-data$Rating
library(moments)
skewness(a)
b<-data$Reviews
skewness(b)
c<-(data$Price_dollars)
skewness(c)
d<-data$Min_Installs
skewness(d)

#application with the highest price
data$App[which(data$Price_dollars==max(data$Price_dollars))]

#applications with highest number of installs
data$App[which(data$Min_Installs==max(data$Min_Installs))]

#applications with lowest number of installs
data$App[which(data$Min_Installs==min(data$Min_Installs))]

#Applications with maximum rating
data$App[which(data$Rating==max(data$Rating))]

#Applications with minimum rating
data$App[which(data$Rating==min(data$Rating))]

#Applications with maximum reviews
data$App[which(data$Reviews==max(data$Reviews))]

#Applications with minimum reviews
data$App[which(data$Reviews==min(data$Reviews))]

#Performing Regression analysis on the above dataset
#Performing regression on dataset
#Graphical analysis
#Linera regression
#Regression analysis is a very widely used statistical tool to establish a relationship model between two variables.
#One of these variable is called predictor variable whose value is gathered through experiments.
#The other variable is called response variable whose value is derived from the predictor variable.
#In Linear Regression these two variables are related through an equation, where exponent (power) of both these variables is
#1. Mathematically a linear relationship represents a straight line when plotted as a graph. A non-linear relationship where the exponent of any variable is not equal to 1 creates a curve.
x<-data$Rating
y<-data$Min_Installs

#lm() Function
#This function creates the relationship model between the predictor and the response variable.

result1 = lm(Rating~Price_dollars, data=data)
result1

result2 = lm(Reviews~Rating, data=data)
result2

result3 = lm(Min_Installs~Rating, data=data)
result3

#QPLOT
x<-data$Rating
y<-data$Price_dollars
plot(y,x,col="blue",main = "Playstore", abline(lm(x~y)),cex=1.0, pch=16, ylab ="Ratings", xlab = "price in dollars")

x<-data$Rating
y<-data$Min_Installs
plot(y,x,col="red",main = "Sales", abline(lm(x~y)),cex=1.0, pch=16, xlab ="Other Sales", ylab = "Global Sales")

#pie Chart with column colour as rainbow
a2<-data$Category[1:5]
pie(c(data$Category[1:5]),labels =data$Category[1:5], col = rainbow(length(a2)) ,main="Category")

#histogram of dataset
hist(c(data$Rating) ,xlab = "Rating",col = "yellow",border = "black")
hist((data$Price_dollars) ,xlab = "Price in dollars",col = "green",border = "brown")

#GGPlot
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)


#Ggplp  ot for bar graph on Rating
ggplot(data=data,aes(x=Rating))+ geom_bar(color = "purple", fill ="pink")


#Ggplot Geometry points for Rating and Reviews color based on the Category
ggplot(data=data, aes(x=Category, y=Rating, color= Category))+geom_line()
ggplot(data=data,aes(x=Rating, y=Reviews, color= Category))+geom_line()


#Ggplot Geometry points for Rating and price color based on the Type
ggplot(data=data, aes(x=Price_dollars, y=Rating, color= Type))+geom_line()

#Ggplot Boxplot for Rating and price color based on type
ggplot(data = data, mapping = aes(y = Rating, x = Price_dollars, color=Type))+geom_boxplot()
