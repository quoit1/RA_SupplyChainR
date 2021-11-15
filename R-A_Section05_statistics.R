skus <- read.csv("Story_of_three_Skus.csv")

head(skus)

#Calculate the average using "apply" --> 1=rows, 2=columns

average <-apply(skus[c(2:4)],2,mean)
summary(skus)

#Calculate median
median_skus<-apply(skus[,2:4],2,median)

#Calculate mode using modeest library --> "mlv" most likely value
library(modeest)
mode_skus<-apply(skus[,2:4],2,mlv)

#sd, var, range

mode_sd<-apply(skus[,2:4],2,sd)
mode_var<-apply(skus[,2:4],2,var)
range_skus<-apply(skus[,2:4],2,function(x){max(x)-min(x)})
min1<-apply(skus[,2:4],2,min)
max1<-apply(skus[,2:4],2,max)
iqr1<-apply(skus[,2:4],2,IQR)

statistical_data <- data.frame(SD=mode_sd,
                               Variance=mode_var,
                               range=range_skus,
                               max=max1,
                               min=min1,
                               IQR=iqr1,
                               mean=average,
                               median=median_skus,
                               mode=mode_skus)

#Co-efficient of variation = (S/Mean)*100
#Tell us how the distribution of data is, high or narrow.
#cv of more than 0.5 means data is spread, less than 0.5 means data is less 
#variable (stable demand). Used in forecasting!

statistical_data$cv <- statistical_data$SD / statistical_data$mean

####correlations

cars <- read.csv("cars.csv")

head(cars)

colnames(cars)

correlation <- cor(cars[,c(13,14)])

#to check for NAN values USE isna()
sum(is.na(cars)==TRUE) #total of 86 NAN

sum(is.na(cars$city_miles_per_galloon == TRUE))

# you can use na.omit will remove the entire row where there is NAN!
# you can use impute to change the value of NAN to e.g mean 

nrow(cars) #428 rows

# remove using na.omit

cars_clean<-na.omit(cars)

nrow(cars_clean) #387 rows left!

# which rows has NAs in a column? use WHICH function

na_rows_city<-which(is.na(cars$city_miles_per_galloon==TRUE))

na_rows_city #27  29  30  74  78  79 183 241 242 259 284 300 370 419

colnames(cars_clean)
cor(cars_clean[,c(13,14)]) #all rows, 13 & 14 column sonly

# correlation matrix

cor_matrix <- cor(cars_clean[,c(9,13,14,16,19)])

#install.packages("corrplot") #plotting correlation matrix
library(corrplot)

corrplot(cor_matrix, method='circle')

corrplot(cor_matrix, method='number')

#### Outliers (1.5*IQR rule)

sales <- c(5,8,10,20,100,2,65,18,32,25,200,9,15)
iqr_sales <-IQR(sales)

first <- quantile(sales, probs = 0.25)
third <- quantile(sales, probs = 0.75)

upper_threshold <- third + 1.5*iqr_sales
lower_threshold <- first - 1.5*iqr_sales

#simple function to detect outliers
outlier_function <-function(x){
  
  iqr <-IQR(x)
  first <- quantile(x, probs = 0.25)
  third <- quantile(x, probs = 0.75)
  upper_threshold <- third + 1.5*iqr
  lower_threshold <- first - 1.5*iqr
  
  outliers<-list(upper_outliers=x[x>upper_threshold], 
                                  lower_outliers=x[x<lower_threshold])
  return (outliers)
  
}

outlier_function(sales)

#### Linear regression
#install.packages('readxl')
library(readxl)
pricing <- read_excel("pricing.xlsx")

pricing <- pricing[1:9,] #drop last row which has NA

model<-lm(Demand~Price, data=pricing)

summary(model) 

#p-value of less than 0.05 means the two variables are statistically
#significant (also shown by three stars). High adjusted R-squared means 
#that the variables are explaining well the outcomes. Higher the 
#R-squared, higher will be the performance of your model. 

sample_data <-data.frame(Price=c(5,8,9,15), Demand=c(rep(NA,4)))

#above we are using rep function to repeat NA four times

#Predict Demand on sample_data from our model from before

sample_data$Demand <- predict(model,sample_data)

#### Distributions
#In supply chain, you use distribution to model for uncertainty

#chi-square test on bike demand
# remember for chi-square, the null hypothesis is that the data
# follows specified distribution. If p-value is high, null flys - 
# i.e cannot reject null hypothesis and data does follow some 
# distribution

# install.packages(c("gamlss","gamlss.dist","gamlss.add"))
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

sku_distributions <- read.csv("sku_distributions.csv")
sku_distributions <- sku_distributions[,c(1:4)]

summary(sku_distributions)

# initial assumption by looking at min and max values, is that
# grape_juice and cantalope_juice are discrete and 
# apple_juice is continuous distribution. To test for this, we use
# gamlss package BUT it takes data in the form of vector. To 
# create a vector we use double square brackets!

apple <- as.vector(sku_distributions[["apple_juice"]])
grape <-as.vector(sku_distributions[["grape_juice"]])
cantalop <- as.vector(sku_distributions[["cantalop_juice"]])

# gamlss will look at the data and check which distribution is better fit
# based on certain criteria. Use help in RStudio and search for fitDist()
# "realAll" is for continuous type, "count" is for discrete type
# k=3.84 corresponds to p-value selection

apple_dist <- fitDist(apple,k=3.84, try.gamlss = TRUE, type="realAll", 
                      trace=FALSE)

# note: ignore errors/warning and run apple_dist to see result
# we see that the best fit for apple data is normal distribution

grape_dist <- fitDist(grape,k=3.84, try.gamlss = TRUE, type="counts", 
                      trace=FALSE)

# above best fit is Poisson dist with lamda of 0.678. 
# you could argue that cantalop is continuous distribution. Use
# table (contingency table) to check for data 
# we see only six different observations, not very dispersed data
# intermittent data = use "count", smooth/regular data = use "realAll"

table(sku_distributions$apple_juice)

cantalop_dist <- fitDist(cantalop,k=3.84, try.gamlss = TRUE, type="count", 
                      trace=FALSE)

#what should be the apple quantity should at any given day to 
# cover 90% demand? QNORM

average <-mean(apple)
sd <- sd(apple)
cover_90 <-qnorm(0.90, mean=average, sd=sd)

# what about grape juice
average_grape <-mean(grape)
cover_90_grape_poisson <- qpois(0.90,lambda = average_grape)

cover_90_grape_normal <- qnorm(0.90, mean = average_grape, sd = sd(grape))

####Section 5 assignment
pineapple <- read.csv("pinapple_juice.csv")

summary(pineapple)
#table(pineapple$Price)
colnames(pineapple)

pineapple_demand <- as.vector(pineapple[["Pinapple.juice"]])
pineapple_dist <- fitDist(pineapple_demand, type = "realAll", k=3.84)

summary(pineapple_dist)

pine_model <- lm(Pinapple.juice~Price, pineapple)
pine_model$coefficients
