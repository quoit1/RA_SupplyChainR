#Import data in R
library(readr)

retail_trans <-read.csv("online_retail2.csv")

head(retail_trans, n=3)
tail(retail_trans, n=4)

# to have general info about the structure of dataframe

str(retail_trans)

# number of rows & columns

nrow(retail_trans)
ncol(retail_trans)

# names of the columns

names(retail_trans)

# change column name

names(retail_trans)[7]<-"Customer_id"

# summary statistics of the data (good for continuous variable)

summary(retail_trans)

# categorical values summary - unique

unique(retail_trans$Country)

# information on categorical value - table (table can take max of 2 columns)

table(retail_trans$Country)

# Selecting data in dataframe 

sample_data <- retail_trans[1:5,] #(row 1 to 5 and ALL columns)

# new dataframe with col 1 and 4 -->Subsetting

invoice_quantitiy <- retail_trans[,c(1,4)] #Method with vector notation
head(invoice_quantitiy)

head(retail_trans[,c("Invoice","Quantity")]) #Method with column names

# Filtering using base method (advance method is with dplyr)
# How to get observations from France

unique(retail_trans$Country)

france_dataset<-retail_trans[retail_trans$Country == 'France',]
nrow(france_dataset)

# Negative quantities

negative_data<-retail_trans[retail_trans$Quantity <= 0,]

# Remove negative data. Modifying the existing data! If Quality is less than 
# equal to zero, make it zero!

retail_trans$Quantity[retail_trans$Quantity <= 0] <-0 
                                                      
summary(retail_trans)

# Making an if function: Operations function

operations_function <- function(x,y){
  #Do something
}

# For-loop on dataframe

head(retail_trans)

first_10_rows<- retail_trans[1:10,c("Invoice","Country")]

first_10_rows$uk_or_not <-ifelse(first_10_rows$Country == "United Kingdom", TRUE, FALSE)
first_10_rows[first_10_rows$uk_or_not == "FALSE"]

for (i in 1:nrow(first_10_rows)){
  print(first_10_rows$Country[i])
}

# Applying function on dataframe

uk_function <-function(x){
  if(x == 'United Kingdom'){
    TRUE
  }else {
    FALSE
  }
}

uk_function("France")
uk_function("United Kingdom")

first_10_rows

for (i in 1:nrow(first_10_rows)){
  first_10_rows$uk[i]<-uk_function(first_10_rows$Country[i])
}
