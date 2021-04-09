rm(list = ls())
library(ggplot2)
library(corrplot)
library(scales)
library(mice) 
library(caret)
library(class)
library(dplyr)
#library(skimr)


data <- read.csv(file = "in-vehicle-coupon-recommendation.csv", header = TRUE)
data[data==""] <- NA

# remove feature 'car' which has excessive amount of missing values
sum(is.na(data$car))/length(data$car)
data = data[,-which(colnames(data)=="car")]

# remove feature 'direction_opp'
sum(data$direction_opp)+sum(data$direction_same) == dim(data)[1]
data = data[,-which(colnames(data)=="direction_opp")]

# combine these three features as one factor with three levels
data <- mutate(data, toCoupon = toCoupon_GEQ5min + toCoupon_GEQ15min + toCoupon_GEQ25min)
toCoupon_index = c(which(colnames(data)=="toCoupon_GEQ5min"),which(colnames(data)=="toCoupon_GEQ15min"),
                   which(colnames(data)=="toCoupon_GEQ25min") )
data = data[,-toCoupon_index]
data <- data[,c(1:20,22,21)]


# turn types of features to factor
for (i in 1:ncol(data)){
  data[,i] = as.factor(data[,i])
}
str(data)

# proportions of Y
table(data$Y)
round(prop.table(table(data$Y)),digits = 4) 

# plots
ggplot(data,aes(Y))+geom_bar(fill = hue_pal()(4)[3], width = 0.6) 



# missing values

md.pattern(data) # show the table of missing values
missing_rate = 1-sum(complete.cases(data))/sum(nrow(data)) # missing rate
missing_rate
missingProportion <- function(x){sum(is.na(x))/length(x)} # function that calculates proportion of missing values
round(sapply(data,missingProportion),digits = 4) # missing rate for each feature
# find which instance has missing value:
whichAreMissing = function(x) { which(is.na(x)) }
missingIndices = sapply(data,whichAreMissing)

