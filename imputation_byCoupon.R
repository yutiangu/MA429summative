library(data.table)
library(dplyr)
library(janitor)
library(fastDummies)
library(mice)



rm(list = ls())
data <- fread("in-vehicle-coupon-recommendation.csv")
data[data==""] <- NA

data$CoffeeHouse[which(data$CoffeeHouse == "never" | data$CoffeeHouse == "less1")] <-"0"
data$Bar[which(data$Bar == "never" | data$Bar == "less1")] <-"0"
data$CarryAway[which(data$CarryAway == "never" | data$CarryAway == "less1")] <-"0"
data$RestaurantLessThan20[which(data$RestaurantLessThan20 == "never" | data$RestaurantLessThan20 == "less1")] <-"0"
data$Restaurant20To50[which(data$Restaurant20To50 == "never" | data$Restaurant20To50 == "less1")] <-"0"

data <- data %>%
  mutate(destination = as.factor(destination),
         passanger = as.factor(passanger),
         weather = as.factor(weather),
         temperature = as.numeric(temperature)/100,
         time = case_when(
           time == "7AM" ~ 7/24,
           time == "10AM" ~ 10/24,
           time == "2PM" ~ 14/24,
           time == "6PM" ~ 18/24,
           time == "10PM" ~ 22/24),
         coupon = as.factor(coupon),
         expiration = case_when(
           expiration == "1d" ~ 1,
           expiration == "2h" ~ 2/24),
         gender = as.factor(gender),
         age = case_when(
           age == "below21" ~ 11/80,
           age == "21" ~ 23/80,
           age == "26" ~ 28/80,
           age == "31" ~ 33/80,
           age == "36" ~ 38/80,
           age == "41" ~ 43/80,
           age == "46" ~ 48/80,
           age == "50plus" ~ 66/80),
         maritalStatus = as.factor(maritalStatus),
         has_children = as.factor(has_children),
         education = case_when(
           education == "Some High School" ~ 11/20,
           education == "High School Graduate" ~ 13/20,
           education == "Some college - no degree" ~ 14/20,
           education == "Associates degree" ~ 15/20,
           education == "Bachelors degree" ~ 17/20,
           education == "Graduate degree (Masters or Doctorate)" ~ 20/20),
         income = case_when(income == "Less than $12500" ~ 1/8 * 0.7,
                            income == "$12500 - $24999" ~ 2/8 * 0.7,
                            income == "$25000 - $37499" ~ 3/8 * 0.7,
                            income == "$37500 - $49999" ~ 4/8 * 0.7,
                            income == "$50000 - $62499" ~ 5/8 * 0.7,
                            income == "$62500 - $74999" ~ 6/8 * 0.7,
                            income == "$75000 - $87499" ~ 7/8 * 0.7,
                            income == "$87500 - $99999" ~ 8/8 * 0.7,
                            income == "$100000 or More" ~ 1),
         occupation = as.factor(occupation),
         CoffeeHouse = case_when(CoffeeHouse == "0" ~ 0,
                                 CoffeeHouse == "1~3" ~ 2/8,
                                 CoffeeHouse == "4~8" ~ 6/8,
                                 CoffeeHouse == "gt8" ~ 8/8),
         CarryAway = case_when(CarryAway == "0" ~ 0,
                               CarryAway == "1~3" ~ 2/8,
                               CarryAway == "4~8" ~ 6/8,
                               CarryAway == "gt8" ~ 8/8),
         RestaurantLessThan20 = case_when(RestaurantLessThan20 == "0" ~ 0,
                                          RestaurantLessThan20 == "1~3" ~ 2/8,
                                          RestaurantLessThan20 == "4~8" ~ 6/8,
                                          RestaurantLessThan20 == "gt8" ~ 8/8),
         Restaurant20To50 = case_when(Restaurant20To50 == "0" ~ 0,
                                      Restaurant20To50 == "1~3" ~ 2/8,
                                      Restaurant20To50 == "4~8" ~ 6/8,
                                      Restaurant20To50 == "gt8" ~ 8/8),
         Bar = case_when(Bar == "0" ~ 0,
                         Bar == "1~3" ~ 2/8,
                         Bar == "4~8" ~ 6/8,
                         Bar == "gt8" ~ 8/8),
         toCoupon = ifelse(toCoupon_GEQ25min == 1, 25/25, ifelse(toCoupon_GEQ15min == 1, 15/25, ifelse(toCoupon_GEQ5min == 1, 5/25, NA))),
         direction_same = as.factor(direction_same),
         Y = as.factor(Y)) 

data <- subset(data, select=-c(car, direction_opp, toCoupon_GEQ25min, toCoupon_GEQ15min, toCoupon_GEQ5min))

# split train and test
coupon_bar <- filter(data, data$coupon == "Bar")
coupon_carry <- filter(data, data$coupon == "Carry out & Take away")
coupon_coffee <- filter(data, data$coupon == "Coffee House")
coupon_restaurant20 <- filter(data, data$coupon == "Restaurant(<20)")
coupon_restaurant2050 <- filter(data, data$coupon == "Restaurant(20-50)")

set.seed(1)
test_indices_bar = sample(1:nrow(coupon_bar),size = 0.2*nrow(coupon_bar),replace = FALSE)
test_indices_carry = sample(1:nrow(coupon_carry),size = 0.2*nrow(coupon_carry),replace = FALSE)
test_indices_coffee = sample(1:nrow(coupon_coffee),size = 0.2*nrow(coupon_coffee),replace = FALSE)
test_indices_restaurant20 = sample(1:nrow(coupon_restaurant20),size = 0.2*nrow(coupon_restaurant20),replace = FALSE)
test_indices_restaurant2050 = sample(1:nrow(coupon_restaurant2050),size = 0.2*nrow(coupon_restaurant2050),replace = FALSE)

train_bar = as.data.frame(coupon_bar[-test_indices_bar,-c(6,21)]);test_bar = as.data.frame(coupon_bar[test_indices_bar,-c(6,21)])
train_carry = as.data.frame(coupon_carry[-test_indices_carry,-c(6,21)]);test_carry = as.data.frame(coupon_carry[test_indices_carry,-c(6,21)])
train_coffee = as.data.frame(coupon_coffee[-test_indices_coffee,-c(6,21)]);test_coffee = as.data.frame(coupon_coffee[test_indices_coffee,-c(6,21)])
train_restaurant20 = as.data.frame(coupon_restaurant20[-test_indices_restaurant20,-c(6,21)]);test_restaurant20 = as.data.frame(coupon_restaurant20[test_indices_restaurant20,-c(6,21)])
train_restaurant2050 = as.data.frame(coupon_restaurant2050[-test_indices_restaurant2050,-c(6,21)]);test_restaurant2050 = as.data.frame(coupon_restaurant2050[test_indices_restaurant2050,-c(6,21)])


# imputation on each training and test set
# Bar:
train_impute = mice(train_bar, maxit=0)
meth = train_impute$method;meth
predM = train_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
train_imputed <- mice(train_bar, method = meth, predictorMatrix = predM, m = 5)
train_bar <- mice::complete(train_imputed)
sapply(train_bar, function(x) sum(is.na(x)))

test_impute = mice(test_bar, maxit=0)
meth = test_impute$method;meth
predM = test_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
test_imputed <- mice(test_bar, method = meth, predictorMatrix = predM, m = 5)
test_bar <- mice::complete(test_imputed)
sapply(test_bar, function(x) sum(is.na(x)))

# CarryAway:
train_impute = mice(train_carry, maxit=0)
meth = train_impute$method;meth
predM = train_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
train_imputed <- mice(train_carry, method = meth, predictorMatrix = predM, m = 5)
train_carry <- mice::complete(train_imputed)
sapply(train_carry, function(x) sum(is.na(x)))

test_impute = mice(test_carry, maxit=0)
meth = test_impute$method;meth
predM = test_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
test_imputed <- mice(test_carry, method = meth, predictorMatrix = predM, m = 5)
test_carry <- mice::complete(test_imputed)
sapply(test_carry, function(x) sum(is.na(x)))

# Coffee House:
train_impute = mice(train_coffee, maxit=0)
meth = train_impute$method;meth
predM = train_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
train_imputed <- mice(train_coffee, method = meth, predictorMatrix = predM, m = 5)
train_coffee <- mice::complete(train_imputed)
sapply(train_coffee, function(x) sum(is.na(x)))

test_impute = mice(test_coffee, maxit=0)
meth = test_impute$method;meth
predM = test_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
test_imputed <- mice(test_coffee, method = meth, predictorMatrix = predM, m = 5)
test_coffee <- mice::complete(test_imputed)
sapply(test_coffee, function(x) sum(is.na(x)))

# Restaurant(<20):
train_impute = mice(train_restaurant20, maxit=0)
meth = train_impute$method;meth
predM = train_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
train_imputed <- mice(train_restaurant20, method = meth, predictorMatrix = predM, m = 5)
train_restaurant20 <- mice::complete(train_imputed)
sapply(train_restaurant20, function(x) sum(is.na(x)))

test_impute = mice(test_restaurant20, maxit=0)
meth = test_impute$method;meth
predM = test_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
test_imputed <- mice(test_restaurant20, method = meth, predictorMatrix = predM, m = 5)
test_restaurant20 <- mice::complete(test_imputed)
sapply(test_restaurant20, function(x) sum(is.na(x)))

# Restaurant(20-50):
train_impute = mice(train_restaurant2050, maxit=0)
meth = train_impute$method;meth
predM = train_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
train_imputed <- mice(train_restaurant2050, method = meth, predictorMatrix = predM, m = 5)
train_restaurant2050 <- mice::complete(train_imputed)
sapply(train_restaurant2050, function(x) sum(is.na(x)))

test_impute = mice(test_restaurant2050, maxit=0)
meth = test_impute$method;meth
predM = test_impute$predictorMatrix
predM[, c("destination","passanger", "weather", "temperature", "time", "expiration","Bar", "CoffeeHouse", 
          "CarryAway", "RestaurantLessThan20","Restaurant20To50", "direction_same", "toCoupon")]=0
predM
test_imputed <- mice(test_restaurant2050, method = meth, predictorMatrix = predM, m = 5)
test_restaurant2050 <- mice::complete(test_imputed)
sapply(test_restaurant2050, function(x) sum(is.na(x)))


# complete data:
train_bar = cbind(train_bar,Y = coupon_bar[-test_indices_bar,]$Y)
train_carry = cbind(train_carry,Y = coupon_carry[-test_indices_carry,]$Y)
train_coffee = cbind(train_coffee,Y = coupon_coffee[-test_indices_coffee,]$Y)
train_restaurant20 = cbind(train_restaurant20,Y = coupon_restaurant20[-test_indices_restaurant20,]$Y)
train_restaurant2050 = cbind(train_restaurant2050,Y = coupon_restaurant2050[-test_indices_restaurant2050,]$Y)

test_bar = cbind(test_bar,Y = coupon_bar[test_indices_bar,]$Y)
test_carry = cbind(test_carry,Y = coupon_carry[test_indices_carry,]$Y)
test_coffee = cbind(test_coffee,Y = coupon_coffee[test_indices_coffee,]$Y)
test_restaurant20 = cbind(test_restaurant20,Y = coupon_restaurant20[test_indices_restaurant20,]$Y)
test_restaurant2050 = cbind(test_restaurant2050,Y = coupon_restaurant2050[test_indices_restaurant2050,]$Y)
