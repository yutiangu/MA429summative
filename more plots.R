library(data.table)
library(dplyr)
library(janitor)
library(fastDummies)
library(ggplot2)

data<-read.csv(file = "/Users/doris/Desktop/MA429/summative/in-vehicle-coupon-recommendation.csv", header = TRUE)
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
           age == "below21" ~ 18/80,
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

#destination vs Y
data_srz1 <- as.data.frame(table(data$destination,data$Y))
data_srz1
ggplot(data_srz1, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity")+theme_minimal()+ggtitle("Y count with destination")

#occupation vs Y
data_srz2 <- as.data.frame(table(data$occupation,data$Y))
data_srz2
ggplot(data_srz2, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity")+theme_minimal()+ggtitle("Y count with occupation")

#passenger vs Y
data_srz3 <- as.data.frame(table(data$passenger,data$Y))
data_srz3
ggplot(data_srz3, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity")+theme_minimal()+ggtitle("Y count with passenger")

#weather vs Y
data_srz4 <- as.data.frame(table(data$weather,data$Y))
data_srz4
ggplot(data_srz4, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity")+theme_minimal()+ggtitle("Y count with weather")

#gender vs Y
data_srz5 <- as.data.frame(table(data$gender,data$Y))
data_srz5
ggplot(data_srz5, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity")+theme_minimal()+ggtitle("Y count with gender")

#maritalStatus vs Y
data_srz6 <- as.data.frame(table(data$maritalStatus,data$Y))
data_srz6
ggplot(data_srz6, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity")+theme_minimal()+ggtitle("Y count with maritalStatus")



