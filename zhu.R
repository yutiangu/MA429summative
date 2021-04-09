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
                  temperature = as.factor(temperature),
                  time = as.factor(time),
                  coupon = as.factor(coupon),
                  expiration = as.factor(expiration),
                  gender = as.factor(gender),
                  age = as.factor(age),
                  maritalStatus = as.factor(maritalStatus),
                  has_children = as.factor(has_children),
                  education = as.factor(education),
                  occupation = as.factor(occupation),
                  Bar = as.factor(Bar),
                  CoffeeHouse = as.factor(CoffeeHouse),
                  CarryAway = as.factor(CarryAway),
                  RestaurantLessThan20 = as.factor(RestaurantLessThan20),
                  Restaurant20To50 = as.factor(Restaurant20To50),
                  toCoupon = ifelse(toCoupon_GEQ25min == 1, "25min", ifelse(toCoupon_GEQ15min == 1, "15min", ifelse(toCoupon_GEQ5min == 1, "5min", NA))),
                  toCoupon = as.factor(toCoupon),
                  direction_same = as.factor(direction_same),
                  Y = as.factor(Y)) 
data <- subset(data, select=-c(car, direction_opp, toCoupon_GEQ25min, toCoupon_GEQ15min, toCoupon_GEQ5min))


data$temperature = factor(data$temperature, order=T)
data$time = factor(data$time, order = T,levels = c("7AM", "10AM", "2PM", "6PM", "10PM"))
data$expiration = factor(data$expiration, order = T, levels = c("2h", "1d"))
data$age = factor(data$age, order = T, levels = c("below21", "21", "26", "31", "36", "41", "46", "50plus"))
data$education = factor(data$education, order = T, levels = c("Some High School","High School Graduate","Some college - no degree","Associates degree","Bachelors degree","Graduate degree (Masters or Doctorate"))
data$income = factor(data$income, order = T, levels = c("Less than $12500 ","$12500 - $24999","$25000 - $37499","$37500 - $49999","$50000 - $62499","$62500 - $74999","$75000 - $87499","$87500 - $99999","$100000 or More"))
data$Bar = factor(data$Bar, order = T, levels = c("0", "1~3", "4~8", "gt8"))
data$CoffeeHouse = factor(data$CoffeeHouse, order = T, levels = c("0", "1~3", "4~8", "gt8"))
data$CarryAway = factor(data$CarryAway, order = T, levels = c("0", "1~3", "4~8", "gt8"))
data$RestaurantLessThan20 = factor(data$RestaurantLessThan20, order = T, levels = c("0", "1~3", "4~8", "gt8"))
data$Restaurant20To50 = factor(data$Restaurant20To50, order = T, levels = c("0", "1~3", "4~8", "gt8"))
data$toCoupon = factor(data$toCoupon, order = T, levels = c("5min","15min","25min"))

factor(data$toCoupon)

sapply(data, class)

missingProportion <- function(x){sum(is.na(x))/length(x)}
round(sapply(data,missingProportion),digits = 4) # missing rate for each column

lm = glm(Y ~ ., data=data, family="binomial")
summary(lm)
