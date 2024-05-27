# ---------------------- Project - 1 REAL ESTATE ---------------------------

# Setting up the working directory.
getwd()

housing_train = read.csv("housing_train.csv",stringsAsFactors = FALSE)
housing_test = read.csv("housing_test.csv",stringsAsFactors = FALSE)

housing_test$Price = NA
housing_train$data = 'train'
housing_test$data = 'test'
all_data = rbind(housing_train,housing_test)

library(dplyr)
glimpse(all_data)

#table(all_data$Suburb)
#round(tapply(all_data$Price,all_data$Suburb,mean),0)

#table(all_data$Address)
all_data = all_data %>% 
  mutate(
    short_address = gsub("/"," ",Address)
  ) %>% 
  select(-Address)


table(all_data$Type)
all_data = all_data %>% 
  mutate(
    h = as.numeric(Type=="h"),
    u = as.numeric(Type=="u")
  ) %>% 
  select(-Type)

glimpse(all_data)
View(all_data)


table(all_data$Method)
all_data = all_data %>% 
  mutate(
    Method_PI = as.numeric(Method=="PI"),
    Method_S = as.numeric(Method=="S"),
    Method_SP = as.numeric(Method=="SP"),
    Method_VB = as.numeric(Method=="VB")
  ) %>% 
  select(-Method)

glimpse(all_data)

#table(all_data$SellerG)
#all_data = all_data %>%
#  select(-SellerG,-Suburb,-short_address) 

table(all_data$Postcode)

counts_vector = table(all_data$CouncilArea)

all_data = all_data %>%
  mutate(
    CouncilAreaCount = ifelse(CouncilArea %in% names(counts_vector), counts_vector[CouncilArea], NA)
  ) %>% 
  select(-CouncilArea)


#sapply(all_data, function(x) any(is.na(x)))
all_data=all_data[!((is.na(all_data$Price)) & all_data$data=='train'), ]

all_data$Price <- log1p(all_data$Price)
View(all_data)

for(col in names(all_data)){
  if(sum(is.na(all_data[,col]))>0 & !(col %in% c("data","Price"))){
    all_data[is.na(all_data[,col]),col]=mean(all_data[all_data$data=='train',col],na.rm=T)
  }
}


#To check columns which are contains NA
apply(all_data,2,function(x) sum(is.na(x)))

#Split the data into train and test , remove the unnecessary column which we have created
housing_train = all_data %>% 
  filter(data=='train') %>% 
  select(-data)

housing_test = all_data %>% 
  filter(data=='test') %>% 
  select(-data,-Price)

#Building Model

fit = lm(Price ~ ., data = housing_train)
summary(fit)

library(car)
t = vif(fit)
sort(t,decreasing = T)

#Final model
fit_train = lm(Price ~ .-Method_S, data = housing_train)
t = vif(fit_train)
sort(t,decreasing = T)

summary(fit_train)

#RMSE for train data
train_res = cbind.data.frame(
  Actual = exp(housing_train$Price) - 1,
  Fitted = exp(fitted(fit_train)) - 1,
  Error = residuals(fit_train)
)
View(train_res)

# RMSE for train data
rmse_train = sqrt(mean(train_res$Error^2))
rmse_train

#Test Data
result_data = housing_test

ir_predict = predict(fit_train, newdata = housing_test)
ir_predict
predicted_prices = expm1(ir_predict)

result_data$Price = predicted_prices
result_data = result_data[-c(1:16)]
write.csv(result_data, 'Shreya_Gupta_P1_part2.csv', row.names = FALSE)

a = housing_train$Price[1:1885]
a
res=a-ir_predict

# RMSE for test data
rmse_test=sqrt(mean(res^2))
rmse_test












