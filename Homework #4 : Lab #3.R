#Cassidy Drummond
#9/22/2022
#Group: Minghao, Liam
#Homework 4, Lab 3

#For our model, we created a subset with the following variables: ownership, housing cost, income, and race. 
#We chose to use PUMA codes, as we thought it would be more interesting/challenge. Through the results found below, we found that using the PUMA code actually was statisticially significantly worse than if we had used boroughs originally. 
#The highest accuracy with k = 9 was 41.91%
#Using a simple linear regression would allow us to increase our accuracy and find larger trends
#With NYC being so densely populated, it is extremely difficult to really narrow down the data to just one neighborhood

load("/Users/cassidydrummond/Desktop/CCNY/acs2017_ny/acs2017_ny_data.RData")
library(dplyr)
install.packages("package:stats")
install.packages("package:base")
install.packages("simple.regression")

dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC ==1) & (acs2017_ny$AGE > 18))
dat_NYC <- dat_NYC %>% mutate(simple_PUMA=ifelse(PUMA >3711,ifelse(PUMA>3811,ifelse(PUMA>3904,ifelse(PUMA>4019,PUMA-4059,PUMA-3977),PUMA-3880),PUMA-3790),PUMA-3700))
attach(dat_NYC)

borough_f <-factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
PUMA_f <- as.factor(simple_PUMA)
norm_varb <- function(X_in) {(X_in - min(X_in,na.rm = TRUE))/(max(X_in,na.rm = TRUE)-min(X_in,na.rm = TRUE))}

is.na(OWNCOST) <- which(OWNCOST == 9999999)
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)
norm_ownership <- norm_varb(OWNERSHP)
norm_race <- norm_varb(RACE)

data_use_prelim <- cbind(norm_inc_tot,norm_housing_cost,norm_race,norm_ownership)
data_use_prelim <- data.frame(data_use_prelim)

good_obs_data_use <- complete.cases(data_use_prelim,PUMA_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(PUMA_f,good_obs_data_use)

borough1 <- complete.cases(data_use_prelim,borough_f)
borough_use <- subset(data_use_prelim,borough1)
actual_borough <- subset(borough_f,borough1)
set.set(35)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- actual_borough[!select1]
summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)


set.seed(35)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
(indx in seq(1, 9, by=2)) 
pred_borough <- knn(train_data, test_data, cl_data, k = indx, 1=0, prob = FALSE, use.all = TRUE)
num_correct_labels <- sum(pred_borough == true_data)
correct_rate <- num_correct_labels/length(true_data)
print(c(indx,correct_rate)) 

