rm(list=ls()) 
data <- read.csv("ComScore.csv")

# Data Cleaning 
data <- na.omit(data)  #remove NA

# Getting all book purchase data 
totalbook <- data[data$prod_category_id == 2003000000 | data$prod_category_id == 2003005000 | data$prod_category_id == 2003005058
| data$prod_category_id == 2003005059 | data$prod_category_id == 2003005060 | data$prod_category_id == 2003005061 | data$prod_category_id == 2003005062
| data$prod_category_id == 2003005063 | data$prod_category_id == 2003005064 | data$prod_category_id == 2003005065 | data$prod_category_id == 2003005066
| data$prod_category_id == 2003005067| data$prod_category_id ==2003005068 | data$prod_category_id == 2003005069 | data$prod_category_id == 2003005070
| data$prod_category_id == 2003005071 | data$prod_category_id == 2003005072 | data$prod_category_id == 2003005073| data$prod_category_id ==2003005074, ] 

# Book purchase data on Amazon.com 
amazonbook <- totalbook[totalbook$domain_name == "amazon.com", ]

# Cheking if there is any deviant value 
library("psych")
multi.hist(amazonbook[, c("hoh_most_education","census_region",	"household_size",	"hoh_oldest_age",	"household_income",	"children", "racial_background",
"connection_speed",	"country_of_origin","basket_tot")])

summary(amazonbook$hoh_most_education)
summary(amazonbook$household_size)
summary(amazonbook$hoh_oldest_age)
summary(amazonbook$household_income)
summary(amazonbook$children)
summary(amazonbook$racial_background)
summary(amazonbook$connection_speed)
summary(amazonbook$country_of_origin)
summary(amazonbook$basket_tot)

# Data Transformation 
library(ggplot2)
amazonbook$basket_tot_trans <- log(amazonbook$basket_tot)
ggplot(amazonbook, aes(x=basket_tot_trans)) + geom_histogram() 
qqnorm(amazonbook$basket_tot_trans, ylim = c(0,5)) 
summary(amazonbook$basket_tot_trans)

amazonbook$prod_name <- NULL
amazonbook$domain_name <- NULL
amazonbook$event_time <- NULL
amazonbook <- amazonbook[is.finite(rowSums(amazonbook)),]

# Regression Model - Backward Elimination 
fitall <- lm(basket_tot_trans ~ factor(hoh_most_education) + factor(census_region) + factor(household_size) + 
factor(hoh_oldest_age) + factor(household_income) + children + factor(amazonbook$racial_background) + 
connection_speed + country_of_origin, data=amazonbook)
summary(fitall)

reg <- step(fitall, direction="backward")

# Final Regression Model 
finalreg <- lm(basket_tot_trans ~ factor(hoh_most_education) + factor(census_region) + factor(household_size) 
+ factor(hoh_oldest_age) + factor(household_income) + children + factor(amazonbook$racial_background) + 
country_of_origin, data=amazonbook) # Removed connection speed only  
summary(finalreg)

library(regclass)
VIF(finalreg)