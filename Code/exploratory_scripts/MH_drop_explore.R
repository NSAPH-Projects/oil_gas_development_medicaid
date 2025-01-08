########################################################################################
# PROJECT       : CCMH
# SPONSOR/PI    : Willis/Nori-Sarma
# PROGRAM NAME  : OGD DiD modeling - data exploration
# DESCRIPTION   : This is an exploratory analysis of mental health counts by OGD strata
#                 Depends on: oil_gas_dev_DID_newAnalyticPlan.R
#                 
# PROGRAMMER    : Nina Cesare
# DATE WRITTEN  : 08/13/2024
########################################################################################
# INPUT FILES   : mental_health_hospitalizations_20XX.csv
#
#test
# OUTPUT FILES  : 	
#######################################################################################
# MODIFICATIONS : 
#
# DATE          :
# PROGRAMMER    : 
# DESCRIPTION   : 	
#######################################################################################


pctlag <- function(x) {x / lag(x) - 1}


test <- files_new_df %>% 
  group_by(residence_county) %>% 
  arrange(year, .by_group = TRUE) %>%
  mutate(mental_health_sd = sd(mental_health_hospitalizations, na.rm =TRUE),
         mental_health_lag = ((mental_health_hospitalizations + 0.0001)/dplyr::lag((mental_health_hospitalizations + 0.0001)) - 1) * 100)


summary(test$mental_health_sd)
quantile(test$mental_health_hospitalizations, c(0.95))



test[which(test$mental_health_sd >= 1390),]
variable_counties <- unique(test$residence_county[which(test$mental_health_sd >= 1390)])


for(i in 1:length(variable_counties)){
  print(as.data.frame(files_new_df[which(files_new_df$residence_county == variable_counties[i]), c("residence_county","year","mental_health_hospitalizations")]))
}

files_new_df[which(files_new_df$residence_county == "36061"), c("residence_county","year","mental_health_hospitalizations")]

#17031 - big drop
#36029 - big drop
#36061 - big drop - NY county NY
#36103 - big drop - Suffolk county NY
#Nassau county drops by like half as well (36059)

sum(dat09[which(dat09$state == "NY"),mental_health_hospitalizations], na.rm = TRUE)
sum(dat10[which(dat10$state == "NY"),mental_health_hospitalizations], na.rm = TRUE)
sum(dat11[which(dat11$state == "NY"),mental_health_hospitalizations], na.rm = TRUE)
#((144821-279853)/144821) * 100 


sum(dat09[which(dat09$residence_county == "36061"),mental_health_hospitalizations], na.rm = TRUE)
sum(dat10[which(dat10$residence_county == "36061"),mental_health_hospitalizations], na.rm = TRUE)
sum(dat11[which(dat11$residence_county == "36061"),mental_health_hospitalizations], na.rm = TRUE)
#((144821-279853)/144821) * 100 


## WE can also look at percent difference, but these are mostly counties with really small values

quantile(test$mental_health_lag, 0.01, 0.10) 
decline_counties <- unique(test$residence_county[which(test$mental_health_lag <= -99)])

for(i in 1:length(decline_counties)){
  print(as.data.frame(files_new_df[which(files_new_df$residence_county == decline_counties[i]), c("residence_county","year","mental_health_hospitalizations")]))
}


## How much of an impact does NYC county have?
sum <- files_new_df %>% group_by(residence_county) %>% summarise(hosp = sum(mental_health_hospitalizations, na.rm = TRUE))
sum[order(-sum$hosp),]

#nyc county has twice as many hospt as the next largest (cook county) which is excluded





