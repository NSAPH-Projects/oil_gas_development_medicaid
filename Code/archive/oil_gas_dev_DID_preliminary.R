########################################################################################
# PROJECT       : CCMH
# SPONSOR/PI    : Willis/Nori-Sarma
# PROGRAM NAME  : OGD DiD preliminary modeling
# DESCRIPTION   : This work is done in support of the OGD R01 grant submission 
#                 Note that it uses the first version of the OGD analytic data shared for CCMH
#                 
#                 
# PROGRAMMER    : Nina Cesare
# DATE WRITTEN  : 07/29/2024
########################################################################################
# INPUT FILES   : komura_did_data.csv
#                 mental_health_hospitalizations_20XX.csv
#
#
# OUTPUT FILES  : 	
#######################################################################################
# MODIFICATIONS : 
#
# DATE          :
# PROGRAMMER    : 
# DESCRIPTION   : 	
#######################################################################################

## Oil and Gas Boom DID: Preliminary
## Programmer: NLC
## Date: 04/10/2024

Sys.setenv(http_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
Sys.setenv(https_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
#install.packages("did")
#install.packages("AER")


library(dplyr)
library(data.table)
library(stargazer)
library(did)
library(lme4)
library(MASS)
library(AER)

county_fips_fix <- function(fips){
  fips <- as.character(fips)
  if(nchar(fips)==4){
    fips <- paste("0", fips, sep="")
  }
  else{
    fips <- fips
  }
  return(fips)
}



state_fips_fix <- function(fips){
  fips <- as.character(fips)
  if(nchar(fips)==1){
    fips <- paste("0", fips, sep="")
  }
  else{
    fips <- fips
  }
  return(fips)
}



## Gas data - by county, year
dat <- read.csv("oil_gas_development_medicaid/Data/komura_did_data.csv")


## MH data aggregated to national, year
dat_national <- dat[,c("year","wells_per_160km","cumsum_160km","wells_per","cumsum")] %>% group_by(year) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
mh_national <- read.csv("oil_gas_development_medicaid/Data/MH Year Counts.csv")



## MH data aggregated to county, year 
dat99 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_1999.csv")
dat00 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2000.csv")
dat01 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2001.csv")
dat02 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2002.csv")
dat03 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2003.csv")
dat04 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2004.csv")
dat05 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2005.csv")
dat06 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2006.csv")
dat07 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2007.csv")
dat08 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2008.csv")
dat09 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2009.csv")
dat10 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2010.csv")
dat11 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2011.csv")
dat12 <- fread("analytic/medicaid_mental_health/mental_health_hospitalizations_2012.csv")


files <- list(dat00, dat01, dat02, dat03, dat04, dat05, dat06, dat07, dat08, dat09, dat10, dat11, dat12)
files_new <- vector(mode='list', length=12)
years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)


for(i in 1:length(years)){
  file <- files[[i]]
  file$residence_county <- as.character(file$residence_county)
  file_new <- file %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new$year <- years[i]
  files_new[[i]] <- file_new
  print(i)
}


files_new_df <- rbind(files_new[[1]],
                      files_new[[2]],
                      files_new[[3]],
                      files_new[[4]],
                      files_new[[5]],
                      files_new[[6]],
                      files_new[[7]],
                      files_new[[8]],
                      files_new[[9]],
                      files_new[[10]],
                      files_new[[11]],
                      files_new[[12]],
                      files_new[[13]])





## Merge county annual MH with county annual oil drilling 

files_new_df$residence_county <- unlist(lapply(files_new_df$residence_county, function(x) county_fips_fix(as.character(x))))
dat$residence_county <- unlist(lapply(dat$GEOID, function(x) county_fips_fix(as.character(x))))

merge <- merge(files_new_df, dat, by = c("residence_county", "year"))
merge$did <- merge$spatial_ind * merge$temoporal_ind


## take a look at some example counties 
merge[which(merge$residence_county == "11001"),]
merge[which(merge$residence_county == "38067"),]


### Demographics of oil boom counties

#run locally: CCMH folder


#### Total hospitalizations ####

files_new_df$idx <- 1
total_vals <- files_new_df %>% dplyr::group_by(idx) %>% dplyr::summarise(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE),
                                                                         mental_health_hospitalizations = sum(mental_health_hospitalizations, na.rm = TRUE),
                                                                         adjustment_reaction_hospitalizations = sum(adjustment_reaction_hospitalizations, na.rm = TRUE),             
                                                                         anxiety_disorders_hospitalizations = sum(anxiety_disorders_hospitalizations, na.rm = TRUE),
                                                                         attention_disorders_hospitalizations = sum(attention_disorders_hospitalizations, na.rm = TRUE),              
                                                                         developmental_disorders_hospitalizations = sum(developmental_disorders_hospitalizations, na.rm = TRUE),
                                                                         infancy_childhood_disorders_hospitalizations = sum(infancy_childhood_disorders_hospitalizations, na.rm = TRUE),      
                                                                         mood_disorders_hospitalizations = sum(mood_disorders_hospitalizations, na.rm = TRUE),
                                                                         personality_disorders_hospitalizations = sum(personality_disorders_hospitalizations, na.rm = TRUE),            
                                                                         schizophrenia_psychotic_disorders_hospitalizations = sum(schizophrenia_psychotic_disorders_hospitalizations, na.rm = TRUE),
                                                                         alcohol_disorders_hospitalizations = sum(alcohol_disorders_hospitalizations, na.rm = TRUE),                
                                                                         substance_disorders_hospitalizations = sum(substance_disorders_hospitalizations, na.rm = TRUE),
                                                                         suicide_self_harm_hospitalizations = sum(suicide_self_harm_hospitalizations, na.rm = TRUE))

total_vals_df <- as.data.frame(total_vals)


sink("oil_gas_development_medicaid/Results/hospitalizations_2000_2012.txt")
total_vals_df 
closeAllConnections()


#### DID linear models, no covariates, year only ####


didreg_mh_lm <- lm(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_adj_lm <- lm(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_attn_lm <- lm(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_anx_lm <- lm(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_mood_lm <- lm(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_pers_lm <- lm(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_schiz_lm <- lm(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_suic_lm <- lm(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_dev_lm <- lm(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_child_lm <- lm(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_sub_lm <- lm(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_alc_lm <- lm(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)


didreg_mh_lm2 <- lm(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_adj_lm2 <- lm(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_attn_lm2<- lm(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_anx_lm2 <- lm(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_mood_lm2 <- lm(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_pers_lm2 <- lm(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_schiz_lm2 <- lm(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_suic_lm2 <- lm(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_dev_lm2 <- lm(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_child_lm2 <- lm(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_sub_lm2 <- lm(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)
didreg_alc_lm2 <- lm(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year,  data = merge)


sink("oil_gas_development_medicaid/Results/did_mods_lm_year_compare.txt")

stargazer(didreg_mh_lm, didreg_mh_lm2, type = "text")
stargazer(didreg_adj_lm, didreg_adj_lm2, type = "text")
stargazer(didreg_attn_lm, didreg_attn_lm2,  type = "text")
stargazer(didreg_anx_lm, didreg_anx_lm2, type = "text")
stargazer(didreg_mood_lm, didreg_mood_lm2, type = "text")
stargazer(didreg_pers_lm, didreg_pers_lm2, type = "text")
stargazer(didreg_schiz_lm, didreg_schiz_lm2, type = "text")
stargazer(didreg_suic_lm, didreg_suic_lm2, type = "text")
stargazer(didreg_dev_lm, didreg_dev_lm2, type = "text")
stargazer(didreg_child_lm, didreg_child_lm2, type = "text")
stargazer(didreg_sub_lm, didreg_sub_lm2, type = "text")
stargazer(didreg_alc_lm, didreg_alc_lm2, type = "text")

closeAllConnections()


## This is also panel data, and treatment is not distributed evenly across panels
## https://www.reddit.com/r/econometrics/comments/188mqmu/difference_in_difference_model_with_varying/ 
## It seems like what we need is a staggered difference in difference with multiple periods and timing
## This seems to be a helpful tutorial: https://tilburgsciencehub.com/topics/analyze/causal-inference/did/staggered-did/ 



#### DID poisson/NB, no covariates ####


# visualize dispersion....
par(mfrow = c(4,3))
plot(density(merge$mental_health_hospitalizations))
plot(density(merge$adjustment_reaction_hospitalizations))
plot(density(merge$attention_disorders_hospitalizations))
plot(density(merge$anxiety_disorders_hospitalizations))
plot(density(merge$mood_disorders_hospitalizations))
plot(density(merge$personality_disorders_hospitalizations))
plot(density(merge$schizophrenia_psychotic_disorders_hospitalizations))
plot(density(merge$suicide_self_harm_hospitalizations))
plot(density(merge$developmental_disorders_hospitalizations))
plot(density(merge$infancy_childhood_disorders_hospitalizations))
plot(density(merge$substance_disorders_hospitalizations))
plot(density(merge$alcohol_disorders_hospitalizations))


## try running poisson models
didreg_mh_poi <- glm(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_adj_poi <- glm(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_attn_poi <- glm(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_anx_poi <- glm(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_mood_poi <- glm(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_pers_poi <- glm(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_schiz_poi <- glm(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_suic_poi <- glm(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_dev_poi <- glm(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_child_poi <- glm(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_sub_poi <- glm(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)
didreg_alc_poi <- glm(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did,  data = merge)


AER::dispersiontest(didreg_mh_poi) ## p<0.05 for all ... data may need quasipoisson or negative binomial
AER::dispersiontest(didreg_adj_poi) 
AER::dispersiontest(didreg_attn_poi)
AER::dispersiontest(didreg_anx_poi)
AER::dispersiontest(didreg_mood_poi)
AER::dispersiontest(didreg_pers_poi)
AER::dispersiontest(didreg_schiz_poi)
AER::dispersiontest(didreg_suic_poi)
AER::dispersiontest(didreg_dev_poi)
AER::dispersiontest(didreg_child_poi)
AER::dispersiontest(didreg_sub_poi)
AER::dispersiontest(didreg_alc_poi)


## Test negative binomial for all cause mental health hospitalization
didreg_mh_poi_nb <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
pchisq(2 * (logLik(didreg_mh_poi_nb) - logLik(didreg_mh_poi)), df = 2, lower.tail = FALSE)
# Negative binomial seems to be a better fit


## negative binom models

didreg_all_nb <- glm.nb(all_cause_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_mh_nb <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_adj_nb <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_attn_nb <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_anx_nb <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_mood_nb <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_pers_nb <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_schiz_nb <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_suic_nb <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_dev_nb <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_child_nb <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_sub_nb <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_alc_nb <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)

sink("oil_gas_development_medicaid/did_mods_nb_output.txt")

stargazer(didreg_all_nb, type = "text")
stargazer(didreg_mh_nb, type = "text")
stargazer(didreg_adj_nb, type = "text")
stargazer(didreg_attn_nb, type = "text")
stargazer(didreg_anx_nb, type = "text")
stargazer(didreg_mood_nb, type = "text")
stargazer(didreg_pers_nb, type = "text")
stargazer(didreg_schiz_nb, type = "text")
stargazer(didreg_suic_nb, type = "text")
stargazer(didreg_dev_nb, type = "text")
stargazer(didreg_child_nb, type = "text")
stargazer(didreg_sub_nb, type = "text")
stargazer(didreg_alc_nb, type = "text")

closeAllConnections()

sink("oil_gas_development_medicaid/Results/did_mods_nb_irr.txt")

print("All cause hospitalizations")

didreg_all_out <- cbind(Estimate = coef(didreg_all_nb), confint(didreg_all_nb))
didreg_all_irr <- exp(didreg_all_out)
stargazer(didreg_all_irr, type = "text")

print("Mental health hospitalization")

didreg_mh_out <- cbind(Estimate = coef(didreg_mh_nb), confint(didreg_mh_nb))
didreg_mh_irr <- exp(didreg_mh_out)
stargazer(didreg_mh_irr, type = "text")

print("Adjustment disorder")

didreg_adj_out <- cbind(Estimate = coef(didreg_adj_nb), confint(didreg_adj_nb))
didreg_adj_irr <- exp(didreg_adj_out)
stargazer(didreg_adj_irr, type = "text")

print("Attention disorder")

didreg_attn_out <- cbind(Estimate = coef(didreg_attn_nb), confint(didreg_attn_nb))
didreg_attn_irr <- exp(didreg_attn_out)
stargazer(didreg_attn_irr, type = "text")

print("Anxiety disorder")

didreg_anx_out <- cbind(Estimate = coef(didreg_anx_nb), confint(didreg_anx_nb))
didreg_anx_irr <- exp(didreg_anx_out)
stargazer(didreg_anx_irr, type = "text")

print("Mood disorder")

didreg_mood_out <- cbind(Estimate = coef(didreg_mood_nb), confint(didreg_mood_nb))
didreg_mood_irr <- exp(didreg_mood_out)
stargazer(didreg_mood_irr, type = "text")

print("Personality disorder")

didreg_pers_out <- cbind(Estimate = coef(didreg_pers_nb), confint(didreg_pers_nb))
didreg_pers_irr <- exp(didreg_pers_out)
stargazer(didreg_pers_irr, type = "text")

print("Schizophrenia")

didreg_schiz_out <- cbind(Estimate = coef(didreg_schiz_nb), confint(didreg_schiz_nb))
didreg_schiz_irr <- exp(didreg_schiz_out)
stargazer(didreg_schiz_irr, type = "text")

print("Suicidality")

didreg_suic_out <- cbind(Estimate = coef(didreg_suic_nb), confint(didreg_suic_nb))
didreg_suic_irr <- exp(didreg_suic_out)
stargazer(didreg_suic_irr, type = "text")

print("Developmental disorder")

didreg_dev_out <- cbind(Estimate = coef(didreg_dev_nb), confint(didreg_dev_nb))
didreg_dev_irr <- exp(didreg_dev_out)
stargazer(didreg_dev_irr, type = "text")

print("Child development disorder")

didreg_child_out <- cbind(Estimate = coef(didreg_child_nb), confint(didreg_child_nb))
didreg_child_irr <- exp(didreg_child_out)
stargazer(didreg_child_irr, type = "text")

print("Substance use")

didreg_sub_out <- cbind(Estimate = coef(didreg_sub_nb), confint(didreg_sub_nb))
didreg_sub_irr <- exp(didreg_sub_out)
stargazer(didreg_sub_irr, type = "text")

print("Alcohol use")

didreg_alc_out <- cbind(Estimate = coef(didreg_alc_nb), confint(didreg_alc_nb))
didreg_alc_irr <- exp(didreg_alc_out)
stargazer(didreg_alc_irr, type = "text")

closeAllConnections()



#### NB Models with state and year added ####

merge$State <- as.factor(merge$State)

didreg_all_nb2 <- glm.nb(all_cause_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_mh_nb2 <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_adj_nb2 <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_attn_nb2 <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_anx_nb2 <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_mood_nb2 <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_pers_nb2 <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_schiz_nb2 <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_suic_nb2 <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_dev_nb2 <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_child_nb2 <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_sub_nb2 <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)
didreg_alc_nb2 <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + State, data = merge)

## Compare old/new models 

sink("oil_gas_development_medicaid/Results/did_mods_nb_output_stateYear_compare.txt")

stargazer(didreg_all_nb, didreg_all_nb2, type = "text")
stargazer(didreg_mh_nb, didreg_mh_nb2, type = "text")
stargazer(didreg_adj_nb, didreg_adj_nb2, type = "text")
stargazer(didreg_attn_nb, didreg_attn_nb2, type = "text") ## This does change the direction, but neither are significant
stargazer(didreg_anx_nb, didreg_anx_nb2, type = "text")
stargazer(didreg_mood_nb, didreg_mood_nb2, type = "text")
stargazer(didreg_pers_nb, didreg_pers_nb2, type = "text")
stargazer(didreg_schiz_nb, didreg_schiz_nb2, type = "text")  
stargazer(didreg_suic_nb, didreg_suic_nb2, type = "text") # this does remove significance  
stargazer(didreg_dev_nb, didreg_dev_nb2, type = "text")
stargazer(didreg_child_nb, didreg_child_nb2, type = "text") # becomes significant 
stargazer(didreg_sub_nb, didreg_sub_nb2, type = "text")
stargazer(didreg_alc_nb, didreg_alc_nb2, type = "text")

closeAllConnections()


sink("oil_gas_development_medicaid/Results/did_mods_nb_stateYear_irr.txt")

print("All cause hospitalizations")

didreg_all_out2 <- cbind(Estimate = coef(didreg_all_nb2), confint(didreg_all_nb2))
didreg_all_irr2 <- exp(didreg_all_out2)
stargazer(didreg_all_irr2, type = "text")

print("Mental health hospitalization")

didreg_mh_out <- cbind(Estimate = coef(didreg_mh_nb2), confint(didreg_mh_nb2))
didreg_mh_irr <- exp(didreg_mh_out)
stargazer(didreg_mh_irr, type = "text")

print("Adjustment disorder")

didreg_adj_out2 <- cbind(Estimate = coef(didreg_adj_nb2), confint(didreg_adj_nb2))
didreg_adj_irr2 <- exp(didreg_adj_out2)
stargazer(didreg_adj_irr2, type = "text")

print("Attention disorder")

didreg_attn_out2 <- cbind(Estimate = coef(didreg_attn_nb2), confint(didreg_attn_nb2))
didreg_attn_irr2 <- exp(didreg_attn_out2)
stargazer(didreg_attn_irr2, type = "text")

print("Anxiety disorder")

didreg_anx_out2 <- cbind(Estimate = coef(didreg_anx_nb2), confint(didreg_anx_nb2))
didreg_anx_irr2 <- exp(didreg_anx_out2)
stargazer(didreg_anx_irr2, type = "text")

print("Mood disorder")

didreg_mood_out2 <- cbind(Estimate = coef(didreg_mood_nb2), confint(didreg_mood_nb2))
didreg_mood_irr2 <- exp(didreg_mood_out2)
stargazer(didreg_mood_irr2, type = "text")

print("Personality disorder")

didreg_pers_out2 <- cbind(Estimate = coef(didreg_pers_nb2), confint(didreg_pers_nb2))
didreg_pers_irr2 <- exp(didreg_pers_out2)
stargazer(didreg_pers_irr2, type = "text")

print("Schizophrenia")

didreg_schiz_out2 <- cbind(Estimate = coef(didreg_schiz_nb2), confint(didreg_schiz_nb2))
didreg_schiz_irr2 <- exp(didreg_schiz_out2)
stargazer(didreg_schiz_irr2, type = "text")

print("Suicidality")

didreg_suic_out2 <- cbind(Estimate = coef(didreg_suic_nb2), confint(didreg_suic_nb2))
didreg_suic_irr2 <- exp(didreg_suic_out2)
stargazer(didreg_suic_irr2, type = "text")

print("Developmental disorder")

didreg_dev_out2 <- cbind(Estimate = coef(didreg_dev_nb2), confint(didreg_dev_nb2))
didreg_dev_irr2 <- exp(didreg_dev_out2)
stargazer(didreg_dev_irr2, type = "text")

print("Child development disorder")

didreg_child_out2 <- cbind(Estimate = coef(didreg_child_nb2), confint(didreg_child_nb2))
didreg_child_irr2 <- exp(didreg_child_out2)
stargazer(didreg_child_irr2, type = "text")

print("Substance use")

didreg_sub_out2 <- cbind(Estimate = coef(didreg_sub_nb2), confint(didreg_sub_nb2))
didreg_sub_irr2 <- exp(didreg_sub_out2)
stargazer(didreg_sub_irr2, type = "text")

print("Alcohol use")

didreg_alc_out2 <- cbind(Estimate = coef(didreg_alc_nb2), confint(didreg_alc_nb2))
didreg_alc_irr2 <- exp(didreg_alc_out2)
stargazer(didreg_alc_irr2, type = "text")

closeAllConnections()



##### NB models - year as only covariate #######


didreg_all_nb3 <- glm.nb(all_cause_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_mh_nb3 <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_adj_nb3 <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_attn_nb3 <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_anx_nb3 <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_mood_nb3 <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_pers_nb3 <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_schiz_nb3 <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_suic_nb3 <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_dev_nb3 <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_child_nb3 <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_sub_nb3 <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)
didreg_alc_nb3 <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge)

## Compare old/new models 

sink("oil_gas_development_medicaid/Results/did_mods_nb_output_year_compare.txt")

stargazer(didreg_all_nb, didreg_all_nb3, type = "text")
stargazer(didreg_mh_nb, didreg_mh_nb3, type = "text")
stargazer(didreg_adj_nb, didreg_adj_nb3, type = "text")
stargazer(didreg_attn_nb, didreg_attn_nb3, type = "text")
stargazer(didreg_anx_nb, didreg_anx_nb3, type = "text")
stargazer(didreg_mood_nb, didreg_mood_nb3, type = "text")
stargazer(didreg_pers_nb, didreg_pers_nb3, type = "text")
stargazer(didreg_schiz_nb, didreg_schiz_nb3, type = "text")  
stargazer(didreg_suic_nb, didreg_suic_nb3, type = "text") 
stargazer(didreg_dev_nb, didreg_dev_nb3, type = "text")
stargazer(didreg_child_nb, didreg_child_nb3, type = "text")
stargazer(didreg_sub_nb, didreg_sub_nb3, type = "text")
stargazer(didreg_alc_nb, didreg_alc_nb3, type = "text")

closeAllConnections()


sink("oil_gas_development_medicaid/Results/did_mods_nb_year_irr.txt")

print("All cause hospitalizations")

didreg_all_out3 <- cbind(Estimate = coef(didreg_all_nb3), confint(didreg_all_nb3))
didreg_all_irr3 <- exp(didreg_all_out3)
stargazer(didreg_all_irr3, type = "text")

print("Mental health hospitalization")

didreg_mh_out <- cbind(Estimate = coef(didreg_mh_nb3), confint(didreg_mh_nb3))
didreg_mh_irr <- exp(didreg_mh_out)
stargazer(didreg_mh_irr, type = "text")

print("Adjustment disorder")

didreg_adj_out3 <- cbind(Estimate = coef(didreg_adj_nb3), confint(didreg_adj_nb3))
didreg_adj_irr3 <- exp(didreg_adj_out3)
stargazer(didreg_adj_irr3, type = "text")

print("Attention disorder")

didreg_attn_out3 <- cbind(Estimate = coef(didreg_attn_nb3), confint(didreg_attn_nb3))
didreg_attn_irr3 <- exp(didreg_attn_out3)
stargazer(didreg_attn_irr3, type = "text")

print("Anxiety disorder")

didreg_anx_out3 <- cbind(Estimate = coef(didreg_anx_nb3), confint(didreg_anx_nb3))
didreg_anx_irr3 <- exp(didreg_anx_out3)
stargazer(didreg_anx_irr3, type = "text")

print("Mood disorder")

didreg_mood_out3 <- cbind(Estimate = coef(didreg_mood_nb3), confint(didreg_mood_nb3))
didreg_mood_irr3 <- exp(didreg_mood_out3)
stargazer(didreg_mood_irr3, type = "text")

print("Personality disorder")

didreg_pers_out3 <- cbind(Estimate = coef(didreg_pers_nb3), confint(didreg_pers_nb3))
didreg_pers_irr3 <- exp(didreg_pers_out3)
stargazer(didreg_pers_irr3, type = "text")

print("Schizophrenia")

didreg_schiz_out3 <- cbind(Estimate = coef(didreg_schiz_nb3), confint(didreg_schiz_nb3))
didreg_schiz_irr3 <- exp(didreg_schiz_out3)
stargazer(didreg_schiz_irr3, type = "text")

print("Suicidality")

didreg_suic_out3 <- cbind(Estimate = coef(didreg_suic_nb3), confint(didreg_suic_nb3))
didreg_suic_irr3 <- exp(didreg_suic_out3)
stargazer(didreg_suic_irr3, type = "text")

print("Developmental disorder")

didreg_dev_out3 <- cbind(Estimate = coef(didreg_dev_nb3), confint(didreg_dev_nb3))
didreg_dev_irr3 <- exp(didreg_dev_out3)
stargazer(didreg_dev_irr3, type = "text")

print("Child development disorder")

didreg_child_out3 <- cbind(Estimate = coef(didreg_child_nb3), confint(didreg_child_nb3))
didreg_child_irr3 <- exp(didreg_child_out3)
stargazer(didreg_child_irr3, type = "text")

print("Substance use")

didreg_sub_out3 <- cbind(Estimate = coef(didreg_sub_nb3), confint(didreg_sub_nb3))
didreg_sub_irr3 <- exp(didreg_sub_out3)
stargazer(didreg_sub_irr3, type = "text")

print("Alcohol use")

didreg_alc_out3 <- cbind(Estimate = coef(didreg_alc_nb3), confint(didreg_alc_nb3))
didreg_alc_irr3 <- exp(didreg_alc_out3)
stargazer(didreg_alc_irr3, type = "text")

closeAllConnections()




##### DID linear with state level fixed effects, year ####


sink("oil_gas_development_medicaid/did_mods_mixed_test.txt")


didreg_mh <- lm(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_mh2 <- lm(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + as.factor(year), data = merge)

didreg_mh3 <- lmer(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + (1 | State), data = merge)
didreg_mh4 <- lmer(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + as.factor(year) + (1 | State), data = merge)

AIC(didreg_mh)
AIC(didreg_mh2)
AIC(didreg_mh3) # state random effect has little impact 
AIC(didreg_mh4) 


stargazer(didreg_mh, didreg_mh2, type = "text")
stargazer(didreg_mh3, didreg_mh4, type = "text")


closeAllConnections() 



sink("oil_gas_development_medicaid/did_mods_prelim.txt")

## without county fixed effects

print("All cause hospitalizations")
didreg_all <- lm(all_cause_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)

stargazer(didreg_all, didreg_mh, type = "text")


print("Disorders - hospitalizations")

didreg_adj <- lm(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_attn <- lm(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_anx <- lm(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_mood <- lm(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_pers <- lm(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_schiz <- lm(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_suic <- lm(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)

stargazer(didreg_adj, didreg_attn, type = "text")
stargazer(didreg_anx, didreg_mood, type = "text")
stargazer(didreg_pers, didreg_schiz, type = "text")
stargazer(didreg_suic, type = "text")


print("Childhood conditions")

didreg_dev <- lm(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_child <- lm(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)

stargazer(didreg_dev, didreg_child, type = "text")


print("Substance use hosptalizations")

didreg_sub <- lm(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)
didreg_alc <- lm(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did, data = merge)

stargazer(didreg_sub, didreg_alc, type = "text") 

## However, this difference in executed across states, each of which might be completely unique contexts for oil dev

print("controlling for state?")

didreg_fixed1 <- lm(mental_health_hospitalizations ~ as.factor(State) + spatial_ind + temoporal_ind + did, data = merge) # no longer signfiicant 
didreg_fixed2 <- lm(substance_disorders_hospitalizations ~ as.factor(State) + spatial_ind + temoporal_ind + did, data = merge)

stargazer(didreg_fixed1, didreg_fixed2, type = "text")

closeAllConnections()





##### Stratified by medicaid expansion #######

medicaid <- read.csv("oil_gas_development_medicaid/Data/raw_data_notes.csv")
state_name_fips <- read.csv("oil_gas_development_medicaid/Data/state_name_fips.csv")
state_name_fips$state_clean <- tolower(state_name_fips$State.Name)

medicaid$state_clean <- tolower(medicaid$State)

medicaid <- merge(medicaid, state_name_fips, by = "state_clean")


medicaid$StFIPS <- unlist(lapply(medicaid$FIPS.Code, function(x) state_fips_fix(x)))
merge$StFIPS <- unlist(lapply(merge$State, function(x) state_fips_fix(x)))

merge_med <- merge(merge, medicaid, by = "StFIPS")


didreg_all_nb_med1 <- glm.nb(all_cause_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_mh_nb_med1 <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_adj_nb_med1 <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_attn_nb_med1 <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_anx_nb_med1 <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_mood_nb_med1 <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_pers_nb_med1 <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_schiz_nb_med1 <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_suic_nb_med1 <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_dev_nb_med1 <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_child_nb_med1 <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_sub_nb_med1 <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)
didreg_alc_nb_med1 <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year + adopted_2014, data = merge_med)



sink("oil_gas_development_medicaid/Results/did_mods_nb_year_medicaid.txt")

stargazer(didreg_mh_nb_med1, didreg_mh_nb3, type = "text") # positive, weaker but still significant 
stargazer(didreg_adj_nb_med1, didreg_adj_nb3, type = "text") # positive, weaker but still significant 
stargazer(didreg_attn_nb_med1, didreg_attn_nb3, type = "text") # not signicant for either; sign reverses when we add in medicaid 
stargazer(didreg_anx_nb_med1, didreg_anx_nb3, type = "text") # positive, weaker but still significant 
stargazer(didreg_mood_nb_med1, didreg_mood_nb3, type = "text") # positive, weaker but still significant 
stargazer(didreg_pers_nb_med1, didreg_pers_nb3, type = "text") ## significance is gone.
stargazer(didreg_schiz_nb_med1, didreg_schiz_nb3, type = "text") ## significance is gone.
stargazer(didreg_suic_nb_med1, didreg_suic_nb3, type = "text") ## now significant, but negative 
stargazer(didreg_dev_nb_med1, didreg_dev_nb3, type = "text") ## positive, weaker but still significant 
stargazer(didreg_child_nb_med1, didreg_child_nb3, type = "text") ## significance is gone.
stargazer(didreg_sub_nb_med1, didreg_sub_nb3, type = "text") ## positive, weaker but still significant 
stargazer(didreg_alc_nb_med1, didreg_alc_nb3, type = "text") ## positive, weaker but still significant 

closeAllConnections()



sink("oil_gas_development_medicaid/Results/did_mods_nb_year_medicaid_irr.txt")

print("All cause hospitalizations")

didreg_all_out_med1 <- cbind(Estimate = coef(didreg_all_nb_med1), confint(didreg_all_nb_med1))
didreg_all_irr_med1 <- exp(didreg_all_out_med1)
stargazer(didreg_all_irr_med1, type = "text")

print("Mental health hospitalization")

didreg_mh_out_med1 <- cbind(Estimate = coef(didreg_mh_nb_med1), confint(didreg_mh_nb_med1))
didreg_mh_irr_med1 <- exp(didreg_mh_out_med1)
stargazer(didreg_mh_irr_med1, type = "text")

print("Adjustment disorder")

didreg_adj_out_med1 <- cbind(Estimate = coef(didreg_adj_nb_med1), confint(didreg_adj_nb_med1))
didreg_adj_irr_med1 <- exp(didreg_adj_out_med1)
stargazer(didreg_adj_irr_med1, type = "text")

print("Attention disorder")

didreg_attn_out_med1 <- cbind(Estimate = coef(didreg_attn_nb_med1), confint(didreg_attn_nb_med1))
didreg_attn_irr_med1 <- exp(didreg_attn_out_med1)
stargazer(didreg_attn_irr_med1, type = "text")

print("Anxiety disorder")

didreg_anx_out_med1 <- cbind(Estimate = coef(didreg_anx_nb_med1), confint(didreg_anx_nb_med1))
didreg_anx_irr_med1 <- exp(didreg_anx_out_med1)
stargazer(didreg_anx_irr_med1, type = "text")

print("Mood disorder")

didreg_mood_out_med1 <- cbind(Estimate = coef(didreg_mood_nb_med1), confint(didreg_mood_nb_med1))
didreg_mood_irr_med1 <- exp(didreg_mood_out_med1)
stargazer(didreg_mood_irr_med1, type = "text")

print("Personality disorder")

didreg_pers_out_med1 <- cbind(Estimate = coef(didreg_pers_nb_med1), confint(didreg_pers_nb_med1))
didreg_pers_irr_med1 <- exp(didreg_pers_out_med1)
stargazer(didreg_pers_irr_med1, type = "text")

print("Schizophrenia")

didreg_schiz_out_med1 <- cbind(Estimate = coef(didreg_schiz_nb_med1), confint(didreg_schiz_nb_med1))
didreg_schiz_irr_med1 <- exp(didreg_schiz_out_med1)
stargazer(didreg_schiz_irr_med1, type = "text")

print("Suicidality")

didreg_suic_out_med1 <- cbind(Estimate = coef(didreg_suic_nb_med1), confint(didreg_suic_nb_med1))
didreg_suic_irr_med1 <- exp(didreg_suic_out_med1)
stargazer(didreg_suic_irr_med1, type = "text")

print("Developmental disorder")

didreg_dev_out_med1 <- cbind(Estimate = coef(didreg_dev_nb_med1), confint(didreg_dev_nb_med1))
didreg_dev_irr_med1 <- exp(didreg_dev_out_med1)
stargazer(didreg_dev_irr_med1, type = "text")

print("Child development disorder")

didreg_child_out_med1 <- cbind(Estimate = coef(didreg_child_nb_med1), confint(didreg_child_nb_med1))
didreg_child_irr_med1 <- exp(didreg_child_out_med1)
stargazer(didreg_child_irr_med1, type = "text")

print("Substance use")

didreg_sub_out_med1 <- cbind(Estimate = coef(didreg_sub_nb_med1), confint(didreg_sub_nb_med1))
didreg_sub_irr_med1 <- exp(didreg_sub_out_med1)
stargazer(didreg_sub_irr_med1, type = "text")

print("Alcohol use")

didreg_alc_out_med1 <- cbind(Estimate = coef(didreg_alc_nb_med1), confint(didreg_alc_nb_med1))
didreg_alc_irr_med1 <- exp(didreg_alc_out_med1)
stargazer(didreg_alc_irr_med1, type = "text")

closeAllConnections()






##### Stratified by demographics #######

#1: Non-Hispanic White
#2: latine (or African-American)
#4: Asian/Pacific Islander
#5: Hispanic


files_new_white <- vector(mode='list', length=12)
files_new_black <- vector(mode='list', length=12)
files_new_latine <- vector(mode='list', length=12)


years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)


for(i in 1:length(years)){
  file <- files[[i]]
  file$residence_county <- as.character(file$residence_county)
  
  file_new_white <- file[which(file$race == 1),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_white$year <- years[i]
  files_new_white[[i]] <- file_new_white
  
  file_new_black <- file[which(file$race == 2),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_black$year <- years[i]
  files_new_black[[i]] <- file_new_black
  
  file_new_latine <- file[which(file$race == 5),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_latine$year <- years[i]
  files_new_latine[[i]] <- file_new_latine
  
  print(i)
}


files_new_white_df <- rbind(files_new_white[[1]],
                            files_new_white[[2]],
                            files_new_white[[3]],
                            files_new_white[[4]],
                            files_new_white[[5]],
                            files_new_white[[6]],
                            files_new_white[[7]],
                            files_new_white[[8]],
                            files_new_white[[9]],
                            files_new_white[[10]],
                            files_new_white[[11]],
                            files_new_white[[12]],
                            files_new_white[[13]])


files_new_black_df <- rbind(files_new_black[[1]],
                            files_new_black[[2]],
                            files_new_black[[3]],
                            files_new_black[[4]],
                            files_new_black[[5]],
                            files_new_black[[6]],
                            files_new_black[[7]],
                            files_new_black[[8]],
                            files_new_black[[9]],
                            files_new_black[[10]],
                            files_new_black[[11]],
                            files_new_black[[12]],
                            files_new_black[[13]])



files_new_latine_df <- rbind(files_new_latine[[1]],
                            files_new_latine[[2]],
                            files_new_latine[[3]],
                            files_new_latine[[4]],
                            files_new_latine[[5]],
                            files_new_latine[[6]],
                            files_new_latine[[7]],
                            files_new_latine[[8]],
                            files_new_latine[[9]],
                            files_new_latine[[10]],
                            files_new_latine[[11]],
                            files_new_latine[[12]],
                            files_new_latine[[13]])


dat$residence_county <- unlist(lapply(dat$GEOID, function(x) county_fips_fix(as.character(x))))

merge_white <- merge(files_new_white_df, dat, by = c("residence_county", "year"))
merge_white$did <- merge_white$spatial_ind * merge_white$temoporal_ind

merge_black <- merge(files_new_black_df, dat, by = c("residence_county", "year"))
merge_black$did <- merge_black$spatial_ind * merge_black$temoporal_ind

merge_latine <- merge(files_new_latine_df, dat, by = c("residence_county", "year"))
merge_latine$did <- merge_latine$spatial_ind * merge_latine$temoporal_ind



didreg_all_nb_w <- glm.nb(all_cause_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_mh_nb_w <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_adj_nb_w <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_attn_nb_w <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_anx_nb_w <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_mood_nb_w <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_pers_nb_w <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_schiz_nb_w <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_suic_nb_w <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_dev_nb_w <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_child_nb_w <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_sub_nb_w <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)
didreg_alc_nb_w <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_white)



didreg_all_nb_b <- glm.nb(all_cause_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_mh_nb_b <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_adj_nb_b <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_attn_nb_b <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_anx_nb_b <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_mood_nb_b <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_pers_nb_b <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_schiz_nb_b <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_suic_nb_b <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_dev_nb_b <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_child_nb_b <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_sub_nb_b <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)
didreg_alc_nb_b <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_black)




didreg_all_nb_l <- glm.nb(all_cause_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_mh_nb_l <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_adj_nb_l <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_attn_nb_l <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_anx_nb_l <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_mood_nb_l <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_pers_nb_l <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_schiz_nb_l <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_suic_nb_l <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_dev_nb_l <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_child_nb_l <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_sub_nb_l <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)
didreg_alc_nb_l <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temoporal_ind + did + year, data = merge_latine)


sink("oil_gas_development_medicaid/Results/did_mods_nb_year_raceEth.txt")

stargazer(didreg_mh_nb_w, didreg_mh_nb_b, didreg_mh_nb_l, type = "text") # equivalent stepwise up from white, Black, Latine
stargazer(didreg_adj_nb_w, didreg_adj_nb_b, didreg_adj_nb_l, type = "text") # much higher for Black/Latine
stargazer(didreg_attn_nb_w, didreg_attn_nb_b, didreg_attn_nb_l, type = "text") # much higher for Black/Latine
stargazer(didreg_anx_nb_w, didreg_anx_nb_b, didreg_anx_nb_l, type = "text") # much higher for Latine
stargazer(didreg_mood_nb_w, didreg_mood_nb_b, didreg_mood_nb_l, type = "text") # much higher for Latine
stargazer(didreg_pers_nb_w, didreg_pers_nb_b, didreg_pers_nb_l, type = "text") # much higher for Latine
stargazer(didreg_schiz_nb_w, didreg_schiz_nb_b, didreg_schiz_nb_l, type = "text") # much higher for Latine
stargazer(didreg_alc_nb_w, didreg_alc_nb_b, didreg_alc_nb_l, type = "text") # much higher for Black/Latine 
stargazer(didreg_sub_nb_w, didreg_sub_nb_b, didreg_sub_nb_l, type = "text") # much higher for Black/Latine 
stargazer(didreg_suic_nb_w, didreg_suic_nb_b, didreg_suic_nb_l, type = "text") # much higher for Black/Latine 

closeAllConnections()




sink("oil_gas_development_medicaid/Results/did_mods_nb_year_raceEth_irr.txt")

print("Mental health hospitalization - White")

didreg_mh_out_w <- cbind(Estimate = coef(didreg_mh_nb_w), confint(didreg_mh_nb_w))
didreg_mh_irr_w <- exp(didreg_mh_out_w)
stargazer(didreg_mh_irr_w, type = "text")

print("Mental health hospitalization - Black")

didreg_mh_out_b <- cbind(Estimate = coef(didreg_mh_nb_b), confint(didreg_mh_nb_b))
didreg_mh_irr_b <- exp(didreg_mh_out_b)
stargazer(didreg_mh_irr_b, type = "text")

print("Mental health hospitalization - Latine")

didreg_mh_out_l <- cbind(Estimate = coef(didreg_mh_nb_l), confint(didreg_mh_nb_l))
didreg_mh_irr_l <- exp(didreg_mh_out_l)
stargazer(didreg_mh_irr_l, type = "text")

print("Adjustment disorder - White")

didreg_adj_out_w <- cbind(Estimate = coef(didreg_adj_nb_w), confint(didreg_adj_nb_w))
didreg_adj_irr_w <- exp(didreg_adj_out_w)
stargazer(didreg_adj_irr_w, type = "text")

print("Adjustment disorder - Black")

didreg_adj_out_b <- cbind(Estimate = coef(didreg_adj_nb_b), confint(didreg_adj_nb_b))
didreg_adj_irr_b <- exp(didreg_adj_out_b)
stargazer(didreg_adj_irr_b, type = "text")

print("Attention disorder - Latine")

didreg_attn_out_l <- cbind(Estimate = coef(didreg_attn_nb_l), confint(didreg_attn_nb_l))
didreg_attn_irr_l <- exp(didreg_attn_out_l)
stargazer(didreg_attn_irr_l, type = "text")

print("Anxiety disorder - White")

didreg_anx_out_w <- cbind(Estimate = coef(didreg_anx_nb_w), confint(didreg_anx_nb_w))
didreg_anx_irr_w <- exp(didreg_anx_out_w)
stargazer(didreg_anx_irr_w, type = "text")


print("Anxiety disorder - Black")

didreg_anx_out_b <- cbind(Estimate = coef(didreg_anx_nb_b), confint(didreg_anx_nb_b))
didreg_anx_irr_b <- exp(didreg_anx_out_b)
stargazer(didreg_anx_irr_b, type = "text")


print("Anxiety disorder - Latine")

didreg_anx_out_l <- cbind(Estimate = coef(didreg_anx_nb_l), confint(didreg_anx_nb_l))
didreg_anx_irr_l <- exp(didreg_anx_out_l)
stargazer(didreg_anx_irr_l, type = "text")

print("Mood disorder - White")

didreg_mood_out_w <- cbind(Estimate = coef(didreg_mood_nb_w), confint(didreg_mood_nb_w))
didreg_mood_irr_w <- exp(didreg_mood_out_w)
stargazer(didreg_mood_irr_w, type = "text")

print("Mood disorder - Black")

didreg_mood_out_b <- cbind(Estimate = coef(didreg_mood_nb_b), confint(didreg_mood_nb_b))
didreg_mood_irr_b <- exp(didreg_mood_out_b)
stargazer(didreg_mood_irr_b, type = "text")

print("Mood disorder - Latine")

didreg_mood_out_l <- cbind(Estimate = coef(didreg_mood_nb_l), confint(didreg_mood_nb_l))
didreg_mood_irr_l <- exp(didreg_mood_out_l)
stargazer(didreg_mood_irr_l, type = "text")

print("Personality disorder - White")

didreg_pers_out_w <- cbind(Estimate = coef(didreg_pers_nb_w), confint(didreg_pers_nb_w))
didreg_pers_irr_w <- exp(didreg_pers_out_w)
stargazer(didreg_pers_irr_w, type = "text")

print("Personality disorder - Black")

didreg_pers_out_b <- cbind(Estimate = coef(didreg_pers_nb_b), confint(didreg_pers_nb_b))
didreg_pers_irr_b <- exp(didreg_pers_out_b)
stargazer(didreg_pers_irr_b, type = "text")

print("Personality disorder - Latine")

didreg_pers_out_l <- cbind(Estimate = coef(didreg_pers_nb_l), confint(didreg_pers_nb_l))
didreg_pers_irr_l <- exp(didreg_pers_out_l)
stargazer(didreg_pers_irr_l, type = "text")

print("Schizophrenia - White")

didreg_schiz_out_w <- cbind(Estimate = coef(didreg_schiz_nb_w), confint(didreg_schiz_nb_w))
didreg_schiz_irr_w <- exp(didreg_schiz_out_w)
stargazer(didreg_schiz_irr_w, type = "text")

print("Schizophrenia - Black")

didreg_schiz_out_b <- cbind(Estimate = coef(didreg_schiz_nb_b), confint(didreg_schiz_nb_b))
didreg_schiz_irr_b <- exp(didreg_schiz_out_b)
stargazer(didreg_schiz_irr_b, type = "text")

print("Schizophrenia - Latine")

didreg_schiz_out_l <- cbind(Estimate = coef(didreg_schiz_nb_l), confint(didreg_schiz_nb_l))
didreg_schiz_irr_l <- exp(didreg_schiz_out_l)
stargazer(didreg_schiz_irr_l, type = "text")

print("Suicidality - White")

didreg_suic_out_w <- cbind(Estimate = coef(didreg_suic_nb_w), confint(didreg_suic_nb_w))
didreg_suic_irr_w <- exp(didreg_suic_out_w)
stargazer(didreg_suic_irr_w, type = "text")

print("Suicidality - Black")

didreg_suic_out_b <- cbind(Estimate = coef(didreg_suic_nb_b), confint(didreg_suic_nb_b))
didreg_suic_irr_b <- exp(didreg_suic_out_b)
stargazer(didreg_suic_irr_b, type = "text")

print("Suicidality - Latine")

didreg_suic_out_l <- cbind(Estimate = coef(didreg_suic_nb_l), confint(didreg_suic_nb_l))
didreg_suic_irr_l <- exp(didreg_suic_out_l)
stargazer(didreg_suic_irr_l, type = "text")

print("Developmental disorder - White")

didreg_dev_out_w <- cbind(Estimate = coef(didreg_dev_nb_w), confint(didreg_dev_nb_w))
didreg_dev_irr_w <- exp(didreg_dev_out_w)
stargazer(didreg_dev_irr_w, type = "text")


print("Developmental disorder - Black")

didreg_dev_out_b <- cbind(Estimate = coef(didreg_dev_nb_b), confint(didreg_dev_nb_b))
didreg_dev_irr_b <- exp(didreg_dev_out_b)
stargazer(didreg_dev_irr_b, type = "text")

print("Developmental disorder - Latine")

didreg_dev_out_l <- cbind(Estimate = coef(didreg_dev_nb_l), confint(didreg_dev_nb_l))
didreg_dev_irr_l <- exp(didreg_dev_out_l)
stargazer(didreg_dev_irr_l, type = "text")

print("Child development disorder - White")

didreg_child_out_w <- cbind(Estimate = coef(didreg_child_nb_w), confint(didreg_child_nb_w))
didreg_child_irr_w <- exp(didreg_child_out_w)
stargazer(didreg_child_irr_w, type = "text")

print("Child development disorder - Black")

didreg_child_out_b <- cbind(Estimate = coef(didreg_child_nb_b), confint(didreg_child_nb_b))
didreg_child_irr_b <- exp(didreg_child_out_b)
stargazer(didreg_child_irr_b, type = "text")


print("Child development disorder - Latine")

didreg_child_out_l <- cbind(Estimate = coef(didreg_child_nb_l), confint(didreg_child_nb_l))
didreg_child_irr_l <- exp(didreg_child_out_l)
stargazer(didreg_child_irr_l, type = "text")


print("Substance use - White")

didreg_sub_out_w <- cbind(Estimate = coef(didreg_sub_nb_w), confint(didreg_sub_nb_w))
didreg_sub_irr_w <- exp(didreg_sub_out_w)
stargazer(didreg_sub_irr_w, type = "text")


print("Substance use - Black")

didreg_sub_out_b <- cbind(Estimate = coef(didreg_sub_nb_b), confint(didreg_sub_nb_b))
didreg_sub_irr_b <- exp(didreg_sub_out_b)
stargazer(didreg_sub_irr_b, type = "text")

print("Substance use - Latine")

didreg_sub_out_l <- cbind(Estimate = coef(didreg_sub_nb_l), confint(didreg_sub_nb_l))
didreg_sub_irr_l <- exp(didreg_sub_out_l)
stargazer(didreg_sub_irr_l, type = "text")


print("Alcohol use - White")

didreg_alc_out_w <- cbind(Estimate = coef(didreg_alc_nb_w), confint(didreg_alc_nb_w))
didreg_alc_irr_w <- exp(didreg_alc_out_w)
stargazer(didreg_alc_irr_w, type = "text")


print("Alcohol use - Black")

didreg_alc_out_b <- cbind(Estimate = coef(didreg_alc_nb_b), confint(didreg_alc_nb_b))
didreg_alc_irr_b <- exp(didreg_alc_out_b)
stargazer(didreg_alc_irr_b, type = "text")


print("Alcohol use - Latine")

didreg_alc_out_l <- cbind(Estimate = coef(didreg_alc_nb_l), confint(didreg_alc_nb_l))
didreg_alc_irr_l <- exp(didreg_alc_out_l)
stargazer(didreg_alc_irr_l, type = "text")

closeAllConnections()




###### N by state by year ######### 

state_name_fips <- read.csv("oil_gas_development_medicaid/Data/state_name_fips.csv")
state_name_fips$StFIPS <- unlist(lapply(state_name_fips$FIPS.Code, function(x) state_fips_fix(x)))


files_new_df$residence_county <- unlist(lapply(files_new_df$residence_county, function(x) county_fips_fix(x)))
files_new_white_df$residence_county <- unlist(lapply(files_new_white_df$residence_county, function(x) county_fips_fix(x)))
files_new_black_df$residence_county <- unlist(lapply(files_new_black_df$residence_county, function(x) county_fips_fix(x)))
files_new_latine_df$residence_county <- unlist(lapply(files_new_latine_df$residence_county, function(x) county_fips_fix(x)))


files_new_df$StFIPS <- substr(files_new_df$residence_county, 1,2)
files_new_white_df$StFIPS <- substr(files_new_white_df$residence_county, 1,2)
files_new_black_df$StFIPS <- substr(files_new_black_df$residence_county, 1,2)
files_new_latine_df$StFIPS <- substr(files_new_latine_df$residence_county, 1,2)



state <- merge(files_new_df, state_name_fips, by = "StFIPS")
state_white <- merge(files_new_white_df, state_name_fips, by = "StFIPS")
state_black <- merge(files_new_black_df, state_name_fips, by = "StFIPS")
state_latine <- merge(files_new_latine_df, state_name_fips, by = "StFIPS")



