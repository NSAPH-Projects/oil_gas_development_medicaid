########################################################################################
# PROJECT       : CCMH
# SPONSOR/PI    : Willis/Nori-Sarma
# PROGRAM NAME  : OGD DiD preliminary modeling
# DESCRIPTION   : This work is done to support the development of a 
#                 Note that it uses the first version of the OGD analytic data shared for CCMH
#                 
#                 
# PROGRAMMER    : Nina Cesare
# DATE WRITTEN  : 07/29/2024
########################################################################################
# INPUT FILES   : allcountyexposures_newIndicators.csv
#                 mental_health_hospitalizations_20XX.csv
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

Sys.setenv(http_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
Sys.setenv(https_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
#install.packages("did")
#install.packages("AER")


library(dplyr)
library(tidyr)
library(data.table)
library(stargazer)
library(did)
library(lme4)
library(MASS)
library(AER)
library(maps)
library(ggplot2)
library(ggplot2) #load ggplot2 for plotting the results
library(purrr)
devtools::install_github("jonathandroth/staggered")
library(staggered)

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
dat <- read.csv("oil_gas_development_medicaid/Data/allcountyexposures_newIndicators.csv")

dat$residence_county <- unlist(lapply(dat$GEOID, function(x) county_fips_fix(as.character(x))))

## Exposures to create
# Standard difference in difference estimator
# OGD/km2 in tertiles


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


files <- list(dat99, dat00, dat01, dat02, dat03, dat04, dat05, dat06, dat07, dat08, dat09, dat10, dat11, dat12)
files_new <- vector(mode='list', length=12)
years <- c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)


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
                      files_new[[13]],
                      files_new[[14]])


files_new_df$residence_county <- unlist(lapply(files_new_df$residence_county, function(x) county_fips_fix(as.character(x))))


## Merge county annual MH with county annual oil drilling

merge <- merge(files_new_df, dat, by = c("residence_county", "year"))
merge$did <- merge$spatial_ind_cum * merge$temporal_ind

merge <- merge[which(merge$year != 1999),] # we don't have OGD data for this atm

### Demographics of oil boom counties

#run locally: //ad.bu.edu/bumcfiles/SPH/DCC/Dept/CCHM/09Programming


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


##sink("oil_gas_development_medicaid/Results/hospitalizations_2000_2012.txt")
total_vals_df
##closeAllConnections()


#### Canonical DID negative binomial models #####


## policy turns 'on' when drilling starts within a state
## county is 'treated' if drilling occurs there

didreg_all_nb <- glm.nb(all_cause_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_mh_nb <- glm.nb(mental_health_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_adj_nb <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_attn_nb <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_anx_nb <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_mood_nb <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_pers_nb <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_schiz_nb <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_suic_nb <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_dev_nb <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_child_nb <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_sub_nb <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)
didreg_alc_nb <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind_cum + temporal_ind + did, data = merge)



#### Generalized DiD negative binomial model #####

didreg_all_nb3 <- glm.nb(all_cause_hospitalizations ~  did + as.factor(year) + as.factor(state), data = merge)
didreg_mh_nb3 <- glm.nb(mental_health_hospitalizations ~  did + as.factor(year) + as.factor(state), data = merge)
didreg_adj_nb3 <- glm.nb(adjustment_reaction_hospitalizations ~  did + as.factor(year) + as.factor(state), data = merge)
didreg_attn_nb3 <- glm.nb(attention_disorders_hospitalizations ~  did + as.factor(year) + as.factor(state), data = merge)
didreg_anx_nb3 <- glm.nb(anxiety_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge)
didreg_mood_nb3 <- glm.nb(mood_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge)
didreg_pers_nb3 <- glm.nb(personality_disorders_hospitalizations ~  did + as.factor(year) + as.factor(state), data = merge)
didreg_schiz_nb3 <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge)
didreg_suic_nb3 <- glm.nb(suicide_self_harm_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge)
didreg_dev_nb3 <- glm.nb(developmental_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge)
didreg_child_nb3 <- glm.nb(infancy_childhood_disorders_hospitalizations ~  did + as.factor(year) + as.factor(state), data = merge)
didreg_sub_nb3 <- glm.nb(substance_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge)
didreg_alc_nb3 <- glm.nb(alcohol_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge)


#### Compare canonical and generalized DiD models and produce output ####

stargazer(didreg_mh_nb, didreg_mh_nb3, type = "text")
stargazer(didreg_adj_nb, didreg_adj_nb3, type = "text")
stargazer(didreg_attn_nb, didreg_attn_nb3, type = "text")
stargazer(didreg_anx_nb, didreg_anx_nb3, type = "text")
stargazer(didreg_mood_nb, didreg_mood_nb3, type = "text")
stargazer(didreg_pers_nb, didreg_pers_nb3, type = "text")
stargazer(didreg_schiz_nb, didreg_schiz_nb3, type = "text")
stargazer(didreg_sub_nb, didreg_sub_nb3, type = "text")
stargazer(didreg_alc_nb, didreg_alc_nb3, type = "text")
stargazer(didreg_suic_nb, didreg_suic_nb3, type = "text")


sink("oil_gas_development_medicaid/Results/20240726/did_mods_nb_irr.txt")

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



sink("oil_gas_development_medicaid/Results/20240726/did_mods_nb3_irr.txt")

print("Mental health hospitalization")

didreg_mh_out <- cbind(Estimate = coef(didreg_mh_nb3), confint(didreg_mh_nb3))
didreg_mh_irr <- exp(didreg_mh_out)
stargazer(didreg_mh_irr, type = "text")

print("Adjustment disorder")

didreg_adj_out <- cbind(Estimate = coef(didreg_adj_nb3), confint(didreg_adj_nb3))
didreg_adj_irr <- exp(didreg_adj_out)
stargazer(didreg_adj_irr, type = "text")

print("Attention disorder")

didreg_attn_out <- cbind(Estimate = coef(didreg_attn_nb3), confint(didreg_attn_nb3))
didreg_attn_irr <- exp(didreg_attn_out)
stargazer(didreg_attn_irr, type = "text")

print("Anxiety disorder")

didreg_anx_out <- cbind(Estimate = coef(didreg_anx_nb3), confint(didreg_anx_nb3))
didreg_anx_irr <- exp(didreg_anx_out)
stargazer(didreg_anx_irr, type = "text")

print("Mood disorder")

didreg_mood_out <- cbind(Estimate = coef(didreg_mood_nb3), confint(didreg_mood_nb3))
didreg_mood_irr <- exp(didreg_mood_out)
stargazer(didreg_mood_irr, type = "text")

print("Personality disorder")

didreg_pers_out <- cbind(Estimate = coef(didreg_pers_nb3), confint(didreg_pers_nb3))
didreg_pers_irr <- exp(didreg_pers_out)
stargazer(didreg_pers_irr, type = "text")

print("Schizophrenia")

didreg_schiz_out <- cbind(Estimate = coef(didreg_schiz_nb3), confint(didreg_schiz_nb3))
didreg_schiz_irr <- exp(didreg_schiz_out)
stargazer(didreg_schiz_irr, type = "text")

print("Suicidality")

didreg_suic_out <- cbind(Estimate = coef(didreg_suic_nb3), confint(didreg_suic_nb3))
didreg_suic_irr <- exp(didreg_suic_out)
stargazer(didreg_suic_irr, type = "text")


print("Substance use")

didreg_sub_out <- cbind(Estimate = coef(didreg_sub_nb3), confint(didreg_sub_nb3))
didreg_sub_irr <- exp(didreg_sub_out)
stargazer(didreg_sub_irr, type = "text")

print("Alcohol use")

didreg_alc_out <- cbind(Estimate = coef(didreg_alc_nb3), confint(didreg_alc_nb3))
didreg_alc_irr <- exp(didreg_alc_out)
stargazer(didreg_alc_irr, type = "text")

closeAllConnections()



#### Race/ethnicity stratified Canonical DiD models ####


files_new_white <- vector(mode='list', length=13)
files_new_black <- vector(mode='list', length=13)
files_new_latine <- vector(mode='list', length=13)


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
merge_white$did <- merge_white$spatial_ind_cum * merge_white$temporal_ind

merge_black <- merge(files_new_black_df, dat, by = c("residence_county", "year"))
merge_black$did <- merge_black$spatial_ind_cum * merge_black$temporal_ind

merge_latine <- merge(files_new_latine_df, dat, by = c("residence_county", "year"))
merge_latine$did <- merge_latine$spatial_ind_cum * merge_latine$temporal_ind


didreg_mh_nb_w <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_adj_nb_w <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_attn_nb_w <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_anx_nb_w <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_mood_nb_w <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_pers_nb_w <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_schiz_nb_w <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_suic_nb_w <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_dev_nb_w <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_child_nb_w <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_sub_nb_w <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)
didreg_alc_nb_w <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_white)



didreg_mh_nb_b <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_adj_nb_b <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_attn_nb_b <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_anx_nb_b <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_mood_nb_b <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_pers_nb_b <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_schiz_nb_b <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_suic_nb_b <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_dev_nb_b <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_child_nb_b <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_sub_nb_b <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)
didreg_alc_nb_b <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_black)




didreg_mh_nb_l <- glm.nb(mental_health_hospitalizations ~ spatial_ind + temporal_ind + did , data = merge_latine)
didreg_adj_nb_l <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_attn_nb_l <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_anx_nb_l <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_mood_nb_l <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_pers_nb_l <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_schiz_nb_l <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_suic_nb_l <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_dev_nb_l <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_child_nb_l <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_sub_nb_l <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)
didreg_alc_nb_l <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temporal_ind + did, data = merge_latine)





sink("oil_gas_development_medicaid/Results/20230726/did_mods_nb_year_raceEth_irr.txt")

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


closeAllConnections()



didreg_mh_nb3_w <- glm.nb(mental_health_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_adj_nb3_w <- glm.nb(adjustment_reaction_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_attn_nb3_w <- glm.nb(attention_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_anx_nb3_w <- glm.nb(anxiety_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_mood_nb3_w <- glm.nb(mood_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_pers_nb3_w <- glm.nb(personality_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_schiz_nb3_w <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_suic_nb3_w <- glm.nb(suicide_self_harm_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_sub_nb3_w <- glm.nb(substance_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)
didreg_alc_nb3_w <- glm.nb(alcohol_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_white)


didreg_mh_nb3_b <- glm.nb(mental_health_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_black)
didreg_adj_nb3_b <- glm.nb(adjustment_reaction_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_black)
didreg_attn_nb3_b <- glm.nb(attention_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_black)
didreg_anx_nb3_b <- glm.nb(anxiety_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_black)
didreg_mood_nb3_b <- glm.nb(mood_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_black)
didreg_pers_nb3_b <- glm.nb(personality_disorders_hospitalizations ~ did, data = merge_black)
didreg_schiz_nb3_b <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_black)
didreg_suic_nb3_b <- glm.nb(suicide_self_harm_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_black)
didreg_sub_nb3_b <- glm.nb(substance_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_black)
didreg_alc_nb3_b <- glm.nb(alcohol_disorders_hospitalizations ~  did + as.factor(year) + as.factor(state), data = merge_black)


didreg_mh_nb3_l <- glm.nb(mental_health_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_adj_nb3_l <- glm.nb(adjustment_reaction_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_attn_nb3_l <- glm.nb(attention_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_anx_nb3_l <- glm.nb(anxiety_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_mood_nb3_l <- glm.nb(mood_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_pers_nb3_l <- glm.nb(personality_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_schiz_nb3_l <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_suic_nb3_l <- glm.nb(suicide_self_harm_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_sub_nb3_l <- glm.nb(substance_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)
didreg_alc_nb3_l <- glm.nb(alcohol_disorders_hospitalizations ~ did + as.factor(year) + as.factor(state), data = merge_latine)






sink("oil_gas_development_medicaid/Results/20230726/did_mods_nb3_year_raceEth_irr.txt")

print("Mental health hospitalization - White")

didreg_mh_out_w <- cbind(Estimate = coef(didreg_mh_nb3_w), confint(didreg_mh_nb3_w))
didreg_mh_irr_w <- exp(didreg_mh_out_w)
stargazer(didreg_mh_irr_w, type = "text")

print("Mental health hospitalization - Black")

didreg_mh_out_b <- cbind(Estimate = coef(didreg_mh_nb3_b), confint(didreg_mh_nb3_b))
didreg_mh_irr_b <- exp(didreg_mh_out_b)
stargazer(didreg_mh_irr_b, type = "text")

print("Mental health hospitalization - Latine")

didreg_mh_out_l <- cbind(Estimate = coef(didreg_mh_nb3_l), confint(didreg_mh_nb3_l))
didreg_mh_irr_l <- exp(didreg_mh_out_l)
stargazer(didreg_mh_irr_l, type = "text")


closeAllConnections()





















### Some wild-ass exploration modeling variation ####

## What about contemporaneous rather than cumulative exposures?

didreg_all_nb4 <- glm.nb(all_cause_hospitalizations ~ spatial_ind + as.factor(year) + as.factor(state), data = merge)
didreg_mh_nb4 <- glm.nb(mental_health_hospitalizations ~ spatial_ind +  as.factor(year) + as.factor(state), data = merge)
didreg_adj_nb4 <- glm.nb(adjustment_reaction_hospitalizations ~ spatial_ind +  as.factor(year) + as.factor(state), data = merge)
didreg_attn_nb4 <- glm.nb(attention_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_anx_nb4 <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_mood_nb4 <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_pers_nb4 <- glm.nb(personality_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_schiz_nb4 <- glm.nb(schizophrenia_psychotic_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_suic_nb4 <- glm.nb(suicide_self_harm_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_dev_nb4 <- glm.nb(developmental_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_child_nb4 <- glm.nb(infancy_childhood_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_sub_nb4 <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)
didreg_alc_nb4 <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind + temporal_ind + did + as.factor(year) + as.factor(state), data = merge)



stargazer(didreg_mh_nb, didreg_mh_nb3, didreg_mh_nb4, type = "text") 
stargazer(didreg_adj_nb, didreg_adj_nb3, didreg_adj_nb4, type = "text")
stargazer(didreg_attn_nb, didreg_attn_nb3, didreg_attn_nb4, type = "text")
stargazer(didreg_anx_nb, didreg_anx_nb3, didreg_anx_nb4, type = "text")
stargazer(didreg_mood_nb, didreg_mood_nb3, didreg_mood_nb4,type = "text")
stargazer(didreg_pers_nb, didreg_pers_nb3, didreg_pers_nb4, type = "text")
stargazer(didreg_schiz_nb, didreg_schiz_nb3, didreg_schiz_nb4, type = "text")
stargazer(didreg_sub_nb, didreg_sub_nb3, didreg_sub_nb4, type = "text")
stargazer(didreg_alc_nb, didreg_alc_nb3, didreg_alc_nb4, type = "text")
stargazer(didreg_suic_nb, didreg_suic_nb3, didreg_suic_nb4, type = "text")

## For the 'canonical' diff-in-diff 
## for many diagnoses, the association changes for contemporaneous indicator (current drilling or not)

# Does proximity matter? 

merge$spatial_ind_close <- ifelse(merge$wells_per > 0 | merge$wells_per_160km > 0, 1, 0)

testmod1b <- glm.nb(mental_health_hospitalizations ~ spatial_ind, data = merge)
testmod1a <- glm.nb(mental_health_hospitalizations ~ spatial_ind_close, data = merge)

stargazer(testmod1a, testmod1b, type = "text")


testmod2a <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind, data = merge)
testmod2b <- glm.nb(anxiety_disorders_hospitalizations ~ spatial_ind_close, data = merge)

stargazer(testmod2a, testmod2b, type = "text")

testmod3a <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind, data = merge)
testmod3b <- glm.nb(mood_disorders_hospitalizations ~ spatial_ind_close, data = merge)

stargazer(testmod3a, testmod3b, type = "text")


testmod4a <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind, data = merge)
testmod4b <- glm.nb(substance_disorders_hospitalizations ~ spatial_ind_close, data = merge)

stargazer(testmod4a, testmod4b, type = "text")


testmod5a <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind, data = merge)
testmod5b <- glm.nb(alcohol_disorders_hospitalizations ~ spatial_ind_close, data = merge)

stargazer(testmod5a, testmod5b, type = "text")



# if we treat this as a poisson and use actual fixed effects, how does a generalized model impact results? 

pois1a <- glm(mental_health_hospitalizations ~ spatial_ind + temporal_ind + did,  family = "poisson", data = merge)

pois2a <- fepois(mental_health_hospitalizations ~ did | year + state, data = merge)
pois2a_alt <- glm(mental_health_hospitalizations ~ did + as.factor(year) + as.factor(state),  family = "poisson", data = merge)


pois1b <- glm(substance_disorders_hospitalizations ~ spatial_ind + temporal_ind + did,  family = "poisson", data = merge)
pois2b <- fepois(substance_disorders_hospitalizations ~ did | year + state, data = merge)


## Exploration of an event study...


period_dat <- data.frame(year = c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012),
                         period = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))


merge <- merge(merge, period_dat, by = "year")
merge$first_county # date of first drilling


#https://cran.r-project.org/web/packages/staggered/readme/README.html

out <- staggered(df = merge, 
          i = "residence_county",
          t = "period",
          g = "firstyr_county",
          y = "mental_health_hospitalizations", 
          estimand = "simple")