########################################################################################
# PROJECT       : CCHM
# SPONSOR/PI    : Willis/Nori-Sarma
# PROGRAM NAME  : OGD DiD manuscript modeling
# DESCRIPTION   : This work is done to support the development of a manuscript analyzing associations between oil and gas drilling and mental health (target: Nature Energy)
#                 
#                 
#                 
# PROGRAMMER    : Nina Cesare
# DATE WRITTEN  : 07/29/2024
########################################################################################
# INPUT FILES   : allcountyexposures_newIndicators.csv
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
library(ggplot2)
library(plm) # plm()
library(fixest) # feglm()


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


'%!in%' <- function(x,y)!('%in%'(x,y))

## Load state names fips


#### Process data: Load oil and gas, MH hospitalization data ####

## Gas data - by county, year
dat <- read.csv("oil_gas_development_medicaid/Data/oilgascounty_1_long_an.csv")

dat$residence_county <- unlist(lapply(dat$FIPS, function(x) county_fips_fix(as.character(x))))
dat$state_fips <- substr(dat$residence_county, 1,2)


dat$boom_year <- as.character(dat$ogd_change_group_cont)
dat$boom_year[which(dat$ogd_change_group_cont == "Boom")] <- 1
dat$boom_year[which(dat$ogd_change_group_cont == "Bust")] <- 0
dat$boom_year[which(dat$ogd_change_group_cont == "Status Quo")] <- 0

dat$bust_year <- as.character(dat$ogd_change_group_cont)
dat$bust_year[which(dat$ogd_change_group_cont == "Boom")] <- 0
dat$bust_year[which(dat$ogd_change_group_cont == "Bust")] <- 1
dat$bust_year[which(dat$ogd_change_group_cont == "Status Quo")] <- 0


dat$boom_year2 <- as.character(dat$ogd_change_group_cont2)
dat$boom_year2[which(dat$ogd_change_group_cont2 == "Boom")] <- 1
dat$boom_year2[which(dat$ogd_change_group_cont2 == "Bust")] <- 0
dat$boom_year2[which(dat$ogd_change_group_cont2 == "Status Quo")] <- 0


dat$bust_year2 <- as.character(dat$ogd_change_group_cont2)
dat$bust_year2[which(dat$ogd_change_group_cont2 == "Boom")] <- 0
dat$bust_year2[which(dat$ogd_change_group_cont2 == "Bust")] <- 1
dat$bust_year2[which(dat$ogd_change_group_cont2 == "Status Quo")] <- 0


dat$boom_year25_2 <- as.character(dat$ogd_change_group_cont25_2)
dat$boom_year25_2[which(dat$ogd_change_group_cont25_2 == "Boom")] <- 1
dat$boom_year25_2[which(dat$ogd_change_group_cont25_2 == "Bust")] <- 0
dat$boom_year25_2[which(dat$ogd_change_group_cont25_2 == "Status Quo")] <- 0

dat$bust_year25_2 <- as.character(dat$ogd_change_group_cont25_2)
dat$bust_year25_2[which(dat$ogd_change_group_cont25_2 == "Boom")] <- 0
dat$bust_year25_2[which(dat$ogd_change_group_cont25_2 == "Bust")] <- 1
dat$bust_year25_2[which(dat$ogd_change_group_cont25_2 == "Status Quo")] <- 0

dat$USDA_boom <- as.character(dat$oil_gas_change_group)
dat$USDA_boom[which(dat$oil_gas_change_group == "H_Growth")] <- 1
dat$USDA_boom[which(dat$oil_gas_change_group == "H_Decline")] <- 0
dat$USDA_boom[which(dat$oil_gas_change_group == "Status Quo")] <- 0

dat$USDA_bust <- as.character(dat$oil_gas_change_group)
dat$USDA_bust[which(dat$oil_gas_change_group == "H_Growth")] <- 0
dat$USDA_bust[which(dat$oil_gas_change_group == "H_Decline")] <- 1
dat$USDA_bust[which(dat$oil_gas_change_group == "Status Quo")] <- 0



## MH data aggregated to county, year
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


files <- list(dat00, dat01, dat02, dat03, dat04, dat05, dat06, dat07, dat08, dat09, dat10, dat11)
files_new <- vector(mode='list', length=12)
years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011)



for(i in 1:length(years)){
  file <- files[[i]]
  file$residence_county <- as.character(file$residence_county)
  file <- file[which(file$age_group != "0-18"),]
  
  file_new <- file %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% dplyr::summarize_all(.funs = c(sum), na.rm = TRUE)
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
                      files_new[[12]])


files_new_df$residence_county <- unlist(lapply(files_new_df$residence_county, function(x) county_fips_fix(as.character(x))))

## check to make sure all years are included and county FIPS codes contain 5 digits
table(files_new_df$year)
table(nchar(files_new_df$residence_county))


## Create beneficiary variables
# Race designations from CMS
#1	WHITE, NOT OF HISPANIC ORIGIN (CHANGED TO "WHITE" BEGINNING 10/98)
#2	BLACK, NOT OF HISPANIC ORIGIN (CHANGED TO "BLACK OR AFRICAN AMERICAN" BEGINNING 10/98)
#5	HISPANIC (CHANGED TO "HISPANIC OR LATINO - NO RACE INFORMATION AVAILABLE" BEGINNING 10/98)
#7	HISPANIC OR LATINO AND ONE OR MORE RACES (NEW CODE BEGINNING 10/98)

dem_files_new <- vector(mode='list', length=12)

for(i in 1:length(years)){
  file <- files[[i]]
  file <- file[which(file$age_group != "0-18"),]
  
  file$residence_county <- as.character(file$residence_county)
  
  total <- file %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("total" = "all_cause_hospitalizations")
  
  
  female <- file[which(file$sex == "F" & !is.na(file$sex)),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("female" = "all_cause_hospitalizations")
  male <- file[which(file$sex == "M"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("male" = "all_cause_hospitalizations")
  unknown <- file[which(file$sex == "U"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("unknown" = "all_cause_hospitalizations")
  
  
  white <- file[which(file$race == 1),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("white" = "all_cause_hospitalizations")
  black <- file[which(file$race == 2),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("black" = "all_cause_hospitalizations")
  hisp <- file[which(file$race == 5),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("hisp" = "all_cause_hospitalizations")
  hisp_oom <- file[which(file$race == 7),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("hisp_oom" = "all_cause_hospitalizations")
  
  age19_24 <- file[which(file$age_group == "19-24"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age19_24" = "all_cause_hospitalizations")
  age25_34 <- file[which(file$age_group == "25-34"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age25_34" = "all_cause_hospitalizations")
  age35_44 <- file[which(file$age_group == "35-44"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age35_44" = "all_cause_hospitalizations")
  age45_54 <- file[which(file$age_group == "45-54"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age45_54" = "all_cause_hospitalizations")
  age55_64 <- file[which(file$age_group == "55-64"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age55_64" = "all_cause_hospitalizations")
  age65_74 <- file[which(file$age_group == "65-74"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age65_74" = "all_cause_hospitalizations")
  age75_84 <- file[which(file$age_group == "75-84"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age75_84" = "all_cause_hospitalizations")
  age85_plus <- file[which(file$age_group == "85+"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age85_plus" = "all_cause_hospitalizations")
  
  file_new <- merge(total, female, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, male, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, unknown, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, white, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, black, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, hisp, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, hisp_oom, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, age19_24, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, age25_34, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, age35_44, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, age45_54, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, age55_64, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, age65_74, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, age75_84, by = "residence_county", all = TRUE)
  file_new <- merge(file_new, age85_plus, by = "residence_county", all = TRUE)
  
  file_new$year <- years[i]
  dem_files_new[[i]] <- file_new
  print(i)
}


dem_files_new_df <- rbind(dem_files_new[[1]],
                          dem_files_new[[2]],
                          dem_files_new[[3]],
                          dem_files_new[[4]],
                          dem_files_new[[5]],
                          dem_files_new[[6]],
                          dem_files_new[[7]],
                          dem_files_new[[8]],
                          dem_files_new[[9]],
                          dem_files_new[[10]],
                          dem_files_new[[11]],
                          dem_files_new[[12]])



dem_files_new_df$residence_county <- unlist(lapply(dem_files_new_df$residence_county, function(x) county_fips_fix(as.character(x))))

## check to make sure all years are included and county FIPS codes contain 5 digits
table(dem_files_new_df$year)
table(nchar(dem_files_new_df$residence_county))


## Merge county annual MH with county annual oil drilling

merge <- merge(files_new_df, dat, by = c("residence_county", "year"), all = TRUE)
merge <- merge(merge, dem_files_new_df, by = c("residence_county", "year"), all = TRUE) # 38299 x 78


## Add in state fips/abbreviations across the board (since this isn't in the medicaid data)
state_names <- read.csv("oil_gas_development_medicaid/Data/state_name_fips.csv")
state_names$state_fips_complete <- unlist(lapply(state_names$FIPS.Code, function(x) state_fips_fix(x)))
state_names <- state_names[which(state_names$State.Name != "VIRGIN ISLANDS" & state_names$State.Name != "AMERICAN SAMOA" & state_names$State.Name != "GUAM" & state_names$State.Name != "PUERTO RICO"),]
  
merge$state_fips_complete <- substr(merge$residence_county, 1,2)
length(which(is.na(merge$state_fips_complete)))

merge <- merge(merge, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80


## Remove states: Oregon (41), Washington (53), Idaho (16), Illinois (17). 
## Also, states where there has been ZERO oil OR gas production during this time. 
##  And AK/HI because USDA doesn't track those.
state_zero <- dat %>% dplyr::group_by(Stabr) %>% dplyr::summarize(oilsum = sum(oil_units, na.rm =TRUE),
                                                    gassum = sum(gas_units, na.rm =TRUE))
state_zero$Stabr[which(state_zero$oilsum == 0 & state_zero$gassum == 0)]


states_to_remove <- c("AK","HI","CT","DC","DE","GA","IA","ID","IL","MA","ME","MN","NC","NH","NJ","OR","RI","SC","VT","WA","WI")
merge <- merge[-which(merge$State.Abbreviation %in% states_to_remove),]


## Remove states with zero oil OR gas production:
counties_zero <- dat %>% dplyr::group_by(residence_county) %>% dplyr::summarize(oilsum = sum(oil_units, na.rm =TRUE),
                                                    gassum = sum(gas_units, na.rm =TRUE))


counties_zero <- counties_zero$residence_county[which(counties_zero$oilsum == 0 & counties_zero$gassum == 0)]
merge <- merge[which(merge$residence_county %!in% counties_zero),] 
length(unique(merge$residence_county)) ## 1171


## confirm there are no missing oil/gas units in the original USDA file 
length(which(is.na(dat$gas_units)))
length(which(is.na(dat$oil_units)))


na_counties <- unique(merge$residence_county[which(is.na(merge$oil_units))]) 
nonna_counties <- unique(merge$residence_county[which(!is.na(merge$oil_units))]) 

length(na_counties) ## 52 counties in the merge file, for which we have zero OGD info even after exclusions
length(nonna_counties) ## 1119 counties in the merge file, for which we have OGD info

na_data <- merge[which(merge$residence_county %in% na_counties),]
table(na_data$year) # most of these are counties that are only avaialable for a few years. Maybe they're defunct?



nrow(na_data)/nrow(merge) # overall, this is about 2.6 of the total dataset. It likely won't change results dramatically if we try to crosswalk

## are there places where we have oil units but no mental health hospitalizations?
length(which(!is.na(merge$oil_units) & is.na(merge$mental_health_hospitalizations))) # just 41 observations in the entire dataset
merge[which(!is.na(merge$oil_units) & is.na(merge$mental_health_hospitalizations)),]
unique(merge$residence_county[which(!is.na(merge$oil_units) & is.na(merge$mental_health_hospitalizations))]) # 17 counties



### Truncate MH outcomes for sensitivity analysis

pct95_mh <- quantile(merge$mental_health_hospitalizations, c(0.95), na.rm = TRUE)[1]
pct95_adj <- quantile(merge$adjustment_reaction_hospitalizations, c(0.95), na.rm = TRUE)[1]
pct95_anx <- quantile(merge$anxiety_disorders_hospitalizations, c(0.95), na.rm = TRUE)[1]
pct95_attn <- quantile(merge$attention_disorders_hospitalizations, c(0.95), na.rm = TRUE)[1]
pct95_mood <- quantile(merge$mood_disorders_hospitalizations, c(0.95), na.rm = TRUE)[1]
pct95_pers <- quantile(merge$personality_disorders_hospitalizations, c(0.95), na.rm = TRUE)[1]
pct95_schiz <- quantile(merge$schizophrenia_psychotic_disorders_hospitalizations, c(0.95), na.rm = TRUE)[1]
pct95_sub <- quantile(merge$substance_disorders_hospitalizations, c(0.95), na.rm = TRUE)[1]
pct95_alc <- quantile(merge$alcohol_disorders_hospitalizations, c(0.95), na.rm =TRUE)[1]
pct95_suic <- quantile(merge$suicide_self_harm_hospitalizations, c(0.95), na.rm = TRUE)[1]

merge$mental_health_hospitalizations_rs <- merge$mental_health_hospitalizations
merge$mental_health_hospitalizations_rs[which(merge$mental_health_hospitalizations > pct95_mh)] <- pct95_mh

merge$adjustment_reaction_hospitalizations_rs <- merge$adjustment_reaction_hospitalizations
merge$adjustment_reaction_hospitalizations_rs[which(merge$adjustment_reaction_hospitalizations > pct95_adj)] <- pct95_adj

merge$anxiety_disorders_hospitalizations_rs <- merge$anxiety_disorders_hospitalizations
merge$anxiety_disorders_hospitalizations_rs[which(merge$anxiety_disorders_hospitalizations > pct95_anx)] <- pct95_anx

merge$attention_disorders_hospitalizations_rs <- merge$attention_disorders_hospitalizations
merge$attention_disorders_hospitalizations_rs[which(merge$attention_disorders_hospitalizations > pct95_attn)] <- pct95_attn

merge$mood_disorders_hospitalizations_rs <- merge$mood_disorders_hospitalizations
merge$mood_disorders_hospitalizations_rs[which(merge$mood_disorders_hospitalizations > pct95_mood)] <- pct95_mood

merge$personality_disorders_hospitalizations_rs <- merge$personality_disorders_hospitalizations
merge$personality_disorders_hospitalizations_rs[which(merge$personality_disorders_hospitalizations > pct95_pers)] <- pct95_pers

merge$schizophrenia_psychotic_disorders_hospitalizations_rs <- merge$schizophrenia_psychotic_disorders_hospitalizations
merge$schizophrenia_psychotic_disorders_hospitalizations_rs[which(merge$schizophrenia_psychotic_disorders_hospitalizations > pct95_schiz)] <- pct95_schiz

merge$substance_disorders_hospitalizations_rs <- merge$substance_disorders_hospitalizations
merge$substance_disorders_hospitalizations_rs[which(merge$substance_disorders_hospitalizations > pct95_sub)] <- pct95_sub

merge$alcohol_disorders_hospitalizations_rs <- merge$alcohol_disorders_hospitalizations
merge$alcohol_disorders_hospitalizations_rs[which(merge$alcohol_disorders_hospitalizations > pct95_alc)] <- pct95_alc

merge$suicide_self_harm_hospitalizations_rs <- merge$suicide_self_harm_hospitalizations
merge$suicide_self_harm_hospitalizations_rs[which(merge$suicide_self_harm_hospitalizations > pct95_suic)] <- pct95_suic


## Create factor indicators

merge$ogd_change_group_cont2_char <- merge$ogd_change_group_cont2
merge$ogd_change_group_cont2_char <- factor(merge$ogd_change_group_cont2_char, levels = c("Status Quo","Boom","Bust"))


merge$oil_gas_change_group_char <- merge$oil_gas_change_group
merge$oil_gas_change_group_char <- factor(merge$oil_gas_change_group_char, levels = c("Status Quo","H_Growth","H_Decline"))

#### TOTAL N ####
## overall, we have non-missing observations in:
length(which(!is.na(merge$oil_units) & !is.na(merge$mental_health_hospitalizations))) # 13387 observations
length(unique(merge$residence_county[which(!is.na(merge$oil_units) & !is.na(merge$mental_health_hospitalizations))])) # 1118 counties


# Confirm OGD stats
dim(merge) #14467
length(unique(merge$residence_county)) # 1171 (1119 with non-missing OGD data)

table(merge$ogd_change_group_cont)

#Expected values:
#Boom       Bust Status Quo 
#2972       3143       6194 

table(merge$ogd_change_group_cont2)

#Expected values: 
#Boom       Bust Status Quo 
#3989       4751       3569


#### Descriptive statistics for source data ####

means <- merge[,c("mental_health_hospitalizations",
                  "adjustment_reaction_hospitalizations",
                  "anxiety_disorders_hospitalizations",
                  "attention_disorders_hospitalizations",
                  "mood_disorders_hospitalizations",
                  "personality_disorders_hospitalizations",
                  "schizophrenia_psychotic_disorders_hospitalizations",
                  "alcohol_disorders_hospitalizations",
                  "substance_disorders_hospitalizations",              
                  "suicide_self_harm_hospitalizations")] %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


vars <- merge[,c("mental_health_hospitalizations",
                 "adjustment_reaction_hospitalizations",
                 "anxiety_disorders_hospitalizations",
                 "attention_disorders_hospitalizations",
                 "mood_disorders_hospitalizations",
                 "personality_disorders_hospitalizations",
                 "schizophrenia_psychotic_disorders_hospitalizations",
                 "alcohol_disorders_hospitalizations",
                 "substance_disorders_hospitalizations",              
                 "suicide_self_harm_hospitalizations")] %>% summarise(across(where(is.numeric), ~ var(.x, na.rm = TRUE)))

medians <- merge[,c("mental_health_hospitalizations",
                  "adjustment_reaction_hospitalizations",
                  "anxiety_disorders_hospitalizations",
                  "attention_disorders_hospitalizations",
                  "mood_disorders_hospitalizations",
                  "personality_disorders_hospitalizations",
                  "schizophrenia_psychotic_disorders_hospitalizations",
                  "alcohol_disorders_hospitalizations",
                  "substance_disorders_hospitalizations",              
                  "suicide_self_harm_hospitalizations")] %>% summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)))

sds <- merge[,c("mental_health_hospitalizations",
                  "adjustment_reaction_hospitalizations",
                  "anxiety_disorders_hospitalizations",
                  "attention_disorders_hospitalizations",
                  "mood_disorders_hospitalizations",
                  "personality_disorders_hospitalizations",
                  "schizophrenia_psychotic_disorders_hospitalizations",
                  "alcohol_disorders_hospitalizations",
                  "substance_disorders_hospitalizations",              
                  "suicide_self_harm_hospitalizations")] %>% summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))





#### Testing modeling strategies #####
# https://libguides.princeton.edu/R-Panel

##### Set 1: Poisson models, no covariates. County/year FE. Boom/bust do NOT carry through #####


plm_mh1 <- feglm(mental_health_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_adj1 <- feglm(adjustment_reaction_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_anx1 <- feglm(anxiety_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_att1 <- feglm(attention_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_mood1 <- feglm(mood_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_pers1 <- feglm(personality_disorders_hospitalizations~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_schiz1 <- feglm(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_alc1 <- feglm(alcohol_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_sub1 <- feglm(substance_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_suic1 <- feglm(suicide_self_harm_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods1.txt")

print("Possion models (plm) with year, county fixed effects. No covariates.")
print("Contemporaneous boom/bust are episodic and NOT carry through.")

print("All mental health hospitalizations")
summary(plm_mh1)

print("Adjustment disorder")
summary(plm_adj1)

print("Anxiety disorder")
summary(plm_anx1)

print("Attention disorder")
summary(plm_att1)

print("Mood disorder")
summary(plm_mood1)

print("Personality disorder")
summary(plm_pers1)

print("Schizophrenia")
summary(plm_schiz1)

print("Alcohol use disorder")
summary(plm_alc1)

print("Substance use disorder")
summary(plm_sub1)

print("Suicide/self-harm")
summary(plm_suic1)

closeAllConnections()


##### Set 2: Poisson models, no covariates. County/year FE. Boom/bust DO carry through #####

plm_mh2 <- feglm(mental_health_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_adj2 <- feglm(adjustment_reaction_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_anx2 <- feglm(anxiety_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_att2 <- feglm(attention_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_mood2 <- feglm(mood_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_pers2 <- feglm(personality_disorders_hospitalizations~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_schiz2 <- feglm(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_alc2 <- feglm(alcohol_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_sub2 <- feglm(substance_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_suic2 <- feglm(suicide_self_harm_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods1_2.txt")

print("Possion models (plm) with year, county fixed effects. No covariates.")
print("Contemporaneous boom/bust are carried through in the second models (boom/bust_year2).")
print("UPDATED: 2000 is now designated as NA for both boom and bust year versions because we do not know prior year's production")


print("All mental health hospitalizations")
summary(plm_mh1)
summary(plm_mh2)

print("Adjustment disorder")
summary(plm_adj1)
summary(plm_adj2)

print("Anxiety disorder")
summary(plm_anx1)
summary(plm_anx2)

print("Attachment disorder")
summary(plm_att1)
summary(plm_att2)

print("Mood disorder")
summary(plm_mood1)
summary(plm_mood2)

print("Personality disorder")
summary(plm_pers1)
summary(plm_pers2)

print("Schizophrenia")
summary(plm_schiz1)
summary(plm_schiz2)

print("Alcohol use disorder")
summary(plm_alc1)
summary(plm_alc2)

print("Substance use disorder")
summary(plm_sub1)
summary(plm_sub2)

print("Suicide/self-harm")
summary(plm_suic1)
summary(plm_suic2)

closeAllConnections()


##### Set 3: Quasi-poisson models, no covariates. County/year FE. Boom/bust do NOT carry through #####


plm_mh3 <- feglm(mental_health_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_adj3 <- feglm(adjustment_reaction_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_anx3 <- feglm(anxiety_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_att3 <- feglm(attention_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_mood3 <- feglm(mood_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_pers3 <- feglm(personality_disorders_hospitalizations~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_schiz3 <- feglm(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_alc3 <- feglm(alcohol_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_sub3 <- feglm(substance_disorders_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)
plm_suic3 <- feglm(suicide_self_harm_hospitalizations ~ boom_year + bust_year | FIPS + year, data=merge, family=quasipoisson)


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods1_3_a.txt")

print("QUASI-possion models with year, county fixed effects. No covariates.")
print("Compared against plm() models using Poisson distribution")
print("Contemporaneous boom/bust are episodic and NOT carry through.")

print("#################################################################")

print("All mental health hospitalizations")
summary(plm_mh1)
summary(plm_mh3)

print("#################################################################")

print("Adjustment disorder")
summary(plm_adj1)
summary(plm_adj3)

print("#################################################################")

print("Anxiety disorder")
summary(plm_anx1)
summary(plm_anx3)

print("#################################################################")

print("Attachment disorder")
summary(plm_att1)
summary(plm_att3)

print("#################################################################")

print("Mood disorder")
summary(plm_mood1)
summary(plm_mood3)

print("#################################################################")

closeAllConnections()

sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods1_3_b.txt")

print("QUASI-possion models with year, county fixed effects. No covariates.")
print("Compared against plm() models using Poisson distribution")
print("Contemporaneous boom/bust are episodic and NOT carry through.")

print("#################################################################")

print("Personality disorder")
summary(plm_pers1)
summary(plm_pers3)

print("#################################################################")

print("Schizophrenia")
summary(plm_schiz1)
summary(plm_schiz3)

print("#################################################################")

print("Alcohol use disorder")
summary(plm_alc1)
summary(plm_alc3)

print("#################################################################")

print("Substance use disorder")
summary(plm_sub1)
summary(plm_sub3)

print("#################################################################")

print("Suicide/self-harm")
summary(plm_suic1)
summary(plm_suic3)

print("#################################################################")

closeAllConnections()




##### Set 4: Quasi-poisson models, no covariates. County/year FE. Boom/bust DO carry through #####


plm_mh4 <- feglm(mental_health_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_adj4 <- feglm(adjustment_reaction_hospitalizations ~ boom_year + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_anx4 <- feglm(anxiety_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_att4 <- feglm(attention_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_mood4 <- feglm(mood_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_pers4 <- feglm(personality_disorders_hospitalizations~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_schiz4 <- feglm(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_alc4 <- feglm(alcohol_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_sub4 <- feglm(substance_disorders_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)
plm_suic4 <- feglm(suicide_self_harm_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods2_4_a.txt")

print("QUASI-possion models with year, county fixed effects. No covariates.")
print("Compared against plm() models using Poisson distribution")
print("Contemporaneous boom/busts DO carry through.")

print("#################################################################")

print("All mental health hospitalizations")
summary(plm_mh2)
summary(plm_mh4)

print("#################################################################")

print("Adjustment disorder")
summary(plm_adj2)
summary(plm_adj4)

print("#################################################################")

print("Anxiety disorder")
summary(plm_anx2)
summary(plm_anx4)

print("#################################################################")

print("Attachment disorder")
summary(plm_att2)
summary(plm_att4)

print("#################################################################")

print("Mood disorder")
summary(plm_mood2)
summary(plm_mood4)

print("#################################################################")

closeAllConnections()

sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods2_4_b.txt")

print("QUASI-possion models with year, county fixed effects. No covariates.")
print("Compared against plm() models using Poisson distribution")
print("Contemporaneous boom/busts DO carry through.")

print("#################################################################")

print("Personality disorder")
summary(plm_pers2)
summary(plm_pers4)

print("#################################################################")

print("Schizophrenia")
summary(plm_schiz2)
summary(plm_schiz4)

print("#################################################################")

print("Alcohol use disorder")
summary(plm_alc2)
summary(plm_alc4)

print("#################################################################")

print("Substance use disorder")
summary(plm_sub2)
summary(plm_sub4)

print("#################################################################")

print("Suicide/self-harm")
summary(plm_suic2)
summary(plm_suic4)

print("#################################################################")

closeAllConnections()



##### Set 5: Negative binomial models, no covariates. County/year FE. Boom/bust designated by USDA. Year interacts with boom/bust #####

merge$year_fact <- factor(merge$year, levels = c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011"))
merge$year_num <- as.numeric(merge$year)

plm_mh5 <- fenegbin(mental_health_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_adj5 <- fenegbin(adjustment_reaction_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_anx5 <- fenegbin(anxiety_disorders_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_att5 <- fenegbin(attention_disorders_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_mood5 <- fenegbin(mood_disorders_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_pers5 <- fenegbin(personality_disorders_hospitalizations~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_schiz5 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_alc5 <- fenegbin(alcohol_disorders_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_sub5 <- fenegbin(substance_disorders_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)
plm_suic5 <- fenegbin(suicide_self_harm_hospitalizations ~ USDA_boom * year_num + USDA_bust * year_num + year_num | state_fips,  data=merge)



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods5.txt")

print("Negative binomial models using USDA continuous boom/bust designation")
print("State fixed effects only due to convergence issues")

print("All mental health hospitalizations")
summary(plm_mh5)

print("Adjustment disorder")
summary(plm_adj5)

print("Anxiety disorder")
summary(plm_anx5)

print("Attachment disorder")
summary(plm_att5)

print("Mood disorder")
summary(plm_mood5)

print("Personality disorder")
summary(plm_pers5)

print("Schizophrenia")
summary(plm_schiz5)

print("Alcohol use disorder")
summary(plm_alc5)

print("Substance use disorder")
summary(plm_sub5)

print("Suicide/self-harm")
summary(plm_suic5)

closeAllConnections()




##### Intermission: Take a step back and compare model fit, explore distributions #####


## Comparing three versions of this model, disregarding fixed effects for now
pois <- plm(mental_health_hospitalizations ~ boom_year2 + bust_year2 + year, data = merge)
quasipois <- feglm(mental_health_hospitalizations ~ boom_year2 + bust_year2 + year, data=merge, family=quasipoisson)
zpois <- psclm1 <- zeroinfl(mental_health_hospitalizations ~ boom_year2 + bust_year2 + year, data=merge)

summary(pois)
summary(quasipois)
summary(zpois)

## These are all over the place...probably because of the wild outcome distribution 
# logging might not make sense, but maybe we can truncate?
pct95_mh <- quantile(merge$mental_health_hospitalizations, c(0.95), na.rm = TRUE)[1]

merge$mental_health_hospitalizations_rescale <- merge$mental_health_hospitalizations
merge$mental_health_hospitalizations_rescale[which(merge$mental_health_hospitalizations > pct95_mh)] <- pct95_mh

summary(merge$mental_health_hospitalizations)
summary(merge$mental_health_hospitalizations_rescale)


## Still overdisperse but not quite as ridiculous
mean(merge$mental_health_hospitalizations, na.rm =TRUE)
var(merge$mental_health_hospitalizations, na.rm =TRUE)


mean(merge$mental_health_hospitalizations_rescale, na.rm =TRUE)
var(merge$mental_health_hospitalizations_rescale, na.rm =TRUE)


plm_mh2 <- plm(mental_health_hospitalizations ~ boom_year2 + bust_year2, data = merge, index = c("FIPS", "year"), model = "within")
plm_mh4 <- feglm(mental_health_hospitalizations ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)

plm_mh2b <- plm(mental_health_hospitalizations_rescale ~ boom_year2 + bust_year2, data = merge, index = c("FIPS", "year"), model = "within")
plm_mh4b <- feglm(mental_health_hospitalizations_rescale ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=quasipoisson)





sink("oil_gas_development_medicaid/Results/newAnalyticPlan/example_counties.txt")

print("####### County == 47076 ########")
as.data.frame(merge[which(merge$residence_county == "47067"),c("year","pct_change_oil","pct_change_gas","ogd_up_10pct","ogd_down_10pct","ogd_change_group_cont", "ogd_change_group_cont2","bust_year","boom_year","bust_year2","boom_year2")])

print("####### County == 28127 ########")
as.data.frame(merge[which(merge$residence_county == "28127"),c("year","pct_change_oil","pct_change_gas","ogd_up_10pct","ogd_down_10pct","ogd_change_group_cont", "ogd_change_group_cont2","bust_year","boom_year","bust_year2","boom_year2")])

print("####### County == 42015 ########")
as.data.frame(merge[which(merge$residence_county == "42015"),c("year","pct_change_oil","pct_change_gas","ogd_up_10pct","ogd_down_10pct","ogd_change_group_cont", "ogd_change_group_cont2","bust_year","boom_year","bust_year2","boom_year2")])


print("####### County == 01003 ########")
as.data.frame(merge[which(merge$residence_county == "01003"),c("year","pct_change_oil","pct_change_gas","ogd_up_10pct","ogd_down_10pct","ogd_change_group_cont", "ogd_change_group_cont2","bust_year","boom_year","bust_year2","boom_year2")])


print("####### County == 01007 ########")
as.data.frame(merge[which(merge$residence_county == "01007"),c("year","pct_change_oil","pct_change_gas","ogd_up_10pct","ogd_down_10pct","ogd_change_group_cont", "ogd_change_group_cont2","bust_year","boom_year","bust_year2","boom_year2")])

closeAllConnections()




## Hospitalization counts by boom/bust year 


sums_contemp <- hospMean <- merge[,c("ogd_change_group_cont25_2",
                                      "year",
                                      "mental_health_hospitalizations",
                                      "adjustment_reaction_hospitalizations",
                                      "anxiety_disorders_hospitalizations",
                                      "attention_disorders_hospitalizations",
                                      "mood_disorders_hospitalizations",
                                      "personality_disorders_hospitalizations",
                                      "schizophrenia_psychotic_disorders_hospitalizations",
                                      "alcohol_disorders_hospitalizations",
                                      "substance_disorders_hospitalizations",              
                                      "suicide_self_harm_hospitalizations")] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))



sums_contin <- hospMean <- merge[,c("oil_gas_change_group_char",
                                     "year",
                                     "mental_health_hospitalizations",
                                     "adjustment_reaction_hospitalizations",
                                     "anxiety_disorders_hospitalizations",
                                     "attention_disorders_hospitalizations",
                                     "mood_disorders_hospitalizations",
                                     "personality_disorders_hospitalizations",
                                     "schizophrenia_psychotic_disorders_hospitalizations",
                                     "alcohol_disorders_hospitalizations",
                                     "substance_disorders_hospitalizations",              
                                     "suicide_self_harm_hospitalizations")] %>% group_by(oil_gas_change_group_char, year) %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))




means_contemp <- hospMean <- merge[,c("ogd_change_group_cont25_2",
                                      "year",
                                      "mental_health_hospitalizations",
                                      "adjustment_reaction_hospitalizations",
                                      "anxiety_disorders_hospitalizations",
                                      "attention_disorders_hospitalizations",
                                      "mood_disorders_hospitalizations",
                                      "personality_disorders_hospitalizations",
                                      "schizophrenia_psychotic_disorders_hospitalizations",
                                      "alcohol_disorders_hospitalizations",
                                      "substance_disorders_hospitalizations",              
                                      "suicide_self_harm_hospitalizations")] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))



means_contin <- hospMean <- merge[,c("oil_gas_change_group_char",
                                     "year",
                                     "mental_health_hospitalizations",
                                     "adjustment_reaction_hospitalizations",
                                     "anxiety_disorders_hospitalizations",
                                     "attention_disorders_hospitalizations",
                                     "mood_disorders_hospitalizations",
                                     "personality_disorders_hospitalizations",
                                     "schizophrenia_psychotic_disorders_hospitalizations",
                                     "alcohol_disorders_hospitalizations",
                                     "substance_disorders_hospitalizations",              
                                     "suicide_self_harm_hospitalizations")] %>% group_by(oil_gas_change_group_char, year) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
                                     

# Contemporaneous - means

means_contemp_melt <- reshape2::melt(means_contemp, id.vars = c("ogd_change_group_cont25_2","year"))
names(means_contemp_melt)[1] <- "boom_bust"


means_contemp_melt$variable_clean <- as.character(means_contemp_melt$variable)
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "mental_health_hospitalizations")] <- "All MH"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "adjustment_reaction_hospitalizations")] <- "Adjustment"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "anxiety_disorders_hospitalizations")] <- "Anxiety"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "attention_disorders_hospitalizations")] <- "Attention"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "mood_disorders_hospitalizations")] <- "Mood"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "personality_disorders_hospitalizations")] <- "Personality"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "schizophrenia_psychotic_disorders_hospitalizations")] <- "Schizophrenia"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "alcohol_disorders_hospitalizations")] <- "Alcohol"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "substance_disorders_hospitalizations")] <- "Substance use"
means_contemp_melt$variable_clean[which(means_contemp_melt$variable == "suicide_self_harm_hospitalizations")] <- "Suicide/self-harm"

png(file="oil_gas_development_medicaid/Figures/mean_hospitalizations_contemporaneous.png", width=8, height=6, units="in", res=300)
ggplot(means_contemp_melt[which(!is.na(means_contemp_melt$boom_bust)),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("Contemporaneous designation, mean values") +
  facet_wrap(.~variable_clean) + 
  ylab("mean hospitalizations") + 
  theme(axis.text.x = element_text(angle = 45))
dev.off()

png(file="oil_gas_development_medicaid/Figures/mean_mh_hospitalizations_contemporaneous.png", width=8, height=6, units="in", res=300)
ggplot(means_contemp_melt[which(!is.na(means_contemp_melt$boom_bust) & means_contemp_melt$variable_clean == "All MH"),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("All Mental Health Hospitalizations: \n Contemporaneous boom/bust designation at 25% change, mean values") +
  ylab("Mean hospitalizations") + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2011, by = 1)) + 
  theme_light()
dev.off()


# Contemporaneous - sums

sums_contemp_melt <- reshape2::melt(sums_contemp, id.vars = c("ogd_change_group_cont25_2","year"))
names(sums_contemp_melt)[1] <- "boom_bust"


sums_contemp_melt$variable_clean <- as.character(sums_contemp_melt$variable)
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "mental_health_hospitalizations")] <- "All MH"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "adjustment_reaction_hospitalizations")] <- "Adjustment"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "anxiety_disorders_hospitalizations")] <- "Anxiety"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "attention_disorders_hospitalizations")] <- "Attention"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "mood_disorders_hospitalizations")] <- "Mood"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "personality_disorders_hospitalizations")] <- "Personality"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "schizophrenia_psychotic_disorders_hospitalizations")] <- "Schizophrenia"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "alcohol_disorders_hospitalizations")] <- "Alcohol"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "substance_disorders_hospitalizations")] <- "Substance use"
sums_contemp_melt$variable_clean[which(sums_contemp_melt$variable == "suicide_self_harm_hospitalizations")] <- "Suicide/self-harm"


png(file="oil_gas_development_medicaid/Figures/sum_hospitalizations_contemporaneous.png", width=8, height=6, units="in", res=300)
ggplot(sums_contemp_melt[which(!is.na(sums_contemp_melt$boom_bust)),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("Contemporaneous designation, sum values") +
  facet_wrap(.~variable_clean) + 
  ylab("sum hospitalizations") + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 45))
dev.off()


# continuous - means

means_contin_melt <- reshape2::melt(means_contin, id.vars = c("oil_gas_change_group_char","year"))
names(means_contin_melt)[1] <- "boom_bust"


means_contin_melt$variable_clean <- as.character(means_contin_melt$variable)
means_contin_melt$variable_clean[which(means_contin_melt$variable == "mental_health_hospitalizations")] <- "All MH"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "adjustment_reaction_hospitalizations")] <- "Adjustment"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "anxiety_disorders_hospitalizations")] <- "Anxiety"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "attention_disorders_hospitalizations")] <- "Attention"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "mood_disorders_hospitalizations")] <- "Mood"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "personality_disorders_hospitalizations")] <- "Personality"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "schizophrenia_psychotic_disorders_hospitalizations")] <- "Schizophrenia"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "alcohol_disorders_hospitalizations")] <- "Alcohol"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "substance_disorders_hospitalizations")] <- "Substance use"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "suicide_self_harm_hospitalizations")] <- "Suicide/self-harm"


means_contin_melt$boom_bust <- as.character(means_contin_melt$boom_bust)
means_contin_melt$boom_bust[means_contin_melt$boom_bust == "H_Growth"] <- "Boom"
means_contin_melt$boom_bust[means_contin_melt$boom_bust == "H_Decline"] <- "Bust"

means_contin_melt$boom_bust <- factor(means_contin_melt$boom_bust, levels = c("Boom","Bust","Status Quo"))


png(file="oil_gas_development_medicaid/Figures/mean_hospitalizations_continuous.png", width=8, height=6, units="in", res=300)
ggplot(means_contin_melt[which(!is.na(means_contin_melt$boom_bust)),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("Continuous (USDA) designation, mean values") +
  facet_wrap(.~variable_clean) + 
  ylab("mean hospitalizations") + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 45))
dev.off()


png(file="oil_gas_development_medicaid/Figures/mean_mh_hospitalizations_continous.png", width=8, height=6, units="in", res=300)
ggplot(means_contin_melt[which(!is.na(means_contin_melt$boom_bust) & means_contin_melt$variable_clean == "All MH"),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("All Mental Health Hospitalizations: \n USDA boom/bust designation, mean values") +
  ylab("Mean hospitalizations") + 
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2011, by = 1)) + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 45)) 
dev.off()


# continuous - sums

sums_contin_melt <- reshape2::melt(sums_contin, id.vars = c("oil_gas_change_group_char","year"))
names(sums_contin_melt)[1] <- "boom_bust"


sums_contin_melt$variable_clean <- as.character(sums_contin_melt$variable)
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "mental_health_hospitalizations")] <- "All MH"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "adjustment_reaction_hospitalizations")] <- "Adjustment"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "anxiety_disorders_hospitalizations")] <- "Anxiety"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "attention_disorders_hospitalizations")] <- "Attention"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "mood_disorders_hospitalizations")] <- "Mood"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "personality_disorders_hospitalizations")] <- "Personality"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "schizophrenia_psychotic_disorders_hospitalizations")] <- "Schizophrenia"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "alcohol_disorders_hospitalizations")] <- "Alcohol"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "substance_disorders_hospitalizations")] <- "Substance use"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "suicide_self_harm_hospitalizations")] <- "Suicide/self-harm"

png(file="oil_gas_development_medicaid/Figures/sum_hospitalizations_continuous.png", width=8, height=6, units="in", res=300)
ggplot(sums_contin_melt[which(!is.na(sums_contin_melt$boom_bust)),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  facet_wrap(.~variable_clean) + 
  ylab("sum hospitalizations") + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 45))

dev.off()


##### Set 6: Poisson models, no covariates. County/year FE. Boom/bust do NOT carry through. Uses truncated outcomes (95th pctl.) ######


plm_mh6 <- feglm(mental_health_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_adj6 <- feglm(adjustment_reaction_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_anx6 <- feglm(anxiety_disorders_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_att6 <- feglm(attention_disorders_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_mood6 <- feglm(mood_disorders_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_pers6 <- feglm(personality_disorders_hospitalizations_rs~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_schiz6 <- feglm(schizophrenia_psychotic_disorders_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_alc6 <- feglm(alcohol_disorders_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_sub6 <- feglm(substance_disorders_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)
plm_suic6 <- feglm(suicide_self_harm_hospitalizations_rs ~ boom_year + bust_year | FIPS + year, data=merge, family=poisson)



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods1_6.txt")

print("Possion models (plm) with year, county fixed effects. No covariates.")
print("Contemporaneous boom/bust indicatores are NOT carried through in both model sets (boom/bust_year2).")
print("NOTE: MH outcomes are truncated at the 95th percentile to manage outliers")


print("All mental health hospitalizations")
summary(plm_mh1)
summary(plm_mh6)

print("Adjustment disorder")
summary(plm_adj1)
summary(plm_adj6)

print("Anxiety disorder")
summary(plm_anx1)
summary(plm_anx6)

print("Attachment disorder")
summary(plm_att1)
summary(plm_att6)

print("Mood disorder")
summary(plm_mood1)
summary(plm_mood6)

print("Personality disorder")
summary(plm_pers1)
summary(plm_pers6)

print("Schizophrenia")
summary(plm_schiz1)
summary(plm_schiz6)

print("Alcohol use disorder")
summary(plm_alc1)
summary(plm_alc6)

print("Substance use disorder")
summary(plm_sub1)
summary(plm_sub6)

print("Suicide/self-harm")
summary(plm_suic1)
summary(plm_suic6)

closeAllConnections()





##### Set 7: Poisson models, no covariates. County/year FE. Boom/bust DO carry through. Uses truncated outcomes (95th pctl.) ######


plm_mh7 <- feglm(mental_health_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_adj7 <- feglm(adjustment_reaction_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_anx7 <- feglm(anxiety_disorders_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_att7 <- feglm(attention_disorders_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_mood7 <- feglm(mood_disorders_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_pers7 <- feglm(personality_disorders_hospitalizations_rs~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_schiz7 <- feglm(schizophrenia_psychotic_disorders_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_alc7 <- feglm(alcohol_disorders_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_sub7 <- feglm(substance_disorders_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)
plm_suic7 <- feglm(suicide_self_harm_hospitalizations_rs ~ boom_year2 + bust_year2 | FIPS + year, data=merge, family=poisson)



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods2_7.txt")

print("Possion models (plm) with year, county fixed effects. No covariates.")
print("Contemporaneous boom/bust indicatores are carried through in both model sets (boom/bust_year2).")
print("NOTE: MH outcomes are truncated at the 95th percentile to manage outliers")


print("All mental health hospitalizations")
summary(plm_mh2)
summary(plm_mh7)

print("Adjustment disorder")
summary(plm_adj2)
summary(plm_adj7)

print("Anxiety disorder")
summary(plm_anx2)
summary(plm_anx7)

print("Attachment disorder")
summary(plm_att2)
summary(plm_att7)

print("Mood disorder")
summary(plm_mood2)
summary(plm_mood7)

print("Personality disorder")
summary(plm_pers2)
summary(plm_pers7)

print("Schizophrenia")
summary(plm_schiz2)
summary(plm_schiz7)

print("Alcohol use disorder")
summary(plm_alc2)
summary(plm_alc7)

print("Substance use disorder")
summary(plm_sub2)
summary(plm_sub7)

print("Suicide/self-harm")
summary(plm_suic2)
summary(plm_suic7)

closeAllConnections()




##### Set 8: Negative binomial models, no covariates. County/year FE. Boom/bust do NOT carry through #####


plm_mh8 <- fenegbin(mental_health_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_adj8 <- fenegbin(adjustment_reaction_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_anx8 <- fenegbin(anxiety_disorders_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_att8 <- fenegbin(attention_disorders_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_mood8 <- fenegbin(mood_disorders_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_pers8 <- fenegbin(personality_disorders_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_schiz8 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_alc8 <- fenegbin(alcohol_disorders_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_sub8 <- fenegbin(substance_disorders_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)
plm_suic8 <- fenegbin(suicide_self_harm_hospitalizations ~ boom_year + bust_year | residence_county + year, data = merge)


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods8.txt")

print("Negative binomial models with year, county fixed effects. No covariates.")
print("Contemporaneous boom/bust indicatores are NOT carried through.")


print("All mental health hospitalizations")
summary(plm_mh8)

print("Adjustment disorder")
summary(plm_adj8)

print("Anxiety disorder")
summary(plm_anx8)

print("Attachment disorder")
summary(plm_att8)

print("Mood disorder")
summary(plm_mood8)

print("Personality disorder")
summary(plm_pers8)

print("Schizophrenia")
summary(plm_schiz8)

print("Alcohol use disorder")
summary(plm_alc8)

print("Substance use disorder")
summary(plm_sub8)

print("Suicide/self-harm")
summary(plm_suic8)

closeAllConnections()



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods1_3_6_8_mh_only.txt")

print("Comparing models with year, county fixed effects. No covariates.")
print("Contemporaneous boom/bust indicators are carried NOT through in all models.")
print("Total mental health hospitalizations only")


print("AIC- All mental health hospitalizations: Poisson")
AIC(plm_mh1)

print("AIC- All mental health hospitalizations: Quasipoisson")
AIC(plm_mh3)

print("AIC - All mental health hospitalizations: Poisson - truncated")
AIC(plm_mh6)

print("AIC - All mental health hospitalizations: Negative binomial")
AIC(plm_mh8)

print("Summary - All mental health hospitalizations: Poisson")
summary(plm_mh1)

print("Summary - All mental health hospitalizations: Quasipoisson")
summary(plm_mh3)

print("Summary - All mental health hospitalizations: Poisson - truncated")
summary(plm_mh6)

print("Summary - All mental health hospitalizations: Negative binomial")
summary(plm_mh8)

closeAllConnections()


##### Set 9: Negative binomial models, no covariates. County/year FE. Boom/bust DO carry through #####

plm_mh9 <- fenegbin(mental_health_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_adj9 <- fenegbin(adjustment_reaction_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_anx9 <- fenegbin(anxiety_disorders_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_att9 <- fenegbin(attention_disorders_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_mood9 <- fenegbin(mood_disorders_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_pers9 <- fenegbin(personality_disorders_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_schiz9 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_alc9 <- fenegbin(alcohol_disorders_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_sub9 <- fenegbin(substance_disorders_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)
plm_suic9 <- fenegbin(suicide_self_harm_hospitalizations ~ boom_year2 + bust_year2 | residence_county + year, data = merge)


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods9.txt")

print("Negative binomial models with year, county fixed effects. No covariates.")
print("Contemporaneous boom/bust indicatores ARE carried through.")


print("All mental health hospitalizations")
summary(plm_mh9)

print("Adjustment disorder")
summary(plm_adj9)

print("Anxiety disorder")
summary(plm_anx9)

print("Attachment disorder")
summary(plm_att9)

print("Mood disorder")
summary(plm_mood9)

print("Personality disorder")
summary(plm_pers9)

print("Schizophrenia")
summary(plm_schiz9)

print("Alcohol use disorder")
summary(plm_alc9)

print("Substance use disorder")
summary(plm_sub9)

print("Suicide/self-harm")
summary(plm_suic9)

closeAllConnections()




sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods2_4_7_9_mh_only.txt")

print("Comparing models with year, county fixed effects. No covariates.")
print("Contemporaneous boom/bust indicators DO carry through in all models.")
print("Total mental health hospitalizations only")


print("AIC- All mental health hospitalizations: Poisson")
AIC(plm_mh2)

print("AIC- All mental health hospitalizations: Quasipoisson")
AIC(plm_mh4)

print("AIC - All mental health hospitalizations: Poisson - truncated")
AIC(plm_mh7)

print("AIC - All mental health hospitalizations: Negative binomial")
AIC(plm_mh9)

print("Summary - All mental health hospitalizations: Poisson")
summary(plm_mh2)

print("Summary - All mental health hospitalizations: Quasipoisson")
summary(plm_mh4)

print("Summary - All mental health hospitalizations: Poisson - truncated")
summary(plm_mh7)

print("Summary - All mental health hospitalizations: Negative binomial")
summary(plm_mh9)

closeAllConnections()




##### Set 10: Negative binomial models, no covariates. County/year FE. Contemporaneous boom/bust/status quo treated as categorical (using carry forward) #####


plm_mh10 <- fenegbin(mental_health_hospitalizations ~  ogd_change_group_cont2_char | residence_county + year, data = merge)
plm_adj10 <- fenegbin(adjustment_reaction_hospitalizations ~ ogd_change_group_cont2_char  | residence_county + year, data = merge)
plm_anx10 <- fenegbin(anxiety_disorders_hospitalizations ~ ogd_change_group_cont2_char  | residence_county + year, data = merge)
plm_att10 <- fenegbin(attention_disorders_hospitalizations ~ ogd_change_group_cont2_char  | residence_county + year, data = merge)
plm_mood10 <- fenegbin(mood_disorders_hospitalizations ~ ogd_change_group_cont2_char | residence_county + year, data = merge)
plm_pers10 <- fenegbin(personality_disorders_hospitalizations ~ ogd_change_group_cont2_char | residence_county + year, data = merge)
plm_schiz10 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ ogd_change_group_cont2_char | residence_county + year, data = merge)
plm_alc10 <- fenegbin(alcohol_disorders_hospitalizations ~ ogd_change_group_cont2_char | residence_county + year, data = merge)
plm_sub10 <- fenegbin(substance_disorders_hospitalizations ~ ogd_change_group_cont2_char | residence_county + year, data = merge)
plm_suic10 <- fenegbin(suicide_self_harm_hospitalizations ~ ogd_change_group_cont2_char | residence_county + year, data = merge)



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods10.txt")

print("Negative binomial models with year, county fixed effects. No covariates.")
print("Uses categorical boom/bust/status quo with our contemporaneous indicator (using carry-through)")


print("All mental health hospitalizations")
summary(plm_mh10)

print("Adjustment disorder")
summary(plm_adj10)

print("Anxiety disorder")
summary(plm_anx10)

print("Attachment disorder")
summary(plm_att10)

print("Mood disorder")
summary(plm_mood10)

print("Personality disorder")
summary(plm_pers10)

print("Schizophrenia")
summary(plm_schiz10)

print("Alcohol use disorder")
summary(plm_alc10)

print("Substance use disorder")
summary(plm_sub10)

print("Suicide/self-harm")
summary(plm_suic10)

closeAllConnections()




##### Set 11: Negative binomial models, state covariate. Year FE. Contemporaneous boom/bust/status quo treated as categorical (using USDA categorization) #####


plm_mh11 <- fenegbin(mental_health_hospitalizations ~  oil_gas_change_group_char + state_fips | year, data = merge)
plm_adj11 <- fenegbin(adjustment_reaction_hospitalizations ~ oil_gas_change_group_char + state_fips  | year, data = merge)
plm_anx11 <- fenegbin(anxiety_disorders_hospitalizations ~ oil_gas_change_group_char + state_fips |  year, data = merge)
plm_att11 <- fenegbin(attention_disorders_hospitalizations ~ oil_gas_gas_change_group_char + state_fips | year, data = merge)
plm_mood11 <- fenegbin(mood_disorders_hospitalizations ~ oil_gas_change_group_char + state_fips | year, data = merge)
plm_pers11 <- fenegbin(personality_disorders_hospitalizations ~ oil_gas_change_group_char + state_fips | year, data = merge)
plm_schiz11 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ oil_gas_change_group_char + state_fips | year, data = merge)
plm_alc11 <- fenegbin(alcohol_disorders_hospitalizations ~ oil_gas_change_group_char + state_fips | year, data = merge)
plm_sub11 <- fenegbin(substance_disorders_hospitalizations ~ oil_gas_change_group_char + state_fips | year, data = merge)
plm_suic11 <- fenegbin(suicide_self_harm_hospitalizations ~ oil_gas_change_group_char + state_fips | year, data = merge)



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods11.txt")

print("Negative binomial models with year fixed effects. State covariate")
print("Uses categorical boom/bust/status quo with USDA continuous indicator")
print("Year fixed effects ONLY")


print("All mental health hospitalizations")
summary(plm_mh11)

print("Adjustment disorder")
summary(plm_adj11)

print("Anxiety disorder")
summary(plm_anx11)

print("Attachment disorder")
summary(plm_att11)

print("Mood disorder")
summary(plm_mood11)

print("Personality disorder")
summary(plm_pers11)

print("Schizophrenia")
summary(plm_schiz11)

print("Alcohol use disorder")
summary(plm_alc11)

print("Substance use disorder")
summary(plm_sub11)

print("Suicide/self-harm")
summary(plm_suic11)

closeAllConnections()





##### Set 12: Negative binomial models. Year fixed effects. #####


plm_mh12 <- fenegbin(mental_health_hospitalizations ~  USDA_boom + USDA_bust + state_fips | year, data = merge)
plm_adj12 <- fenegbin(adjustment_reaction_hospitalizations ~ ogd_change_group_cont2_char + state_fips  | year, data = merge)
plm_anx12 <- fenegbin(anxiety_disorders_hospitalizations ~ ogd_change_group_cont2_char  + state_fips |  year, data = merge)
plm_att12 <- fenegbin(attention_disorders_hospitalizations ~ ogd_change_group_cont2_char  + state_fips | year, data = merge)
plm_mood12 <- fenegbin(mood_disorders_hospitalizations ~ ogd_change_group_cont2_char  + state_fips | year, data = merge)
plm_pers12 <- fenegbin(personality_disorders_hospitalizations ~ ogd_change_group_cont2_char + state_fips | year, data = merge)
plm_schiz12 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ ogd_change_group_cont2_char  + state_fips | year, data = merge)
plm_alc12 <- fenegbin(alcohol_disorders_hospitalizations ~ ogd_change_group_cont2_char  + state_fips | year, data = merge)
plm_sub12 <- fenegbin(substance_disorders_hospitalizations ~ ogd_change_group_cont2_char  + state_fips | year, data = merge)
plm_suic12 <- fenegbin(suicide_self_harm_hospitalizations ~ ogd_change_group_cont2_char  + state_fips | year, data = merge)





##### Set 13: Negative binomial models, no covariates. County/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through #####

# Test what happens if we don't include the odd years
#summary(fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$year != 2010 & merge$year != 2011),]))
#summary(fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge))

plm_mh13 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_adj13 <- fenegbin(adjustment_reaction_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_anx13 <- fenegbin(anxiety_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_att13 <- fenegbin(attention_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_mood13 <- fenegbin(mood_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_pers13 <- fenegbin(personality_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_schiz13 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_alc13 <- fenegbin(alcohol_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_sub13 <- fenegbin(substance_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)
plm_suic13 <- fenegbin(suicide_self_harm_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge)


## missingness checks
agg_cnty <- merge %>% group_by(residence_county) %>% summarise(mhsum = sum(mental_health_hospitalizations, na.rm = TRUE),
                                                               navals = length(which(is.na(mental_health_hospitalizations))),
                                                               len = length(mental_health_hospitalizations))
#zerosum_cnty <- agg_cnty$residence_county[which(agg_cnty$mhsum == 0)] # 6 counties with all zero values 
#length(which(agg_cnty$navals == agg_cnty$len)) # 1 of these has all NA


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods13.txt")

print("Negative binomial models with county and year fixed effects. No covariates")
print("Uses contemporaneous boom/bust indicator, carried through, with 25% threshold")
print("")

print("All mental health hospitalizations")
summary(plm_mh13)

print("Adjustment disorder")
summary(plm_adj13)

print("Anxiety disorder")
summary(plm_anx13)

print("Attachment disorder")
summary(plm_att13)

print("Mood disorder")
summary(plm_mood13)

print("Personality disorder")
summary(plm_pers13)

print("Schizophrenia")
summary(plm_schiz13)

print("Alcohol use disorder")
summary(plm_alc13)

print("Substance use disorder")
summary(plm_sub13)

print("Suicide/self-harm")
summary(plm_suic13)

closeAllConnections()


## Export IRR for these models
sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods13_irr.txt")

print("Mental health hospitalizations")

plm_mh13_out <- cbind(Estimate = coef(plm_mh13), confint(plm_mh13))
plm_mh13_irr <- exp(plm_mh13_out)
plm_mh13_irr

print("Adjustment disorder")
plm_adj13_out <- cbind(Estimate = coef(plm_adj13), confint(plm_adj13))
plm_adj13_irr <- exp(plm_adj13_out)
plm_adj13_irr

print("Anxiety disorder")
plm_anx13_out <- cbind(Estimate = coef(plm_anx13), confint(plm_anx13))
plm_anx13_irr <- exp(plm_anx13_out)
plm_anx13_irr


print("Attachment disorder")
plm_att13_out <- cbind(Estimate = coef(plm_att13), confint(plm_att13))
plm_att13_irr <- exp(plm_att13_out)
plm_att13_irr


print("Mood disorder")
plm_mood13_out <- cbind(Estimate = coef(plm_mood13), confint(plm_mood13))
plm_mood13_irr <- exp(plm_mood13_out)
plm_mood13_irr

print("Personality disorder")
plm_pers13_out <- cbind(Estimate = coef(plm_pers13), confint(plm_pers13))
plm_pers13_irr <- exp(plm_pers13_out)
plm_pers13_irr

print("Schizophrenia")
plm_schiz13_out <- cbind(Estimate = coef(plm_schiz13), confint(plm_schiz13))
plm_schiz13_irr <- exp(plm_schiz13_out)
plm_schiz13_irr

print("Alcohol use disorder")
plm_alc13_out <- cbind(Estimate = coef(plm_alc13), confint(plm_alc13))
plm_alc13_irr <- exp(plm_alc13_out)
plm_alc13_irr


print("Substance use disorder")
plm_sub13_out <- cbind(Estimate = coef(plm_sub13), confint(plm_sub13))
plm_sub13_irr <- exp(plm_sub13_out)
plm_sub13_irr


print("Suicide/self-harm")
plm_suic13_out <- cbind(Estimate = coef(plm_suic13), confint(plm_suic13))
plm_suic13_irr <- exp(plm_suic13_out)
plm_suic13_irr

closeAllConnections()


## N values 


mh_n <- sum(merge$mental_health_hospitalizations[-plm_mh13$obsRemoved])
adj_n <- sum(merge$adjustment_reaction_hospitalizations[-plm_adj13$obsRemoved])
att_n <- sum(merge$attention_disorders_hospitalizations[-plm_att13$obsRemoved])
anx_n <- sum(merge$anxiety_disorders_hospitalizations[-plm_anx13$obsRemoved])
mood_n <- sum(merge$mood_disorders_hospitalizations[-plm_mood13$obsRemoved])
pers_n <- sum(merge$personality_disorders_hospitalizations[-plm_pers13$obsRemoved])
schiz_n <- sum(merge$schizophrenia_psychotic_disorders_hospitalizations[-plm_schiz13$obsRemoved])
sub_n <- sum(merge$substance_disorders_hospitalizations[-plm_sub13$obsRemoved])
alc_n <- sum(merge$alcohol_disorders_hospitalizations[-plm_alc13$obsRemoved])
suic_n <- sum(merge$suicide_self_harm_hospitalizations[-plm_suic13$obsRemoved])

mh_cnty <- length(unique(merge$residence_county[-plm_mh13$obsRemoved]))
adj_cnty <- length(unique(merge$residence_county[-plm_adj13$obsRemoved]))
att_cnty <- length(unique(merge$residence_county[-plm_att13$obsRemoved]))
anx_cnty <- length(unique(merge$residence_county[-plm_anx13$obsRemoved]))
mood_cnty <- length(unique(merge$residence_county[-plm_mood13$obsRemoved]))
pers_cnty <- length(unique(merge$residence_county[-plm_pers13$obsRemoved]))
schiz_cnty <- length(unique(merge$residence_county[-plm_schiz13$obsRemoved]))
sub_cnty <- length(unique(merge$residence_county[-plm_sub13$obsRemoved]))
alc_cnty <- length(unique(merge$residence_county[-plm_alc13$obsRemoved]))
suic_cnty <- length(unique(merge$residence_county[-plm_suic13$obsRemoved]))




tab3_n <- data.frame(conditions = c("All MH","adj", "attn", "anxiety","mood","pers","schiz","sub","alc","suic"),
                     hosp = c(mh_n, adj_n, att_n, anx_n, mood_n, pers_n, schiz_n, sub_n, alc_n, suic_n),
                     county = c(mh_cnty, adj_cnty, att_cnty, anx_cnty, mood_cnty, pers_cnty, schiz_cnty, sub_cnty, alc_cnty, suic_cnty))


write.csv(tab3_n, "/n/dominici_nsaph_l3/Lab/projects/oil_gas_development_medicaid/Results/newAnalyticPlan/tab3_n.csv")




##### Stratifying by Urban/Rural ######

#0 = micro
#1 = noncore
#2 = metro

#Metro_Nonmetro_2013
#0 = nonmetro
#1 = metro

plm_mh13_nonmetro <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Nonmetro_2013 == 0),])
plm_mh13_metro <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Nonmetro_2013 == 1),])


plm_mh13_micro <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 0),])
plm_mh13_noncore <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 1),])
plm_mh13_metrocore <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 2),])


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods13_mh_metroNonmetro.txt")

print("Models stratified by metro status")
print("Negative binomial models, no covariates. Year/county FE")
print("Contemporaneous boom/bust w/25% threshold. DOES carry through")


print("##########################################")
print("All-nonmetro")
summary(plm_mh13_nonmetro)
print("All metro")
summary(plm_mh13_metro)


print("##########################################")
print("Micro only")
summary(plm_mh13_micro)
print("Metro noncore")
summary(plm_mh13_noncore)
print("Metro core")
summary(plm_mh13_metrocore)

closeAllConnections()



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods13_mh_metroNonmetro_irr.txt")


print("Rurality models - Micro")
plm_mh13_micro_out <- cbind(Estimate = coef(plm_mh13_micro), confint(plm_mh13_micro))
plm_mh13_micro_irr <- exp(plm_mh13_micro_out)
plm_mh13_micro_irr

print("Rurality models - Noncore")
plm_mh13_noncore_out <- cbind(Estimate = coef(plm_mh13_noncore), confint(plm_mh13_noncore))
plm_mh13_noncore_irr <- exp(plm_mh13_noncore_out)
plm_mh13_noncore_irr


print("Rurality models - Core")
plm_mh13_metrocore_out <- cbind(Estimate = coef(plm_mh13_metrocore), confint(plm_mh13_metrocore))
plm_mh13_metrocore_irr <- exp(plm_mh13_metrocore_out)
plm_mh13_metrocore_irr


closeAllConnections()

## N values (will export as table later)
merge_micro <- merge[which(merge$Metro_Micro_Noncore_2013 == 0),]
merge_noncore <- merge[which(merge$Metro_Micro_Noncore_2013 == 1),]
merge_metro <- merge[which(merge$Metro_Micro_Noncore_2013 == 2),]

sum(merge_micro$mental_health_hospitalizations[-plm_mh13_micro$obsRemoved])
sum(merge_noncore$mental_health_hospitalizations[-plm_mh13_noncore$obsRemoved])
sum(merge_metro$mental_health_hospitalizations[-plm_mh13_metrocore$obsRemoved])


length(unique(merge_micro$residence_county[-plm_mh13_micro$obsRemoved]))
length(unique(merge_noncore$residence_county[-plm_mh13_noncore$obsRemoved]))
length(unique(merge_metro$residence_county[-plm_mh13_metrocore$obsRemoved]))


##### Exploring demographics as covariates ########

merge$pct_female <- merge$female/merge$all_cause_hospitalizations * 100
merge$pct_white <- merge$white/merge$all_cause_hospitalizations * 100
merge$pct_black <- merge$black/merge$all_cause_hospitalizations * 100
merge$pct_hisp <- merge$hisp/merge$all_cause_hospitalizations * 100
merge$pct_65up <- (merge$age65_74 + merge$age75_84 + merge$age85_plus)/merge$all_cause_hospitalizations * 100


plm_mh13_dem1 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 + pct_female | residence_county + year, data = merge)
plm_mh13_dem2 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 + pct_female + pct_black | residence_county + year, data = merge)
plm_mh13_dem3 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 + pct_female + pct_black + pct_hisp | residence_county + year, data = merge)
plm_mh13_dem4 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 + pct_female + pct_black + pct_hisp + pct_65up | residence_county + year, data = merge)

AIC(plm_mh13)
AIC(plm_mh13_dem1)
AIC(plm_mh13_dem2)
AIC(plm_mh13_dem3)
AIC(plm_mh13_dem4)


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods13_mh_dems.txt")

print("Models including demographics of all cause hospitalization")
print("Negative binomial models, no covariates. Year/county FE")
print("Contemporaneous boom/bust w/25% threshold. DOES carry through")


print(AIC(plm_mh13))
print(AIC(plm_mh13_dem1))
print(AIC(plm_mh13_dem2))
print(AIC(plm_mh13_dem3))
print(AIC(plm_mh13_dem4))


summary(plm_mh13_dem1)
summary(plm_mh13_dem2)
summary(plm_mh13_dem3)
summary(plm_mh13_dem4)


closeAllConnections()



##### Stratifying by demographics ########



#1: Non-Hispanic White
#2: latine (or African-American)
#4: Asian/Pacific Islander
#5: Hispanic

files_new_female <- vector(mode='list', length=12)
files_new_male <- vector(mode='list', length=12)

files_new_white <- vector(mode='list', length=12)
files_new_black <- vector(mode='list', length=12)
files_new_latine <- vector(mode='list', length=12)

files_new_19to44 <- vector(mode='list', length=12)
files_new_45to64 <- vector(mode='list', length=12)
files_new_65plus <- vector(mode='list', length=12)



years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011)


for(i in 1:length(years)){
  file <- files[[i]]
  file <- file[which(file$age_group != "0-18"),]
  file$residence_county <- as.character(file$residence_county)
  
  file_new_female <- file[which(file$sex == "F"),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_female$year <- years[i]
  files_new_female[[i]] <- file_new_female
  
  file_new_male <- file[which(file$sex == "M"),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_male$year <- years[i]
  files_new_male[[i]] <- file_new_male
  
  file_new_white <- file[which(file$race == 1),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_white$year <- years[i]
  files_new_white[[i]] <- file_new_white
  
  file_new_black <- file[which(file$race == 2),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_black$year <- years[i]
  files_new_black[[i]] <- file_new_black
  
  file_new_latine <- file[which(file$race == 5),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_latine$year <- years[i]
  files_new_latine[[i]] <- file_new_latine
  
  
  file_new_19to44 <- file[which(file$age_group == "19-24" | file$age_group == "25-34" | file$age_group == "35-44"),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_19to44$year <- years[i]
  files_new_19to44[[i]] <- file_new_19to44
  
  file_new_45to64 <- file[which(file$age_group == "45-54" | file$age_group == "55-64"),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_45to64$year <- years[i]
  files_new_45to64[[i]] <- file_new_45to64
  
  file_new_65plus <- file[which(file$age_group == "65-74" | file$age_group == "75-84" | file$age_group == "85+"),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
  file_new_65plus$year <- years[i]
  files_new_65plus[[i]] <- file_new_65plus
  
  
  print(i)
}


files_new_female_df <- rbind(files_new_female[[1]],
                             files_new_female[[2]],
                             files_new_female[[3]],
                             files_new_female[[4]],
                             files_new_female[[5]],
                             files_new_female[[6]],
                             files_new_female[[7]],
                             files_new_female[[8]],
                             files_new_female[[9]],
                             files_new_female[[10]],
                             files_new_female[[11]],
                             files_new_female[[12]])
files_new_female_df$residence_county <- unlist(lapply(files_new_female_df$residence_county, function(x) county_fips_fix(as.character(x))))


files_new_male_df <- rbind(files_new_male[[1]],
                             files_new_male[[2]],
                             files_new_male[[3]],
                             files_new_male[[4]],
                             files_new_male[[5]],
                             files_new_male[[6]],
                             files_new_male[[7]],
                             files_new_male[[8]],
                             files_new_male[[9]],
                             files_new_male[[10]],
                             files_new_male[[11]],
                             files_new_male[[12]])
files_new_male_df$residence_county <- unlist(lapply(files_new_male_df$residence_county, function(x) county_fips_fix(as.character(x))))


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
                            files_new_white[[12]])
files_new_white_df$residence_county <- unlist(lapply(files_new_white_df$residence_county, function(x) county_fips_fix(as.character(x))))


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
                            files_new_black[[12]])
files_new_black_df$residence_county <- unlist(lapply(files_new_black_df$residence_county, function(x) county_fips_fix(as.character(x))))


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
                             files_new_latine[[12]])
files_new_latine_df$residence_county <- unlist(lapply(files_new_latine_df$residence_county, function(x) county_fips_fix(as.character(x))))


files_new_19to44_df <- rbind(files_new_19to44[[1]],
                             files_new_19to44[[2]],
                             files_new_19to44[[3]],
                             files_new_19to44[[4]],
                             files_new_19to44[[5]],
                             files_new_19to44[[6]],
                             files_new_19to44[[7]],
                             files_new_19to44[[8]],
                             files_new_19to44[[9]],
                             files_new_19to44[[10]],
                             files_new_19to44[[11]],
                             files_new_19to44[[12]])
files_new_19to44_df$residence_county <- unlist(lapply(files_new_19to44_df$residence_county, function(x) county_fips_fix(as.character(x))))


files_new_45to64_df <- rbind(files_new_45to64[[1]],
                             files_new_45to64[[2]],
                             files_new_45to64[[3]],
                             files_new_45to64[[4]],
                             files_new_45to64[[5]],
                             files_new_45to64[[6]],
                             files_new_45to64[[7]],
                             files_new_45to64[[8]],
                             files_new_45to64[[9]],
                             files_new_45to64[[10]],
                             files_new_45to64[[11]],
                             files_new_45to64[[12]])
files_new_45to64_df$residence_county <- unlist(lapply(files_new_45to64_df$residence_county, function(x) county_fips_fix(as.character(x))))


files_new_65plus_df <- rbind(files_new_65plus[[1]],
                             files_new_65plus[[2]],
                             files_new_65plus[[3]],
                             files_new_65plus[[4]],
                             files_new_65plus[[5]],
                             files_new_65plus[[6]],
                             files_new_65plus[[7]],
                             files_new_65plus[[8]],
                             files_new_65plus[[9]],
                             files_new_65plus[[10]],
                             files_new_65plus[[11]],
                             files_new_65plus[[12]])

files_new_65plus_df$residence_county <- unlist(lapply(files_new_65plus_df$residence_county, function(x) county_fips_fix(as.character(x))))


merge_white <- merge(files_new_white_df, dat, by = c("residence_county", "year"))
merge_black <- merge(files_new_black_df, dat, by = c("residence_county", "year"))
merge_latine <- merge(files_new_latine_df, dat, by = c("residence_county", "year"))
merge_female <- merge(files_new_female_df, dat, by = c("residence_county", "year"))
merge_male <- merge(files_new_male_df, dat, by = c("residence_county", "year"))
merge_19to44 <- merge(files_new_19to44_df, dat, by = c("residence_county", "year"))
merge_45to64 <- merge(files_new_45to64_df, dat, by = c("residence_county", "year"))
merge_65plus <- merge(files_new_65plus_df, dat, by = c("residence_county", "year"))

merge_white$state_fips_complete <- substr(merge_white$residence_county, 1,2)
merge_black$state_fips_complete <- substr(merge_black$residence_county, 1,2)
merge_latine$state_fips_complete <- substr(merge_latine$residence_county, 1,2)
merge_male$state_fips_complete <- substr(merge_male$residence_county, 1,2)
merge_female$state_fips_complete <- substr(merge_female$residence_county, 1,2)
merge_19to44$state_fips_complete <- substr(merge_19to44$residence_county, 1,2)
merge_45to64$state_fips_complete <- substr(merge_45to64$residence_county, 1,2)
merge_65plus$state_fips_complete <- substr(merge_65plus$residence_county, 1,2)


## state names from above
merge_white <- merge(merge_white, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80
merge_black <- merge(merge_black, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80
merge_latine <- merge(merge_latine, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80
merge_male <- merge(merge_male, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80
merge_female <- merge(merge_female, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80
merge_19to44 <- merge(merge_19to44, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80
merge_45to64 <- merge(merge_45to64, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80
merge_65plus <- merge(merge_65plus, state_names[,c("state_fips_complete","State.Abbreviation")], by = "state_fips_complete", all = TRUE) # 38299 x 80




## objects states_to_remove and counties_zero created above
merge_white <- merge_white[-which(merge_white$State.Abbreviation %in% states_to_remove),]
merge_white <- merge_white[-which(merge_white$residence_county %in% counties_zero),]

merge_black <- merge_black[-which(merge_black$State.Abbreviation %in% states_to_remove),]
merge_black <- merge_black[-which(merge_black$residence_county %in% counties_zero),]

merge_latine <- merge_latine[-which(merge_latine$State.Abbreviation %in% states_to_remove),]
merge_latine <- merge_latine[-which(merge_latine$residence_county %in% counties_zero),]

merge_female <- merge_female[-which(merge_female$State.Abbreviation %in% states_to_remove),]
merge_female <- merge_female[-which(merge_female$residence_county %in% counties_zero),]

merge_male <- merge_male[-which(merge_male$State.Abbreviation %in% states_to_remove),]
merge_male <- merge_male[-which(merge_male$residence_county %in% counties_zero),]

merge_19to44 <- merge_19to44[-which(merge_19to44$State.Abbreviation %in% states_to_remove),]
merge_19to44 <- merge_19to44[-which(merge_19to44$residence_county %in% counties_zero),]

merge_45to64 <- merge_45to64[-which(merge_45to64$State.Abbreviation %in% states_to_remove),]
merge_45to64 <- merge_45to64[-which(merge_45to64$residence_county %in% counties_zero),]

merge_65plus <- merge_65plus[-which(merge_65plus$State.Abbreviation %in% states_to_remove),]
merge_65plus <- merge_65plus[-which(merge_65plus$residence_county %in% counties_zero),]




## models - MH only - stratified 

plm_mh13_white <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_white)
plm_mh13_black <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_black)
plm_mh13_latine <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_latine)
plm_mh13_male <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_male)
plm_mh13_female <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_female)
plm_mh13_19to44 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_19to44)
plm_mh13_45to64 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_45to64)
plm_mh13_65plus <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_65plus)



## Export coefficients
sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods13_demStratified.txt")

print("Mental health hospitalizations: White beneficiaries")
summary(plm_mh13_white)

print("Mental health hospitalizations: Black beneficiaries")
summary(plm_mh13_black)

print("Mental health hospitalizations: Latine beneficiaries")
summary(plm_mh13_latine)

print("Mental health hospitalizations: Male beneficiaries")
summary(plm_mh13_male)

print("Mental health hospitalizations: Female beneficiaries")
summary(plm_mh13_female)

print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
summary(plm_mh13_19to44)

print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
summary(plm_mh13_45to64)

print("Mental health hospitalizations: Age 65 plus beneficiaries")
summary(plm_mh13_65plus)


closeAllConnections()




## Export IRR 
sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods13_demStratified_irr.txt")

print("Mental health hospitalizations: White beneficiaries")
plm_mh13_white_out <- cbind(Estimate = coef(plm_mh13_white), confint(plm_mh13_white))
plm_mh13_white_irr <- exp(plm_mh13_white_out)
plm_mh13_white_irr


print("Mental health hospitalizations: Black beneficiaries")
plm_mh13_black_out <- cbind(Estimate = coef(plm_mh13_black), confint(plm_mh13_black))
plm_mh13_black_irr <- exp(plm_mh13_black_out)
plm_mh13_black_irr


print("Mental health hospitalizations: Latine beneficiaries")
plm_mh13_latine_out <- cbind(Estimate = coef(plm_mh13_latine), confint(plm_mh13_latine))
plm_mh13_latine_irr <- exp(plm_mh13_latine_out)
plm_mh13_latine_irr


print("Mental health hospitalizations: Male beneficiaries")
plm_mh13_male_out <- cbind(Estimate = coef(plm_mh13_male), confint(plm_mh13_male))
plm_mh13_male_irr <- exp(plm_mh13_male_out)
plm_mh13_male_irr


print("Mental health hospitalizations: Female beneficiaries")
plm_mh13_female_out <- cbind(Estimate = coef(plm_mh13_female), confint(plm_mh13_female))
plm_mh13_female_irr <- exp(plm_mh13_female_out)
plm_mh13_female_irr


print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
plm_mh13_19to44_out <- cbind(Estimate = coef(plm_mh13_19to44), confint(plm_mh13_19to44))
plm_mh13_19to44_irr <- exp(plm_mh13_19to44_out)
plm_mh13_19to44_irr


print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
plm_mh13_45to64_out <- cbind(Estimate = coef(plm_mh13_45to64), confint(plm_mh13_45to64))
plm_mh13_45to64_irr <- exp(plm_mh13_45to64_out)
plm_mh13_45to64_irr

print("Mental health hospitalizations: Age 65 plus beneficiaries")
plm_mh13_65plus_out <- cbind(Estimate = coef(plm_mh13_65plus), confint(plm_mh13_65plus))
plm_mh13_65plus_irr <- exp(plm_mh13_65plus_out)
plm_mh13_65plus_irr


closeAllConnections()



## N values (will export as table later)

sum(merge_female$mental_health_hospitalizations[-plm_mh13_female$obsRemoved])
sum(merge_male$mental_health_hospitalizations[-plm_mh13_male$obsRemoved])
sum(merge_white$mental_health_hospitalizations[-plm_mh13_white$obsRemoved])
sum(merge_black$mental_health_hospitalizations[-plm_mh13_black$obsRemoved])
sum(merge_latine$mental_health_hospitalizations[-plm_mh13_latine$obsRemoved])
sum(merge_19to44$mental_health_hospitalizations[-plm_mh13_19to44$obsRemoved])
sum(merge_45to64$mental_health_hospitalizations[-plm_mh13_45to64$obsRemoved])
sum(merge_65plus$mental_health_hospitalizations[-plm_mh13_65plus$obsRemoved])




length(unique(merge_female$residence_county[-plm_mh13_female$obsRemoved]))
length(unique(merge_male$residence_county[-plm_mh13_male$obsRemoved]))
length(unique(merge_white$residence_county[-plm_mh13_white$obsRemoved]))
length(unique(merge_black$residence_county[-plm_mh13_black$obsRemoved]))
length(unique(merge_latine$residence_county[-plm_mh13_latine$obsRemoved]))
length(unique(merge_19to44$residence_county[-plm_mh13_19to44$obsRemoved]))
length(unique(merge_45to64$residence_county[-plm_mh13_45to64$obsRemoved]))
length(unique(merge_65plus$residence_county[-plm_mh13_65plus$obsRemoved]))



##### Table 1 ##### 

total_year <- merge %>% group_by(year) %>% summarise(count_total = sum(total, na.rm = TRUE))
female_year <- merge %>% group_by(year) %>% summarise(count_female = sum(female, na.rm = TRUE))
white_year <- merge %>% group_by(year) %>% summarise(count_white = sum(white, na.rm = TRUE))
black_year <- merge %>% group_by(year) %>% summarise(count_black = sum(black, na.rm = TRUE))
hisp_year <- merge %>% group_by(year) %>% summarise(count_hisp = sum(hisp, na.rm = TRUE))


total_ogd <- merge %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_total = sum(total, na.rm = TRUE))
female_ogd <- merge %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_female = sum(female, na.rm = TRUE))
white_ogd <- merge %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_white = sum(white, na.rm = TRUE))
black_ogd <- merge %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_black = sum(black, na.rm = TRUE))
hisp_ogd <- merge %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_hisp = sum(hisp, na.rm = TRUE))

# Columns: total, boom/bust/status quo
# Row 1 - N for total hospitalizations
# 9 x 6 
#Number of hospitalizations (n) 
#Female (%) 
#Race/Ethnicity (%) 
#White 
#Black 
#Hispanic/Latinx 


tab1 <- data.frame(group = c("Num. of hospitalizations (n)","Female (%)","Race/Ethnicity (%)","White","Black or African American","Hispanic/Latino/a"),
                   total_2001 = rep(NA, 6),
                   total_2011 = rep(NA, 6),
                   status_quo_2001 = rep(NA, 6),
                   status_quo_2011 = rep(NA, 6),
                   boom_2001 = rep(NA, 6),
                   boom_2011 = rep(NA, 6),
                   bust_2001 = rep(NA, 6),
                   bust_2011 = rep(NA, 6))


## First row - totals 
tab1$total_2001[1] <- total_year$count_total[which(total_year$year == 2001)]
tab1$total_2011[1] <- total_year$count_total[which(total_year$year == 2001)]

tab1$status_quo_2001[1] <- total_ogd$count_total[which(total_ogd$year == 2001 & total_ogd$ogd_change_group_cont25_2 == "Status Quo")]
tab1$status_quo_2011[1] <- total_ogd$count_total[which(total_ogd$year == 2011 & total_ogd$ogd_change_group_cont25_2 == "Status Quo")]

tab1$boom_2001[1] <- total_ogd$count_total[which(total_ogd$year == 2001 & total_ogd$ogd_change_group_cont25_2 == "Boom")]
tab1$boom_2011[1] <- total_ogd$count_total[which(total_ogd$year == 2011 & total_ogd$ogd_change_group_cont25_2 == "Boom")]

tab1$bust_2001[1] <- total_ogd$count_total[which(total_ogd$year == 2001 & total_ogd$ogd_change_group_cont25_2 == "Bust")]
tab1$bust_2011[1] <- total_ogd$count_total[which(total_ogd$year == 2011 & total_ogd$ogd_change_group_cont25_2 == "Bust")]



## Second row - percent female

tab1$total_2001[2] <- round(((female_year$count_female[which(female_year$year == 2001)]/tab1$total_2001[1]) * 100), 2)
tab1$total_2011[2] <- round(((female_year$count_female[which(female_year$year == 2011)]/tab1$total_2011[1]) * 100), 2)
tab1$status_quo_2001[2] <- round(((female_ogd$count_female[which(female_ogd$year == 2001 & female_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2001[1]) * 100), 2)
tab1$status_quo_2011[2] <- round(((female_ogd$count_female[which(female_ogd$year == 2011 & female_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2011[1]) * 100), 2)
tab1$boom_2001[2] <- round(((female_ogd$count_female[which(female_ogd$year == 2001 & female_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2001[1]) * 100), 2)
tab1$boom_2011[2] <- round(((female_ogd$count_female[which(female_ogd$year == 2011 & female_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2011[1]) * 100), 2)
tab1$bust_2001[2] <- round(((female_ogd$count_female[which(female_ogd$year == 2001 & female_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2001[1]) * 100), 2)
tab1$bust_2011[2] <- round(((female_ogd$count_female[which(female_ogd$year == 2011 & female_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2011[1]) * 100), 2)


tab1$total_2001[4] <- round(((white_year$count_white[which(white_year$year == 2001)]/tab1$total_2001[1]) * 100), 2)
tab1$total_2011[4] <- round(((white_year$count_white[which(white_year$year == 2011)]/tab1$total_2011[1]) * 100), 2)
tab1$status_quo_2001[4] <- round(((white_ogd$count_white[which(white_ogd$year == 2001 & white_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2001[1]) * 100), 2)
tab1$status_quo_2011[4] <- round(((white_ogd$count_white[which(white_ogd$year == 2011 & white_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2011[1]) * 100), 2)
tab1$boom_2001[4] <- round(((white_ogd$count_white[which(white_ogd$year == 2001 & white_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2001[1]) * 100), 2)
tab1$boom_2011[4] <- round(((white_ogd$count_white[which(white_ogd$year == 2011 & white_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2011[1]) * 100), 2)
tab1$bust_2001[4] <- round(((white_ogd$count_white[which(white_ogd$year == 2001 & white_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2001[1]) * 100), 2)
tab1$bust_2011[4] <- round(((white_ogd$count_white[which(white_ogd$year == 2011 & white_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2011[1]) * 100), 2)


tab1$total_2001[5] <- round(((black_year$count_black[which(black_year$year == 2001)]/tab1$total_2001[1]) * 100), 2)
tab1$total_2011[5] <- round(((black_year$count_black[which(black_year$year == 2011)]/tab1$total_2011[1]) * 100), 2)
tab1$status_quo_2001[5] <- round(((black_ogd$count_black[which(black_ogd$year == 2001 & black_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2001[1]) * 100), 2)
tab1$status_quo_2011[5] <- round(((black_ogd$count_black[which(black_ogd$year == 2011 & black_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2011[1]) * 100), 2)
tab1$boom_2001[5] <- round(((black_ogd$count_black[which(black_ogd$year == 2001 & black_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2001[1]) * 100), 2)
tab1$boom_2011[5] <- round(((black_ogd$count_black[which(black_ogd$year == 2011 & black_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2011[1]) * 100), 2)
tab1$bust_2001[5] <- round(((black_ogd$count_black[which(black_ogd$year == 2001 & black_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2001[1]) * 100), 2)
tab1$bust_2011[5] <- round(((black_ogd$count_black[which(black_ogd$year == 2011 & black_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2011[1]) * 100), 2)



tab1$total_2001[6] <- round(((hisp_year$count_hisp[which(hisp_year$year == 2001)]/tab1$total_2001[1]) * 100), 2)
tab1$total_2011[6] <- round(((hisp_year$count_hisp[which(hisp_year$year == 2011)]/tab1$total_2011[1]) * 100), 2)
tab1$status_quo_2001[6] <- round(((hisp_ogd$count_hisp[which(hisp_ogd$year == 2001 & hisp_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2001[1]) * 100), 2)
tab1$status_quo_2011[6] <- round(((hisp_ogd$count_hisp[which(hisp_ogd$year == 2011 & hisp_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2011[1]) * 100), 2)
tab1$boom_2001[6] <- round(((hisp_ogd$count_hisp[which(hisp_ogd$year == 2001 & hisp_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2001[1]) * 100), 2)
tab1$boom_2011[6] <- round(((hisp_ogd$count_hisp[which(hisp_ogd$year == 2011 & hisp_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2011[1]) * 100), 2)
tab1$bust_2001[6] <- round(((hisp_ogd$count_hisp[which(hisp_ogd$year == 2001 & hisp_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2001[1]) * 100), 2)
tab1$bust_2011[6] <- round(((hisp_ogd$count_hisp[which(hisp_ogd$year == 2011 & hisp_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2011[1]) * 100), 2)


write.csv(tab1, "/n/dominici_nsaph_l3/Lab/projects/oil_gas_development_medicaid/Results/newAnalyticPlan/table1_demographics.csv", row.names = FALSE)



##### Set 14: Negative binomial models, no covariates. state/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through #####


plm_mh14 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_adj14 <- fenegbin(adjustment_reaction_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_anx14 <- fenegbin(anxiety_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_att14 <- fenegbin(attention_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_mood14 <- fenegbin(mood_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_pers14 <- fenegbin(personality_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_schiz14 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_alc14 <- fenegbin(alcohol_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_sub14 <- fenegbin(substance_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)
plm_suic14 <- fenegbin(suicide_self_harm_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + state_fips, data = merge)



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods14.txt")



print("Negative binomial models with state and fixed effects. No covariates")
print("Uses USDA boom/bust indicator")

print("All mental health hospitalizations")
summary(plm_mh14)

print("Adjustment disorder")
summary(plm_adj14)

print("Anxiety disorder")
summary(plm_anx14)

print("Attachment disorder")
summary(plm_att14)

print("Mood disorder")
summary(plm_mood14)

print("Personality disorder")
summary(plm_pers14)

print("Schizophrenia")
summary(plm_schiz14)

print("Alcohol use disorder")
summary(plm_alc14)

print("Substance use disorder")
summary(plm_sub14)

print("Suicide/self-harm")
summary(plm_suic14)

closeAllConnections()





##### Set 15: Negative binomial models, no covariates. State fixed effects. Boom/bust designated by USDA #####


plm_mh15 <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_adj15 <- fenegbin(adjustment_reaction_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_anx15 <- fenegbin(anxiety_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_att15 <- fenegbin(attention_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_mood15 <- fenegbin(mood_disorders_hospitalizations  ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_pers15 <- fenegbin(personality_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_schiz15 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_alc15 <- fenegbin(alcohol_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_sub15 <- fenegbin(substance_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)
plm_suic15 <- fenegbin(suicide_self_harm_hospitalizations ~ USDA_boom + USDA_bust | state_fips + year,  data=merge)



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods15.txt")


print("Comparing USDA and our change group designation")
print("Ours = Boom/bust/status quo")
print("USDA = H_Growth/H_Decline/Status quo")
print("These are strongly associated but not one-to-one")
table(merge$ogd_change_group_cont25_2, merge$oil_gas_change_group)


print("Negative binomial models using USDA continuous boom/bust designation")
print("state and year fixed effects")

print("All mental health hospitalizations")
summary(plm_mh15)

print("Adjustment disorder")
summary(plm_adj15)

print("Anxiety disorder")
summary(plm_anx15)

print("Attention disorder")
summary(plm_att15)

print("Mood disorder")
summary(plm_mood15)

print("Personality disorder")
summary(plm_pers15)

print("Schizophrenia")
summary(plm_schiz15)

print("Alcohol use disorder")
summary(plm_alc15)

print("Substance use disorder")
summary(plm_sub15)

print("Suicide/self-harm")
summary(plm_suic15)

closeAllConnections()



##### Set 16: Event analysis using boom or bust as treatment, feols() ######
## Framework: #https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

# Busts 

# Keep only rows after the first bust
test <- dat %>%
  dplyr::group_by(residence_county, year) %>%
  dplyr::slice(match(TRUE, ogd_change_group_cont25_2 == "Bust")) 
  #dplyr::slice(match(TRUE, ogd_change_group_cont25_2 == "Boom")) 

test$state_fips_complete <- substr(test$residence_county, 1,2) # gotta match the merge file varname 

# Create variables that flag the first bust year
test2_county <- test %>% 
  dplyr::group_by(residence_county) %>%
  dplyr::mutate(
    first_county = dplyr::first(year),
  )

test2_state <- test %>% 
  dplyr::group_by(state_fips_complete) %>%
  dplyr::mutate(
    first_state = dplyr::first(year),
  )

# Isolate first year and county
test3_county <- unique(test2_county[,c("residence_county","first_county")])
test3_state <- unique(test2_state[,c("state_fips_complete","first_state")])


# Merge first year into merge dataset (and reorder so it's easy to check)
test_county_sub <- test3_county[which(test3_county$residence_county %in% merge$residence_county),]
test_state_sub <- test3_state[which(test3_state$state_fips_complete %in% merge$state_fips_complete),]


merge_prepost <- merge(merge, test_county_sub, by = c("residence_county"), all = TRUE)
merge_prepost <- merge(merge_prepost, test_state_sub, by = c("state_fips_complete"), all = TRUE)
merge_prepost <- merge_prepost[order(merge_prepost$residence_county, merge_prepost$year),]

merge_prepost$first <- merge_prepost$first_county
merge_prepost$first[which(is.na(merge_prepost$first))] <- merge_prepost$first_state[which(is.na(merge_prepost$first))]


#  Create indicator of which states received treatment
merge_prepost$treat <- ifelse(is.na(merge_prepost$first_county), 0, 1)

## Create indicator of time to treatment
merge_prepost$time_to_treat <- merge_prepost$year - merge_prepost$first


# Make the -1 period the central period
merge_prepost$time_to_treat2 <- merge_prepost$time_to_treat 
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -1)] <- 1
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -10)] <- 2
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -9)] <- 3
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -8)] <- 4
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -7)] <- 5
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -6)] <- 6
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -5)] <- 7
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -4)] <- 8
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -3)] <- 9
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -2)] <- 10
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 0)] <- 11
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 1)] <- 12
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 2)] <- 13
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 3)] <- 14
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 4)] <- 15
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 5)] <- 16
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 6)] <- 17
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 7)] <- 18
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 8)] <- 19
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 9)] <- 20
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 10)] <- 21

merge_prepost$time_to_treat2 <- as.factor(merge_prepost$time_to_treat2)


mod_event_mh_bust = feols(mental_health_hospitalizations ~ i(time_to_treat, treat)| 
                               residence_county + year, 
                             data = merge_prepost)

mod_event_mh_bust_factor = feols(mental_health_hospitalizations ~ relevel(time_to_treat2, ref = 1) * treat| 
                            residence_county + year, 
                          data = merge_prepost)




# booms 

# Keep only rows after the first boom
test <- dat %>%
  dplyr::group_by(residence_county, year) %>%
  #dplyr::slice(match(TRUE, ogd_change_group_cont25_2 == "Bust")) 
  dplyr::slice(match(TRUE, ogd_change_group_cont25_2 == "Boom")) 

test$state_fips_complete <- substr(test$residence_county, 1,2) # gotta match the merge file varname 

# Create variables that flag the first bust year
test2_county <- test %>% 
  dplyr::group_by(residence_county) %>%
  dplyr::mutate(
    first_county = dplyr::first(year),
  )

test2_state <- test %>% 
  dplyr::group_by(state_fips_complete) %>%
  dplyr::mutate(
    first_state = dplyr::first(year),
  )

# Isolate first year and county
test3_county <- unique(test2_county[,c("residence_county","first_county")])
test3_state <- unique(test2_state[,c("state_fips_complete","first_state")])


# Merge first year into merge dataset (and reorder so it's easy to check)
test_county_sub <- test3_county[which(test3_county$residence_county %in% merge$residence_county),]
test_state_sub <- test3_state[which(test3_state$state_fips_complete %in% merge$state_fips_complete),]


merge_prepost <- merge(merge, test_county_sub, by = c("residence_county"), all = TRUE)
merge_prepost <- merge(merge_prepost, test_state_sub, by = c("state_fips_complete"), all = TRUE)
merge_prepost <- merge_prepost[order(merge_prepost$residence_county, merge_prepost$year),]

merge_prepost$first <- merge_prepost$first_county
merge_prepost$first[which(is.na(merge_prepost$first))] <- merge_prepost$first_state[which(is.na(merge_prepost$first))]


#  Create indicator of which states received treatment
merge_prepost$treat <- ifelse(is.na(merge_prepost$first_county), 0, 1)

## Create indicator of time to treatment
merge_prepost$time_to_treat <- merge_prepost$year - merge_prepost$first

# Make the -1 period the central period
merge_prepost$time_to_treat2 <- merge_prepost$time_to_treat 
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -1)] <- 1
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -10)] <- 2
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -9)] <- 3
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -8)] <- 4
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -7)] <- 5
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -6)] <- 6
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -5)] <- 7
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -4)] <- 8
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -3)] <- 9
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -2)] <- 10
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 0)] <- 11
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 1)] <- 12
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 2)] <- 13
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 3)] <- 14
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 4)] <- 15
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 5)] <- 16
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 6)] <- 17
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 7)] <- 18
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 8)] <- 19
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 9)] <- 20
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 10)] <- 21

merge_prepost$time_to_treat2 <- as.factor(merge_prepost$time_to_treat2)



mod_event_mh_boom = feols(mental_health_hospitalizations ~ i(time_to_treat, treat) | 
                               residence_county + year, 
                             data = merge_prepost)

mod_event_mh_boom_factor = feols(mental_health_hospitalizations ~ relevel(time_to_treat2, ref = 1) * treat | 
                            residence_county + year, 
                          data = merge_prepost)



## make plotable table - nonfactor

mh_boom_coef <- mod_event_mh_boom$coeftable
mh_bust_coef <- mod_event_mh_bust$coeftable

mh_boom_plotdat <- data.frame(time = c(seq(-10,9, by = 1), -1),
                              coef = rep(NA, 21),
                              se = rep(NA, 21),
                              group = c(rep("Diff. from ref.",20),"Observed"))

for(i in 1:20){
  mh_boom_plotdat$coef[i] <- mh_boom_coef$Estimate[i] - mh_boom_coef$Estimate[10] 
  mh_boom_plotdat$se[i] <- sqrt((mh_boom_coef$`Std. Error`[i] ^ 2) + (mh_boom_coef$`Std. Error`[10]^2))
  print(i)
}

mh_boom_plotdat$coef[21] <- mh_boom_coef$Estimate[10]
mh_boom_plotdat$se[21] <- mh_boom_coef$`Std. Error`[10]

mh_boom_plotdat$upper <- mh_boom_plotdat$coef + (1.96*mh_boom_plotdat$se)
mh_boom_plotdat$lower <- mh_boom_plotdat$coef - (1.96*mh_boom_plotdat$se)

mh_boom_plotdat$upper[21] <- NA
mh_boom_plotdat$lower[21] <- NA

mh_bust_plotdat <- data.frame(time = c(seq(-10,9, by = 1), -1),
                              coef = rep(NA, 21),
                              se = rep(NA, 21),
                              group = c(rep("Diff. from ref.",20),"Observed"))

for(i in 1:20){
  mh_bust_plotdat$coef[i] <- mh_bust_coef$Estimate[i] - mh_bust_coef$Estimate[10]
  mh_bust_plotdat$se[i] <- sqrt(((mh_bust_coef$`Std. Error`[i] ^ 2) + mh_bust_coef$`Std. Error`[10]^2))
  print(i)
}

mh_bust_plotdat$coef[21] <- mh_bust_coef$Estimate[10]
mh_bust_plotdat$se[21] <- mh_bust_coef$`Std. Error`[10]

mh_bust_plotdat$upper <- mh_bust_plotdat$coef + (1.96*mh_bust_plotdat$se)
mh_bust_plotdat$lower <- mh_bust_plotdat$coef - (1.96*mh_bust_plotdat$se)

mh_bust_plotdat$upper[21] <- NA
mh_bust_plotdat$lower[21] <- NA


png(file="oil_gas_development_medicaid/Figures/feols_mh_effect_plots_boom_effectDifference.png", width=8, height=6, units="in", res=300)
ggplot(mh_boom_plotdat, aes(time, coef, color = group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  xlab("Time to treat") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  scale_color_manual(values = c("Black","Red")) +
  theme_minimal()
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_mh_effect_plots_bust_effectDifference.png", width=8, height=6, units="in", res=300)
ggplot(mh_bust_plotdat, aes(time, coef, color = group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  xlab("Time to treat") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  scale_color_manual(values = c("Black","Red")) +
  theme_minimal()
dev.off()


### New factor plots 

mh_boom_plotdat <- mod_event_mh_boom_factor$coeftable[c(20:39),]
mh_boom_plotdat$time <- c(seq(-10,-2, by = 1), seq(0,10, by = 1))
mh_boom_plotdat <- rbind(mh_boom_plotdat, c(0,0,0,0,-1))
names(mh_boom_plotdat)[2] <- "SE"

mh_bust_plotdat <- mod_event_mh_bust_factor$coeftable[c(20:39),]
mh_bust_plotdat$time <- c(seq(-10,-2, by = 1), seq(0,10, by = 1))
mh_bust_plotdat <- rbind(mh_bust_plotdat, c(0,0,0,0,-1))
names(mh_bust_plotdat)[2] <- "SE"



png(file="oil_gas_development_medicaid/Figures/feols_mh_effect_plots_boom_newReference.png", width=8, height=6, units="in", res=300)
ggplot(mh_boom_plotdat, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate - (1.96 * SE), ymax=Estimate + (1.96 * SE))) +
  xlab("Time to treat") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  theme_minimal()
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_mh_effect_plots_bust_newReference.png", width=8, height=6, units="in", res=300)
ggplot(mh_bust_plotdat, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate - (1.96 * SE), ymax=Estimate + (1.96 * SE))) +
  xlab("Time to treat") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  theme_minimal()
dev.off()


## original iPlot 

png(file="oil_gas_development_medicaid/Figures/feols_mh_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_mh_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/feols_mh_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_mh_bust)
dev.off()




sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods16_mh.txt")


print("Event models, no covariates. County and year fixed effects.")
print("Uses feols()")
print("Treatment is first boom or bust for a county as indicated by 25% continuous carry through indicator.")
print("Some counties have no boom/bust.")
print("time to treatment is is years preceeding/following boom/bust 'treatment' event")


print("###### Treatment == Bust #######")

print("All mental health")
summary(mod_event_mh_bust)



print("###### Treatment == Boom #######")

print("All mental health")
summary(mod_event_mh_boom)


closeAllConnections()


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods16_mh_newReference.txt")


print("Event models, no covariates. County and year fixed effects.")
print("Uses feols()")
print("Treatment is first boom or bust for a county as indicated by 25% continuous carry through indicator.")
print("Some counties have no boom/bust.")
print("time to treatment is is years preceeding/following boom/bust 'treatment' event")


print("###### Treatment == Bust #######")

print("All mental health")
summary(mod_event_mh_bust_factor)



print("###### Treatment == Boom #######")

print("All mental health")
summary(mod_event_mh_boom_factor)


closeAllConnections()





##### Set 17: Event analysis using boom or bust as treatment, fenegbin() ######

# Busts 

# Keep only rows after the first bust
test <- dat %>%
  dplyr::group_by(residence_county, year) %>%
  dplyr::slice(match(TRUE, ogd_change_group_cont25_2 == "Bust")) 
#dplyr::slice(match(TRUE, ogd_change_group_cont25_2 == "Boom")) 

test$state_fips_complete <- substr(test$residence_county, 1,2) # gotta match the merge file varname 

# Create variables that flag the first bust year
test2_county <- test %>% 
  dplyr::group_by(residence_county) %>%
  dplyr::mutate(
    first_county = dplyr::first(year),
  )

test2_state <- test %>% 
  dplyr::group_by(state_fips_complete) %>%
  dplyr::mutate(
    first_state = dplyr::first(year),
  )

# Isolate first year and county
test3_county <- unique(test2_county[,c("residence_county","first_county")])
test3_state <- unique(test2_state[,c("state_fips_complete","first_state")])


# Merge first year into merge dataset (and reorder so it's easy to check)
test_county_sub <- test3_county[which(test3_county$residence_county %in% merge$residence_county),]
test_state_sub <- test3_state[which(test3_state$state_fips_complete %in% merge$state_fips_complete),]


merge_prepost <- merge(merge, test_county_sub, by = c("residence_county"), all = TRUE)
merge_prepost <- merge(merge_prepost, test_state_sub, by = c("state_fips_complete"), all = TRUE)
merge_prepost <- merge_prepost[order(merge_prepost$residence_county, merge_prepost$year),]

merge_prepost$first <- merge_prepost$first_county
merge_prepost$first[which(is.na(merge_prepost$first))] <- merge_prepost$first_state[which(is.na(merge_prepost$first))]


#  Create indicator of which states received treatment
merge_prepost$treat <- ifelse(is.na(merge_prepost$first_county), 0, 1)

## Create indicator of time to treatment
merge_prepost$time_to_treat <- merge_prepost$year - merge_prepost$first

## truncate at -8, 8
merge_prepost$time_to_treat_trunc <- merge_prepost$time_to_treat
merge_prepost$time_to_treat_trunc[which(merge_prepost$time_to_treat >= 8)] <- 8
merge_prepost$time_to_treat_trunc[which(merge_prepost$time_to_treat <= -8)] <- -8


# Make the -1 period the central period
merge_prepost$time_to_treat2 <- merge_prepost$time_to_treat 
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -1)] <- 1
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -10)] <- 2
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -9)] <- 3
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -8)] <- 4
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -7)] <- 5
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -6)] <- 6
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -5)] <- 7
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -4)] <- 8
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -3)] <- 9
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -2)] <- 10
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 0)] <- 11
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 1)] <- 12
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 2)] <- 13
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 3)] <- 14
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 4)] <- 15
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 5)] <- 16
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 6)] <- 17
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 7)] <- 18
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 8)] <- 19
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 9)] <- 20
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 10)] <- 21

merge_prepost$time_to_treat2 <- as.factor(merge_prepost$time_to_treat2)


# Make the -1 period the central period - truncated
merge_prepost$time_to_treat_trunc2 <- merge_prepost$time_to_treat_trunc 
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -1)] <- 1
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -8)] <- 2
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -7)] <- 3
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -6)] <- 4
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -5)] <- 5
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -4)] <- 6
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -3)] <- 7
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -2)] <- 8
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 0)] <- 9
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 1)] <- 10
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 2)] <- 11
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 3)] <- 12
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 4)] <- 13
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 5)] <- 14
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 6)] <- 15
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 7)] <- 16
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 8)] <- 17

merge_prepost$time_to_treat_trunc2 <- as.factor(merge_prepost$time_to_treat_trunc2)


mod_event_mh_bust = fenegbin(mental_health_hospitalizations ~ i(time_to_treat, treat) | 
                   state_fips_complete + year, 
                   data = merge_prepost)

mod_event_mh_bust_factor = fenegbin(mental_health_hospitalizations ~ relevel(time_to_treat2, ref = 1) * treat | 
                                    state_fips_complete + year, 
                                    data = merge_prepost)


mod_event_mh_bust_trunc = fenegbin(mental_health_hospitalizations ~ i(time_to_treat_trunc, treat) | 
                               state_fips_complete + year, 
                             data = merge_prepost)

mod_event_mh_bust_factor_trunc = fenegbin(mental_health_hospitalizations ~ relevel(time_to_treat_trunc2, ref = 1) * treat | 
                                      state_fips_complete + year, 
                                    data = merge_prepost)


# booms 

# Keep only rows after the first boom
test <- dat %>%
  dplyr::group_by(residence_county, year) %>%
  #dplyr::slice(match(TRUE, ogd_change_group_cont25_2 == "Bust")) 
  dplyr::slice(match(TRUE, ogd_change_group_cont25_2 == "Boom")) 

test$state_fips_complete <- substr(test$residence_county, 1,2) # gotta match the merge file varname 

# Create variables that flag the first bust year
test2_county <- test %>% 
  dplyr::group_by(residence_county) %>%
  dplyr::mutate(
    first_county = dplyr::first(year),
  )

test2_state <- test %>% 
  dplyr::group_by(state_fips_complete) %>%
  dplyr::mutate(
    first_state = dplyr::first(year),
  )

# Isolate first year and county
test3_county <- unique(test2_county[,c("residence_county","first_county")])
test3_state <- unique(test2_state[,c("state_fips_complete","first_state")])


# Merge first year into merge dataset (and reorder so it's easy to check)
test_county_sub <- test3_county[which(test3_county$residence_county %in% merge$residence_county),]
test_state_sub <- test3_state[which(test3_state$state_fips_complete %in% merge$state_fips_complete),]


merge_prepost <- merge(merge, test_county_sub, by = c("residence_county"), all = TRUE)
merge_prepost <- merge(merge_prepost, test_state_sub, by = c("state_fips_complete"), all = TRUE)
merge_prepost <- merge_prepost[order(merge_prepost$residence_county, merge_prepost$year),]

merge_prepost$first <- merge_prepost$first_county
merge_prepost$first[which(is.na(merge_prepost$first))] <- merge_prepost$first_state[which(is.na(merge_prepost$first))]


#  Create indicator of which states received treatment
merge_prepost$treat <- ifelse(is.na(merge_prepost$first_county), 0, 1)

## Create indicator of time to treatment
merge_prepost$time_to_treat <- merge_prepost$year - merge_prepost$first

## truncate at -8, 8
merge_prepost$time_to_treat_trunc <- merge_prepost$time_to_treat
merge_prepost$time_to_treat_trunc[which(merge_prepost$time_to_treat >= 8)] <- 8
merge_prepost$time_to_treat_trunc[which(merge_prepost$time_to_treat <= -8)] <- -8


# Make the -1 period the central period
merge_prepost$time_to_treat2 <- merge_prepost$time_to_treat 
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -1)] <- 1
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -10)] <- 2
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -9)] <- 3
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -8)] <- 4
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -7)] <- 5
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -6)] <- 6
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -5)] <- 7
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -4)] <- 8
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -3)] <- 9
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -2)] <- 10
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 0)] <- 11
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 1)] <- 12
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 2)] <- 13
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 3)] <- 14
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 4)] <- 15
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 5)] <- 16
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 6)] <- 17
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 7)] <- 18
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 8)] <- 19
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 9)] <- 20
merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 10)] <- 21

merge_prepost$time_to_treat2 <- as.factor(merge_prepost$time_to_treat2)


# Make the -1 period the central period - truncated
merge_prepost$time_to_treat_trunc2 <- merge_prepost$time_to_treat_trunc 
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -1)] <- 1
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -8)] <- 2
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -7)] <- 3
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -6)] <- 4
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -5)] <- 5
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -4)] <- 6
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -3)] <- 7
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -2)] <- 8
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 0)] <- 9
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 1)] <- 10
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 2)] <- 11
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 3)] <- 12
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 4)] <- 13
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 5)] <- 14
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 6)] <- 15
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 7)] <- 16
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 8)] <- 17

merge_prepost$time_to_treat_trunc2 <- as.factor(merge_prepost$time_to_treat_trunc2)


mod_event_mh_boom = fenegbin(mental_health_hospitalizations ~ i(time_to_treat, treat) | 
                               state_fips_complete + year, 
                             data = merge_prepost)

mod_event_mh_boom_factor = fenegbin(mental_health_hospitalizations ~ relevel(time_to_treat2, ref = 1) * treat | 
                                      state_fips_complete + year, 
                                    data = merge_prepost)


mod_event_mh_boom_trunc = fenegbin(mental_health_hospitalizations ~ i(time_to_treat_trunc, treat) | 
                                   state_fips_complete + year, 
                                   data = merge_prepost)

mod_event_mh_boom_factor_trunc = fenegbin(mental_health_hospitalizations ~ relevel(time_to_treat_trunc2, ref = 1) * treat | 
                                            state_fips_complete + year, 
                                          data = merge_prepost)






### New factor plots 

mh_boom_plotdat <- mod_event_mh_boom_factor$coeftable[c(22:41),]
mh_boom_plotdat$Estimate <- exp(mh_boom_plotdat$Estimate)
mh_boom_plotdat$SE <- mh_boom_plotdat$Estimate * mh_boom_plotdat$`Std. Error` 
mh_boom_plotdat$time <- c(seq(-10,-2, by = 1), seq(0,10, by = 1))
#mh_boom_plotdat <- rbind(mh_boom_plotdat, c(0,0,0,0,0,-1))


mh_bust_plotdat <- mod_event_mh_bust_factor$coeftable[c(22:41),]
mh_bust_plotdat$Estimate <- exp(mh_bust_plotdat$Estimate)
mh_bust_plotdat$SE <- mh_bust_plotdat$Estimate * mh_bust_plotdat$`Std. Error` 
mh_bust_plotdat$time <- c(seq(-10,-2, by = 1), seq(0,10, by = 1))
#mh_bust_plotdat <- rbind(mh_bust_plotdat, c(0,0,0,0,0,-1))


png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_boom_newReference.png", width=8, height=6, units="in", res=300)
ggplot(mh_boom_plotdat, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate - (1.96 * SE), ymax=Estimate + (1.96 * SE))) +
  xlab("Time to treat") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  theme_minimal()
dev.off()

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_bust_newReference.png", width=8, height=6, units="in", res=300)
ggplot(mh_bust_plotdat, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate - (1.96 * SE), ymax=Estimate + (1.96 * SE))) +
  xlab("Time to treat") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  theme_minimal()
dev.off()


### New factor plots - truncated



mh_boom_plotdat_trunc <- mod_event_mh_boom_factor_trunc$coeftable[c(18:33),]
mh_boom_plotdat_trunc$Estimate <- exp(mh_boom_plotdat_trunc$Estimate)
mh_boom_plotdat_trunc$SE <- mh_boom_plotdat_trunc$Estimate * mh_boom_plotdat_trunc$`Std. Error` 
mh_boom_plotdat_trunc$time <- c(seq(-8,-2, by = 1), seq(0,8, by = 1))
#mh_boom_plotdat_trunc <- rbind(mh_boom_plotdat_trunc, c(0,0,0,0,0,-1))


mh_bust_plotdat_trunc <- mod_event_mh_bust_factor_trunc$coeftable[c(18:33),]
mh_bust_plotdat_trunc$Estimate <- exp(mh_bust_plotdat_trunc$Estimate)
mh_bust_plotdat_trunc$SE <- mh_bust_plotdat_trunc$Estimate * mh_bust_plotdat_trunc$`Std. Error` 
mh_bust_plotdat_trunc$time <- c(seq(-8,-2, by = 1), seq(0,8, by = 1))
#mh_bust_plotdat_trunc <- rbind(mh_bust_plotdat_trunc, c(0,0,0,0,0,-1))



png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_boom_newReference_trunc.png", width=8, height=6, units="in", res=300)
ggplot(mh_boom_plotdat_trunc, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate - (1.96 * SE), ymax=Estimate + (1.96 * SE))) +
  xlab("Time to treat (truncated)") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  theme_minimal()
dev.off()

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_bust_newReference_trunc.png", width=8, height=6, units="in", res=300)
ggplot(mh_bust_plotdat_trunc, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate - (1.96 * SE), ymax=Estimate + (1.96 * SE))) +
  xlab("Time to treat (truncated)") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  theme_minimal()
dev.off()


## original iPlot 

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_boom.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_mh_boom)
dev.off()

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_bust.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_mh_bust)
dev.off()


## original iPlot - truncated

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_boom_trunc.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_mh_boom_trunc)
dev.off()

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_bust_trunc.png", width=8, height=6, units="in", res=300)
fixest::iplot(mod_event_mh_bust_trunc)
dev.off()



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods17_mh.txt")


print("Event models, no covariates. State fixed effects.")
print("Uses fenegbin()")
print("Treatment is first boom or bust for a county as indicated by 25% continuous carry through indicator.")
print("Some counties have no boom/bust.")
print("time to treatment is is years preceeding/following boom/bust 'treatment' event")


print("###### Treatment == Bust #######")

print("All mental health")
summary(mod_event_mh_bust)

print("All mental health - truncated")
summary(mod_event_mh_bust_trunc)

print("###### Treatment == Boom #######")

print("All mental health")
summary(mod_event_mh_boom)

print("All mental health - truncated")
summary(mod_event_mh_boom_trunc)


closeAllConnections()


sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods17_mh_newReference.txt")


print("Event models, no covariates. State effects.")
print("Uses fenegbin()")
print("Treatment is first boom or bust for a county as indicated by 25% continuous carry through indicator.")
print("Some counties have no boom/bust.")
print("time to treatment is is years preceeding/following boom/bust 'treatment' event")


print("Make the -1 period the central period:")
print("merge_prepost$time_to_treat2 <- merge_prepost$time_to_treat")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -1)] <- 1")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -10)] <- 2")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -9)] <- 3")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -8)] <- 4")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -7)] <- 5")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -6)] <- 6")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -5)] <- 7")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -4)] <- 8")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -3)] <- 9")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == -2)] <- 10")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 0)] <- 11")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 1)] <- 12")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 2)] <- 13")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 3)] <- 14")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 4)] <- 15")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 5)] <- 16")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 6)] <- 17")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 7)] <- 18")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 8)] <- 19")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 9)] <- 20")
print("merge_prepost$time_to_treat2[which(merge_prepost$time_to_treat == 10)] <- 21")

merge_prepost$time_to_treat2 <- as.factor(merge_prepost$time_to_treat2)

print("###### Treatment == Bust #######")

print("All mental health")
summary(mod_event_mh_bust_factor)


print("All mental health - truncated")
summary(mod_event_mh_bust_factor_trunc)


print("###### Treatment == Boom #######")

print("All mental health")
summary(mod_event_mh_boom_factor)


print("All mental health - truncated")
summary(mod_event_mh_bust_factor_trunc)



closeAllConnections()




