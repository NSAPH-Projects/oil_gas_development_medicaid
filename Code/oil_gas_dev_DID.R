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
# MODIFICATIONS : Removed exploratory modeling; reduced only to essential models for the paper. 
#                 For exploratory modeling, see: pre_20241007
#                 Model sets 13, 14, 15, 17 are the ones in the final manuscript 
#
#
# DATE          : 11/07/2024
# PROGRAMMER    : Flannery Black-Ingersoll
# DESCRIPTION   : Added annotations (see initials)	
#######################################################################################

Sys.setenv(http_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
Sys.setenv(https_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
#install.packages("did")
#install.packages("AER")
#install.packages("plm")
#install.packages("fixest")
#install.packages("stargazer")


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

# FBI: the following three functions are for fixing the number of characters in FIPS for county and state
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


county_partial_fips <- function(fips){
  fips <- as.character(fips)
  if(nchar(fips) == 1){
    fips_new <- paste0("00",fips)
  }
  if(nchar(fips) == 2){
    fips_new <- paste0("0",fips)
  }
  if(nchar(fips) == 3){
    fips_new <- fips
  }
  if(is.na(fips)){
    fips_new <-NA 
  }
  return(fips_new)
}

# FBI: this makes a function to identify what is not in a list
'%!in%' <- function(x,y)!('%in%'(x,y))


# helpful function for exporting model tables (fixest results - non-stratified models)
ntable <- function(model, dataframe){
  if(length(model$obs_selection) == 0){
    hosp_n <- sum(dataframe$mental_health_hospitalizations)
    state_n <- length(unique(dataframe$state_fips_complete))
    county_n <- length(unique(dataframe$residence_county))
  }
  if(length(model$obs_selection) > 0){
    hosp_n <- sum(dataframe$mental_health_hospitalizations[-model$obs_selection$obsRemoved * -1])
    state_n <- length(unique(dataframe$state_fips_complete[-model$obs_selection$obsRemoved * -1]))
    county_n <- length(unique(dataframe$residence_county[-model$obs_selection$obsRemoved * -1]))
  }
  return(c(hosp_n, state_n, county_n))
}


## Load state names fips


#### Process data: Load oil and gas, MH hospitalization data ####

## Gas data - by county, year
dat <- read.csv("oil_gas_development_medicaid/Data/oilgascounty_1_long_an.csv")
# FBI: FIPS to character for residence_county
dat$residence_county <- unlist(lapply(dat$FIPS, function(x) county_fips_fix(as.character(x))))
# FBI: state number is first two digits of county number
dat$state_fips <- substr(dat$residence_county, 1,2)


## Create binary boom/bust measures: Threshold 10%, boom/bust not carried through
# FBI: Ok, so boom_year == 1 iff ogd_change_group_cont == "Boom" and
# bust_year == 1 iff ogd_change_group_cont == "Bust"
dat$boom_year <- as.character(dat$ogd_change_group_cont)
dat$boom_year[which(dat$ogd_change_group_cont == "Boom")] <- 1
dat$boom_year[which(dat$ogd_change_group_cont == "Bust")] <- 0
dat$boom_year[which(dat$ogd_change_group_cont == "Status Quo")] <- 0

dat$bust_year <- as.character(dat$ogd_change_group_cont)
dat$bust_year[which(dat$ogd_change_group_cont == "Boom")] <- 0
dat$bust_year[which(dat$ogd_change_group_cont == "Bust")] <- 1
dat$bust_year[which(dat$ogd_change_group_cont == "Status Quo")] <- 0


## Create binary boom/bust measures: Threshold 10%, boom/bust carried through
dat$boom_year2 <- as.character(dat$ogd_change_group_cont2)
dat$boom_year2[which(dat$ogd_change_group_cont2 == "Boom")] <- 1
dat$boom_year2[which(dat$ogd_change_group_cont2 == "Bust")] <- 0
dat$boom_year2[which(dat$ogd_change_group_cont2 == "Status Quo")] <- 0

dat$bust_year2 <- as.character(dat$ogd_change_group_cont2)
dat$bust_year2[which(dat$ogd_change_group_cont2 == "Boom")] <- 0
dat$bust_year2[which(dat$ogd_change_group_cont2 == "Bust")] <- 1
dat$bust_year2[which(dat$ogd_change_group_cont2 == "Status Quo")] <- 0


## Create binary boom/bust measures: Threshold 25%, boom/bust carried through
dat$boom_year25_2 <- as.character(dat$ogd_change_group_cont25_2)
dat$boom_year25_2[which(dat$ogd_change_group_cont25_2 == "Boom")] <- 1
dat$boom_year25_2[which(dat$ogd_change_group_cont25_2 == "Bust")] <- 0
dat$boom_year25_2[which(dat$ogd_change_group_cont25_2 == "Status Quo")] <- 0

dat$bust_year25_2 <- as.character(dat$ogd_change_group_cont25_2)
dat$bust_year25_2[which(dat$ogd_change_group_cont25_2 == "Boom")] <- 0
dat$bust_year25_2[which(dat$ogd_change_group_cont25_2 == "Bust")] <- 1
dat$bust_year25_2[which(dat$ogd_change_group_cont25_2 == "Status Quo")] <- 0

dat$quo_year25_2 <- as.character(dat$ogd_change_group_cont25_2)
dat$quo_year25_2[which(dat$ogd_change_group_cont25_2 == "Boom")] <- 0
dat$quo_year25_2[which(dat$ogd_change_group_cont25_2 == "Bust")] <- 0
dat$quo_year25_2[which(dat$ogd_change_group_cont25_2 == "Status Quo")] <- 1


## Create binary boom/bust measures: USDA measures
dat$USDA_boom <- as.character(dat$oil_gas_change_group)
dat$USDA_boom[which(dat$oil_gas_change_group == "H_Growth")] <- 1
dat$USDA_boom[which(dat$oil_gas_change_group == "H_Decline")] <- 0
dat$USDA_boom[which(dat$oil_gas_change_group == "Status Quo")] <- 0

dat$USDA_bust <- as.character(dat$oil_gas_change_group)
dat$USDA_bust[which(dat$oil_gas_change_group == "H_Growth")] <- 0
dat$USDA_bust[which(dat$oil_gas_change_group == "H_Decline")] <- 1
dat$USDA_bust[which(dat$oil_gas_change_group == "Status Quo")] <- 0

dat$USDA_quo <- as.character(dat$oil_gas_change_group)
dat$USDA_quo[which(dat$oil_gas_change_group == "H_Growth")] <- 0
dat$USDA_quo[which(dat$oil_gas_change_group == "H_Decline")] <- 0
dat$USDA_quo[which(dat$oil_gas_change_group == "Status Quo")] <- 1

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
  file <- file[which(file$age_group != "0-18"),] # FBI: drop children
  
  file_new <- file %>% 
    dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize_all(.funs = c(sum), na.rm = TRUE) # FBI: gets counts by county for all included variables
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


## Add leading zeros to county fips codes
files_new_df$residence_county <- unlist(lapply(files_new_df$residence_county, function(x) county_fips_fix(as.character(x))))

## check to make sure all years are included and county FIPS codes contain 5 digits
table(files_new_df$year) # FBI: 2000-2011, looks good
table(nchar(files_new_df$residence_county)) # FBI: N = n obs in dataset, looks good

## Create beneficiary variables
# Race designations from CMS
#1	WHITE, NOT OF HISPANIC ORIGIN (CHANGED TO "WHITE" BEGINNING 10/98)
#2	BLACK, NOT OF HISPANIC ORIGIN (CHANGED TO "BLACK OR AFRICAN AMERICAN" BEGINNING 10/98)
#5	HISPANIC (CHANGED TO "HISPANIC OR LATINO - NO RACE INFORMATION AVAILABLE" BEGINNING 10/98)
#7	HISPANIC OR LATINO AND ONE OR MORE RACES (NEW CODE BEGINNING 10/98)

dem_files_new <- vector(mode='list', length=12)

for(i in 1:length(years)){
  file <- files[[i]]
  file <- file[which(file$age_group != "0-18"),] # FBI: exclude children
  
  file$residence_county <- as.character(file$residence_county)
  # FBI: total is the count by county
  total <- file %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% 
    dplyr::rename("total" = "all_cause_hospitalizations")
  # FBI: totals by sex and county
  female <- file[which(file$sex == "F" & !is.na(file$sex)),] %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% 
    dplyr::rename("female" = "all_cause_hospitalizations")
  male <- file[which(file$sex == "M"),] %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% 
    dplyr::rename("male" = "all_cause_hospitalizations")
  unknown <- file[which(file$sex == "U"),] %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% 
    dplyr::rename("unknown" = "all_cause_hospitalizations")
  # FBI: totals by race and county
  white <- file[which(file$race == 1),] %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% 
    dplyr::rename("white" = "all_cause_hospitalizations")
  black <- file[which(file$race == 2),] %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% 
    dplyr::rename("black" = "all_cause_hospitalizations")
  hisp <- file[which(file$race == 5),] %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% 
    dplyr::rename("hisp" = "all_cause_hospitalizations")
  hisp_oom <- file[which(file$race == 7),] %>% 
    dplyr::group_by(residence_county) %>% 
    dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% 
    dplyr::rename("hisp_oom" = "all_cause_hospitalizations")
  # FBI: totals by age group and county
  age19_24 <- file[which(file$age_group == "19-24"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age19_24" = "all_cause_hospitalizations")
  age25_34 <- file[which(file$age_group == "25-34"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age25_34" = "all_cause_hospitalizations")
  age35_44 <- file[which(file$age_group == "35-44"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age35_44" = "all_cause_hospitalizations")
  age45_54 <- file[which(file$age_group == "45-54"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age45_54" = "all_cause_hospitalizations")
  age55_64 <- file[which(file$age_group == "55-64"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age55_64" = "all_cause_hospitalizations")
  age65_74 <- file[which(file$age_group == "65-74"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age65_74" = "all_cause_hospitalizations")
  age75_84 <- file[which(file$age_group == "75-84"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age75_84" = "all_cause_hospitalizations")
  age85_plus <- file[which(file$age_group == "85+"),] %>% dplyr::group_by(residence_county) %>% dplyr::summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% dplyr::rename("age85_plus" = "all_cause_hospitalizations")
  # FBI: Combine all grouped totals
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

# FBI: removing Alaska, Hawaii, any states with 0 for oilsum and gassum, and states without outcome data
states_to_remove <- c("AK","HI","CT","DC","DE","GA","IA","ID","IL","MA","ME","MN","NC","NH","NJ","OR","RI","SC","VT","WA","WI")
merge <- merge[-which(merge$State.Abbreviation %in% states_to_remove),]


## confirm there are no missing oil/gas units in the original USDA file 
length(which(is.na(dat$gas_units)))
length(which(is.na(dat$oil_units)))
# FBI: looks good

## Remove counties with zero oil OR gas production during this time
counties_zero <- dat %>% dplyr::group_by(residence_county) %>% dplyr::summarize(oilsum = sum(oil_units, na.rm =TRUE),
                                                    gassum = sum(gas_units, na.rm =TRUE))


counties_zero <- counties_zero$residence_county[which(counties_zero$oilsum == 0 & counties_zero$gassum == 0)]
merge <- merge[which(merge$residence_county %!in% counties_zero),] 
length(unique(merge$residence_county)) ## 1171


## Explore characteristics of missing/nonmissing data:
na_counties <- unique(merge$residence_county[which(is.na(merge$oil_units))]) 
nonna_counties <- unique(merge$residence_county[which(!is.na(merge$oil_units))]) 

length(na_counties) ## 52 counties in the merge file, for which we have zero OGD info even after exclusions
length(nonna_counties) ## 1119 counties in the merge file, for which we have OGD info




### Truncate MH outcomes for sensitivity analysis
# FBI: labeling observations at or above the 95th percentile
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





#### TOTAL N ####
## overall, we have non-missing observations in:
length(which(!is.na(merge$oil_units) & !is.na(merge$mental_health_hospitalizations))) # 13370 observations
length(unique(merge$residence_county[which(!is.na(merge$oil_units) & !is.na(merge$mental_health_hospitalizations))])) # 1118 counties


# Confirm OGD stats
dim(merge) # 13789 x 90
length(unique(merge$residence_county)) # 1171 

table(merge$ogd_change_group_cont)

#Expected values:
#Boom       Bust Status Quo 
#2972       3143       6194 

table(merge$ogd_change_group_cont2)

#Expected values: 
#Boom       Bust Status Quo 
#3989       4751       3569

#### Correlations between our booms/busts/status quos and USDAs
cor.test(as.numeric(merge$bust_year25_2), as.numeric(merge$USDA_bust), method = "spearman")
cor.test(as.numeric(merge$boom_year25_2), as.numeric(merge$USDA_boom), method = "spearman")
cor.test(as.numeric(merge$quo_year25_2), as.numeric(merge$USDA_quo), method = "spearman")



#### Add in economic data (10/07 addition) #####

saipe2001 <- readr::read_fwf("oil_gas_development_medicaid/Data/est01all.dat")

saipe2001$X2 <- unlist(lapply(saipe2001$X2, function(x) county_partial_fips(x)))
saipe2001$residence_county <- paste0(saipe2001$X1, saipe2001$X2)

saipe2001$median_household_income <- saipe2001$X21
saipe2001 <- saipe2001[,c("residence_county","median_household_income")]

merge_income <- merge(merge, saipe2001, by = c("residence_county"), all = TRUE)
merge_income <- merge_income[which(merge_income$residence_county %in% unique(merge$residence_county)),]


dim(merge_income) # 13789 x 90
length(unique(merge_income$residence_county)) # 1171 

#Expected values:
table(merge_income$ogd_change_group_cont)
#Boom       Bust Status Quo 
#2972       3143       6194 

table(merge_income$ogd_change_group_cont2)

#Expected values: 
#Boom       Bust Status Quo 
#3989       4751       3569


## Confirmed that this matches and create income strata 

merge_income$income_ntile <- ntile(merge_income$median_household_income, 3)




##### Figures: Hospitalization sums/means by boom/bust year   ######


sums_contemp <- merge[,c("ogd_change_group_cont25_2",
                         "year",
                         "mental_health_hospitalizations",
                         "adjustment_reaction_hospitalizations",
                         "anxiety_disorders_hospitalizations",
                         "attention_disorders_hospitalizations",
                         "mood_disorders_hospitalizations",
                         "personality_disorders_hospitalizations",
                         "schizophrenia_psychotic_disorders_hospitalizations")] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))


                                    
means_contemp <- merge[,c("ogd_change_group_cont25_2",
                          "year",
                          "mental_health_hospitalizations",
                          "adjustment_reaction_hospitalizations",
                          "anxiety_disorders_hospitalizations",
                          "attention_disorders_hospitalizations",
                          "mood_disorders_hospitalizations",
                          "personality_disorders_hospitalizations",
                          "schizophrenia_psychotic_disorders_hospitalizations")] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))



sums_contin <- merge[,c("oil_gas_change_group",
                         "year",
                         "mental_health_hospitalizations",
                         "adjustment_reaction_hospitalizations",
                         "anxiety_disorders_hospitalizations",
                         "attention_disorders_hospitalizations",
                         "mood_disorders_hospitalizations",
                         "personality_disorders_hospitalizations",
                         "schizophrenia_psychotic_disorders_hospitalizations")] %>% group_by(oil_gas_change_group, year) %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))



means_contin <- merge[,c("oil_gas_change_group",
                          "year",
                          "mental_health_hospitalizations",
                          "adjustment_reaction_hospitalizations",
                          "anxiety_disorders_hospitalizations",
                          "attention_disorders_hospitalizations",
                          "mood_disorders_hospitalizations",
                          "personality_disorders_hospitalizations",
                          "schizophrenia_psychotic_disorders_hospitalizations")] %>% group_by(oil_gas_change_group, year) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))



## For means, we can obfuscate to ensure values exceed mean
## Take minimum, find how much is needed to bring this to 12, and add this value across all


means_contin$adjustment_reaction_hospitalizations <- means_contin$adjustment_reaction_hospitalizations + (12-1.911)
means_contin$attention_disorders_hospitalizations <- means_contin$attention_disorders_hospitalizations + (12-0.8073)
means_contin$personality_disorders_hospitalizations <- means_contin$personality_disorders_hospitalizations + (12-4.159)


means_contemp$adjustment_reaction_hospitalizations <- means_contemp$adjustment_reaction_hospitalizations + (12-2.456)
means_contemp$attention_disorders_hospitalizations <- means_contemp$attention_disorders_hospitalizations + (12-1.050)
means_contemp$personality_disorders_hospitalizations <- means_contemp$personality_disorders_hospitalizations + (12-4.925)



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

means_contemp_melt$variable_clean <- factor(means_contemp_melt$variable_clean, levels = c("All MH","Adjustment","Attention","Anxiety","Mood","Personality","Schizophrenia"))


png(file="oil_gas_development_medicaid/Figures/mean_hospitalizations_contemporaneous.png", width=8, height=6, units="in", res=300)
ggplot(means_contemp_melt[which(!is.na(means_contemp_melt$boom_bust) & means_contemp_melt$variable_clean != "All MH" & means_contemp_melt$year != 2001),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("All mental Health Hospitalizations: \n Contemporaneous boom/bust designation at 25% change, mean values") +
  ylab("Mean hospitalizations") + 
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2011, by = 1)) + 
  facet_wrap(.~variable_clean) + 
  theme_light() + 
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 45))
dev.off()

png(file="oil_gas_development_medicaid/Figures/mean_mh_hospitalizations_contemporaneous.png", width=8, height=6, units="in", res=300)
ggplot(means_contemp_melt[which(!is.na(means_contemp_melt$boom_bust) & means_contemp_melt$variable_clean == "All MH" & means_contemp_melt$year != 2000),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("Mental Health Hospitalizations: \n Contemporaneous boom/bust designation at 25% change, mean values") +
  ylab("Mean hospitalizations") + 
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2011, by = 1)) + 
  theme_light() + 
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 45))
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

sums_contemp_melt$variable_clean <- factor(sums_contemp_melt$variable_clean, levels = c("All MH","Adjustment","Attention","Anxiety","Mood","Personality","Schizophrenia"))

png(file="oil_gas_development_medicaid/Figures/sum_hospitalizations_contemporaneous.png", width=8, height=6, units="in", res=300)
ggplot(sums_contemp_melt[which(!is.na(sums_contemp_melt$boom_bust) & sums_contemp_melt$variable_clean != "All MH" & sums_contemp_melt$year != 2000),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("All mental Health Hospitalizations: \n Contemporaneous boom/bust designation at 25% change, sum values") +
  ylab("sum hospitalizations") + 
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2011, by = 1)) + 
  facet_wrap(.~variable_clean) + 
  theme_light() + 
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 45))
dev.off()



# continuous - means

means_contin_melt <- reshape2::melt(means_contin, id.vars = c("oil_gas_change_group","year"))
names(means_contin_melt)[1] <- "boom_bust"
means_contin_melt$boom_bust[which(means_contin_melt$boom_bust == "H_Decline")] <- "Bust"
means_contin_melt$boom_bust[which(means_contin_melt$boom_bust == "H_Growth")] <- "Boom"


means_contin_melt$variable_clean <- as.character(means_contin_melt$variable)
means_contin_melt$variable_clean[which(means_contin_melt$variable == "mental_health_hospitalizations")] <- "All MH"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "adjustment_reaction_hospitalizations")] <- "Adjustment"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "anxiety_disorders_hospitalizations")] <- "Anxiety"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "attention_disorders_hospitalizations")] <- "Attention"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "mood_disorders_hospitalizations")] <- "Mood"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "personality_disorders_hospitalizations")] <- "Personality"
means_contin_melt$variable_clean[which(means_contin_melt$variable == "schizophrenia_psychotic_disorders_hospitalizations")] <- "Schizophrenia"

means_contin_melt$variable_clean <- factor(means_contin_melt$variable_clean, levels = c("All MH","Adjustment","Attention","Anxiety","Mood","Personality","Schizophrenia"))


png(file="oil_gas_development_medicaid/Figures/mean_hospitalizations_continuous.png", width=8, height=6, units="in", res=300)
ggplot(means_contin_melt[which(!is.na(means_contin_melt$boom_bust) & means_contin_melt$variable_clean != "All MH" & means_contin_melt$year != 2000),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("All mental Health Hospitalizations: \n continuous boom/bust designation from USDA, mean values") +
  ylab("Mean hospitalizations") + 
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_x_continuous(breaks = seq(from = 2001, to = 2011, by = 1)) + 
  facet_wrap(.~variable_clean) + 
  theme_light() + 
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 45))
dev.off()

png(file="oil_gas_development_medicaid/Figures/mean_mh_hospitalizations_continuous.png", width=8, height=6, units="in", res=300)
ggplot(means_contin_melt[which(!is.na(means_contin_melt$boom_bust) & means_contin_melt$variable_clean == "All MH" & means_contin_melt$year != 2000),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("Mental Health Hospitalizations: \n continuous boom/bust designation from USDA, mean values") +
  ylab("Mean hospitalizations") + 
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2011, by = 1)) + 
  theme_light() + 
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 45))
dev.off()


# continuous - sums

sums_contin_melt <- reshape2::melt(sums_contin, id.vars = c("oil_gas_change_group","year"))
names(sums_contin_melt)[1] <- "boom_bust"
sums_contin_melt$boom_bust[which(sums_contin_melt$boom_bust == "H_Decline")] <- "Bust"
sums_contin_melt$boom_bust[which(sums_contin_melt$boom_bust == "H_Growth")] <- "Boom"


sums_contin_melt$variable_clean <- as.character(sums_contin_melt$variable)
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "mental_health_hospitalizations")] <- "All MH"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "adjustment_reaction_hospitalizations")] <- "Adjustment"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "anxiety_disorders_hospitalizations")] <- "Anxiety"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "attention_disorders_hospitalizations")] <- "Attention"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "mood_disorders_hospitalizations")] <- "Mood"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "personality_disorders_hospitalizations")] <- "Personality"
sums_contin_melt$variable_clean[which(sums_contin_melt$variable == "schizophrenia_psychotic_disorders_hospitalizations")] <- "Schizophrenia"

sums_contin_melt$variable_clean <- factor(sums_contin_melt$variable_clean, levels = c("All MH","Adjustment","Attention","Anxiety","Mood","Personality","Schizophrenia"))

png(file="oil_gas_development_medicaid/Figures/sum_hospitalizations_continuous.png", width=8, height=6, units="in", res=300)
ggplot(sums_contin_melt[which(!is.na(sums_contin_melt$boom_bust) & sums_contin_melt$variable_clean != "All MH" & sums_contin_melt$year != 2000),], aes(year, value, color = boom_bust)) + 
  geom_line() + 
  ggtitle("All mental Health Hospitalizations: \n continuous boom/bust designation from USDA, sum values") +
  ylab("sum hospitalizations") + 
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2011, by = 1)) + 
  facet_wrap(.~variable_clean) + 
  theme_light() + 
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 45))
dev.off()


##### Set 13: Negative binomial models, no covariates. County/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through #####


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



plm_mh13_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_adj13_clust <- fenegbin(adjustment_reaction_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_anx13_clust <- fenegbin(anxiety_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_att13_clust <- fenegbin(attention_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_mood13_clust <- fenegbin(mood_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_pers13_clust <- fenegbin(personality_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_schiz13_clust <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_alc13_clust <- fenegbin(alcohol_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_sub13_clust <- fenegbin(substance_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)
plm_suic13_clust <- fenegbin(suicide_self_harm_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge, cluster = ~ residence_county + year)


sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13.txt")

print("Negative binomial models with county and year fixed effects. No covariates")
print("Uses contemporaneous boom/bust indicator, carried through, with 25% threshold")
print("")

print("All mental health hospitalizations")
summary(plm_mh13)

print("Adjustment disorder")
summary(plm_adj13)

print("Anxiety disorder")
summary(plm_anx13)

print("Attention disorder")
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





sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_clusteredSE.txt")

print("Negative binomial models with county and year fixed effects. No covariates")
print("Uses contemporaneous boom/bust indicator, carried through, with 25% threshold")
print("")

print("All mental health hospitalizations")
summary(plm_mh13_clust)

print("Adjustment disorder")
summary(plm_adj13_clust)

print("Anxiety disorder")
summary(plm_anx13_clust)

print("Attention disorder")
summary(plm_att13_clust)

print("Mood disorder")
summary(plm_mood13_clust)

print("Personality disorder")
summary(plm_pers13_clust)

print("Schizophrenia")
summary(plm_schiz13_clust)

print("Alcohol use disorder")
summary(plm_alc13_clust)

print("Substance use disorder")
summary(plm_sub13_clust)

print("Suicide/self-harm")
summary(plm_suic13_clust)

closeAllConnections()



## Export IRR for these models
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_irr.txt")

print("Mental health hospitalizations")

plm_mh13_out <- cbind(Estimate = coef(plm_mh13), stats::confint(plm_mh13)[-3,])
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


print("Attention disorder")
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




## Export IRR for these models
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_irr_clusteredSE.txt")

print("Mental health hospitalizations")

plm_mh13_out <- cbind(Estimate = coef(plm_mh13_clust), stats::confint(plm_mh13_clust)[-3,])
plm_mh13_irr <- exp(plm_mh13_out)
plm_mh13_irr

print("Adjustment disorder")
plm_adj13_out <- cbind(Estimate = coef(plm_adj13_clust), confint(plm_adj13_clust)[-3,])
plm_adj13_irr <- exp(plm_adj13_out)
plm_adj13_irr

print("Anxiety disorder")
plm_anx13_out <- cbind(Estimate = coef(plm_anx13_clust), confint(plm_anx13_clust)[-3,])
plm_anx13_irr <- exp(plm_anx13_out)
plm_anx13_irr


print("Attention disorder")
plm_att13_out <- cbind(Estimate = coef(plm_att13_clust), confint(plm_att13_clust)[-3,])
plm_att13_irr <- exp(plm_att13_out)
plm_att13_irr


print("Mood disorder")
plm_mood13_out <- cbind(Estimate = coef(plm_mood13_clust), confint(plm_mood13_clust)[-3,])
plm_mood13_irr <- exp(plm_mood13_out)
plm_mood13_irr

print("Personality disorder")
plm_pers13_out <- cbind(Estimate = coef(plm_pers13_clust), confint(plm_pers13_clust)[-3,])
plm_pers13_irr <- exp(plm_pers13_out)
plm_pers13_irr

print("Schizophrenia")
plm_schiz13_out <- cbind(Estimate = coef(plm_schiz13_clust), confint(plm_schiz13_clust)[-3,])
plm_schiz13_irr <- exp(plm_schiz13_out)
plm_schiz13_irr

print("Alcohol use disorder")
plm_alc13_out <- cbind(Estimate = coef(plm_alc13_clust), confint(plm_alc13_clust)[-3,])
plm_alc13_irr <- exp(plm_alc13_out)
plm_alc13_irr


print("Substance use disorder")
plm_sub13_out <- cbind(Estimate = coef(plm_sub13_clust), confint(plm_sub13_clust)[-3,])
plm_sub13_irr <- exp(plm_sub13_out)
plm_sub13_irr


print("Suicide/self-harm")
plm_suic13_out <- cbind(Estimate = coef(plm_suic13_clust), confint(plm_suic13_clust)[-3,])
plm_suic13_irr <- exp(plm_suic13_out)
plm_suic13_irr

closeAllConnections()



## N values 

mh_n <- sum(merge$mental_health_hospitalizations[-plm_mh13$obs_selection$obsRemoved * -1])
adj_n <- sum(merge$adjustment_reaction_hospitalizations[-plm_adj13$obs_selection$obsRemoved * -1])
att_n <- sum(merge$attention_disorders_hospitalizations[-plm_att13$obs_selection$obsRemoved * -1])
anx_n <- sum(merge$anxiety_disorders_hospitalizations[-plm_anx13$obs_selection$obsRemoved * -1])
mood_n <- sum(merge$mood_disorders_hospitalizations[-plm_mood13$obs_selection$obsRemoved * -1])
pers_n <- sum(merge$personality_disorders_hospitalizations[-plm_pers13$obs_selection$obsRemoved * -1])
schiz_n <- sum(merge$schizophrenia_psychotic_disorders_hospitalizations[-plm_schiz13$obs_selection$obsRemoved * -1])
sub_n <- sum(merge$substance_disorders_hospitalizations[-plm_sub13$obs_selection$obsRemoved * -1])
alc_n <- sum(merge$alcohol_disorders_hospitalizations[-plm_alc13$obs_selection$obsRemoved * -1])
suic_n <- sum(merge$suicide_self_harm_hospitalizations[-plm_suic13$obs_selection$obsRemoved * -1])

mh_cnty <- length(unique(merge$residence_county[-plm_mh13$obs_selection$obsRemoved * -1]))
adj_cnty <- length(unique(merge$residence_county[-plm_adj13$obs_selection$obsRemoved * -1]))
att_cnty <- length(unique(merge$residence_county[-plm_att13$obs_selection$obsRemoved * -1]))
anx_cnty <- length(unique(merge$residence_county[-plm_anx13$obs_selection$obsRemoved * -1]))
mood_cnty <- length(unique(merge$residence_county[-plm_mood13$obs_selection$obsRemoved * -1]))
pers_cnty <- length(unique(merge$residence_county[-plm_pers13$obs_selection$obsRemoved * -1]))
schiz_cnty <- length(unique(merge$residence_county[-plm_schiz13$obs_selection$obsRemoved * -1]))
sub_cnty <- length(unique(merge$residence_county[-plm_sub13$obs_selection$obsRemoved * -1]))
alc_cnty <- length(unique(merge$residence_county[-plm_alc13$obs_selection$obsRemoved * -1]))
suic_cnty <- length(unique(merge$residence_county[-plm_suic13$obs_selection$obsRemoved * -1]))




tab3_n <- data.frame(conditions = c("All MH","adj", "attn", "anxiety","mood","pers","schiz","sub","alc","suic"),
                     hosp = c(mh_n, adj_n, att_n, anx_n, mood_n, pers_n, schiz_n, sub_n, alc_n, suic_n),
                     county = c(mh_cnty, adj_cnty, att_cnty, anx_cnty, mood_cnty, pers_cnty, schiz_cnty, sub_cnty, alc_cnty, suic_cnty))



test_dat_boom <- unique(merge[,c("year","residence_county")])
test_dat_boom$boom_year25_2 <- 1
test_dat_boom$bust_year25_2 <- 0
test_dat_boom$boom_year25_2 <- as.factor(test_dat_boom$boom_year25_2)
test_dat_boom$bust_year25_2 <- as.factor(test_dat_boom$bust_year25_2)

test_dat_bust <- unique(merge[,c("year","residence_county")])
test_dat_bust$bust_year25_2 <- 1
test_dat_bust$boom_year25_2 <- 0
test_dat_bust$boom_year25_2 <- as.factor(test_dat_bust$boom_year25_2)
test_dat_bust$bust_year25_2 <- as.factor(test_dat_bust$bust_year25_2)


test_dat_boom <- cbind(test_dat_boom, predict(plm_mh13, test_dat_boom))
test_dat_bust <- cbind(test_dat_bust, predict(plm_mh13, test_dat_bust))

round(sum(test_dat_boom$`predict(plm_mh13, test_dat_boom)`, na.rm = TRUE) - mh_n)
round(sum(test_dat_bust$`predict(plm_mh13, test_dat_bust)`, na.rm = TRUE) - mh_n)


##### Set 13: Negative binomial models, no covariates. County/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through. Stratified by demographics ########



#1: Non-Hispanic White
#2: Black (or African-American)
#4: Asian/Pacific Islander
#5/7: Hispanic

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
  
  file_new_latine <- file[which(file$race == 5 | file$race == 7),] %>% dplyr::select(-contains(c("year","month","state","sex","race","age_group"))) %>% dplyr::group_by(residence_county) %>% summarize_all(.funs = c(sum), na.rm = TRUE)
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

plm_mh13_white_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_white, cluster = ~residence_county + year)
plm_mh13_black_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_black, cluster = ~residence_county + year)
plm_mh13_latine_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_latine, cluster = ~residence_county + year)
plm_mh13_male_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_male, cluster = ~residence_county + year)
plm_mh13_female_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_female, cluster = ~residence_county + year)
plm_mh13_19to44_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_19to44, cluster = ~residence_county + year)
plm_mh13_45to64_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_45to64, cluster = ~residence_county + year)
plm_mh13_65plus_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_65plus, cluster = ~residence_county + year)


## Export coefficients
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_demStratified.txt")

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
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_demStratified_irr.txt")

print("Mental health hospitalizations: White beneficiaries")
plm_mh13_white_out <- cbind(Estimate = coef(plm_mh13_white), confint(plm_mh13_white)[-3,])
plm_mh13_white_irr <- exp(plm_mh13_white_out)
plm_mh13_white_irr


print("Mental health hospitalizations: Black beneficiaries")
plm_mh13_black_out <- cbind(Estimate = coef(plm_mh13_black), confint(plm_mh13_black)[-3,])
plm_mh13_black_irr <- exp(plm_mh13_black_out)
plm_mh13_black_irr


print("Mental health hospitalizations: Latine beneficiaries")
plm_mh13_latine_out <- cbind(Estimate = coef(plm_mh13_latine), confint(plm_mh13_latine)[-3,])
plm_mh13_latine_irr <- exp(plm_mh13_latine_out)
plm_mh13_latine_irr


print("Mental health hospitalizations: Male beneficiaries")
plm_mh13_male_out <- cbind(Estimate = coef(plm_mh13_male), confint(plm_mh13_male)[-3,])
plm_mh13_male_irr <- exp(plm_mh13_male_out)
plm_mh13_male_irr


print("Mental health hospitalizations: Female beneficiaries")
plm_mh13_female_out <- cbind(Estimate = coef(plm_mh13_female), confint(plm_mh13_female)[-3,])
plm_mh13_female_irr <- exp(plm_mh13_female_out)
plm_mh13_female_irr


print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
plm_mh13_19to44_out <- cbind(Estimate = coef(plm_mh13_19to44), confint(plm_mh13_19to44)[-3,])
plm_mh13_19to44_irr <- exp(plm_mh13_19to44_out)
plm_mh13_19to44_irr


print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
plm_mh13_45to64_out <- cbind(Estimate = coef(plm_mh13_45to64), confint(plm_mh13_45to64)[-3,])
plm_mh13_45to64_irr <- exp(plm_mh13_45to64_out)
plm_mh13_45to64_irr

print("Mental health hospitalizations: Age 65 plus beneficiaries")
plm_mh13_65plus_out <- cbind(Estimate = coef(plm_mh13_65plus), confint(plm_mh13_65plus)[-3,])
plm_mh13_65plus_irr <- exp(plm_mh13_65plus_out)
plm_mh13_65plus_irr


closeAllConnections()





## Export coefficients
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_demStratified_clusteredSE.txt")

print("Mental health hospitalizations: White beneficiaries")
summary(plm_mh13_white_clust)

print("Mental health hospitalizations: Black beneficiaries")
summary(plm_mh13_black_clust)

print("Mental health hospitalizations: Latine beneficiaries")
summary(plm_mh13_latine_clust)

print("Mental health hospitalizations: Male beneficiaries")
summary(plm_mh13_male_clust)

print("Mental health hospitalizations: Female beneficiaries")
summary(plm_mh13_female_clust)

print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
summary(plm_mh13_19to44_clust)

print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
summary(plm_mh13_45to64_clust)

print("Mental health hospitalizations: Age 65 plus beneficiaries")
summary(plm_mh13_65plus_clust)


closeAllConnections()




## Export IRR 
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_demStratified_irr_clusteredSE.txt")

print("Mental health hospitalizations: White beneficiaries")
plm_mh13_white_out <- cbind(Estimate = coef(plm_mh13_white_clust), confint(plm_mh13_white_clust)[-3,])
plm_mh13_white_irr <- exp(plm_mh13_white_out)
plm_mh13_white_irr


print("Mental health hospitalizations: Black beneficiaries")
plm_mh13_black_out <- cbind(Estimate = coef(plm_mh13_black_clust), confint(plm_mh13_black_clust)[-3,])
plm_mh13_black_irr <- exp(plm_mh13_black_out)
plm_mh13_black_irr


print("Mental health hospitalizations: Latine beneficiaries")
plm_mh13_latine_out <- cbind(Estimate = coef(plm_mh13_latine_clust), confint(plm_mh13_latine_clust)[-3,])
plm_mh13_latine_irr <- exp(plm_mh13_latine_out)
plm_mh13_latine_irr


print("Mental health hospitalizations: Male beneficiaries")
plm_mh13_male_out <- cbind(Estimate = coef(plm_mh13_male_clust), confint(plm_mh13_male_clust)[-3,])
plm_mh13_male_irr <- exp(plm_mh13_male_out)
plm_mh13_male_irr


print("Mental health hospitalizations: Female beneficiaries")
plm_mh13_female_out <- cbind(Estimate = coef(plm_mh13_female_clust), confint(plm_mh13_female_clust)[-3,])
plm_mh13_female_irr <- exp(plm_mh13_female_out)
plm_mh13_female_irr


print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
plm_mh13_19to44_out <- cbind(Estimate = coef(plm_mh13_19to44_clust), confint(plm_mh13_19to44_clust)[-3,])
plm_mh13_19to44_irr <- exp(plm_mh13_19to44_out)
plm_mh13_19to44_irr


print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
plm_mh13_45to64_out <- cbind(Estimate = coef(plm_mh13_45to64_clust), confint(plm_mh13_45to64_clust)[-3,])
plm_mh13_45to64_irr <- exp(plm_mh13_45to64_out)
plm_mh13_45to64_irr

print("Mental health hospitalizations: Age 65 plus beneficiaries")
plm_mh13_65plus_out <- cbind(Estimate = coef(plm_mh13_65plus_clust), confint(plm_mh13_65plus_clust)[-3,])
plm_mh13_65plus_irr <- exp(plm_mh13_65plus_out)
plm_mh13_65plus_irr


closeAllConnections()



## N values
ntable(plm_mh13_female_clust, merge_female)
ntable(plm_mh13_male_clust, merge_male)
ntable(plm_mh13_white_clust, merge_white)
ntable(plm_mh13_black_clust, merge_black)
ntable(plm_mh13_latine_clust, merge_latine)
ntable(plm_mh13_19to44_clust, merge_19to44)
ntable(plm_mh13_45to64_clust, merge_45to64)
ntable(plm_mh13_65plus_clust, merge_65plus)


## Predicted values 

test_dat_female_boom <- unique(merge[,c("year","residence_county")])
test_dat_boom$boom_year25_2 <- 1
test_dat_boom$bust_year25_2 <- 0
test_dat_boom$boom_year25_2 <- as.factor(test_dat_boom$boom_year25_2)
test_dat_boom$bust_year25_2 <- as.factor(test_dat_boom$bust_year25_2)

test_dat_bust <- unique(merge[,c("year","residence_county")])
test_dat_bust$bust_year25_2 <- 1
test_dat_bust$boom_year25_2 <- 0
test_dat_bust$boom_year25_2 <- as.factor(test_dat_bust$boom_year25_2)
test_dat_bust$bust_year25_2 <- as.factor(test_dat_bust$bust_year25_2)


test_dat_boom <- cbind(test_dat_boom, predict(plm_mh13, test_dat_boom))
test_dat_bust <- cbind(test_dat_bust, predict(plm_mh13, test_dat_bust))

round(sum(test_dat_boom$`predict(plm_mh13, test_dat_boom)`, na.rm = TRUE) - mh_n)
round(sum(test_dat_bust$`predict(plm_mh13, test_dat_bust)`, na.rm = TRUE) - mh_n)



##### Set 13: Negative binomial models, no covariates. County/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through. Stratified by urban/rural ######

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


plm_mh13_nonmetro_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Nonmetro_2013 == 0),], cluster = ~ residence_county + year)
plm_mh13_metro_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Nonmetro_2013 == 1),], cluster = ~ residence_county + year)


plm_mh13_micro_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 0),], cluster = ~ residence_county + year)
plm_mh13_noncore_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 1),], cluster = ~ residence_county + year)
plm_mh13_metrocore_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 2),], cluster = ~ residence_county + year)




sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_mh_metroNonmetro.txt")

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



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_mh_metroNonmetro_irr.txt")


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





sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_mh_metroNonmetro_clusteredSE.txt")

print("Models stratified by metro status")
print("Negative binomial models, no covariates. Year/county FE")
print("Contemporaneous boom/bust w/25% threshold. DOES carry through")


print("##########################################")
print("All-nonmetro")
summary(plm_mh13_nonmetro_clust)
print("All metro")
summary(plm_mh13_metro_clust)


print("##########################################")
print("Micro only")
summary(plm_mh13_micro_clust)
print("Metro noncore")
summary(plm_mh13_noncore_clust)
print("Metro core")
summary(plm_mh13_metrocore_clust)

closeAllConnections()



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_mh_metroNonmetro_irr_clusteredSE.txt")


print("Rurality models - Micro")
plm_mh13_micro_out <- cbind(Estimate = coef(plm_mh13_micro_clust), confint(plm_mh13_micro_clust)[-3,])
plm_mh13_micro_irr <- exp(plm_mh13_micro_out)
plm_mh13_micro_irr

print("Rurality models - Noncore")
plm_mh13_noncore_out <- cbind(Estimate = coef(plm_mh13_noncore_clust), confint(plm_mh13_noncore_clust)[-3,])
plm_mh13_noncore_irr <- exp(plm_mh13_noncore_out)
plm_mh13_noncore_irr


print("Rurality models - Core")
plm_mh13_metrocore_out <- cbind(Estimate = coef(plm_mh13_metrocore_clust), confint(plm_mh13_metrocore_clust)[-3,])
plm_mh13_metrocore_irr <- exp(plm_mh13_metrocore_out)
plm_mh13_metrocore_irr


closeAllConnections()


## N values 
merge_micro <- merge[which(merge$Metro_Micro_Noncore_2013 == 0),]
merge_noncore <- merge[which(merge$Metro_Micro_Noncore_2013 == 1),]
merge_metro <- merge[which(merge$Metro_Micro_Noncore_2013 == 2),]

ntable(plm_mh13_micro, merge_micro)
ntable(plm_mh13_noncore, merge_noncore)
ntable(plm_mh13_metrocore, merge_metro)



##### Set 13: Negative binomial models, no covariates. County/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through. Stratified by income ######



plm_mh13_income1 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_income[which(merge_income$income_ntile == 1),])
plm_mh13_income2 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_income[which(merge_income$income_ntile == 2),])
plm_mh13_income3 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_income[which(merge_income$income_ntile == 3),])



plm_mh13_income1_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_income[which(merge_income$income_ntile == 1),], cluster = ~ residence_county + year)
plm_mh13_income2_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_income[which(merge_income$income_ntile == 2),], cluster = ~ residence_county + year)
plm_mh13_income3_clust <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | residence_county + year, data = merge_income[which(merge_income$income_ntile == 3),], cluster = ~ residence_county + year)



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_mh_income.txt")

print("Models stratified by metro status")
print("Negative binomial models, no covariates. Year/county FE")
print("Contemporaneous boom/bust w/25% threshold. DOES carry through")

print("First income tertile")
summary(plm_mh13_income1)
print("Second income tertile")
summary(plm_mh13_income2)
print("Third income tertile")
summary(plm_mh13_income3)

closeAllConnections()



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_mh_income_irr.txt")


print("First income tertile")
plm_mh13_income1_out <- cbind(Estimate = coef(plm_mh13_income1), confint(plm_mh13_income1)[-3,])
plm_mh13_income1_irr <- exp(plm_mh13_income1_out)
plm_mh13_income1_irr

print("Second income tertile")
plm_mh13_income2_out <- cbind(Estimate = coef(plm_mh13_income2), confint(plm_mh13_income2)[-3,])
plm_mh13_income2_irr <- exp(plm_mh13_income2_out)
plm_mh13_income2_irr

print("Third income tertile")
plm_mh13_income3_out <- cbind(Estimate = coef(plm_mh13_income3), confint(plm_mh13_income3)[-3,])
plm_mh13_income3_irr <- exp(plm_mh13_income3_out)
plm_mh13_income3_irr


closeAllConnections()




sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_mh_income_clusteredSE.txt")

print("Models stratified by metro status")
print("Negative binomial models, no covariates. Year/county FE")
print("Contemporaneous boom/bust w/25% threshold. DOES carry through")

print("First income tertile")
summary(plm_mh13_income1_clust)
print("Second income tertile")
summary(plm_mh13_income2_clust)
print("Third income tertile")
summary(plm_mh13_income3_clust)

closeAllConnections()



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods13_mh_income_irr_clusteredSE.txt")


print("First income tertile")
plm_mh13_income1_out <- cbind(Estimate = coef(plm_mh13_income1_clust), confint(plm_mh13_income1_clust)[-3,])
plm_mh13_income1_irr <- exp(plm_mh13_income1_out)
plm_mh13_income1_irr

print("Second income tertile")
plm_mh13_income2_out <- cbind(Estimate = coef(plm_mh13_income2_clust), confint(plm_mh13_income2_clust)[-3,])
plm_mh13_income2_irr <- exp(plm_mh13_income2_out)
plm_mh13_income2_irr

print("Third income tertile")
plm_mh13_income3_out <- cbind(Estimate = coef(plm_mh13_income3_clust), confint(plm_mh13_income3_clust)[-3,])
plm_mh13_income3_irr <- exp(plm_mh13_income3_out)
plm_mh13_income3_irr


closeAllConnections()



## N values 
merge_income_income1 <- merge_income[which(merge_income$income_ntile == 1),]
merge_income_income2 <- merge_income[which(merge_income$income_ntile == 2),]
merge_income_income3 <- merge_income[which(merge_income$income_ntile == 3),]

ntable(plm_mh13_income1, merge_income_income1)
ntable(plm_mh13_income2, merge_income_income2)
ntable(plm_mh13_income3, merge_income_income3)




##### Table 1 ##### 

merge$age19_44 <- merge$age19_24 + merge$age25_34 + merge$age35_44
merge$age45_64 <- merge$age45_54 + merge$age55_64
merge$age65_plus <- merge$age65_74 + merge$age75_84 + merge$age85_plus


total_year <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(year) %>% summarise(count_total = sum(total, na.rm = TRUE))
female_year <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(year) %>% summarise(count_female = sum(female, na.rm = TRUE))
white_year <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(year) %>% summarise(count_white = sum(white, na.rm = TRUE))
black_year <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(year) %>% summarise(count_black = sum(black, na.rm = TRUE))
hisp_year <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(year) %>% summarise(count_hisp = sum(hisp, na.rm = TRUE))
age19to44_year <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(year) %>% summarise(count_age19to44 = sum(age19_44, na.rm = TRUE))
age45to64_year <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(year) %>% summarise(count_age45to64 = sum(age45_64, na.rm = TRUE))
age65plus_year <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(year) %>% summarise(count_age65plus = sum(age65_plus, na.rm = TRUE))


total_ogd <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_total = sum(total, na.rm = TRUE))
female_ogd <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_female = sum(female, na.rm = TRUE))
white_ogd <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_white = sum(white, na.rm = TRUE))
black_ogd <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_black = sum(black, na.rm = TRUE))
hisp_ogd <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_hisp = sum(hisp, na.rm = TRUE))
age19to44_ogd <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_age19to44 = sum(age19_44, na.rm = TRUE))
age45to64_ogd <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_age45to64 = sum(age45_64, na.rm = TRUE))
age65plus_ogd <- merge[which(!is.na(merge$ogd_change_group_cont25_2)),] %>% group_by(ogd_change_group_cont25_2, year) %>% summarise(count_age65plus = sum(age65_plus, na.rm = TRUE))


# Columns: total, boom/bust/status quo
# Row 1 - N for total hospitalizations
# 9 x 6 
#Number of hospitalizations (n) 
#Female (%) 
#Race/Ethnicity (%) 
#White 
#Black 
#Hispanic/Latinx 
#Age 19 to 44
#Age 45 to 64
#Age 65+

tab1 <- data.frame(group = c("Num. of hospitalizations (n)","Female (%)","Race/Ethnicity (%)","White","Black or African American","Hispanic/Latino/a", "Age 19 to 44","Age 45 to 64", "Age 65+"),
                   total_2001 = rep(NA, 9),
                   total_2011 = rep(NA, 9),
                   status_quo_2001 = rep(NA, 9),
                   status_quo_2011 = rep(NA, 9),
                   boom_2001 = rep(NA, 9),
                   boom_2011 = rep(NA, 9),
                   bust_2001 = rep(NA, 9),
                   bust_2011 = rep(NA, 9))


## First row - totals 
tab1$total_2001[1] <- total_year$count_total[which(total_year$year == 2001)]
tab1$total_2011[1] <- total_year$count_total[which(total_year$year == 2011)]

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



tab1$total_2001[7] <- round(((age19to44_year$count_age19to44[which(age19to44_year$year == 2001)]/tab1$total_2001[1]) * 100), 2)
tab1$total_2011[7] <- round(((age19to44_year$count_age19to44[which(age19to44_year$year == 2011)]/tab1$total_2011[1]) * 100), 2)
tab1$status_quo_2001[7] <- round(((age19to44_ogd$count_age19to44[which(age19to44_ogd$year == 2001 & age19to44_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2001[1]) * 100), 2)
tab1$status_quo_2011[7] <- round(((age19to44_ogd$count_age19to44[which(age19to44_ogd$year == 2011 & age19to44_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2011[1]) * 100), 2)
tab1$boom_2001[7] <- round(((age19to44_ogd$count_age19to44[which(age19to44_ogd$year == 2001 & age19to44_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2001[1]) * 100), 2)
tab1$boom_2011[7] <- round(((age19to44_ogd$count_age19to44[which(age19to44_ogd$year == 2011 & age19to44_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2011[1]) * 100), 2)
tab1$bust_2001[7] <- round(((age19to44_ogd$count_age19to44[which(age19to44_ogd$year == 2001 & age19to44_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2001[1]) * 100), 2)
tab1$bust_2011[7] <- round(((age19to44_ogd$count_age19to44[which(age19to44_ogd$year == 2011 & age19to44_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2011[1]) * 100), 2)



tab1$total_2001[8] <- round(((age45to64_year$count_age45to64[which(age45to64_year$year == 2001)]/tab1$total_2001[1]) * 100), 2)
tab1$total_2011[8] <- round(((age45to64_year$count_age45to64[which(age45to64_year$year == 2011)]/tab1$total_2011[1]) * 100), 2)
tab1$status_quo_2001[8] <- round(((age45to64_ogd$count_age45to64[which(age45to64_ogd$year == 2001 & age45to64_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2001[1]) * 100), 2)
tab1$status_quo_2011[8] <- round(((age45to64_ogd$count_age45to64[which(age45to64_ogd$year == 2011 & age45to64_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2011[1]) * 100), 2)
tab1$boom_2001[8] <- round(((age45to64_ogd$count_age45to64[which(age45to64_ogd$year == 2001 & age45to64_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2001[1]) * 100), 2)
tab1$boom_2011[8] <- round(((age45to64_ogd$count_age45to64[which(age45to64_ogd$year == 2011 & age45to64_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2011[1]) * 100), 2)
tab1$bust_2001[8] <- round(((age45to64_ogd$count_age45to64[which(age45to64_ogd$year == 2001 & age45to64_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2001[1]) * 100), 2)
tab1$bust_2011[8] <- round(((age45to64_ogd$count_age45to64[which(age45to64_ogd$year == 2011 & age45to64_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2011[1]) * 100), 2)


tab1$total_2001[9] <- round(((age65plus_year$count_age65plus[which(age65plus_year$year == 2001)]/tab1$total_2001[1]) * 100), 2)
tab1$total_2011[9] <- round(((age65plus_year$count_age65plus[which(age65plus_year$year == 2011)]/tab1$total_2011[1]) * 100), 2)
tab1$status_quo_2001[9] <- round(((age65plus_ogd$count_age65plus[which(age65plus_ogd$year == 2001 & age65plus_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2001[1]) * 100), 2)
tab1$status_quo_2011[9] <- round(((age65plus_ogd$count_age65plus[which(age65plus_ogd$year == 2011 & age65plus_ogd$ogd_change_group_cont25_2 == "Status Quo")]/tab1$status_quo_2011[1]) * 100), 2)
tab1$boom_2001[9] <- round(((age65plus_ogd$count_age65plus[which(age65plus_ogd$year == 2001 & age65plus_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2001[1]) * 100), 2)
tab1$boom_2011[9] <- round(((age65plus_ogd$count_age65plus[which(age65plus_ogd$year == 2011 & age65plus_ogd$ogd_change_group_cont25_2 == "Boom")]/tab1$boom_2011[1]) * 100), 2)
tab1$bust_2001[9] <- round(((age65plus_ogd$count_age65plus[which(age65plus_ogd$year == 2001 & age65plus_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2001[1]) * 100), 2)
tab1$bust_2011[9] <- round(((age65plus_ogd$count_age65plus[which(age65plus_ogd$year == 2011 & age65plus_ogd$ogd_change_group_cont25_2 == "Bust")]/tab1$bust_2011[1]) * 100), 2)


write.csv(tab1, "/n/dominici_nsaph_l3/Lab/projects/oil_gas_development_medicaid/Results/manuscriptResults/table1_demographics_OGD.csv", row.names = FALSE)



##### Set 14: Negative binomial models, no covariates. state/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through #####


plm_mh14 <- fixest::fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_adj14 <- fenegbin(adjustment_reaction_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_anx14 <- fenegbin(anxiety_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_att14 <- fenegbin(attention_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_mood14 <- fenegbin(mood_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_pers14 <- fenegbin(personality_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_schiz14 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_alc14 <- fenegbin(alcohol_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_sub14 <- fenegbin(substance_disorders_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)
plm_suic14 <- fenegbin(suicide_self_harm_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge)



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods14.txt")



print("Negative binomial models with state and fixed effects. No covariates")
print("Uses USDA boom/bust indicator")

print("All mental health hospitalizations")
summary(plm_mh14)

print("Adjustment disorder")
summary(plm_adj14)

print("Anxiety disorder")
summary(plm_anx14)

print("Attention disorder")
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





## Export IRR for these models
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods14_irr.txt")

print("Mental health hospitalizations")

plm_mh14_out <- cbind(Estimate = coef(plm_mh14), confint(plm_mh14)[-3,])
plm_mh14_irr <- exp(plm_mh14_out)
plm_mh14_irr

print("Adjustment disorder")
plm_adj14_out <- cbind(Estimate = coef(plm_adj14), confint(plm_adj14)[-3,])
plm_adj14_irr <- exp(plm_adj14_out)
plm_adj14_irr

print("Anxiety disorder")
plm_anx14_out <- cbind(Estimate = coef(plm_anx14), confint(plm_anx14)[-3,])
plm_anx14_irr <- exp(plm_anx14_out)
plm_anx14_irr


print("Attention disorder")
plm_att14_out <- cbind(Estimate = coef(plm_att14), confint(plm_att14)[-3,])
plm_att14_irr <- exp(plm_att14_out)
plm_att14_irr


print("Mood disorder")
plm_mood14_out <- cbind(Estimate = coef(plm_mood14), confint(plm_mood14)[-3,])
plm_mood14_irr <- exp(plm_mood14_out)
plm_mood14_irr

print("Personality disorder")
plm_pers14_out <- cbind(Estimate = coef(plm_pers14), confint(plm_pers14)[-3,])
plm_pers14_irr <- exp(plm_pers14_out)
plm_pers14_irr

print("Schizophrenia")
plm_schiz14_out <- cbind(Estimate = coef(plm_schiz14), confint(plm_schiz14)[-3,])
plm_schiz14_irr <- exp(plm_schiz14_out)
plm_schiz14_irr

print("Alcohol use disorder")
plm_alc14_out <- cbind(Estimate = coef(plm_alc14), confint(plm_alc14)[-3,])
plm_alc14_irr <- exp(plm_alc14_out)
plm_alc14_irr


print("Substance use disorder")
plm_sub14_out <- cbind(Estimate = coef(plm_sub14), confint(plm_sub14)[-3,])
plm_sub14_irr <- exp(plm_sub14_out)
plm_sub14_irr


print("Suicide/self-harm")
plm_suic14_out <- cbind(Estimate = coef(plm_suic14), confint(plm_suic14)[-3,])
plm_suic14_irr <- exp(plm_suic14_out)
plm_suic14_irr

closeAllConnections()



## N values 

mh_n <- sum(merge$mental_health_hospitalizations[-plm_mh14$obs_selection$obsRemoved * -1])
adj_n <- sum(merge$adjustment_reaction_hospitalizations[-plm_adj14$obs_selection$obsRemoved * -1])
att_n <- sum(merge$attention_disorders_hospitalizations[-plm_att14$obs_selection$obsRemoved * -1])
anx_n <- sum(merge$anxiety_disorders_hospitalizations[-plm_anx14$obs_selection$obsRemoved * -1])
mood_n <- sum(merge$mood_disorders_hospitalizations[-plm_mood14$obs_selection$obsRemoved * -1])
pers_n <- sum(merge$personality_disorders_hospitalizations[-plm_pers14$obs_selection$obsRemoved * -1])
schiz_n <- sum(merge$schizophrenia_psychotic_disorders_hospitalizations[-plm_schiz14$obs_selection$obsRemoved * -1])
sub_n <- sum(merge$substance_disorders_hospitalizations[-plm_sub14$obs_selection$obsRemoved * -1])
alc_n <- sum(merge$alcohol_disorders_hospitalizations[-plm_alc14$obs_selection$obsRemoved * -1])
suic_n <- sum(merge$suicide_self_harm_hospitalizations[-plm_suic14$obs_selection$obsRemoved * -1])

mh_cnty <- length(unique(merge$residence_county[-plm_mh14$obs_selection$obsRemoved * -1 * -1]))
adj_cnty <- length(unique(merge$residence_county[-plm_adj14$obs_selection$obsRemoved * -1]))
att_cnty <- length(unique(merge$residence_county[-plm_att14$obs_selection$obsRemoved * -1]))
anx_cnty <- length(unique(merge$residence_county[-plm_anx14$obs_selection$obsRemoved * -1]))
mood_cnty <- length(unique(merge$residence_county[-plm_mood14$obs_selection$obsRemoved * -1]))
pers_cnty <- length(unique(merge$residence_county[-plm_pers14$obs_selection$obsRemoved * -1]))
schiz_cnty <- length(unique(merge$residence_county[-plm_schiz14$obs_selection$obsRemoved * -1]))
sub_cnty <- length(unique(merge$residence_county[-plm_sub14$obs_selection$obsRemoved * -1]))
alc_cnty <- length(unique(merge$residence_county[-plm_alc14$obs_selection$obsRemoved * -1]))
suic_cnty <- length(unique(merge$residence_county[-plm_suic14$obs_selection$obsRemoved * -1]))




tabS2_n <- data.frame(conditions = c("All MH","adj", "attn", "anxiety","mood","pers","schiz","sub","alc","suic"),
                     hosp = c(mh_n, adj_n, att_n, anx_n, mood_n, pers_n, schiz_n, sub_n, alc_n, suic_n),
                     county = c(mh_cnty, adj_cnty, att_cnty, anx_cnty, mood_cnty, pers_cnty, schiz_cnty, sub_cnty, alc_cnty, suic_cnty))




##### Set 14: Negative binomial models, no covariates. state/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through. Stratified by demographics #####


plm_mh14_white <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge_white)
plm_mh14_black <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge_black)
plm_mh14_latine <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge_latine)
plm_mh14_male <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge_male)
plm_mh14_female <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge_female)
plm_mh14_19to44 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge_19to44)
plm_mh14_45to64 <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge_45to64)
plm_mh14_65plus <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge_65plus)



## Export coefficients
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods14_demStratified.txt")

print("Mental health hospitalizations: White beneficiaries")
summary(plm_mh14_white)

print("Mental health hospitalizations: Black beneficiaries")
summary(plm_mh14_black)

print("Mental health hospitalizations: Latine beneficiaries")
summary(plm_mh14_latine)

print("Mental health hospitalizations: Male beneficiaries")
summary(plm_mh14_male)

print("Mental health hospitalizations: Female beneficiaries")
summary(plm_mh14_female)

print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
summary(plm_mh14_19to44)

print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
summary(plm_mh14_45to64)

print("Mental health hospitalizations: Age 65 plus beneficiaries")
summary(plm_mh14_65plus)


closeAllConnections()




## Export IRR 
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods14_demStratified_irr.txt")

print("Mental health hospitalizations: White beneficiaries")
plm_mh14_white_out <- cbind(Estimate = coef(plm_mh14_white), confint(plm_mh14_white)[-3,])
plm_mh14_white_irr <- exp(plm_mh14_white_out)
plm_mh14_white_irr


print("Mental health hospitalizations: Black beneficiaries")
plm_mh14_black_out <- cbind(Estimate = coef(plm_mh14_black), confint(plm_mh14_black)[-3,])
plm_mh14_black_irr <- exp(plm_mh14_black_out)
plm_mh14_black_irr


print("Mental health hospitalizations: Latine beneficiaries")
plm_mh14_latine_out <- cbind(Estimate = coef(plm_mh14_latine), confint(plm_mh14_latine)[-3,])
plm_mh14_latine_irr <- exp(plm_mh14_latine_out)
plm_mh14_latine_irr


print("Mental health hospitalizations: Male beneficiaries")
plm_mh14_male_out <- cbind(Estimate = coef(plm_mh14_male), confint(plm_mh14_male)[-3,])
plm_mh14_male_irr <- exp(plm_mh14_male_out)
plm_mh14_male_irr


print("Mental health hospitalizations: Female beneficiaries")
plm_mh14_female_out <- cbind(Estimate = coef(plm_mh14_female), confint(plm_mh14_female)[-3,])
plm_mh14_female_irr <- exp(plm_mh14_female_out)
plm_mh14_female_irr


print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
plm_mh14_19to44_out <- cbind(Estimate = coef(plm_mh14_19to44), confint(plm_mh14_19to44)[-3,])
plm_mh14_19to44_irr <- exp(plm_mh14_19to44_out)
plm_mh14_19to44_irr


print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
plm_mh14_45to64_out <- cbind(Estimate = coef(plm_mh14_45to64), confint(plm_mh14_45to64)[-3,])
plm_mh14_45to64_irr <- exp(plm_mh14_45to64_out)
plm_mh14_45to64_irr

print("Mental health hospitalizations: Age 65 plus beneficiaries")
plm_mh14_65plus_out <- cbind(Estimate = coef(plm_mh14_65plus), confint(plm_mh14_65plus)[-3,])
plm_mh14_65plus_irr <- exp(plm_mh14_65plus_out)
plm_mh14_65plus_irr


closeAllConnections()



ntable(plm_mh14_female, merge_female)
ntable(plm_mh14_male, merge_male)
ntable(plm_mh14_white, merge_white)
ntable(plm_mh14_black, merge_black)
ntable(plm_mh14_latine, merge_latine)
ntable(plm_mh14_19to44, merge_19to44)
ntable(plm_mh14_45to64, merge_45to64)
ntable(plm_mh14_65plus, merge_65plus)




##### Set 14: Negative binomial models, no covariates. state/year FE. boom/bust threshold at 25%. Contemporaneous indicator w/carry through. Stratified by urban/rural #####

#0 = micro
#1 = noncore
#2 = metro

#Metro_Nonmetro_2014
#0 = nonmetro
#1 = metro

plm_mh14_nonmetro <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge[which(merge$Metro_Nonmetro_2013 == 0),])
plm_mh14_metro <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge[which(merge$Metro_Nonmetro_2013 == 1),])


plm_mh14_micro <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 0),])
plm_mh14_noncore <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 1),])
plm_mh14_metrocore <- fenegbin(mental_health_hospitalizations ~ boom_year25_2 + bust_year25_2 | state_fips_complete + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 2),])


sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods14_mh_metroNonmetro.txt")

print("Models stratified by metro status")
print("Negative binomial models, no covariates. Year/county FE")
print("Contemporaneous boom/bust w/25% threshold. DOES carry through")


print("##########################################")
print("All-nonmetro")
summary(plm_mh14_nonmetro)
print("All metro")
summary(plm_mh14_metro)


print("##########################################")
print("Micro only")
summary(plm_mh14_micro)
print("Metro noncore")
summary(plm_mh14_noncore)
print("Metro core")
summary(plm_mh14_metrocore)

closeAllConnections()



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods14_mh_metroNonmetro_irr.txt")


print("Rurality models - Micro")
plm_mh14_micro_out <- cbind(Estimate = coef(plm_mh14_micro), confint(plm_mh14_micro)[-3,])
plm_mh14_micro_irr <- exp(plm_mh14_micro_out)
plm_mh14_micro_irr

print("Rurality models - Noncore")
plm_mh14_noncore_out <- cbind(Estimate = coef(plm_mh14_noncore), confint(plm_mh14_noncore)[-3,])
plm_mh14_noncore_irr <- exp(plm_mh14_noncore_out)
plm_mh14_noncore_irr


print("Rurality models - Core")
plm_mh14_metrocore_out <- cbind(Estimate = coef(plm_mh14_metrocore), confint(plm_mh14_metrocore)[-3,])
plm_mh14_metrocore_irr <- exp(plm_mh14_metrocore_out)
plm_mh14_metrocore_irr


closeAllConnections()

## N values 
merge_micro <- merge[which(merge$Metro_Micro_Noncore_2013 == 0),]
merge_noncore <- merge[which(merge$Metro_Micro_Noncore_2013 == 1),]
merge_metro <- merge[which(merge$Metro_Micro_Noncore_2013 == 2),]

ntable(plm_mh14_micro, merge_micro)
ntable(plm_mh14_noncore, merge_noncore)
ntable(plm_mh14_metrocore, merge_metro)


##### Set 15: Negative binomial models, no covariates. State fixed effects. Boom/bust designated by USDA #####


plm_mh15 <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_adj15 <- fenegbin(adjustment_reaction_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_anx15 <- fenegbin(anxiety_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_att15 <- fenegbin(attention_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_mood15 <- fenegbin(mood_disorders_hospitalizations  ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_pers15 <- fenegbin(personality_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_schiz15 <- fenegbin(schizophrenia_psychotic_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_alc15 <- fenegbin(alcohol_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_sub15 <- fenegbin(substance_disorders_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)
plm_suic15 <- fenegbin(suicide_self_harm_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year,  data=merge)



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods15.txt")


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




## Export IRR for these models
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods15_irr.txt")

print("Mental health hospitalizations")

plm_mh15_out <- cbind(Estimate = coef(plm_mh15), confint(plm_mh15)[-3,])
plm_mh15_irr <- exp(plm_mh14_out)
plm_mh15_irr

print("Adjustment disorder")
plm_adj15_out <- cbind(Estimate = coef(plm_adj15), confint(plm_adj15)[-3,])
plm_adj15_irr <- exp(plm_adj15_out)
plm_adj15_irr

print("Anxiety disorder")
plm_anx15_out <- cbind(Estimate = coef(plm_anx15), confint(plm_anx15)[-3,])
plm_anx15_irr <- exp(plm_anx15_out)
plm_anx15_irr


print("Attention disorder")
plm_att15_out <- cbind(Estimate = coef(plm_att15), confint(plm_att15)[-3,])
plm_att15_irr <- exp(plm_att15_out)
plm_att15_irr


print("Mood disorder")
plm_mood15_out <- cbind(Estimate = coef(plm_mood15), confint(plm_mood14)[-3,])
plm_mood15_irr <- exp(plm_mood15_out)
plm_mood15_irr

print("Personality disorder")
plm_pers15_out <- cbind(Estimate = coef(plm_pers15), confint(plm_pers15)[-3,])
plm_pers15_irr <- exp(plm_pers15_out)
plm_pers15_irr

print("Schizophrenia")
plm_schiz15_out <- cbind(Estimate = coef(plm_schiz15), confint(plm_schiz15)[-3,])
plm_schiz15_irr <- exp(plm_schiz15_out)
plm_schiz15_irr

print("Alcohol use disorder")
plm_alc15_out <- cbind(Estimate = coef(plm_alc15), confint(plm_alc15)[-3,])
plm_alc15_irr <- exp(plm_alc15_out)
plm_alc15_irr


print("Substance use disorder")
plm_sub15_out <- cbind(Estimate = coef(plm_sub15), confint(plm_sub15)[-3,])
plm_sub15_irr <- exp(plm_sub15_out)
plm_sub15_irr


print("Suicide/self-harm")
plm_suic15_out <- cbind(Estimate = coef(plm_suic15), confint(plm_suic15)[-3,])
plm_suic15_irr <- exp(plm_suic15_out)
plm_suic15_irr

closeAllConnections()


## Export Ns

mh_n <- sum(merge$mental_health_hospitalizations[-plm_mh15$obs_selection$obsRemoved * -1])
adj_n <- sum(merge$adjustment_reaction_hospitalizations[-plm_adj15$obs_selection$obsRemoved * -1])
att_n <- sum(merge$attention_disorders_hospitalizations[-plm_att15$obs_selection$obsRemoved * -1])
anx_n <- sum(merge$anxiety_disorders_hospitalizations[-plm_anx15$obs_selection$obsRemoved * -1])
mood_n <- sum(merge$mood_disorders_hospitalizations[-plm_mood15$obs_selection$obsRemoved * -1])
pers_n <- sum(merge$personality_disorders_hospitalizations[-plm_pers15$obs_selection$obsRemoved * -1])
schiz_n <- sum(merge$schizophrenia_psychotic_disorders_hospitalizations[-plm_schiz15$obs_selection$obsRemoved * -1])
sub_n <- sum(merge$substance_disorders_hospitalizations[-plm_sub15$obs_selection$obsRemoved * -1])
alc_n <- sum(merge$alcohol_disorders_hospitalizations[-plm_alc15$obs_selection$obsRemoved * -1])
suic_n <- sum(merge$suicide_self_harm_hospitalizations[-plm_suic15$obs_selection$obsRemoved * -1])

mh_cnty <- length(unique(merge$residence_county[-plm_mh15$obs_selection$obsRemoved * -1]))
adj_cnty <- length(unique(merge$residence_county[-plm_adj15$obs_selection$obsRemoved * -1]))
att_cnty <- length(unique(merge$residence_county[-plm_att15$obs_selection$obsRemoved * -1]))
anx_cnty <- length(unique(merge$residence_county[-plm_anx15$obs_selection$obsRemoved * -1]))
mood_cnty <- length(unique(merge$residence_county[-plm_mood15$obs_selection$obsRemoved * -1]))
pers_cnty <- length(unique(merge$residence_county[-plm_pers15$obs_selection$obsRemoved * -1]))
schiz_cnty <- length(unique(merge$residence_county[-plm_schiz15$obs_selection$obsRemoved * -1]))
sub_cnty <- length(unique(merge$residence_county[-plm_sub15$obs_selection$obsRemoved * -1]))
alc_cnty <- length(unique(merge$residence_county[-plm_alc15$obs_selection$obsRemoved * -1]))
suic_cnty <- length(unique(merge$residence_county[-plm_suic15$obs_selection$obsRemoved * -1]))




tab4_n <- data.frame(conditions = c("All MH","adj", "attn", "anxiety","mood","pers","schiz","sub","alc","suic"),
                     hosp = c(mh_n, adj_n, att_n, anx_n, mood_n, pers_n, schiz_n, sub_n, alc_n, suic_n),
                     county = c(mh_cnty, adj_cnty, att_cnty, anx_cnty, mood_cnty, pers_cnty, schiz_cnty, sub_cnty, alc_cnty, suic_cnty))




##### Set 15: Negative binomial models, no covariates. state/year FE. boom/bust threshold desginated by USDA. Contemporaneous indicator w/carry through. Stratified by demographics #####

## Demographic files are created under model set 13

plm_mh15_white <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust  | state_fips_complete + year, data = merge_white)
plm_mh15_black <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust  | state_fips_complete + year, data = merge_black)
plm_mh15_latine <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust  | state_fips_complete + year, data = merge_latine)
plm_mh15_male <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year, data = merge_male)
plm_mh15_female <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust  | state_fips_complete + year, data = merge_female)
plm_mh15_19to44 <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust  | state_fips_complete + year, data = merge_19to44)
plm_mh15_45to64 <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust  | state_fips_complete + year, data = merge_45to64)
plm_mh15_65plus <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust  | state_fips_complete + year, data = merge_65plus)



## Export coefficients
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods15_demStratified.txt")

print("Mental health hospitalizations: White beneficiaries")
summary(plm_mh15_white)

print("Mental health hospitalizations: Black beneficiaries")
summary(plm_mh15_black)

print("Mental health hospitalizations: Latine beneficiaries")
summary(plm_mh15_latine)

print("Mental health hospitalizations: Male beneficiaries")
summary(plm_mh15_male)

print("Mental health hospitalizations: Female beneficiaries")
summary(plm_mh15_female)

print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
summary(plm_mh15_19to44)

print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
summary(plm_mh15_45to64)

print("Mental health hospitalizations: Age 65 plus beneficiaries")
summary(plm_mh15_65plus)


closeAllConnections()




## Export IRR 
sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods15_demStratified_irr.txt")

print("Mental health hospitalizations: White beneficiaries")
plm_mh15_white_out <- cbind(Estimate = coef(plm_mh15_white), confint(plm_mh15_white)[-3,])
plm_mh15_white_irr <- exp(plm_mh15_white_out)
plm_mh15_white_irr


print("Mental health hospitalizations: Black beneficiaries")
plm_mh15_black_out <- cbind(Estimate = coef(plm_mh15_black), confint(plm_mh15_black)[-3,])
plm_mh15_black_irr <- exp(plm_mh15_black_out)
plm_mh15_black_irr


print("Mental health hospitalizations: Latine beneficiaries")
plm_mh15_latine_out <- cbind(Estimate = coef(plm_mh15_latine), confint(plm_mh15_latine)[-3,])
plm_mh15_latine_irr <- exp(plm_mh15_latine_out)
plm_mh15_latine_irr


print("Mental health hospitalizations: Male beneficiaries")
plm_mh15_male_out <- cbind(Estimate = coef(plm_mh15_male), confint(plm_mh15_male)[-3,])
plm_mh15_male_irr <- exp(plm_mh15_male_out)
plm_mh15_male_irr


print("Mental health hospitalizations: Female beneficiaries")
plm_mh15_female_out <- cbind(Estimate = coef(plm_mh15_female), confint(plm_mh15_female)[-3,])
plm_mh15_female_irr <- exp(plm_mh15_female_out)
plm_mh15_female_irr


print("Mental health hospitalizations: Age 19 to 44 beneficiaries")
plm_mh15_19to44_out <- cbind(Estimate = coef(plm_mh15_19to44), confint(plm_mh15_19to44)[-3,])
plm_mh15_19to44_irr <- exp(plm_mh15_19to44_out)
plm_mh15_19to44_irr


print("Mental health hospitalizations: Age 45 to 64 beneficiaries")
plm_mh15_45to64_out <- cbind(Estimate = coef(plm_mh15_45to64), confint(plm_mh15_45to64)[-3,])
plm_mh15_45to64_irr <- exp(plm_mh15_45to64_out)
plm_mh15_45to64_irr

print("Mental health hospitalizations: Age 65 plus beneficiaries")
plm_mh15_65plus_out <- cbind(Estimate = coef(plm_mh15_65plus), confint(plm_mh15_65plus)[-3,])
plm_mh15_65plus_irr <- exp(plm_mh15_65plus_out)
plm_mh15_65plus_irr


closeAllConnections()


ntable(plm_mh15_female, merge_female)
ntable(plm_mh15_male, merge_male)
ntable(plm_mh15_white, merge_white)
ntable(plm_mh15_black, merge_black)
ntable(plm_mh15_latine, merge_latine)
ntable(plm_mh15_19to44, merge_19to44)
ntable(plm_mh15_45to64, merge_45to64)
ntable(plm_mh15_65plus, merge_65plus)



##### Set 15: Negative binomial models, no covariates. state/year FE. boom/bust threshold designated by USDA. Contemporaneous indicator w/carry through. Stratified by urban/rural #####

#0 = micro
#1 = noncore
#2 = metro

#Metro_Nonmetro_2015
#0 = nonmetro
#1 = metro

plm_mh15_nonmetro <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year, data = merge[which(merge$Metro_Nonmetro_2013 == 0),])
plm_mh15_metro <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year, data = merge[which(merge$Metro_Nonmetro_2013 == 1),])


plm_mh15_micro <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 0),])
plm_mh15_noncore <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 1),])
plm_mh15_metrocore <- fenegbin(mental_health_hospitalizations ~ USDA_boom + USDA_bust | state_fips_complete + year, data = merge[which(merge$Metro_Micro_Noncore_2013 == 2),])


sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods15_mh_metroNonmetro.txt")

print("Models stratified by metro status")
print("Negative binomial models, no covariates. Year/county FE")
print("Contemporaneous boom/bust w/25% threshold. DOES carry through")


print("##########################################")
print("All-nonmetro")
summary(plm_mh15_nonmetro)
print("All metro")
summary(plm_mh15_metro)


print("##########################################")
print("Micro only")
summary(plm_mh15_micro)
print("Metro noncore")
summary(plm_mh15_noncore)
print("Metro core")
summary(plm_mh15_metrocore)

closeAllConnections()



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods15_mh_metroNonmetro_irr.txt")


print("Rurality models - Micro")
plm_mh15_micro_out <- cbind(Estimate = coef(plm_mh15_micro), confint(plm_mh15_micro)[-3,])
plm_mh15_micro_irr <- exp(plm_mh15_micro_out)
plm_mh15_micro_irr

print("Rurality models - Noncore")
plm_mh15_noncore_out <- cbind(Estimate = coef(plm_mh15_noncore), confint(plm_mh15_noncore)[-3,])
plm_mh15_noncore_irr <- exp(plm_mh15_noncore_out)
plm_mh15_noncore_irr


print("Rurality models - Core")
plm_mh15_metrocore_out <- cbind(Estimate = coef(plm_mh15_metrocore), confint(plm_mh15_metrocore)[-3,])
plm_mh15_metrocore_irr <- exp(plm_mh15_metrocore_out)
plm_mh15_metrocore_irr


closeAllConnections()

## N values 
merge_micro <- merge[which(merge$Metro_Micro_Noncore_2013 == 0),]
merge_noncore <- merge[which(merge$Metro_Micro_Noncore_2013 == 1),]
merge_metro <- merge[which(merge$Metro_Micro_Noncore_2013 == 2),]

ntable(plm_mh15_micro, merge_micro)
ntable(plm_mh15_noncore, merge_noncore)
ntable(plm_mh15_metrocore, merge_metro)






##### Set 17: Event analysis using boom or bust as treatment, fenegbin() ######
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

## truncate at -8, 8
merge_prepost$time_to_treat_trunc <- merge_prepost$time_to_treat
merge_prepost$time_to_treat_trunc[which(merge_prepost$time_to_treat >= 5)] <- 5
merge_prepost$time_to_treat_trunc[which(merge_prepost$time_to_treat <= -5)] <- -5


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
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -5)] <- 2
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -4)] <- 3
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -3)] <- 4
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -2)] <- 5
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 0)] <- 6
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 1)] <- 7
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 2)] <- 8
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 3)] <- 9
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 4)] <- 10
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 5)] <- 11


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
merge_prepost$time_to_treat_trunc[which(merge_prepost$time_to_treat >= 5)] <- 5
merge_prepost$time_to_treat_trunc[which(merge_prepost$time_to_treat <= -5)] <- -5


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
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -5)] <- 2
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -4)] <- 3
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -3)] <- 4
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == -2)] <- 5
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 0)] <- 6
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 1)] <- 7
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 2)] <- 8
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 3)] <- 9
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 4)] <- 10
merge_prepost$time_to_treat_trunc2[which(merge_prepost$time_to_treat_trunc == 5)] <- 11

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
mh_boom_plotdat$Estimate_orig <- mh_boom_plotdat$Estimate
mh_boom_plotdat$Estimate <- exp(mh_boom_plotdat$Estimate)
mh_boom_plotdat$upper <- exp(mh_boom_plotdat$Estimate_orig + (1.96 * mh_boom_plotdat$`Std. Error`))
mh_boom_plotdat$lower <- exp(mh_boom_plotdat$Estimate_orig - (1.96 * mh_boom_plotdat$`Std. Error`))
mh_boom_plotdat$time <- c(seq(-10,-2, by = 1), seq(0,10, by = 1))

mh_bust_plotdat <- mod_event_mh_bust_factor$coeftable[c(22:41),]
mh_bust_plotdat$Estimate_orig <- mh_bust_plotdat$Estimate
mh_bust_plotdat$Estimate <- exp(mh_bust_plotdat$Estimate)
mh_bust_plotdat$upper <- exp(mh_bust_plotdat$Estimate_orig + (1.96 * mh_bust_plotdat$`Std. Error`))
mh_bust_plotdat$lower <- exp(mh_bust_plotdat$Estimate_orig - (1.96 * mh_bust_plotdat$`Std. Error`))
mh_bust_plotdat$time <- c(seq(-10,-2, by = 1), seq(0,10, by = 1))
mh_bust_plotdat <- rbind(mh_bust_plotdat, newrow)

newrow <- c(1,0,0,0,0,0, 0, -1)


mh_bust_plotdat <- rbind(mh_bust_plotdat, newrow)
mh_boom_plotdat <- rbind(mh_boom_plotdat, newrow)


png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_boom_newReference.png", width=8, height=6, units="in", res=300)
ggplot(mh_boom_plotdat, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  xlab("Time to treat") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = -1, color = "red", cex = 0.2, lty = "dashed") +
  theme_minimal()
dev.off()

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_bust_newReference.png", width=8, height=6, units="in", res=300)
ggplot(mh_bust_plotdat, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  xlab("Time to treat") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = -1, color = "red", cex = 0.2, lty = "dashed") +
  theme_minimal()
dev.off()


### New factor plots - truncated

mh_boom_plotdat_trunc <- mod_event_mh_boom_factor_trunc$coeftable[c(12:21),]
mh_boom_plotdat_trunc$Estimate_orig <- mh_boom_plotdat_trunc$Estimate
mh_boom_plotdat_trunc$Estimate <- exp(mh_boom_plotdat_trunc$Estimate)
mh_boom_plotdat_trunc$upper <- exp(mh_boom_plotdat_trunc$Estimate_orig + (1.96 * mh_boom_plotdat_trunc$`Std. Error`))
mh_boom_plotdat_trunc$lower <- exp(mh_boom_plotdat_trunc$Estimate_orig - (1.96 * mh_boom_plotdat_trunc$`Std. Error`))
mh_boom_plotdat_trunc$time <- c(seq(-5,-2, by = 1), seq(0,5, by = 1))


mh_bust_plotdat_trunc <- mod_event_mh_bust_factor_trunc$coeftable[c(12:21),]
mh_bust_plotdat_trunc$Estimate_orig <- mh_bust_plotdat_trunc$Estimate
mh_bust_plotdat_trunc$Estimate <- exp(mh_bust_plotdat_trunc$Estimate)
mh_bust_plotdat_trunc$upper <- exp(mh_bust_plotdat_trunc$Estimate_orig + (1.96 * mh_bust_plotdat_trunc$`Std. Error`))
mh_bust_plotdat_trunc$lower <- exp(mh_bust_plotdat_trunc$Estimate_orig - (1.96 * mh_bust_plotdat_trunc$`Std. Error`))
mh_bust_plotdat_trunc$time <- c(seq(-5,-2, by = 1), seq(0,5, by = 1))

newrow <- c(1,0,0,0,0,0, 0, -1)


mh_bust_plotdat_trunc <- rbind(mh_bust_plotdat_trunc, newrow)
mh_boom_plotdat_trunc <- rbind(mh_boom_plotdat_trunc, newrow)

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_boom_newReference_trunc.png", width=8, height=6, units="in", res=300)
ggplot(mh_boom_plotdat_trunc, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  xlab("Time to treat (truncated)") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Boom years\n Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = -1, color = "red", cex = 0.2, lty = "dashed") +
  geom_hline(yintercept = 1, color = "blue", cex = 0.2, lty = "dashed") +
  theme_minimal()
dev.off()

png(file="oil_gas_development_medicaid/Figures/fenegbin_mh_effect_plots_bust_newReference_trunc.png", width=8, height=6, units="in", res=300)
ggplot(mh_bust_plotdat_trunc, aes(time, Estimate)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  xlab("Time to treat (truncated)") +
  ylab("Estmate and 95% Conf. Int.") +
  ggtitle("Bust years \n Effect on mental health hospitalizations \n Values represent difference in effect from ref. year = -1.") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = -1, color = "red", cex = 0.2, lty = "dashed") +
  geom_hline(yintercept = 1, color = "blue", cex = 0.2, lty = "dashed") +
  theme_minimal()
dev.off()



sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods17_mh.txt")


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


sink("oil_gas_development_medicaid/Results/manuscriptResults/plm_mods17_mh_newReference.txt")


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




