########################################################################################
# PROJECT       : CCMH
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
  file$residence_county <- as.character(file$residence_county)
  
  total <- file %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("female" = "all_cause_hospitalizations")
  
  
  female <- file[which(file$sex == "F"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("female" = "all_cause_hospitalizations")
  male <- file[which(file$sex == "M"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("male" = "all_cause_hospitalizations")
  
  
  white <- file[which(file$race == 1),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("white" = "all_cause_hospitalizations")
  black <- file[which(file$race == 2),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("black" = "all_cause_hospitalizations")
  hisp <- file[which(file$race == 5),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("hisp" = "all_cause_hospitalizations")
  hisp_oom <- file[which(file$race == 7),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("hisp_oom" = "all_cause_hospitalizations")
  
  age19_24 <- file[which(file$age_group == "19-24"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("age19_24" = "all_cause_hospitalizations")
  age25_34 <- file[which(file$age_group == "25-34"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("age25_34" = "all_cause_hospitalizations")
  age35_44 <- file[which(file$age_group == "35-44"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("age35_44" = "all_cause_hospitalizations")
  age45_54 <- file[which(file$age_group == "45-54"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("age45_54" = "all_cause_hospitalizations")
  age55_64 <- file[which(file$age_group == "55-64"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("age55_64" = "all_cause_hospitalizations")
  age65_74 <- file[which(file$age_group == "65-74"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("age65_74" = "all_cause_hospitalizations")
  age75_84 <- file[which(file$age_group == "75-84"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("age75_84" = "all_cause_hospitalizations")
  age85_plus <- file[which(file$age_group == "85+"),] %>% dplyr::group_by(residence_county) %>% summarize(all_cause_hospitalizations = sum(all_cause_hospitalizations, na.rm = TRUE)) %>% rename("age85_plus" = "all_cause_hospitalizations")
  
  file_new <- merge(female, male, by = "residence_county", all = TRUE)
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
merge <- merge(merge, dem_files_new_df, by = c("residence_county", "year"), all = TRUE)

## Remove states: Oregon (41), Washington (53), Idaho (16), Illinois (17). Also, states where there has been ZERO oil OR gas production during this time.
## We will create this list manually, because there are some we need to exclude due to policy tracking issues
state_zero <- dat %>% group_by(Stabr) %>% summarize(oilsum = sum(oil_units, na.rm =TRUE),
                                                    gassum = sum(gas_units, na.rm =TRUE))

states_to_remove <- c("CT","DC","DE","GA","IA","ID","IL","MA","ME","MN","NC","NH","NJ","OR","RI","SC","VT","WA","WI")
merge <- merge[-which(merge$Stabr %in% states_to_remove),]


## Remove states with zero oil OR gas production:
counties_zero <- dat %>% group_by(residence_county) %>% summarize(oilsum = sum(oil_units, na.rm =TRUE),
                                                    gassum = sum(gas_units, na.rm =TRUE))



counties_zero <- counties_zero$residence_county[which(counties_zero$oilsum == 0 & counties_zero$gassum == 0)]
merge <- merge[which(merge$residence_county %!in% counties_zero),] ## This leaves 1237 counties



### Truncate outcomes for sensitivity analysis

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


## Create categorical indicators 

merge$ogd_change_group_cont2_char <- merge$ogd_change_group_cont2
merge$ogd_change_group_cont2_char[which(is.na(merge$ogd_change_group_cont2))] <- "Status quo"
merge$ogd_change_group_cont2_char[which(is.na(merge$ogd_change_group_cont))] <- NA

merge$ogd_change_group_cont2_char <- factor(merge$ogd_change_group_cont2_char, levels = c("Status quo","Boom","Bust"))


merge$oil_gas_change_group_char <- merge$oil_gas_change_group
merge$oil_gas_change_group_char <- factor(merge$oil_gas_change_group_char, levels = c("Status Quo","H_Growth","H_Decline"))


### Demographics of oil boom counties

#run locally: //ad.bu.edu/bumcfiles/SPH/DCC/Dept/CCHM/09Programming
#we'll add this in later 
#demogs <- readxl::read_excel("oil_gas_development_medicaid/Data/usrace19agesadj_analytic.xlsx")


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

sink("oil_gas_development_medicaid/Results/newAnalyticPlan/MH_mean_variance_median_sd.txt")

print("###### Means ######")
means

print("###### Variances ######")
vars

print("###### Medians ######")
medians

print("###### Std. dev. ######")
sds
closeAllConnections()






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

print("Attachment disorder")
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




##### Intermision: Take a step back and compare model fit, explore distributions #####


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


sums_contemp <- hospMean <- merge[,c("ogd_change_group_cont2_char",
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
                                      "suicide_self_harm_hospitalizations")] %>% group_by(ogd_change_group_cont2_char, year) %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))



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




means_contemp <- hospMean <- merge[,c("ogd_change_group_cont2_char",
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
                                      "suicide_self_harm_hospitalizations")] %>% group_by(ogd_change_group_cont2_char, year) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))



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

means_contemp_melt <- reshape2::melt(means_contemp, id.vars = c("ogd_change_group_cont2_char","year"))
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
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 45))
dev.off()


# Contemporaneous - sums

sums_contemp_melt <- reshape2::melt(sums_contemp, id.vars = c("ogd_change_group_cont2_char","year"))
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



sink("oil_gas_development_medicaid/Results/newAnalyticPlan/plm_mods12.txt")

print("Negative binomial models with year fixed effects. State covariate")
print("Uses contemporaneous boom/bust indicator")
print("Year fixed effects ONLY")


print("All mental health hospitalizations")
summary(plm_mh12)

print("Adjustment disorder")
summary(plm_adj12)

print("Anxiety disorder")
summary(plm_anx12)

print("Attachment disorder")
summary(plm_att12)

print("Mood disorder")
summary(plm_mood12)

print("Personality disorder")
summary(plm_pers12)

print("Schizophrenia")
summary(plm_schiz12)

print("Alcohol use disorder")
summary(plm_alc12)

print("Substance use disorder")
summary(plm_sub12)

print("Suicide/self-harm")
summary(plm_suic12)

closeAllConnections()


