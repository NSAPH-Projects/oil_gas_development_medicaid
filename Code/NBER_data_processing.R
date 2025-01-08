########################################################################################
# PROJECT       :	VA LEAPS
# SPONSOR/PI    : Amruta Nori-Sarma
# PROGRAM NAME  : 
# DESCRIPTION   : Subset historical demographic data from NBER
#                 
#                 
# PROGRAMMER    : Nina Cesare
# DATE WRITTEN  : 07/11/2024
########################################################################################
# INPUT FILES   : 	
# OUTPUT FILES  : 	
#######################################################################################
# MODIFICATIONS : 
#
# DATE          :
# PROGRAMMER    : 
# DESCRIPTION   : 	
#######################################################################################

library(data.table)
library(dplyr)
library(tidyr)

## Data downloaded from: https://www.nber.org/research/data/survey-epidemiology-and-end-results-seer-us-state-and-county-population-data-age-race-sex-hispanic
## Dictionary located here: https://seer.cancer.gov/popdata/popdic.html

rawDir <- "oil_gas_development_medicaid/Data/Raw/"
analyticDir <- "oil_gas_development_medicaid/Data/"

test <- fread(paste0(rawDir, "usrace19agesadj.csv"))

years <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012")
test_sub <- test[which(test$year %in% years),]

### 

pop_year_county <- test_sub %>% group_by(year, county) %>% summarise(pop = sum(pop))

pop_year_county_sex <- test_sub %>% group_by(year, county, sex) %>% summarise(pop = sum(pop))
pop_year_county_sex2 <- pop_year_county_sex %>% 
  pivot_wider(names_from = sex, values_from = pop) %>% 
  rename(Male = `1`, 
         Female = `2`)

pop_year_county_hisp <- test_sub %>% group_by(year, county, hispanic) %>% summarise(pop = sum(pop))
pop_year_county_hisp2 <- pop_year_county_hisp %>% 
  pivot_wider(names_from = hispanic, values_from = pop) %>% 
  rename(NonHispanic = `0`, 
         Hispanic = `1`)

pop_year_county_race <- test_sub %>% group_by(year, county, race) %>% summarise(pop = sum(pop))
pop_year_county_race2 <- pop_year_county_race %>% 
  pivot_wider(names_from = race, values_from = pop) %>% 
  rename(White = `1`, 
         Black = `2`,
         AIAN = `3`,
         AAPI = `4`)


pop_year_county_age <- test_sub %>% group_by(year, county, age) %>% summarise(pop = sum(pop))
pop_year_county_age2 <- pop_year_county_age %>% 
  pivot_wider(names_from = age, values_from = pop) %>% 
  rename(age_0 = `0`, 
         age_1_4 = `1`,
         age_5_9 = `2`,
         age_10_14 = `3`,
         age_15_19 = `4`,
         age_20_24 = `5`,
         age_25_29 = `6`,
         age_30_34 = `7`,
         age_35_39 = `8`,
         age_40_44 = `9`,
         age_45_49 = `10`,
         age_50_54 = `11`,
         age_55_59 = `12`,
         age_60_64 = `13`,
         age_65_69 = `14`,
         age_70_74 = `15`,
         age_75_79 = `16`,
         age_80_84 = `17`,
         age_85_up = `18`)



merge <- merge(pop_year_county, pop_year_county_sex2, by = c("year","county"), all =TRUE)
merge <- merge(merge, pop_year_county_hisp2, by = c("year","county"), all =TRUE)
merge <- merge(merge, pop_year_county_race2, by = c("year","county"), all =TRUE)
merge <- merge(merge, pop_year_county_age2, by = c("year","county"), all =TRUE)

merge$pct_female <- (merge$Female/merge$pop) * 100
merge$pct_white <- (merge$White/merge$pop) * 100
merge$pct_black <- (merge$Black/merge$pop) * 100
merge$pct_AIAN <- (merge$AIAN/merge$pop) * 100
merge$pct_AAPI <- (merge$AAPI/merge$pop) * 100
merge$pct_hisp <- (merge$Hispanic/merge$pop) * 100



openxlsx::write.xlsx(merge, paste0(analyticDir, "usrace19agesadj_analytic.xlsx"), rowNames = FALSE)
