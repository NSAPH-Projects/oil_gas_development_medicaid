########################################################################################
# PROJECT       :	CCMH
# SPONSOR/PI    : Amrutal Nori-Sarma/Mary Willis
# PROGRAM NAME  : 
# DESCRIPTION   : Generating USDA (continuous) and custom (contemporaneous) boom/bust maps for OGD development manuscript (target: Nature energy)
#                 
#                 
# PROGRAMMER    : Nina Cesare
# DATE WRITTEN  : 08/18/2024
########################################################################################
# INPUT FILES   : 
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

library(ggplot2)
library(urbnmapr)
library(dplyr)
library(readxl)
library(sf)
library(haven)


'%!in%' <- function(x,y)!('%in%'(x,y))


localDat <- ""

## Load and process analytic data
dat <- read.csv(paste0(localDat, "oilgascounty_1_long_an.csv"))
dat$FIPS <- unlist(lapply(dat$FIPS, function(x) county_fips_fix(as.character(x))))

# Ensure Shannon County (46113) is categorized as Oglala County (46102)
dat$FIPS[which(dat$FIPS == "46113")] <- "46102"


####### Correlation across full dataset between our measure and the USDA measure #######

dat$ogd_change_group_cont25_2_num <- dat$ogd_change_group_cont25_2
dat$ogd_change_group_cont25_2_num[which(dat$ogd_change_group_cont25_2 == "Boom")] <- 1
dat$ogd_change_group_cont25_2_num[which(dat$ogd_change_group_cont25_2 == "Bust")] <- 2
dat$ogd_change_group_cont25_2_num[which(dat$ogd_change_group_cont25_2 == "Status Quo")] <- 3
dat$ogd_change_group_cont25_2_num <- as.numeric(dat$ogd_change_group_cont25_2_num)

dat$oil_gas_change_group_num <- dat$oil_gas_change_group
dat$oil_gas_change_group_num[which(dat$oil_gas_change_group == "H_Growth")] <- 1
dat$oil_gas_change_group_num[which(dat$oil_gas_change_group == "H_Decline")] <- 2
dat$oil_gas_change_group_num[which(dat$oil_gas_change_group == "Status Quo")] <- 3
dat$oil_gas_change_group_num <- as.numeric(dat$oil_gas_change_group_num)

cor.test(dat$ogd_change_group_cont25_2_num, dat$oil_gas_change_group_num, method = "spearman")


states_to_remove <- c("CT","DC","DE","GA","IA","ID","IL","MA","ME","MN","NC","NH","NJ","OR","RI","SC","VT","WA","WI")
dat_sub <- dat[-which(dat$Stabr %in% states_to_remove),]


## Remove states with zero oil OR gas production:
counties_zero <- dat %>% group_by(FIPS) %>% summarize(oilsum = sum(oil_units, na.rm =TRUE),
                                                      gassum = sum(gas_units, na.rm =TRUE))


counties_zero <- counties_zero$FIPS[which(counties_zero$oilsum == 0 & counties_zero$gassum == 0)]
dat_sub <- dat_sub[which(dat_sub$FIPS %!in% counties_zero),]  ## 1119 counties



########### USDA Maps #######

## USDA - subsample
dat_sub_counties <- unique(dat_sub[,c("FIPS","oil_gas_change_group")])
names(dat_sub_counties)[1] <- "county_fips"


## USDA - full sample
dat_counties <- unique(dat[,c("FIPS","oil_gas_change_group")])
names(dat_counties)[1] <- "county_fips"

## Prepare the shapefile 

counties <- urbnmapr::counties


## Create map data 
dat_counties_map <- left_join(counties, dat_counties, by = "county_fips")
dat_counties_map$oil_gas_change_group[which(is.na(dat_counties_map$oil_gas_change_group))] <- "Excluded"


dat_sub_counties_map <- left_join(counties, dat_sub_counties, by = "county_fips")
dat_sub_counties_map$oil_gas_change_group[which(is.na(dat_sub_counties_map$oil_gas_change_group))] <- "Excluded"



dat_counties_map$oil_gas_change_group2 <- as.character(dat_counties_map$oil_gas_change_group)
dat_counties_map$oil_gas_change_group2[which(dat_counties_map$oil_gas_change_group == "H_Decline")] <- "Decline"
dat_counties_map$oil_gas_change_group2[which(dat_counties_map$oil_gas_change_group == "H_Growth")] <- "Growth"


dat_sub_counties_map$oil_gas_change_group2 <- as.character(dat_sub_counties_map$oil_gas_change_group)
dat_sub_counties_map$oil_gas_change_group2[which(dat_sub_counties_map$oil_gas_change_group == "H_Decline")] <- "Decline"
dat_sub_counties_map$oil_gas_change_group2[which(dat_sub_counties_map$oil_gas_change_group == "H_Growth")] <- "Growth"


dat_counties_map$oil_gas_change_group2 <- factor(dat_counties_map$oil_gas_change_group2, levels = c("Growth","Decline","Status Quo","Excluded"))
dat_sub_counties_map$oil_gas_change_group2 <- factor(dat_sub_counties_map$oil_gas_change_group2, levels = c("Growth","Decline","Status Quo","Excluded"))


## USDA - subsample


dat_sub_counties_map[which(dat_sub_counties_map$state_abbv != "AK" & dat_sub_counties_map$state_abbv != "HI"),] %>%
  ggplot(aes(long, lat, group = group, fill = oil_gas_change_group2))+
  geom_polygon(color = "#636363") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "USDA OGD \n Change Group") + 
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3","#D3D3D3")) + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

## USDA - all
dat_counties_map[which(dat_counties_map$state_abbv != "AK" & dat_counties_map$state_abbv != "HI"),] %>%
  ggplot(aes(long, lat, group = group, fill = oil_gas_change_group2))+
  geom_polygon(color = "#636363") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "USDA OGD \n Change Group") + 
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3","#D3D3D3"))+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 





######## Contemporaneous maps (our indicators) #######

## ours - subsample
dat_sub_counties2 <- dat_sub[,c("FIPS","year","ogd_change_group_cont25_2")]
names(dat_sub_counties2)[1] <- "county_fips"


## ours - full sample
dat_counties2 <- dat[,c("FIPS","year","ogd_change_group_cont25_2")]
names(dat_counties2)[1] <- "county_fips"

## Prepare the shapefile 

counties <- urbnmapr::counties


## Create map data 
dat_counties_map_2001 <- left_join(counties, dat_counties2[which(dat_counties2$year == "2001"),], by = "county_fips")
dat_counties_map_2006 <- left_join(counties, dat_counties2[which(dat_counties2$year == "2006"),], by = "county_fips")
dat_counties_map_2011 <- left_join(counties, dat_counties2[which(dat_counties2$year == "2011"),], by = "county_fips")


dat_counties_map_2001$ogd_change_group_cont25_2[which(is.na(dat_counties_map_2001$ogd_change_group_cont25_2))] <- "Excluded"
dat_counties_map_2006$ogd_change_group_cont25_2[which(is.na(dat_counties_map_2006$ogd_change_group_cont25_2))] <- "Excluded"
dat_counties_map_2011$ogd_change_group_cont25_2[which(is.na(dat_counties_map_2011$ogd_change_group_cont25_2))] <- "Excluded"


dat_sub_counties_map_2001 <- left_join(counties, dat_sub_counties2[which(dat_sub_counties2$year == "2001"),], by = "county_fips")
dat_sub_counties_map_2006 <- left_join(counties, dat_sub_counties2[which(dat_sub_counties2$year == "2006"),], by = "county_fips")
dat_sub_counties_map_2011 <- left_join(counties, dat_sub_counties2[which(dat_sub_counties2$year == "2011"),], by = "county_fips")


dat_sub_counties_map_2001$ogd_change_group_cont25_2[which(is.na(dat_sub_counties_map_2001$ogd_change_group_cont25_2))] <- "Excluded"
dat_sub_counties_map_2006$ogd_change_group_cont25_2[which(is.na(dat_sub_counties_map_2006$ogd_change_group_cont25_2))] <- "Excluded"
dat_sub_counties_map_2011$ogd_change_group_cont25_2[which(is.na(dat_sub_counties_map_2011$ogd_change_group_cont25_2))] <- "Excluded"



dat_counties_map_2001$ogd_change_group_cont25_2 <- factor(dat_counties_map_2001$ogd_change_group_cont25_2, levels = c("Boom","Bust","Status Quo","Excluded"))
dat_counties_map_2006$ogd_change_group_cont25_2 <- factor(dat_counties_map_2006$ogd_change_group_cont25_2, levels = c("Boom","Bust","Status Quo","Excluded"))
dat_counties_map_2011$ogd_change_group_cont25_2 <- factor(dat_counties_map_2011$ogd_change_group_cont25_2, levels = c("Boom","Bust","Status Quo","Excluded"))


dat_sub_counties_map_2001$ogd_change_group_cont25_2 <- factor(dat_sub_counties_map_2001$ogd_change_group_cont25_2, levels = c("Boom","Bust","Status Quo","Excluded"))
dat_sub_counties_map_2006$ogd_change_group_cont25_2 <- factor(dat_sub_counties_map_2006$ogd_change_group_cont25_2, levels = c("Boom","Bust","Status Quo","Excluded"))
dat_sub_counties_map_2011$ogd_change_group_cont25_2 <- factor(dat_sub_counties_map_2011$ogd_change_group_cont25_2, levels = c("Boom","Bust","Status Quo","Excluded"))




## custom - subsample

dat_sub_counties_map_2001[which(dat_sub_counties_map_2001$state_abbv != "AK" & dat_sub_counties_map_2001$state_abbv != "HI"),] %>%
  ggplot(aes(long, lat, group = group, fill = ogd_change_group_cont25_2))+
  geom_polygon(color = "#636363") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "OGD Change Group: \n 2001") + 
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3","#D3D3D3")) + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

dat_sub_counties_map_2006[which(dat_sub_counties_map_2006$state_abbv != "AK" & dat_sub_counties_map_2006$state_abbv != "HI"),] %>%
  ggplot(aes(long, lat, group = group, fill = ogd_change_group_cont25_2))+
  geom_polygon(color = "#636363") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "OGD Change Group: \n 2006") + 
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3","#D3D3D3")) + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 



dat_sub_counties_map_2011[which(dat_sub_counties_map_2011$state_abbv != "AK" & dat_sub_counties_map_2011$state_abbv != "HI"),] %>%
  ggplot(aes(long, lat, group = group, fill = ogd_change_group_cont25_2))+
  geom_polygon(color = "#636363") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "OGD Change Group: \n 2011") + 
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3","#D3D3D3")) + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 



## custom - subsample

dat_counties_map_2001[which(dat_counties_map_2001$state_abbv != "AK" & dat_counties_map_2001$state_abbv != "HI"),] %>%
  ggplot(aes(long, lat, group = group, fill = ogd_change_group_cont25_2))+
  geom_polygon(color = "#636363") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "OGD Change Group: \n 2001") + 
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3","#D3D3D3")) + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 



dat_counties_map_2006[which(dat_counties_map_2006$state_abbv != "HI" & dat_counties_map_2006$state_abbv != "AK"),] %>%
  ggplot(aes(long, lat, group = group, fill = ogd_change_group_cont25_2))+
  geom_polygon(color = "#636363") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "OGD Change Group: \n 2006") + 
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3","#D3D3D3")) + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 



dat_counties_map_2011[which(dat_counties_map_2011$state_abbv != "HI" & dat_counties_map_2011$state_abbv != "AK"),] %>%
  ggplot(aes(long, lat, group = group, fill = ogd_change_group_cont25_2))+
  geom_polygon(color = "#636363") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "OGD Change Group: \n 2011") + 
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3","#D3D3D3")) + 
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 


###### Tables ####

# SVI/education
svi2000 <- read.csv(paste0(localDat, "SVI_2000_US_county.csv"))
svi2010 <- read.csv(paste0(localDat, "SVI_2010_US_county.csv"))

svi2000$STCOFIPS <- unlist(lapply(svi2000$STCOFIPS, function(x) county_fips_fix(x)))
svi2010$STCOFIPS <- unlist(lapply(svi2010$FIPS, function(x) county_fips_fix(x)))

# income
saipe2001 <- readr::read_fwf(paste0(localDat, "est01all.dat"))
saipe2011 <- readxl::read_excel(paste0(localDat, "est11all.xls"), skip =2)

saipe2001$X2 <- unlist(lapply(saipe2001$X2, function(x) county_partial_fips(x)))
saipe2001$STCOFIPS <- paste0(saipe2001$X1, saipe2001$X2)

saipe2011 <- saipe2011[which(!is.na(saipe2011$`County FIPS`)),]
saipe2011$`County FIPS` <- unlist(lapply(saipe2011$`County FIPS`, function(x) county_partial_fips(x)))
saipe2011$STCOFIPS <- paste0(saipe2011$`State FIPS`, saipe2011$`County FIPS`)

# Geo
geo2000 <- read_sf(paste0(localDat, "tl_2010_us_county00.shp"))
geo2010 <- read_sf(paste0(localDat, "tl_2010_us_county10.shp"))

geo2000 <- geo2000 %>% rename(STCOFIPS = CNTYIDFP00)
geo2010 <- geo2010 %>% rename(STCOFIPS = GEOID10)


pop <- read_excel(paste0(localDat, "usrace19agesadj_analytic.xlsx"))

pop$STCOFIPS <- unlist(lapply(pop$county, function(x) county_fips_fix(x)))
pop2000 <- pop[which(pop$year == "2000"),]
pop2010 <- pop[which(pop$year == "2010"),]


popgeo2000 <- merge(geo2000[,c("STCOFIPS","ALAND00")], pop2000[,c("STCOFIPS","pop")], by = "STCOFIPS", all = TRUE)
popgeo2010 <- merge(geo2010[,c("STCOFIPS","ALAND10")], pop2010[,c("STCOFIPS","pop")], by = "STCOFIPS", all = TRUE)


## migration 
in2000_1 <- read_excel(paste0(localDat, "inflowpart1.xls"), skip = 1)
in2000_2 <- read_excel(paste0(localDat, "inflowpart2.xls"), skip = 1)
in2000_3 <- read_excel(paste0(localDat, "inflowpart3.xls"), skip = 1)
in2000_4 <- read_excel(paste0(localDat, "inflowpart4.xls"), skip = 1)
in2000_5 <- read_excel(paste0(localDat, "inflowpart5.xls"), skip = 1)


in2000 <- rbind(in2000_1, in2000_2, in2000_3, in2000_4, in2000_5)
in2000$STCOFIPS <- paste0(in2000$`FIPS State in 2000`, in2000$`FIPS County in 2000`)
in2000$inflow <- as.numeric(as.character(in2000$`In Flow`))

in2000_agg <- in2000 %>% group_by(STCOFIPS) %>% summarize(inflow = sum(inflow, na.rm = TRUE))

in2000_agg <- merge(in2000_agg, popgeo2000[,c("STCOFIPS","pop")], by = "STCOFIPS", all = TRUE)
in2000_agg$inflow[which(is.na(in2000_agg$inflow))] <- 0

in2000_agg$inflow_pct <- in2000_agg$inflow/in2000_agg$pop * 100

in2010 <- NULL

for(i in 1:51){
  state <- read_excel(paste0(localDat, "county-to-county-2006-2010-previous-residence-sort.xls"), skip = 1, sheet = i)
  in2010 <- rbind(in2010, state)
  print(i)
}

in2010$STCOFIPS <- paste0(in2010$`Current Residence FIPS State Code`, in2010$`Current Residence FIPS County Code`)
in2010$STCOFIPS <- gsub("^0", "", in2010$STCOFIPS)


in2010$inflow <- as.numeric(as.numeric(in2010$`Movers in County-to-County Flow`))

in2010_agg <- in2010 %>% group_by(STCOFIPS) %>% summarize(inflow = sum(inflow, na.rm = TRUE))

in2010_agg <- merge(in2010_agg, popgeo2010[,c("STCOFIPS","pop")], by = "STCOFIPS", all = TRUE)
in2010_agg$inflow[which(is.na(in2010_agg$inflow))] <- 0
in2010_agg$inflow_pct <- in2010_agg$inflow/in2010_agg$pop * 100


#### Make sure original dataset is ok ######

dat_sub$STCOFIPS <- unlist(lapply(dat_sub$geoid, function(x) county_fips_fix(x)))



#Population density (people/km2) 
popgeo2000 <- merge(popgeo2000, dat_sub[which(dat_sub$year == "2001"),], by = "STCOFIPS", all = TRUE) 
popgeo2010 <- merge(popgeo2010, dat_sub[which(dat_sub$year == "2010"),], by = "STCOFIPS", all = TRUE) 

popgeo2000$ALAND002 <- popgeo2000$ALAND00/1000000
popgeo2010$ALAND102 <- popgeo2010$ALAND10/1000000

popgeo2000$pop_density <- popgeo2000$pop/popgeo2000$ALAND002
popgeo2010$pop_density <- popgeo2010$pop/popgeo2010$ALAND102

c_1_1 <- mean(popgeo2000$pop_density, na.rm = TRUE)
c_1_2 <- mean(popgeo2010$pop_density, na.rm = TRUE)

c_1_3 <- mean(popgeo2010$pop_density[which(popgeo2000$ogd_change_group_cont25_2 == "Status Quo")], na.rm = TRUE)
c_1_4 <- mean(popgeo2010$pop_density[which(popgeo2010$ogd_change_group_cont25_2 == "Status Quo")], na.rm = TRUE)

c_1_5 <- mean(popgeo2010$pop_density[which(popgeo2000$ogd_change_group_cont25_2 == "Boom")], na.rm = TRUE)
c_1_6 <- mean(popgeo2010$pop_density[which(popgeo2010$ogd_change_group_cont25_2 == "Boom")], na.rm = TRUE)

c_1_7 <- mean(popgeo2010$pop_density[which(popgeo2000$ogd_change_group_cont25_2 == "Bust")], na.rm = TRUE)
c_1_8 <- mean(popgeo2010$pop_density[which(popgeo2010$ogd_change_group_cont25_2 == "Bust")], na.rm = TRUE)


#Median household income ($) 
saipe2001 <- merge(saipe2001, dat_sub[which(dat_sub$year == "2001"),], by = "STCOFIPS", all = FALSE)
saipe2001$median_household_income <- saipe2001$X21

saipe2011 <- merge(saipe2011, dat_sub[which(dat_sub$year == "2010"),], by = "STCOFIPS", all = FALSE)
saipe2011$median_household_income <- as.numeric(saipe2011$`Median Household Income`)

c_2_1 <- mean(saipe2001$median_household_income, na.rm = TRUE)
c_2_2 <- mean(saipe2011$median_household_income, na.rm = TRUE)

c_2_3 <- mean(saipe2001$median_household_income[which(saipe2001$ogd_change_group_cont25_2 == "Status Quo")], na.rm = TRUE)
c_2_4 <- mean(saipe2011$median_household_income[which(saipe2011$ogd_change_group_cont25_2 == "Status Quo")], na.rm = TRUE)

c_2_5 <- mean(saipe2001$median_household_income[which(saipe2001$ogd_change_group_cont25_2 == "Boom")], na.rm = TRUE)
c_2_6 <- mean(saipe2011$median_household_income[which(saipe2011$ogd_change_group_cont25_2 == "Boom")], na.rm = TRUE)

c_2_7 <- mean(saipe2001$median_household_income[which(saipe2001$ogd_change_group_cont25_2 == "Bust")], na.rm = TRUE)
c_2_8 <- mean(saipe2011$median_household_income[which(saipe2011$ogd_change_group_cont25_2 == "Bust")], na.rm = TRUE)


#No high-school diploma (%) 
svi2000 <- merge(svi2000, dat_sub[which(dat_sub$year == "2001"),], by = "STCOFIPS", all = TRUE) 
svi2010 <- merge(svi2010, dat_sub[which(dat_sub$year == "2010"),], by = "STCOFIPS", all = TRUE) 


c_3_1 <- mean((svi2000$G1V4R * 100), na.rm = TRUE)
c_3_2 <- mean((svi2010$E_P_NOHSDIP * 100), na.rm = TRUE)

c_3_3 <- mean((svi2000$G1V4R[which(svi2010$ogd_change_group_cont25_2 == "Status Quo")] * 100), na.rm = TRUE)
c_3_4 <- mean((svi2010$E_P_NOHSDIP[which(svi2010$ogd_change_group_cont25_2 == "Status Quo")] * 100), na.rm = TRUE)

c_3_5 <- mean((svi2000$G1V4R[which(svi2010$ogd_change_group_cont25_2 == "Boom")] * 100), na.rm = TRUE)
c_3_6 <- mean((svi2010$E_P_NOHSDIP[which(svi2010$ogd_change_group_cont25_2 == "Boom")] * 100), na.rm = TRUE)

c_3_7 <- mean((svi2000$G1V4R[which(svi2010$ogd_change_group_cont25_2 == "Bust")] * 100), na.rm = TRUE)
c_3_8 <- mean((svi2010$E_P_NOHSDIP[which(svi2010$ogd_change_group_cont25_2 == "Bust")] * 100), na.rm = TRUE)



#Social vulnerability index 
c_4_1 <- mean((svi2000$G1V4R * 100), na.rm = TRUE)
c_4_2 <- mean((svi2010$E_P_NOHSDIP * 100), na.rm = TRUE)

c_4_3 <- mean((svi2000$G1V4R[which(svi2010$ogd_change_group_cont25_2 == "Status Quo")] * 100), na.rm = TRUE)
c_4_4 <- mean((svi2010$E_P_NOHSDIP[which(svi2010$ogd_change_group_cont25_2 == "Status Quo")] * 100), na.rm = TRUE)

c_4_5 <- mean((svi2000$G1V4R[which(svi2010$ogd_change_group_cont25_2 == "Boom")] * 100), na.rm = TRUE)
c_4_6 <- mean((svi2010$E_P_NOHSDIP[which(svi2010$ogd_change_group_cont25_2 == "Boom")] * 100), na.rm = TRUE)

c_4_7 <- mean((svi2000$G1V4R[which(svi2010$ogd_change_group_cont25_2 == "Bust")] * 100), na.rm = TRUE)
c_4_8 <- mean((svi2010$E_P_NOHSDIP[which(svi2010$ogd_change_group_cont25_2 == "Bust")] * 100), na.rm = TRUE)


# Migration
in2000_agg <- merge(in2000_agg, dat_sub[which(dat_sub$year == "2001"),], by = "STCOFIPS", all = TRUE) 
in2010_agg <- merge(in2010_agg, dat_sub[which(dat_sub$year == "2010"),], by = "STCOFIPS", all = TRUE) 

c_5_1 <- mean((in2000_agg$inflow_pct), na.rm = TRUE)
c_5_2 <- mean((in2010_agg$inflow_pct), na.rm = TRUE)

c_5_3 <- mean((in2000_agg$inflow_pct[which(in2000_agg$ogd_change_group_cont25_2 == "Status Quo")]), na.rm = TRUE)
c_5_4 <- mean((in2010_agg$inflow_pct[which(in2010_agg$ogd_change_group_cont25_2 == "Status Quo")]), na.rm = TRUE)

c_5_5 <- mean((in2000_agg$inflow_pct[which(in2000_agg$ogd_change_group_cont25_2 == "Boom")]), na.rm = TRUE)
c_5_6 <- mean((in2010_agg$inflow_pct[which(in2010_agg$ogd_change_group_cont25_2 == "Boom")]), na.rm = TRUE)

c_5_7 <- mean((in2000_agg$inflow_pct[which(in2000_agg$ogd_change_group_cont25_2 == "Bust")]), na.rm = TRUE)
c_5_8 <- mean((in2010_agg$inflow_pct[which(in2010_agg$ogd_change_group_cont25_2 == "Bust")]), na.rm = TRUE)


#Total N/% 2001/2011
#Status quo N/% 2001/2011
#Boom N/% 2001/2011
#Bust N/% 2001/2011

tab <- matrix(NA, nrow = 5, ncol = 8)

tab[1,1] <- c_1_1
tab[1,2] <- c_1_2
tab[1,3] <- c_1_3
tab[1,4] <- c_1_4
tab[1,5] <- c_1_5
tab[1,6] <- c_1_6
tab[1,7] <- c_1_7
tab[1,8] <- c_1_8

tab[2,1] <- c_2_1
tab[2,2] <- c_2_2
tab[2,3] <- c_2_3
tab[2,4] <- c_2_4
tab[2,5] <- c_2_5
tab[2,6] <- c_2_6
tab[2,7] <- c_2_7
tab[2,8] <- c_2_8


tab[3,1] <- c_3_1
tab[3,2] <- c_3_2
tab[3,3] <- c_3_3
tab[3,4] <- c_3_4
tab[3,5] <- c_3_5
tab[3,6] <- c_3_6
tab[3,7] <- c_3_7
tab[3,8] <- c_3_8

tab[5,1] <- c_5_1
tab[5,2] <- c_5_2
tab[5,3] <- c_5_3
tab[5,4] <- c_5_4
tab[5,5] <- c_5_5
tab[5,6] <- c_5_6
tab[5,7] <- c_5_7
tab[5,8] <- c_5_8
