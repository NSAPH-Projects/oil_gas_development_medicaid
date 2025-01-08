########################################################################################
# PROJECT       :	CCMH
# SPONSOR/PI    : Amruta Nori-Sarma/Mary Willis
# PROGRAM NAME  : 
# DESCRIPTION   : Creating a boom and bust indicator that can be used for DiD modeling
#                 NOTE: This file uses USDA data, which is what we used for the NSAPH collaboration paper
#                 
# PROGRAMMER    : Nina Cesare
# DATE WRITTEN  : 08/01/2024
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

library(readxl)
library(tidyr)
library(dplyr)


inDir <- ""
outDir <- ""


dat <- read_excel(paste0(inDir, "oilgascounty_1_long.xlsx"))


counties <- unique(dat$FIPS)
years <- unique(dat$year)

dat$pct_change_oil <- NA
dat$pct_change_gas <- NA


for(i in 1:length(counties)){
  
  for(j in 2:length(years)){
    
    # If both values are zeros
    if(dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j])] == 0 & dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 0){
      dat$pct_change_oil[which(dat$FIPS == counties[i] & dat$year == years[j])] <- 0
    }
    
    if(dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j])] ==  0 & dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 0){
      dat$pct_change_gas[which(dat$FIPS == counties[i] & dat$year == years[j])] <- 0
    }
    
    ## if the starting value is zero, we have to make an executive decision. Here, I'm adding 0.001
    if(dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j])] > 0 & dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 0){
      dat$pct_change_oil[which(dat$FIPS == counties[i] & dat$year == years[j])] <- ((dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j])] - (dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] + 0.0001))/(dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] + 0.0001)) * 100
    }
    
    if(dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j])] > 0 & dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 0){
      dat$pct_change_gas[which(dat$FIPS == counties[i] & dat$year == years[j])] <- ((dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j])] - (dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] + 0.0001))/(dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] + 0.0001)) * 100
    }
    
    # if both are above zero, keep standard formula
    if(dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j])] > 0 & dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] > 0){
      dat$pct_change_oil[which(dat$FIPS == counties[i] & dat$year == years[j])] <- ((dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j])] - dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])])/dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])]) * 100
    }
    
    if(dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j])] > 0 & dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] > 0){
      dat$pct_change_gas[which(dat$FIPS == counties[i] & dat$year == years[j])] <- ((dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j])] - dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])])/dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])]) * 100
    }
    
    # if only the end value is above zero, keep standard formula
    if(dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j])] == 0 & dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] > 0){
      dat$pct_change_oil[which(dat$FIPS == counties[i] & dat$year == years[j])] <- ((dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j])] - dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])])/dat$oil_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])]) * 100
    }
    
    if(dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j])] == 0 & dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])] > 0){
      dat$pct_change_gas[which(dat$FIPS == counties[i] & dat$year == years[j])] <- ((dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j])] - dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])])/dat$gas_units[which(dat$FIPS == counties[i] & dat$year == years[j-1])]) * 100
    }
    
  }
  
  print(paste0("Finished with county FIPS ", counties[i]))
  
}


## Flag >=5% production increase/decrease for each year

# gas 
dat$gas_up_5pct <- ifelse(dat$pct_change_gas >= 5 & !is.na(dat$pct_change_gas), 1, 0)
dat$gas_up_5pct[is.na(dat$pct_change_gas)] <- NA

dat$gas_down_5pct <- ifelse(dat$pct_change_gas <= -5 & !is.na(dat$pct_change_gas), 1, 0)
dat$gas_down_5pct[is.na(dat$pct_change_gas)] <- NA


# oil
dat$oil_up_5pct <- ifelse(dat$pct_change_oil >= 5 & !is.na(dat$pct_change_oil), 1, 0)
dat$oil_up_5pct[is.na(dat$pct_change_oil)] <- NA

dat$oil_down_5pct <- ifelse(dat$pct_change_oil <= -5 & !is.na(dat$pct_change_oil), 1, 0)
dat$oil_down_5pct[is.na(dat$pct_change_oil)] <- NA


# oil or gas
dat$ogd_up_5pct <- ifelse(dat$pct_change_oil >= 5 | dat$pct_change_gas >= 5 & !is.na(dat$pct_change_oil) & !is.na(dat$pct_change_gas), 1, 0)
dat$ogd_up_5pct[is.na(dat$pct_change_oil) & is.na(dat$pct_change_gas)] <- NA

dat$ogd_down_5pct <- ifelse(dat$pct_change_oil <= -5 | dat$pct_change_gas <=- 5 & !is.na(dat$pct_change_oil) & !is.na(dat$pct_change_gas), 1, 0)
dat$ogd_down_5pct[is.na(dat$pct_change_oil) & is.na(dat$pct_change_gas)] <- NA



## Flag >=10% production increase/decrease for each year

## gas 
dat$gas_up_10pct <- ifelse(dat$pct_change_gas >= 10 & !is.na(dat$pct_change_gas), 1, 0)
dat$gas_up_10pct[is.na(dat$pct_change_gas)] <- NA

dat$gas_down_10pct <- ifelse(dat$pct_change_gas <= -10 & !is.na(dat$pct_change_gas), 1, 0)
dat$gas_down_10pct[is.na(dat$pct_change_gas)] <- NA


## oil
dat$oil_up_10pct <- ifelse(dat$pct_change_oil >= 10 & !is.na(dat$pct_change_oil), 1, 0)
dat$oil_up_10pct[is.na(dat$pct_change_oil)] <- NA

dat$oil_down_10pct <- ifelse(dat$pct_change_oil <= -10 & !is.na(dat$pct_change_oil), 1, 0)
dat$oil_down_10pct[is.na(dat$pct_change_oil)] <- NA


# oil or gas 
dat$ogd_up_10pct <- ifelse(dat$pct_change_oil >= 10 | dat$pct_change_gas >= 10 & !is.na(dat$pct_change_oil) & !is.na(dat$pct_change_gas), 1, 0)
dat$ogd_up_10pct[is.na(dat$pct_change_oil) & is.na(dat$pct_change_gas)] <- NA

dat$ogd_down_10pct <- ifelse(dat$pct_change_oil <= -10 | dat$pct_change_gas <=- 10 & !is.na(dat$pct_change_oil) & !is.na(dat$pct_change_gas), 1, 0)
dat$ogd_down_10pct[is.na(dat$pct_change_oil) & is.na(dat$pct_change_gas)] <- NA



## Flag >=25% production increase/decrease for each year

# gas 
dat$gas_up_25pct <- ifelse(dat$pct_change_gas >= 25 & !is.na(dat$pct_change_gas), 1, 0)
dat$gas_up_25pct[is.na(dat$pct_change_gas)] <- NA

dat$gas_down_25pct <- ifelse(dat$pct_change_gas <= -25 & !is.na(dat$pct_change_gas), 1, 0)
dat$gas_down_25pct[is.na(dat$pct_change_gas)] <- NA


# oil
dat$oil_up_25pct <- ifelse(dat$pct_change_oil >= 25 & !is.na(dat$pct_change_oil), 1, 0)
dat$oil_up_25pct[is.na(dat$pct_change_oil)] <- NA

dat$oil_down_25pct <- ifelse(dat$pct_change_oil <= -25 & !is.na(dat$pct_change_oil), 1, 0)
dat$oil_down_25pct[is.na(dat$pct_change_oil)] <- NA


# oil or gas
dat$ogd_up_25pct <- ifelse(dat$pct_change_oil >= 25 | dat$pct_change_gas >= 25 & !is.na(dat$pct_change_oil) & !is.na(dat$pct_change_gas), 1, 0)
dat$ogd_up_25pct[is.na(dat$pct_change_oil) & is.na(dat$pct_change_gas)] <- NA

dat$ogd_down_25pct <- ifelse(dat$pct_change_oil <= -25 | dat$pct_change_gas <=- 25 & !is.na(dat$pct_change_oil) & !is.na(dat$pct_change_gas), 1, 0)
dat$ogd_down_25pct[is.na(dat$pct_change_oil) & is.na(dat$pct_change_gas)] <- NA



### Flag contemporaneous booms and busts

dat$ogd_change_group_cont <- ifelse(is.na(dat$ogd_up_10pct), NA, "Status Quo")

# vectors counties and years from above


for(i in 1:length(counties)){
  
  for(j in 2:length(years)){
    
    # The first row
    if(j == 2){
      if(dat$ogd_down_10pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_down_10pct[which(dat$FIPS == counties[i] & dat$year == years[j+1])] == 1)){
        dat$ogd_change_group_cont[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Bust"
      }
      if(dat$ogd_up_10pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_up_10pct[which(dat$FIPS == counties[i] & dat$year == years[j+1])] == 1)){
        dat$ogd_change_group_cont[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Boom"
      }
    }
    
    # The middle rows
    if(j > 2 & j < length(years)){
      if(dat$ogd_down_10pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_down_10pct[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 1 | dat$ogd_down_10pct[which(dat$FIPS == counties[i] & dat$year == years[j+1])] == 1)){
        dat$ogd_change_group_cont[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Bust"
      }
      if(dat$ogd_up_10pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_up_10pct[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 1 | dat$ogd_up_10pct[which(dat$FIPS == counties[i] & dat$year == years[j+1])] == 1)){
        dat$ogd_change_group_cont[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Boom"
      }
    }
    
    
    # The last row
    if(j == length(years)){
      if(dat$ogd_down_10pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_down_10pct[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 1)){
        dat$ogd_change_group_cont[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Bust"
      }
      if(dat$ogd_up_10pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_up_10pct[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 1)){
        dat$ogd_change_group_cont[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Boom"
      }
    }
    
  }
  print(paste0("Finished with county FIPS ", counties[i]))
}







### Flag contemporaneous booms and busts - 25 pct threshold

dat$ogd_change_group_cont25 <- ifelse(is.na(dat$ogd_up_25pct), NA, "Status Quo")

# vectors counties and years from above


for(i in 1:length(counties)){
  
  for(j in 2:length(years)){
    
    # The first row
    if(j == 2){
      if(dat$ogd_down_25pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_down_25pct[which(dat$FIPS == counties[i] & dat$year == years[j+1])] == 1)){
        dat$ogd_change_group_cont25[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Bust"
      }
      if(dat$ogd_up_25pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_up_25pct[which(dat$FIPS == counties[i] & dat$year == years[j+1])] == 1)){
        dat$ogd_change_group_cont25[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Boom"
      }
    }
    
    # The middle rows
    if(j > 2 & j < length(years)){
      if(dat$ogd_down_25pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_down_25pct[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 1 | dat$ogd_down_25pct[which(dat$FIPS == counties[i] & dat$year == years[j+1])] == 1)){
        dat$ogd_change_group_cont25[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Bust"
      }
      if(dat$ogd_up_25pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_up_25pct[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 1 | dat$ogd_up_25pct[which(dat$FIPS == counties[i] & dat$year == years[j+1])] == 1)){
        dat$ogd_change_group_cont25[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Boom"
      }
    }
    
    
    # The last row
    if(j == length(years)){
      if(dat$ogd_down_25pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_down_25pct[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 1)){
        dat$ogd_change_group_cont25[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Bust"
      }
      if(dat$ogd_up_25pct[which(dat$FIPS == counties[i] & dat$year == years[j])] == 1 & (dat$ogd_up_25pct[which(dat$FIPS == counties[i] & dat$year == years[j-1])] == 1)){
        dat$ogd_change_group_cont25[which(dat$FIPS == counties[i] & dat$year == years[j])] <- "Boom"
      }
    }
    
  }
  print(paste0("Finished with county FIPS ", counties[i]))
}





## Move boom/bust forward - 10% threshold
dat$ogd_change_group_cont2 <- dat$ogd_change_group_cont
dat$ogd_change_group_cont2[which(dat$ogd_change_group_cont2 == "Status Quo")] <- NA

dat <- dat %>% 
  dplyr::group_by(FIPS) %>% 
  tidyr::fill(ogd_change_group_cont2, .direction = c("down"))



dat$ogd_change_group_cont2[which(is.na(dat$ogd_change_group_cont2))] <- "Status Quo"
dat$ogd_change_group_cont2[which(is.na(dat$ogd_change_group_cont))] <- NA



## Move boom/bust forward - 25% threshold
dat$ogd_change_group_cont25_2 <- dat$ogd_change_group_cont25
dat$ogd_change_group_cont25_2[which(dat$ogd_change_group_cont25_2 == "Status Quo")] <- NA

dat <- dat %>% 
  dplyr::group_by(FIPS) %>% 
  tidyr::fill(ogd_change_group_cont25_2, .direction = c("down"))


dat$ogd_change_group_cont25_2[which(is.na(dat$ogd_change_group_cont25_2))] <- "Status Quo"
dat$ogd_change_group_cont25_2[which(is.na(dat$ogd_change_group_cont25))] <- NA




## Export 

write.csv(dat, paste0(outDir, "oilgascounty_1_long_an.csv"), row.names = FALSE)


