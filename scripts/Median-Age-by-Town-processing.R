library(dplyr)
library(acs)
library(devtools)
load_all('../datapkg')
library(datapkg)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Median Age by Town
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Get state data
geography=geo.make(state=09)
yearlist=c(2010:2018)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

tables <- c("", "A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("All", "White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    #needed to grab all columns for all years    
    variable =list()      
    for (k in seq_along(1:3)) {
     number = number=paste0("B01002", tbl, "_", sprintf("%03d",k))
     variable = c(variable, number)
     k=k+1
    }    
    variable <- as.character(variable)    
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                      variable = variable, key=key)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$NAME <- NULL
    total <- acsSum(data, 1, "Median Age Total")
    total.m <- acsSum(data, 2, "Median Age Male")
    total.f <- acsSum(data, 3, "Median Age Female")
    estimates <- data.table(
            geo, 
            estimate(total),
            estimate(total.m),
            estimate(total.f),
            year,
            race, 
            "Measure Type" = "Number", 
            "Variable" = "Median Age"
        )
    moes <- data.table(
            geo,
            standard.error(total) * 1.645,
            standard.error(total.m) * 1.645,
            standard.error(total.f) * 1.645,
            year,
            race, 
            "Measure Type" = "Number", 
            "Variable" = "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Total",
            "Male",
            "Female",
            "Year",
            "Race/Ethnicity", 
            "Measure Type", 
            "Variable"
         )
    setnames(estimates, numberNames)
    setnames(moes, numberNames)
    data.melt <- melt(
            rbind(estimates, moes),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Gender",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
     inter_data <- rbind(inter_data, data.melt)
  }
  state_data <- rbind(state_data, inter_data)
}

#Get state data
geography=geo.make(state=09, county="*", county.subdivision = "*")

town_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    #needed to grab all columns for all years    
    variable =list()      
    for (k in seq_along(1:3)) {
     number = number=paste0("B01002", tbl, "_", sprintf("%03d",k))
     variable = c(variable, number)
     k=k+1
    }    
    variable <- as.character(variable)    
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                      variable = variable, key=key)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$county <- sprintf("%02d", geo$county)
    geo$county <- gsub("^", "090", geo$county)
    geo$FIPS <- paste0(geo$county, geo$countysubdivision)
    geo$state <- NULL
    geo$NAME <- NULL
    geo$countysubdivision <- NULL
    geo$county <- NULL   
    total <- acsSum(data, 1, "Median Age Total")
    total.m <- acsSum(data, 2, "Median Age Male")
    total.f <- acsSum(data, 3, "Median Age Female")
    estimates <- data.table(
            geo, 
            estimate(total),
            estimate(total.m),
            estimate(total.f),
            year,
            race,
            "Measure Type" = "Number", 
            "Variable" = "Median Age"
            
        )
    moes <- data.table(
            geo,
            standard.error(total) * 1.645,
            standard.error(total.m) * 1.645,
            standard.error(total.f) * 1.645,
            year,
            race, 
            "Measure Type" = "Number", 
            "Variable" = "Margins of Error"
            
        )
    numberNames <- c(
            "FIPS",
            "Total",
            "Male",
            "Female",
            "Year",
            "Race/Ethnicity", 
            "Measure Type", 
            "Variable"            
         )
    setnames(estimates, numberNames)
    setnames(moes, numberNames)
    data.melt <- melt(
            rbind(estimates, moes),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Gender",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
     inter_data <- rbind(inter_data, data.melt)
  }
  town_data <- rbind(town_data, inter_data)
}

med_age_data <- rbind(state_data, town_data)

med_age_data$Value[med_age_data$Value == -666666666] <- NA
med_age_data$Value[med_age_data$Value == "-222222222"] <- NA
med_age_data$Value[med_age_data$Value == -333333333.0] <- NA

#Merge in towns by FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

med_age_data <- merge(med_age_data, towns, by = "FIPS")

med_age_data <- med_age_data %>% 
  select(Town, FIPS, Year, Gender, `Race/Ethnicity`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, Gender, `Race/Ethnicity`, `Measure Type`, desc(Variable))

write.table (
  med_age_data,
  file.path(getwd(), "data", "median_age_town_2018.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)



