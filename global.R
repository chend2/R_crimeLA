library(tidyverse)
library(lubridate)

# read data from dataset and specify parsers for columns that have issues with variable type due to col type determination limitations
# Crm Cd 3 and 4 have no values for the rows used for col typ determination and thus coltype has to be declared 

Crime2010_original <- readr::read_csv('./Data/Crime_Data_from_2010_to_Present.csv', 
                                      col_types = 
                                        cols(
                                          `Crm Cd 3` = col_double(),
                                          `Crm Cd 4` = col_double()
                                        )
                                      )



# I want Date rptd, Date occ, month occurred, hour occurred, Area Name, Rpt Dist No, mo codes, vict age, sex, descent, premise desc, 
# status desc, crm cd 1-4, lat, lon 


# format date columns
Processed_Dates_df <- Crime2010_original%>%
  mutate(., `TIME OCC` = {str_sub(`TIME OCC`, -2, -3) <- ":" ; `TIME OCC`}, 
         `Date Rptd` = substr(`Date Rptd`,1,10),
         `DATE OCC` = substr(`DATE OCC`,1,10))

# parse date columns, create new datetime, month, hour, and days_delayed columns 

Processed_Dates_df <- Processed_Dates_df%>%
  mutate(., `DT_OCC`= mdy_hm(paste(`DATE OCC`, `TIME OCC`)),
         `Date Rptd` = parse_date(`Date Rptd`,"%m/%d/%Y"), 
         `DATE OCC` = parse_date(`DATE OCC`, "%m/%d/%Y"),
         `Days_Delayed`= as.double(difftime(`Date Rptd`, `DATE OCC`), units="days"),
         `Month` = month(`DT_OCC`),
         `Hour` = hour(`DT_OCC`))

# drop uneccessary columns and flatten MO code column similar to Crm Cds, and handle invalid values for factoring 
trimmed_crime_df <- Processed_Dates_df%>%
  select(., -c(`AREA`,
               `Part 1-2`,
               `TIME OCC`,
               `Crm Cd`,
               `Crm Cd Desc`,
               `Premis Cd`,
               `Weapon Used Cd`,
               `Status`,
               `LOCATION`,
               `Cross Street`))%>%
  separate(., `Mocodes`, c("Mocode 1", "Mocode 2", "Mocode 3","Mocode 4","Mocode 5",
                           "Mocode 6","Mocode 7","Mocode 8","Mocode 9","Mocode 10"),sep=" ", fill="right")%>%
  mutate(., `Vict Age` = ifelse(`Vict Age` < 1, NA, `Vict Age`))%>%
  mutate(., `Vict Descent`= case_when(`Vict Descent` == "A" ~ "Other Asian",
                                      `Vict Descent` == "B" ~ "Black",
                                      `Vict Descent` == "C" ~ "Chinese",
                                      `Vict Descent` == "D" ~ "Cambodian",
                                      `Vict Descent` == "F" ~ "Filipino",
                                      `Vict Descent` == "G" ~ "Guamanian",
                                      `Vict Descent` == "H" ~ "Hispanic",
                                      `Vict Descent` == "I" ~ "American Indian",
                                      `Vict Descent` == "J" ~ "Japanese",
                                      `Vict Descent` == "K" ~ "Korean",
                                      `Vict Descent` == "L" ~ "Laotian",
                                      `Vict Descent` == "O" ~ "Other",
                                      `Vict Descent` == "P" ~ "Pacific Islander",
                                      `Vict Descent` == "S" ~ "Samoan",
                                      `Vict Descent` == "U" ~ "Hawaiian",
                                      `Vict Descent` == "V" ~ "Vietnamese",
                                      `Vict Descent` == "W" ~ "White",
                                      `Vict Descent` == "X" ~ "Unknown",
                                      `Vict Descent` == "Z" ~ "Asian Indian"))%>%
  mutate(., `Crm Cd 1` = ifelse(is.na(`Crm Cd 1`), `Crm Cd 2`, `Crm Cd 1`))%>%
  mutate(., `Crm Cd 2` = ifelse(`Crm Cd 2` == `Crm Cd 1`, `Crm Cd 3`, `Crm Cd 2`))%>%
  mutate(., `LAT` = ifelse(`LAT`!=0, `LAT`, NA), `LON`= ifelse(`LON`!=0, `LON`, NA))

# factorize to examine missingness 
descent_levels <- c("Other Asian", "Black", "Chinese", "Cambodian", "Filipino", "Guamanian", "Hispanic", "American Indian",
                    "Japanese", "Korean", "Laotian", "Other", "Pacfic Islander", "Samoan", "Hawaiian", "Vietnamese",
                    "White", "Unknown", "Asian Indian")

gender_levels <- c("M", "F")

factorized_df <- trimmed_crime_df%>%
  mutate(.,  `AREA NAME` = factor(`AREA NAME`),
         `Mocode 1` = factor(`Mocode 1`),
         `Mocode 2` = factor(`Mocode 2`),
         `Mocode 3` = factor(`Mocode 3`),
         `Mocode 4` = factor(`Mocode 4`),
         `Mocode 5` = factor(`Mocode 5`),
         `Mocode 6` = factor(`Mocode 6`),
         `Mocode 7` = factor(`Mocode 7`),
         `Mocode 8` = factor(`Mocode 8`),
         `Mocode 9` = factor(`Mocode 9`),
         `Mocode 10` = factor(`Mocode 10`),
         `Vict Sex` = factor(`Vict Sex`, gender_levels), 
         `Vict Descent`=factor(`Vict Descent`, descent_levels),
         `Premis Desc` = factor(`Premis Desc`),
         `Weapon Desc` = factor(`Weapon Desc`),
         `Status Desc` = factor(`Status Desc`),
         `Crm Cd 1` = factor(`Crm Cd 1`),
         `Crm Cd 2` = factor(`Crm Cd 2`),
         `Crm Cd 3` = factor(`Crm Cd 3`),
         `Crm Cd 4` = factor(`Crm Cd 4`),
         `Month` = factor(`Month`),
         `Hour` = factor(`Hour`),
         `Status Desc` = factor(`Status Desc`),
         `Weapon Desc` = factor(`Weapon Desc`)
  )

# rename columns for easier manipulation 
colnames(factorized_df) <- c("DR_NO", "Date_Rptd", "Date_Occ", "Area", "Dist_No", "Mocode1", "Mocode2", "Mocode3", "Mocode4",
                             "Mocode5", "Mocode6", "Mocode7", "Mocode8", "Mocode9", "Mocode10", "Age", "Sex", "Descent", "Premise", "Weapon",
                             "Status", "CC1", "CC2","CC3", "CC4", "LAT", "LON", "DateTime_Occ", "Days_Delayed", "Month", "Hour")

