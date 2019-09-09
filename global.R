library(tidyverse)
library(lubridate)

Crime2010_original <- readr::read_csv('./Data/Crime_Data_from_2010_to_Present.csv', col_types = 
                                        cols(
                                          DR_NO = col_character(),
                                          `Date Rptd` = col_character(),
                                          `DATE OCC` = col_character(),
                                          `TIME OCC` = col_character(),
                                          AREA = col_character(),
                                          `AREA NAME` = col_character(),
                                          `Rpt Dist No` = col_character(),
                                          `Crm Cd` = col_double(),
                                          `Crm Cd Desc` = col_character(),
                                          Mocodes = col_character(),
                                          `Vict Age` = col_double(),
                                          `Vict Sex` = col_character(),
                                          `Vict Descent` = col_character(),
                                          `Premis Cd` = col_double(),
                                          `Premis Desc` = col_character(),
                                          `Weapon Used Cd` = col_double(),
                                          `Weapon Desc` = col_character(),
                                          Status = col_character(),
                                          `Status Desc` = col_character(),
                                          `Crm Cd 1` = col_double(),
                                          `Crm Cd 2` = col_double(),
                                          `Crm Cd 3` = col_double(),
                                          `Crm Cd 4` = col_double(),
                                          LOCATION = col_character(),
                                          `Cross Street` = col_character(),
                                          LAT = col_double(),
                                          LON = col_double()
                                        ))