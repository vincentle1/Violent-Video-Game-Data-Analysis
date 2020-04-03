library(rvest)
library(stringr)
library(tidyverse)
library(readxl)

###########################Scraper function##########################
vg_scraper <- function(year){
#Scrape the chart
url <- paste("https://www.vgchartz.com/yearly/", year, "/USA/", sep = "")

#html_nodes finds the full element including text.

vg_list <- url %>%
  read_html() %>%
  html_nodes('#chart_body > table') %>%
  html_table(fill = TRUE)

vg_dataframe <- vg_list[[1]]

##Clean up the chart

#1. Select columns
vg_dataframe <- vg_dataframe[c(2, 8)]

#2. Rename columns
names(vg_dataframe)[2] <- "Yearly_Sales"

#3. Remove all rows with NA
vg_dataframe <- vg_dataframe %>%
  filter(!is.na(vg_dataframe$Yearly_Sales))

#4. Remove commas from Yearly_Sales using gsub and convert to numeric
vg_dataframe$Yearly_Sales <- as.numeric(gsub(",", "", vg_dataframe$Yearly_Sales))

#5. Filter by genre. Action, shooter and fighting
vg_dataframe <- vg_dataframe %>%
  filter(str_detect(Game, "Action|Shooter|Fighting" ))

#6. Return dataframe
vg_dataframe
}

#################End of scraper function ######################

#Available data from 2005 to 2018##

####### Summarize videogame function ###########

vvg_sum <- function(){
final_data <- data.frame(year = integer(0), sum_year_sale = double(0))
for (val in c(2005:2018)){
  a <- vg_scraper(val)
  sum_yr <- a %>%
    summarize(sum_year_sale = sum(Yearly_Sales)) %>%
    mutate(year = val) %>%
    select(year, sum_year_sale)
  final_data <- rbind(final_data, sum_yr)
}
final_data <- final_data %>%
  mutate_at(1, as.factor)
final_data
}

vvg_data <- vvg_sum()

####### FBI Violent Crime Data ###########################################################
library(readxl)
violent_crime <- read_excel("/home/vincentle/Violent Videogame Project/FBI Tables/violent_crime.xls")

###### Clean Violent Crime Data ########################

#Choose rows
violent_crime <- violent_crime[3:23, ]

#Change headers
names(violent_crime) <- violent_crime[1, ]

#Delete first row
violent_crime <- violent_crime[-1, ]

#Select desired columns and filter desired rows, then change 20176 to 2017
violent_crime <- violent_crime[7:20 ,1:4]
violent_crime[13,1] <- 2017

#Convert columns to desired types, rename column properly, clean up final
violent_crime <- violent_crime %>%
  mutate_at(c(2:4), as.double) %>%
  mutate_at(1, as.factor) %>%
  rename(year = Year)

violent_crime
###### Join vvg_data and violent_crime data! ######

vvg_fbi <- vvg_data %>%
  inner_join(violent_crime)

##### Rename columns to take out spaces #####

names(vvg_fbi)[3] <- 'population'
names(vvg_fbi)[4] <- 'violent_crime_abs'
names(vvg_fbi)[5] <- 'violent_crime_rate'

####### Alternate violent crime chart with all years included ###########
violent_crime <- read_excel("/home/vincentle/Violent Videogame Project/FBI Tables/violent_crime.xls")
#Choose rows
violent_crime <- violent_crime[3:23, ]

#Change headers
names(violent_crime) <- violent_crime[1, ]

#Delete first row
violent_crime <- violent_crime[-1, ]
names(violent_crime)[2] <- 'population'
names(violent_crime)[3] <- 'violent_crime_abs'
names(violent_crime)[4] <- 'violent_crime_rate'

#Alternate desired columns, filters including all years
violent_crime <- violent_crime[,1:4]
violent_crime[3,1] <- 2001
violent_crime[19,1] <- 2017
violent_crime <- violent_crime[,1:4]

violent_crime <- violent_crime %>%
  mutate_at(c(2:4), as.double) %>%
  mutate_at(1, as.factor) %>%
  rename(year = Year)

