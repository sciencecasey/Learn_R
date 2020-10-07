#import organization packages
library(tidyr)
library(dplyr)

VADeaths #view the base death table in non-tidy version
class(VADeaths) #matrix

#save the base table as Tidy table
VADeathsTidy = VADeaths %>%
  #convert to tibble: tidy's version of DF which leads to cleaner data
  tbl_df() %>% 
  #take the ages listed as rownames as a variable with colname age
  mutate(age = row.names(VADeaths)) %>%
  #elongate the table by separating the colNames into their own variable types
  #in decending age order, add a column for "urban" and "gender" based on the ColNames from orginal
  #take the values stored in table and put into one column titled death_rate
  pivot_longer(-age, names_to = c("urban", "gender"), names_sep = " ", values_to = "death_rate") #%>%
  #change the columns to factors rather than chr data
  mutate(age = factor(age), urban = factor(urban), gender = factor(gender))
  #check the df to see that it worked
  str(VADeathsTidy)
  
#Summarizing
  #file from readR.R script (Hurricane data)
  #summarize the number of observations in the ext_tracks dataset, max windspeed and min pressure
ext_tracks %>%
  summarize(n_obs=n(), worst_wind = max(max_wind), worst_pressure = min(min_pressure))
#wind pressure is in knots, changing to mph by creating a function
knots_to_mph = function(knots){
  mph = 1.152 * knots
}

ext_tracks %>%
  summarize(n_obs=n(), worst_wind = knots_to_mph(max(max_wind)), worst_pressure = min(min_pressure))

#Summarizing by groups
grouped_storms= ext_tracks %>%
  group_by(storm_name, year)
grouped_storms %>%
  summarize(n_obs = n(), worst_wind = max(max_wind), worst_pressure = min(min_pressure))


#plot the summary
library(ggplot2)
grouped_storms %>%
  summarize(n_obs = n(), worst_wind = knots_to_mph(max(max_wind)), worst_pressure = min(min_pressure)) %>% 
  ggplot(aes(x = worst_wind)) + geom_histogram()



#Selecting data
#only select certain cols by name
ext_tracks %>%
  select(storm_name, latitude, longitude, starts_with("radius_34"))
#select certain columns by name or whos name matches the pattern underscoreNumberNumberunderscore
ext_tracks %>% 
  select(storm_name, latitude, longitude, matches("_[0-9][0-9]_"))


#Filter Data
#select rows that follow a logical condition
ext_tracks%>%
  select(storm_name, hour, max_wind) %>%
  filter(hour =="00") %>%
  head(9)
#Filter Summarized Data
ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(worst_wind= max(max_wind)) %>%
  filter(worst_wind >= 160)

#save Katrina Data
kat_data = ext_tracks %>% 
  #select the year of Katrina
  select(storm_name, year, month, day, hour, max_wind, min_pressure, rad_max_wind) %>%
  filter(storm_name == "KATRINA" & year == "2005")


library(faraway)
data("worldcup")
str(worldcup)
head(worldcup)
#data not tidy, observation rownames are by player rather than columns by player

#make the player names its own column so that rownames are general and players are manipulatable
worldcup = worldcup %>%
  mutate(player_name = rownames(worldcup))

#check the first three (these do the same thing)
worldcup %>% slice(1:3)
head(worldcup, 3)

#add a column with avg shots per player position
worldcup %>% 
group_by(Position) %>%
  mutate(avg_shats = mean(Shots)) %>%
  ungroup()

#does the same calculation but can't ungroup and simply returns a 
#4x2 tibble of the positions and the avg shots for each
#worldcup %>% 
#  group_by(Position) %>%
#  summarize(avg_shats = mean(Shots)) %>%
#  ungroup()

#rename a column
worldcup %>%
  rename(Name = player_name) %>%
  slice(1:3)

data("VADeaths")
head(VADeaths)
#Convert to tibble and remove agegroups as rownames, creating a column of these names
VADeaths = VADeaths %>%
  tbl_df() %>% #convert to tibble
  mutate(age_group = rownames(VADeaths))
str(VADeaths)

#Gather by agegroup
#VADeaths = VADeaths %>%
#  gather (key = gender, value = death_rate, -age_group)

VADeaths %>%
  pivot_longer(cols = -age_group, names_to = "gender", values_to = "death rate")
#make all the columns except age_group into a row, name the old colnames (now rows) "gender", and the values of the separated the "death rate")
VADeaths %>%
  pivot_longer(cols = - age_group, names_to = c("location", "gender"), names_sep = ' ', values_to = "death_rate")
