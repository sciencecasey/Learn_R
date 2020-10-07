#This is based on the example in the riem package
#uses an API of current weather around the world at airports


library(httr)
library(riem)
library(lubridate)

#if want the wind in speed for Denver, CO
meso_url = "https://mesonet.agron.iastate.edu/api/"
denver <- GET(url = meso_url,
              query = list(station = "DEN",
                           data = "sped",
                           year1 = "2016",
                           month1 = "6",
                           day1 = "1",
                           year2 = "2016",
                           month2 = "6",
                           day2 = "30",
                           tz = "America/Denver",
                           format = "comma")) %>%
  content() %>% 
  read_csv(denver, skip = 5, na = "M") 

denver %>% slice(1:3)
