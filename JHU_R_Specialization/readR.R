library(readr)
#read in data
teams= read_csv("/Users/CaseyJayne/OneDrive/JHU/personalSoftwareLearning/RProgrammingSpecialization/AdvancedRProgramming/data/team_standings.csv")
str(teams)


#instead of reading in from computer, read in from website
#first, save location as variable
ext_tracks_file = paste0("http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2015.txt")
#if fixed file width (this one is) see how wide each col is 

# Create a vector of the width of each column
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
# if there is no documentation you can try fwf_empty in readr which guesses widths, but doesn't
## work for mess (most) datasets

# Create a vector of column names, based on the online documentation for this data
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")


#read in the data; read fwf puts empty data in as -99 so set this to na
# Read the file in from its url
ext_tracks <- read_fwf(ext_tracks_file, 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")
str(ext_tracks)
ext_tracks = ext_tracks %>%
  mutate(month = factor(month), day = factor(day), hour = factor(hour), year = factor(year))
str(ext_tracks)

#print select data to view
ext_tracks[1:3, 1:9] #print first 3 rows and first 9 cols

####NEW DATASET: ZIKA

zikaFile = paste0("https://raw.githubusercontent.com/cdcepi/zika/master/Brazil/COES_Microcephaly/data/COES_Microcephaly-2016-06-25.csv")

zika_Brazil = read_csv(zikaFile)

zika_Brazil %>%
  select(location, value, unit)

