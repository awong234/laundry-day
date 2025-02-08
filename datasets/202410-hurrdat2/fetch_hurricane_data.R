# Start R here using .Rproj file

# Strategy for reproducible environment. Install is sufficiently simple that a
# tempdir is used here.
options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/2024-10-20'))
.libPaths(tempdir())

if (!'pak' %in% .packages(all.available=TRUE)) install.packages('pak', type = 'binary')

pak::pkg_install(c("dplyr", "readr", "stringr", "tidyr", "lubridate", "janitor", "data.table"))

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(janitor)
library(data.table)

# Optionally retrieve hurricane data from hurrdat2 data

hurrdat2_url = "https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2023-051124.txt"

# Structure is as follows
# There are header lines with the storm ID (1-8), name (19-28), and number of rows of data for the storm (34-36).

# The data lines are structured as follows:
# 20210829, 1655, L, HU, 29.1N, 90.2W, 130, 931, 130, 110, 80, 110, 70, 60, 40, 60, 45, 35, 20, 30, 10
# 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345
#          10        20        30        40        50        60        70        80        90        100       110       120
# 2021 (Spaces 1-4) – Year
# 08 (Spaces 5-6) – Month
# 29 (Spaces 7-8, before 1st comma) – Day
# 16 (Spaces 11-12) – Hours in UTC (Universal Time Coordinate)
# 55 (Spaces 13-14, before 2nd comma) – Minutes
# L (Space 17, before 3rd comma) – Record identifier (see notes below)
# C – Closest approach to a coast, not followed by a landfall
# G – Genesis
# I – An intensity peak in terms of both pressure and wind
# L – Landfall (center of system crossing a coastline)
# P – Minimum in central pressure
# R – Provides additional detail on the intensity of the cyclone when rapid changes are underway
# S – Change of status of the system
# T – Provides additional detail on the track (position) of the cyclone
# W – Maximum sustained wind speed
# HU (Spaces 20-21, before 4th comma) – Status of system. Options are:
#     TD – Tropical cyclone of tropical depression intensity (< 34 knots)
# TS – Tropical cyclone of tropical storm intensity (34-63 knots)
# HU – Tropical cyclone of hurricane intensity (> 64 knots)
# EX – Extratropical cyclone (of any intensity)
# SD – Subtropical cyclone of subtropical depression intensity (< 34 knots)
# SS – Subtropical cyclone of subtropical storm intensity (> 34 knots)
# LO – A low that is neither a tropical cyclone, a subtropical cyclone, nor an extratropical cyclone (of any intensity)
# WV – Tropical Wave (of any intensity)
# DB – Disturbance (of any intensity)
# 29.1 (Spaces 24-27) – Latitude
# N (Space 28, before 5th comma) – Hemisphere – North or South
# 90.2 (Spaces 31-35) – Longitude
# W (Space 36, before 6th comma) – Hemisphere – West or East
# 130 (Spaces 39-41, before 7th comma) – Maximum sustained wind (in knots)

hurrdat = readLines(hurrdat2_url)

# These lines identify the start of each storm's data -- we anchor to these to identify the header data lines
header_lines = substr(hurrdat, 1, 1) == "A"
headers = hurrdat[header_lines]

# We can extract the header data by using the text column indices given above.
header_data = list()
header_data$hurricane_id = headers %>% substr(1,8)
header_data$hurricane_name = headers %>% substr(19,28)
header_data$header_id = which(header_lines) # Gives us a unique id for each section of header data -- allows us to join later.
header_data = as.data.frame(header_data)
header_data = header_data %>%
    tidyr::separate(hurricane_id, sep = c(2, 4), into = c("basin", "cyclone_number", "year"), remove = FALSE) %>%
    relocate(hurricane_name, .before = basin)
# only atlantic values in this dataset
header_data$basin = sapply(header_data$basin, \(x) switch(x, "AL" = "Atlantic", "EP" = "Northeast Pacific", "CP" = "North Central Pacific"))

# As provided by hurrdat2 format pdf
data_cnames = c(
    "Raw Date",
    "Raw Time in UTC",
    "Record identifier",
    "Status of system",
    "Raw Latitude",
    "Raw Longitude",
    "Maximum sustained wind (in knots)",
    "Minimum Pressure (in millibars)",
    "34 kt wind radii maximum extent in northeastern quadrant (in nautical miles)",
    "34 kt wind radii maximum extent in southeastern quadrant (in nautical miles)",
    "34 kt wind radii maximum extent in southwestern quadrant (in nautical miles)",
    "34 kt wind radii maximum extent in northwestern quadrant (in nautical miles)",
    "50 kt wind radii maximum extent in northeastern quadrant (in nautical miles)",
    "50 kt wind radii maximum extent in southeastern quadrant (in nautical miles)",
    "50 kt wind radii maximum extent in southwestern quadrant (in nautical miles)",
    "50 kt wind radii maximum extent in northwestern quadrant (in nautical miles)",
    "64 kt wind radii maximum extent in northeastern quadrant (in nautical miles)",
    "64 kt wind radii maximum extent in southeastern quadrant (in nautical miles)",
    "64 kt wind radii maximum extent in southwestern quadrant (in nautical miles)",
    "64 kt wind radii maximum extent in northwestern quadrant (in nautical miles)",
    "Radius of Maximum Wind (in nautical miles)"
)

cnames = janitor::make_clean_names(data_cnames)

# Now we read the actual data -- this is all of the data that are NOT header rows
data = hurrdat[!header_lines] %>% paste0(collapse = '\n') %>% readr::read_csv(col_names = cnames)
data_row_counts = headers %>% substr(34, 36) %>% as.integer()
# Create a header_id to join on using the header lines, and the number of the records in each segment.
data$header_id = rep(which(header_lines), data_row_counts)
data = data %>% left_join(header_data, by = c("header_id"))
data$header_id = NULL # Drop header ID no use any longer
# Clean up the location data a little more
data = data %>%
    mutate(latitude = stringr::str_replace_all(raw_latitude, '[a-zA-Z]', '') |> as.numeric(),
           lat_hemisphere = stringr::str_extract(raw_latitude, '[a-zA-Z]'),
           longitude = stringr::str_replace_all(raw_longitude, '[a-zA-Z]', '') |> as.numeric(),
           lon_hemisphere = stringr::str_extract(raw_longitude, '[a-zA-Z]'),
           longitude = ifelse(lon_hemisphere == 'W', -longitude, longitude),
           datetime = as.POSIXct(paste0(raw_date, raw_time_in_utc), format = '%Y%m%d%H%M', tz = "UTC"),
           data_year = lubridate::year(datetime),
           data_month = lubridate::month(datetime),
           data_day = lubridate::day(datetime),
           data_hour = lubridate::hour(datetime),
           data_minute = lubridate::minute(datetime))

# Add more information for status of system
status_lookup = data.frame(
    status_of_system = c("TD", "TS", "HU", "EX", "SD", "SS", "LO", "WV", "DB"),
    status_of_system_detail = c(
        "Tropical cyclone of tropical depression intensity (< 34 knots)",
        "Tropical cyclone of tropical storm intensity (34-63 knots)",
        "Tropical cyclone of hurricane intensity (> 64 knots)",
        "Extratropical cyclone (of any intensity)",
        "Subtropical cyclone of subtropical depression intensity (< 34 knots)",
        "Subtropical cyclone of subtropical storm intensity (> 34 knots)",
        "A low that is neither a tropical cyclone, a subtropical cyclone, nor an extratropical cyclone (of any intensity)",
        "Tropical Wave (of any intensity)",
        "Disturbance (of any intensity)")
)
# Add more information for record identifiers
record_id_lookup = data.frame(
    record_identifier = c("C", "G", "I", "L", "P", "R", "S", "T", "W"),
    record_identifier_detail = c(
        "Closest approach to a coast, not followed by a landfall",
        "Genesis",
        "An intensity peak in terms of both pressure and wind",
        "Landfall (center of system crossing a coastline)",
        "Minimum in central pressure",
        "Provides additional detail on the intensity of the cyclone when rapid changes are underway",
        "Change of status of the system",
        "Provides additional detail on the track (position) of the cyclone",
        "Maximum sustained wind speed"
    )
)

data = data %>%
    left_join(status_lookup, by = 'status_of_system') %>%
    left_join(record_id_lookup, by = c('record_identifier'))

# Format header names for final output. This part is not strictly necessary but
# it is nice to match up with the defined column names.

header_cnames = c(
    "Hurricane ID",
    "Hurricane Name",
    "Basin",
    "ATCF cyclone number for that year",
    "Hurricane Year",
    "Latitude",
    "Latitude Hemisphere",
    "Longitude",
    "Longitude Hemisphere",
    "Datetime",
    "Data Year",
    "Data Month",
    "Data Day",
    "Data Hour UTC",
    "Data Minute"
)

final_cnames = c(data_cnames, header_cnames, 'Status of system detail', 'Record identifier detail')

# Ready for export
data_output = data
colnames(data_output) = final_cnames
data.table::fwrite(data_output, file = 'hurr.csv')
