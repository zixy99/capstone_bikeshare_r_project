# Install packages we need
###########################
# The basic for data manipulation
install.packages("tidyverse")
library(tidyverse)

# Additional formatting ggplot2
install.packages("ggeasy")
library(ggeasy)

# Color Palette
install.packages("ggsci")  
library(ggsci)

# For mapping
install.packages("leaflet")
library( leaflet )
install.packages("geosphere")                
library(geosphere) 
install.packages("magrittr")
library( magrittr )
install.packages("htmltools")
library(htmltools)

# For cleaning data
install.packages("janitor")
library(janitor)

# Extra date manipulation
install.packages("lubridate")
library(lubridate)

# For merging and splitting large data
install.packages("plyr")
library(plyr)

# For better (faster) tables
install.packages("data.table")
library(data.table)

# For easy finding of files
install.packages("here")
library(here)

# For reporting
install.packages("knitr")
library(knitr)
install.packages("rmarkdown")
library(rmarkdown)

# For making interactive presentation
install.packages("plotly") # plots
library(plotly)
install.packages("DT") # tables
library(DT)
# Save the interactive output generated from plotly and DT as HTML widgets 
# and then insert widgets in desired place using code, <iframe src="/path/name.html"></iframe>.
install.packages("htmlwidgets")  
library(htmlwidgets)

# For finding the traces of ggplotly (refer to different parts of the chart)
install.packages("listviewer", repos = "http://cran.us.r-project.org")  
library(listviewer)
install.packages("jsonlite", repos = "http://cran.us.r-project.org")  
library(jsonlite)

# Clear R Studio environment from all objects including hidden ones.
rm(list = ls(all.names = TRUE)) 
# Free up memory and report the memory usage.
gc() 


# Create year ride observation frame
####################################
# Load rides data from the bicycle trips CSV files 
# All file from CSV sub-directory are loaded and their full path stored in a list
all_rides <- list.files(path = "./CSV/Ride Data", pattern = "*tripdata.csv", full.names = TRUE) %>%  
  # repeatedly apply read_csv to all files
  lapply(read_csv) %>% 
  # Combine data sets into one data set 
  bind_rows                                                       
# Let's take a look at few rows of data, set size and column headers
glimpse(all_rides)

# Create station info frame from all available data to try to be used later
###########################################################################
# Load stations info from the stations CSV (single) file
available_stations <- read_csv("./CSV/Station Data/Divvy_Stations_2014-Q3Q4.csv") %>% 
  select("id", "name", "latitude", "longitude")

Divvy_Trips_2020_Q1 <- read_csv("./CSV/Station Data/Divvy_Trips_2020_Q1.csv")
temp <- Divvy_Trips_2020_Q1 %>% 
  select(start_station_id, start_station_name, start_lat, start_lng) %>% 
  distinct(start_station_name, .keep_all= TRUE) %>% 
  setNames(c("id", "name", "latitude", "longitude")) %>% 
  na.omit()
temp

available_stations <- available_stations %>% rbind(temp) %>% 
  distinct(name, .keep_all= TRUE)

temp <- Divvy_Trips_2020_Q1 %>% 
  select(end_station_id, end_station_name, end_lat, end_lng) %>% 
  distinct(end_station_name, .keep_all= TRUE) %>% 
  setNames(c("id", "name", "latitude", "longitude")) %>% 
  na.omit()
temp

available_stations <- available_stations %>% rbind(temp) %>% 
  distinct(name, .keep_all= TRUE)

temp <- all_rides %>% 
  select(start_station_id, start_station_name, start_lat, start_lng) %>% 
  distinct(start_station_name, .keep_all= TRUE) %>% 
  setNames(c("id", "name", "latitude", "longitude")) %>% 
  na.omit()
temp

available_stations <- available_stations %>% rbind(temp) %>% 
  distinct(name, .keep_all= TRUE)

temp <- all_rides %>% 
  select(end_station_id, end_station_name, end_lat, end_lng) %>% 
  distinct(end_station_name, .keep_all= TRUE) %>% 
  setNames(c("id", "name", "latitude", "longitude")) %>% 
  na.omit()
temp

available_stations <- available_stations %>% rbind(temp) %>% 
  distinct(name, .keep_all= TRUE) %>% 
  na.omit() %>% 
  arrange(name)

rm(temp)
gc()

# Preparing the  data
#####################

# Check data for validity, clean if necessary and sort it
#########################################################

# We remove leading and trailing spaces from string fields
# and replace the empty values with NA. 
# We will also record how many NA values for essential variables are
cleaned_rides <- all_rides %>%  
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(where(is.character), ~na_if(., "")))

rm(all_rides)
gc()

num_empty_start_station_name <- sum(is.na(cleaned_rides$start_station_name))
num_empty_start_station_name
num_empty_start_station_name_and_id <- sum(is.na(cleaned_rides$start_station_name) & 
                                             is.na(cleaned_rides$start_station_id)    )
num_empty_start_station_name_and_id
num_empty_started_at <- sum(is.na(cleaned_rides$started_at))
num_empty_started_at
num_empty_ended_at <- sum(is.na(cleaned_rides$ended_at))
num_empty_ended_at

# We have columns of starting and ending date-time for each trip so we will 
# filter out end records where start is greater than end or either is NA. 
# We could have flip these to produce # correct time duration but we do not 
# know what is the source of the error in the data set so it is better to 
# eliminate these records from the analysis.

# Let's count the date-time error cases
num_date_time_errors <- sum(is.na(cleaned_rides$started_at) | 
                            is.na(cleaned_rides$ended_at) | 
                            (cleaned_rides$ended_at <= cleaned_rides$started_at))
num_date_time_errors
# Now filter out those records with date-time errors
cleaned_rides <- filter(cleaned_rides, !is.na(started_at) & !is.na(ended_at) & 
                          (ended_at > started_at))
glimpse(cleaned_rides)

# Check for duplicates based on ride _id since ride_id supposed to be unique.
# we first record how many duplicates and then remove them
num_duplicates <- cleaned_rides %>% get_dupes(ride_id) %>% tally()
cleaned_rides <- distinct(cleaned_rides, ride_id, .keep_all= TRUE)

# We verified no duplication of ride records and empty fields were handled.
# Sorting the dataset by the start_at date-time from earliest to newest.
cleaned_rides <- arrange(cleaned_rides, started_at)
glimpse(cleaned_rides)

num_cleaned_rides <- nrow(cleaned_rides)
num_cleaned_rides

# The data is cleaned of duplicates, missing values, 
# errors and formatting issues so we proceed to analysis
########################################################

# Add new useful calculated columns: 
# start trip day of the week and trip duration in minutes.
# Note: data.table has its own wday function (R’s global scoping) 
# so work around it by prefixing the call to wday.
cleaned_rides <- cleaned_rides %>%
  mutate(started_at_date = lubridate::as_date(started_at),
         started_at_month = lubridate::month(started_at, label = TRUE, abbr = FALSE),
         started_at_week_day = lubridate::wday(started_at, label = TRUE, abbr = FALSE),
         #started_at_hour = format(started_at, format = "%I %p"),  # as a character object
         started_at_hour = hms::as.hms(started_at, "%I %p"),  # as time object HH:MM:SS
         trip_duration_minutes = round(difftime(ended_at, started_at, units = "mins"), 2)
  )
glimpse(cleaned_rides)


# Get a summary table
sum_ride_duration_min <- sum(cleaned_rides$trip_duration_minutes) 
average_ride_duration_min <- mean(cleaned_rides$trip_duration_minutes)
std_all_ride_duration <- sd(cleaned_rides$trip_duration_minutes)
all_ride_coef_of_variance <- as.numeric(std_all_ride_duration) / as.numeric(average_ride_duration_min)


total_member_rides <- sum(cleaned_rides$member_casual == "member")  
sum_member_ride_duration_min <-  sum(cleaned_rides$trip_duration_minutes[cleaned_rides$member_casual == "member"])  

average_member_ride_min <- mean(cleaned_rides$trip_duration_minutes[cleaned_rides$member_casual == "member"])
std_member_ride_duration <- sd(cleaned_rides$trip_duration_minutes[cleaned_rides$member_casual == "member"])
member_ride_coef_of_variance <- as.numeric(std_member_ride_duration) / as.numeric(average_member_ride_min)


total_casual_rides <- sum(cleaned_rides$member_casual == "casual")  
sum_casual_ride_duration_min <-  sum(cleaned_rides$trip_duration_minutes[cleaned_rides$member_casual == "casual"])  
average_casual_ride_min <- mean(cleaned_rides$trip_duration_minutes[cleaned_rides$member_casual == "casual"])  
std_casual_ride_duration <- sd(cleaned_rides$trip_duration_minutes[cleaned_rides$member_casual == "casual"])
casual_ride_coef_of_variance <- as.numeric(std_casual_ride_duration) / as.numeric(average_casual_ride_min)


total_num_rides <- c(total_member_rides, total_casual_rides, num_cleaned_rides) 

sum_ride_duration_minutes <- c(sum_member_ride_duration_min, sum_casual_ride_duration_min, sum_ride_duration_min)

avg_ride_duration_minutes <- c(average_member_ride_min, average_casual_ride_min, 
                               average_ride_duration_min) 

std_ride_duration <- c(std_member_ride_duration, std_casual_ride_duration, std_all_ride_duration) 

ride_duration_ceof_of_variance <- 
  c(member_ride_coef_of_variance, casual_ride_coef_of_variance, all_ride_coef_of_variance) 

summary_table <- data.frame(total_num_rides, 
                            sum_ride_duration_minutes,
                            round(avg_ride_duration_minutes, 2), 
                            round(std_ride_duration, 2),
                            round(ride_duration_ceof_of_variance, 2))

rownames(summary_table) <- c('Member', 'Casual', 'All')
colnames(summary_table) <- c('Total Rides', 'Total Rides Duration', 'Average Ride Duration (min)',
                             'Ride Duration STD', 'Ride Duration CV')
glimpse(summary_table)

# ratio between casual and members 
# using as.numeric since difftime object cant be divided by another difftime object
round(as.numeric(sum_casual_ride_duration_min) / as.numeric(sum_member_ride_duration_min), 2)
round(as.numeric(average_casual_ride_min) / as.numeric(average_member_ride_min), 2)
round(casual_ride_coef_of_variance / member_ride_coef_of_variance, 2)


# FYI how many unique geographical points?
unique_stations_rides_count <- plyr::count(cleaned_rides, vars=c("start_lat","start_lng"))
nrow(unique_stations_rides_count)

# Calculate and store in a tibble station info 
# and its ride frequency including NA (we can omit the na later if required)
temp1 <- cleaned_rides %>% 
  distinct(start_station_name, .keep_all= TRUE) %>% 
  select(start_station_name, start_lat, start_lng) 

# We use suppressWarning on min and max function so as not to get -Inf warning for 0 rides
temp2 <- cleaned_rides %>%
  dplyr::group_by(start_station_name) %>%
  dplyr::summarise(total_rides = n(), 
                   total_member_rides = sum(member_casual == "member"),
                   total_casual_rides = sum(member_casual == "casual"),
                   mean_duration_minutes_member = mean(trip_duration_minutes[member_casual == "member"], na.rm = TRUE), 
                   mean_duration_minutes_casual = mean(trip_duration_minutes[member_casual == "casual"], na.rm = TRUE),
                   min_duration_minutes_member = suppressWarnings(min(trip_duration_minutes[member_casual == "member"], na.rm = TRUE)), 
                   min_duration_minutes_casual = suppressWarnings(min(trip_duration_minutes[member_casual == "casual"], na.rm = TRUE)),
                   max_duration_minutes_member = suppressWarnings(max(trip_duration_minutes[member_casual == "member"], na.rm = TRUE)), 
                   max_duration_minutes_casual = suppressWarnings(max(trip_duration_minutes[member_casual == "casual"], na.rm = TRUE))
                   ) %>% 
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  mutate_all(function(x) ifelse(is.nan(x), 0, x))
station_coordinates <- merge(temp1, temp2, by = "start_station_name", sort = TRUE) %>% 
  arrange(start_station_name)
rm(temp1)
rm(temp2)
gc()
glimpse(station_coordinates)

# Now we will create several "pivot-table-like" tibbles for specific relationships
# Daily rides statistics per rider type
daily_member_type_rides <- cleaned_rides %>% 
  dplyr::group_by(started_at_date) %>%
  dplyr::summarise(total_rides = n(), 
                   total_member_rides = sum(member_casual == "member"),
                   total_casual_rides = sum(member_casual == "casual"),
                   mean_duration_minutes_member = mean(trip_duration_minutes[member_casual == "member"], na.rm = TRUE), 
                   mean_duration_minutes_casual = mean(trip_duration_minutes[member_casual == "casual"], na.rm = TRUE),
                   min_duration_minutes_member = suppressWarnings(min(trip_duration_minutes[member_casual == "member"], na.rm = TRUE)), 
                   min_duration_minutes_casual = suppressWarnings(min(trip_duration_minutes[member_casual == "casual"], na.rm = TRUE)),
                   max_duration_minutes_member = suppressWarnings(max(trip_duration_minutes[member_casual == "member"], na.rm = TRUE)), 
                   max_duration_minutes_casual = suppressWarnings(max(trip_duration_minutes[member_casual == "casual"], na.rm = TRUE))
  ) %>% 
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
  mutate_all(function(x) ifelse(is.nan(x), 0, x)) %>% 
  # When grouping by datetime field it is restored in the new tibbles as double so
  # let's reformat it as date
  mutate(started_at_date = lubridate::as_date(started_at_date))
glimpse(daily_member_type_rides)

# Week day rides statistics per rider type
month_day_member_type_rides <- ddply(cleaned_rides, c("started_at_month", "started_at_week_day", "member_casual"), summarise,
               total_rides      = length(trip_duration_minutes),
               avg_ride_minutes = round(mean(trip_duration_minutes), 2),
               min_ride_minutes = min(trip_duration_minutes),
               max_ride_minutes = max(trip_duration_minutes))
glimpse(month_day_member_type_rides)

# Station usage per member type
station_member_type_rides <- cleaned_rides %>% 
  dplyr::group_by(start_station_name, member_casual) %>% 
  dplyr::summarise(total_rides = n())
glimpse(station_member_type_rides)

# Bike type usage per member type
# Note we must specify the dplyr library for the summarise since plyr
ridetype_member_type_rides <- cleaned_rides %>% 
  dplyr::group_by(rideable_type, member_casual) %>% 
  dplyr::summarise(total_rides = n())
glimpse(ridetype_member_type_rides)

# We now proceed to visualize the various relationship and identify 
# trends, correlations, and perhaps potential causes and effects.
###################################################################

# useful colors
r_default_peach <- "#F8766D" 
dark_peach <- "#CA463D"
r_default_cyan <- "#00BFC4"
dark_cyan <- "#005F62"

# Plotting the member type total rides daily for a whole year
daily_member_type_rides_plot <- daily_member_type_rides %>%  
  ggplot(aes(x = started_at_date)) +
  geom_line(aes(y = total_member_rides), color="pink") +
  geom_smooth(aes(y = total_member_rides, color="darkred")) + 
  geom_line(aes(y = total_casual_rides), color="lightblue") +
  geom_smooth(aes(y = total_casual_rides, color="darkblue")) + 
  scale_color_manual(guide = 'legend', 
                     name='Member Type',
                     labels=c('member', 'casual'),
                     values=c('darkred' = 'darkred', 'darkblue' = 'darkblue')) + 
  labs(title = "Daily Rides Per Rider Type", x = "Date", y = "Total Rides", 
       caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.1)) +
  easy_remove_legend_title() + 
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b %y")
daily_member_type_rides_plot

# Plotting the member type average rides duration daily for a whole year
daily_member_type_rides_plot <- daily_member_type_rides %>%  
  ggplot(aes(x = started_at_date)) +
  geom_line(aes(y = mean_duration_minutes_member), color="pink") +
  geom_smooth(aes(y = mean_duration_minutes_member, color="darkred")) + 
  geom_line(aes(y = mean_duration_minutes_casual), color="lightblue") +
  geom_smooth(aes(y = mean_duration_minutes_casual, color="darkblue")) + 
  scale_color_manual(guide = 'legend', 
                     name='Member Type',
                     labels=c('member', 'casual'),
                     values=c('darkred' = 'darkred', 'darkblue' = 'darkblue')) + 
  labs(title = "Daily Ride Mena Duration in Minutes Per Rider Type", x = "Date", y = "Mean Duration in Minutes", 
       caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.1)) +
  easy_remove_legend_title() + 
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b %y")
daily_member_type_rides_plot

# Plot the hourly histogram of number of rides per rider type
hourly_member_type_rides_histogram <- cleaned_rides %>%
  ggplot(aes(x = started_at_hour, fill = member_casual)) +
  geom_histogram(position = 'identity', alpha = 0.4, bins = 24, boundary= -0.1) +
  labs(x = "Hour", y = "Total Rides",
       caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.1)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) + 
  scale_x_time(breaks=hours(seq(0,24,1)))
hourly_member_type_rides_histogram

# Plotting the member type total rides per month
month_member_type_rides_plot <- month_day_member_type_rides %>%  
  ggplot(aes(x = started_at_month, y = total_rides, fill = member_casual)) +
  geom_col() +
  labs(title = "Monthly Rides Per Rider Type", x = "Month", y = "Total Rides", 
       caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.1)) +
  easy_remove_legend_title() + 
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'))
month_member_type_rides_plot

# Plotting the member type average ride duration per month
month_member_type_mean_duration_plot <- month_day_member_type_rides %>%  
  ggplot(aes(x = started_at_month, y = avg_ride_minutes, fill = member_casual)) +
  geom_col() +
  labs(title = "Monthly Avergae Ride Duration Per Rider Type", x = "Month", y = "Average Ride Duration in Minutes", 
       caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.1)) +
  easy_remove_legend_title() + 
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'))
month_member_type_mean_duration_plot


# Plotting the member type total rides per week day
day_member_type_rides_plot <- month_day_member_type_rides %>%  
  ggplot(aes(x = started_at_week_day, y = total_rides, fill = member_casual)) +
  geom_col() +
  labs(title = "Week Day Rides Per Rider Type", x = "Week Day", y = "Total Rides", 
       caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.3)) +
  easy_remove_legend_title() + 
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'))
day_member_type_rides_plot

# Plotting the member type average ride duration per week day
day_member_type_mean_duration_plot <- month_day_member_type_rides %>%  
  ggplot(aes(x = started_at_week_day, y = avg_ride_minutes, fill = member_casual)) +
  geom_col() +
  labs(title = "Week Day Average Ride Duration Per Rider Type", x = "Week Day", y = "Total Rides", 
       caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.3)) +
  easy_remove_legend_title() + 
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'))
day_member_type_mean_duration_plot

# Plotting the member types total rides per week day faceted by months
month_day_member_type_rides_plot <- month_day_member_type_rides %>%  
  ggplot(aes(x = started_at_week_day, y = total_rides, fill = member_casual)) +
    geom_col() +
    facet_wrap(~started_at_month) +
    labs(title = "Daily Rides Per Rider Type", x = "Week Day", y = "Total Rides", 
         caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.3)) +
    easy_remove_legend_title() + 
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'))
month_day_member_type_rides_plot

# Plotting the member types total rides per bike type
ridetype_member_type_rides_plot <- ridetype_member_type_rides %>%  
  ggplot(aes(x = rideable_type, y = total_rides, fill = member_casual)) +
  geom_col() +
  geom_text(aes(label = total_rides), size = 3, position = position_stack(vjust = 0.5)) + 
  labs(title = "Bike Type Per Rider Type", x = "Bike Type", y = "Total Rides", 
       caption = "data downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html (12-27-2021)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  easy_remove_legend_title() + 
  scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'))
ridetype_member_type_rides_plot

# For the RMarkdown we will use interactive plots using plotly. 
# Unfortunately it appears that the hover over the accuracy band does not handle date.
# I need to further investigate, but meanwhile I shall prevent that by 
# finding the trace number for graphs element (lines, accuracy bands tc) 
# and we disable the hover for items such as labels and accuracy bands limiting the 
# hover only to the line parts. 
p_json <- plotly_json(ridetype_member_type_rides_plot, jsonedit = TRUE, pretty = TRUE)
p_json 


# Mapping the member types total rides per station
station_coordinates_no_na <- station_coordinates %>% drop_na(start_station_name)
station_membertype_rides_map <- leaflet() %>% addTiles() %>% 
    addCircleMarkers(data = station_coordinates_no_na,
                     lat = ~start_lat, lng = ~start_lng,
                     popup = paste(station_coordinates$start_station_name, "<br>",
                                   "member: ",station_coordinates$total_member_rides, "<br>",
                                   "casual: ", station_coordinates$total_casual_rides), 
                     color = r_default_peach,
                     radius <- findInterval(station_coordinates$total_casual_rides ,c(100,500,2000,4000,6000,8000,10000,12000,14000,16000,18000,20000)) * 2,
                     stroke = FALSE, 
                     fillOpacity = 0.8,
                     group = "casual") %>% 
    addCircleMarkers(data = station_coordinates_no_na,
                     lat = ~start_lat, lng = ~start_lng,
                     popup = paste(station_coordinates$start_station_name, "<br>",
                                   "member: ",station_coordinates$total_member_rides, "<br>",
                                   "casual: ", station_coordinates$total_casual_rides), 
                     color = r_default_cyan,
                     radius <- findInterval(station_coordinates$total_member_rides ,c(100,500,2000,4000,6000,8000,10000,12000,14000,16000,18000,20000)) * 2,
                     stroke = FALSE, 
                     fillOpacity = 0.4,
                     group = "member") %>%
    # Legend Layer
    addLegend("bottomright", colors= c(r_default_cyan), labels=c("member"), group = "member", layerId = "member") %>% 
    addLegend("bottomright", colors= c(r_default_peach), labels=c("casual"), group = "casual", layerId = "casual") %>% 
    # Layers control
    addLayersControl(
      baseGroups = c("OSM (default)"),
      overlayGroups = c("member", "casual"),
      options = layersControlOptions(collapsed = FALSE)
    )
station_membertype_rides_map
