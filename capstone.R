# https://divvy-tripdata.s3.amazonaws.com/index.html

library(xml2)
library(tidyverse)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

fpath <- 'https://divvy-tripdata.s3.amazonaws.com/'
get_filename <- function(x) {
  x$Key[[1]][[1]]
}

files_list <- as_list(read_xml(fpath))
filenames_list <- unlist(sapply(files_list$ListBucketResult, get_filename))

trips <- data.frame()
counter <- 1

for (filename in filenames_list) {
  if (str_detect(filename, 'zip') & (str_detect(filename, '2021') | str_detect(filename, '2022'))) { # Get data for 2021 and 2022 only
    temp <- tempfile()
    print(paste('Processing file', counter, 'of', length(filenames_list)))
    download.file(paste0(fpath, filename), temp)
    trips <- rbind(trips, read_csv(temp))
    counter <- counter + 1
    unlink(temp)
  }
}

saveRDS(trips, file = 'biketrips.rds')
trips <- readRDS('biketrips.rds')

trips_clean <- trips %>% 
  mutate(trip_duration = as.numeric(difftime(ended_at, started_at))) %>% 
  filter(!is.na(trip_duration)) %>% 
  filter(trip_duration > 29 & trip_duration < 60*60*10) %>%    # drop trips less than 30 seconds and more than 10 hours
  filter(started_at > max(started_at) - days(365)) %>%  # last 180 days only due to memory limits
  mutate(trip_duration_bin = case_when(
    trip_duration < 300 ~ '1: < 5 mins',
    trip_duration >= 300 & trip_duration < 900 ~ '2: 5-15 mins',
    trip_duration >= 900 & trip_duration < 1800 ~ '3: 15-30 mins',
    trip_duration >= 1800 & trip_duration < 3600 ~ '4: 30-60 mins',
    trip_duration >= 3600 & trip_duration < 7200 ~ '5: 1-2 hours',
    trip_duration >= 7200 ~ '6: > 2 hours'
  )) # set bins for trip durations

rm(trips) # clean memory

table(wday(trips_clean$started_at, label = TRUE, locale = 'en_US.UTF8'), trips_clean$member_casual)

ggplot(data = trips_clean, aes(x = wday(started_at, label = TRUE, locale = 'en_US.UTF8'), fill = trip_duration_bin)) +
  geom_bar() +
  facet_wrap(~member_casual) +
  scale_y_continuous(labels = number) +
  xlab('') +
  ylab('Rides count') +
  labs(caption = 'Source: Cyclistic bike-share dataset, last 365 days usage')

ggplot(data = trips_clean, aes(x = trip_duration_bin)) + 
  geom_histogram(stat = 'count') +
  facet_wrap(~member_casual + rideable_type) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = number) +
  xlab('Trip duration bin') +
  ylab('Rides') +
  labs(title = 'Number of rides by duration and rider type', caption = 'Source: Cyclistic bike-share dataset, last 365 days usage')
