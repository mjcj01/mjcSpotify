library(tidyverse)
library(jsonlite)
library(spotifyr)

### Reading in access code to access Spotify Web API to get additional track information
### NOTE: You do not need to use the Web API to use your data. The API gives you access
### to track information, like how long the song is, how popular it is, and other stuff.
### Check out https://www.rcharlie.com/spotifyr/ for getting started with the spotifyr
### package, which is used to access the Web API.
access_token <- get_spotify_access_token()

### Reading in Extended Streaming History folder. This method of reading in files is helpful because
### there were 10 files, so this reads in all .json files, which makes it so I don't have to individually
### read in 10 files.
spotify_history <- dir("./Extended Streaming History", pattern = "*.json") %>%
  map_df(~fromJSON(file.path("./Extended Streaming History", .), flatten = TRUE)) %>%
  mutate(spotify_track_uri = gsub("spotify:track:", "", spotify_track_uri)) %>%
  distinct()

### Function to take track URIs (Spotify's name for track IDs) and extract information.
### Each iteration of get_track() returns a list different types of data structures 
### (e.g., strings, data frames), which are difficult to use with lapply(). This
### function is a workaround since a lot of the more complicated to extract information
### is already contained in the extended streaming history files.
track_info_extract <- function(uri) {
  tryCatch(
    {track <- get_track(id = uri)
    data.frame(track$duration_ms, track$is_local, track$name, track$popularity, track$uri) %>%
      mutate(track.uri = gsub("spotify:track:", "", track.uri)) %>%
      rename(spotify_track_uri = "track.uri")},
    error = function(cond) {
      message(paste("Error while running the following URI:", uri))
      message("Original error message:")
      message(conditionMessage(cond))},
    warning = function(cond) {
      message(paste("Warning while running the following URI:", uri))
      message("Original warning message:")
      message(conditionMessage(cond))},
    finally = function(cond) {
      message(paste("Processed URI:", uri))}
  )
}

### Function to take the data frame created in line 13 and use track_info_tract().
### This will merge the input data frame with track info pulled using the above
### function.
track_info_merge <- function(tracks_df) {
  tracks_df %>%
    mutate(spotify_track_uri = gsub("spotify:track:", "", spotify_track_uri)) %>%
    drop_na(spotify_track_uri)
  uri_list <- tracks_df$spotify_track_uri %>%
    unique()
  track_info_lapply <- lapply(uri_list, track_info_extract) %>%
    bind_rows()
  merge(tracks_df, track_info_lapply, by = "spotify_track_uri")
}

### Data frame that has songs that have been played a certain number of times.
### Because the extended history has *every* song ever played, it has data on songs
### played only 1 time ever; this data frame excludes those. It then uses track_info_merge()
### to pull in additional track info. This is also designed to be merged back with the larger 
### log, as it only has 1 row per song, not 1 row per time the song was played.
spotify_top_tracks <- spotify_history %>%
  drop_na(master_metadata_track_name) %>%
  group_by(master_metadata_track_name, master_metadata_album_artist_name, master_metadata_album_album_name, spotify_track_uri) %>%
  summarise("count" = n()) %>%
  select(master_metadata_track_name, count, master_metadata_album_artist_name, master_metadata_album_album_name, spotify_track_uri) %>%
  filter(count > 25) %>%
  mutate(spotify_track_uri = gsub("spotify:track:", "", spotify_track_uri)) %>%
  drop_na(spotify_track_uri) %>%
  track_info_merge()

### This merges the top tracks data frame with the extended history data frame initially loaded.
### It will only keep rows for the tracks contained in the top tracks data frame.
### This also converts the "ts" column to a date and creates a year column.
spotify_log_merge <- merge(spotify_history, spotify_top_tracks, by = "spotify_track_uri") %>%
  mutate(ts = as.Date(ts, tz = "%d/%m/%Y %h:%m:%s")) %>%
  mutate(year = format(as.Date(ts, format ="%d/%m/%Y"),"%Y")) %>%
  mutate(nchar = nchar(master_metadata_track_name.x))

### Creates a data frame with the median and mean lengths of songs and song title lengths per year, excluding 2015 and 2024
### due to less than 1 month of data in those years.
yearly_length <- spotify_log_merge %>%
  mutate(year = format(as.Date(ts, format ="%d/%m/%Y"),"%Y")) %>%
  group_by(year) %>%
  summarise("median_song_length" = median(track.duration_ms) / 1000 / 60,
            "mean_song_length" = mean(track.duration_ms) / 1000/ 60,
            "median_title_length" = median(nchar),
            "mean_title_length" = mean(nchar)) %>%
  filter(year != 2024 & year != 2015)

### Test plot 
spotify_history %>% 
  mutate(ts = as.Date(ts, tz = "%d/%m/%Y %h:%m:%s")) %>% 
  drop_na(master_metadata_track_name) %>% 
  mutate(year = format(as.Date(ts, format ="%d/%m/%Y"),"%Y"), 
         month = format(as.Date(ts, format ="%d/%m/%Y"),"%m")) %>% 
  group_by(month, year) %>% 
  summarise("day" = 1,
            "minutes_played" = sum(ms_played) / 1000 / 60,
            "count" = n()) %>%
  mutate("date" = as.Date(paste(year, "-", month, "-", day, sep = ""), tz = "%Y-%m-%d")) %>%
  ggplot(data = ., aes(x = count, y = minutes_played)) +
  geom_point()

### Plot showing mean song lengths and boxplots per year
ggplot() +
  geom_violin(data = spotify_log_merge %>% 
                filter(year != 2024 & year != 2015), #%>% 
                #mutate(track.duration_ms = track.duration_ms / 1000 / 60) %>%
                #filter(track.duration_ms < 50), 
             aes(x = year, y = nchar), color = "grey", alpha = 0.25, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_line(data = yearly_length, aes(x = year, y = median_title_length, group = 1, color = "Median Song Length"), linewidth = 1) +
  geom_point(data = yearly_length, aes(x = year, y = median_title_length, group = 1, color = "Median Song Length"), shape = 16, size = 2) +
  geom_line(data = yearly_length, aes(x = year, y = mean_title_length, group = 1, color = "Mean Song Length"), linewidth = 1) +
  geom_point(data = yearly_length, aes(x = year, y = mean_title_length, group = 1, color = "Mean Song Length"), shape = 16, size = 2) +
  #geom_text(data = ) +
  labs(title = "Length of Songs Listened to From 2016 - 2023",
       subtitle = "Calculated using data from Spotify's Extended Streaming History",
       x = "Year",
       y = "Length of Songs (in minutes)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "white"))

ggsave(filename = "test.jpeg", width = 6, height = 4.2, device = 'tiff', dpi = 300)
