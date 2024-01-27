library(tidyverse)
library(jsonlite)
library(spotifyr)

access_token <- get_spotify_access_token()

spotify_history <- dir("./Extended Streaming History", pattern = "*.json") %>%
  map_df(~fromJSON(file.path("./Extended Streaming History", .), flatten = TRUE)) %>%
  mutate(spotify_track_uri = gsub("spotify:track:", "", spotify_track_uri)) %>%
  distinct()

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

spotify_top_tracks <- spotify_history %>%
  drop_na(master_metadata_track_name) %>%
  group_by(master_metadata_track_name, master_metadata_album_artist_name, master_metadata_album_album_name, spotify_track_uri) %>%
  summarise("count" = n()) %>%
  select(master_metadata_track_name, count, master_metadata_album_artist_name, master_metadata_album_album_name, spotify_track_uri) %>%
  filter(count > 25) %>%
  mutate(spotify_track_uri = gsub("spotify:track:", "", spotify_track_uri)) %>%
  drop_na(spotify_track_uri) %>%
  track_info_merge()

spotify_log_merge <- merge(spotify_history, spotify_top_tracks, by = "spotify_track_uri") %>%
  mutate(ts = as.Date(ts, tz = "%d/%m/%Y %h:%m:%s")) %>%
  mutate(year = format(as.Date(ts, format ="%d/%m/%Y"),"%Y"))

median_yearly_length <- spotify_log_merge %>%
  mutate(year = format(as.Date(ts, format ="%d/%m/%Y"),"%Y")) %>%
  group_by(year) %>%
  summarise("median_length" = median(track.duration_ms) / 1000 / 60,
            "mean_length" = mean(track.duration_ms) / 1000/ 60) %>%
  filter(year != 2024 & year != 2015)

ggplot() +
  geom_line(data = median_yearly_length, aes(x = year, y = median_length, group = 1, color = "Median Song Length")) +
  geom_line(data = median_yearly_length, aes(x = year, y = mean_length, group = 1, color = "Mean Song Length")) +
  geom_boxplot(data = spotify_log_merge %>% 
                filter(year != 2024 & year != 2015) %>% 
                mutate(track.duration_ms = track.duration_ms / 1000 / 60) %>%
                filter(track.duration_ms < 50), 
             aes(x = year, y = track.duration_ms), color = "grey", alpha = 0.25)
