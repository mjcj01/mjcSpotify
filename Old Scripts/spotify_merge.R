library(tidyverse)
library(jsonlite)
library(spotifyr)

access_token <- get_spotify_access_token()

df <- as.data.frame(fromJSON("Extended Streaming History//Streaming_History_Audio_2015-2017_0.json")) %>%
  mutate(spotify_track_uri = gsub("spotify:track:", "", spotify_track_uri)) %>%
  drop_na(spotify_track_uri)

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

track_info_extract("6iMjntVYkchLBJcqsiSNYZ")

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

df_merge <- track_info_merge(df)

df_merge %>%
  mutate(ts = gsub("T", " ", ts),
         ts = gsub("Z", "", ts),
         ts = as.POSIXct(ts)) %>%
  ggplot(data = ., aes(x = ts, y = track.duration_ms)) +
           geom_point()

         