library(tidyverse)
library(jsonlite)

streaming_history <- rbind(as.data.frame(fromJSON("Spotify Account Data//StreamingHistory0.json")),
                           as.data.frame(fromJSON("Spotify Account Data//StreamingHistory1.json")),
                           as.data.frame(fromJSON("Spotify Account Data//StreamingHistory2.json")),
                           as.data.frame(fromJSON("Spotify Account Data//StreamingHistory3.json")))

streaming_history %>%
  filter(artistName != "5-4" &
         artistName != "Phillies Therapy" &
         artistName != "High Hopes: A Phillies Podcast" &
         artistName != "Well There‘s Your Problem" &
         artistName != "Hittin' Season: A Philadelphia Phillies podcast" &
         artistName != "Talkin' Baseball (MLB Podcast)" &
         artistName != "New Heights with Jason and Travis Kelce" &
         artistName != "Dare to Lead with Brené Brown") %>%
  mutate(trackName = as.factor(trackName),
         artistName = as.factor(artistName)) %>%
  group_by(artistName) %>%
  reframe("time" = (sum(msPlayed) / 1000) / 60,
          "artist" = artistName) %>%
  distinct() %>%
  View()

