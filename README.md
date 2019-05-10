---
title: "Rabeony-car316-FinalProject"
author: "Christopher Rabeony"
date: "5/6/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
library(tidyverse)
```

<style>
pre {
  overflow-x: auto;
}
pre code {
  word-wrap: normal;
  white-space: pre;
}
</style>


```{r, include=FALSE}
library(tidyverse)
library(lubridate)
library(dplyr)
library(spotifyr)
library(ggjoy)
library(readr)
library(knitr)
library(kableExtra)
library(wordcloud)
library(wordcloud2)
library(purrr)
library(magrittr)
library(rvest)
library(stringr)
```


## Spotify
```{r echo = FALSE, out.width = '60px', fig.align= "center"}
knitr::include_graphics('spotifyLogo.png')
```

Background Information: Spotify is a Swedish audio streaming platform that provides music and podcasts from record labels and media companies.

My data source for this project will be extracted from the 'Spotify Top 200' weekly streaming data. The url for this website is <https://spotifycharts.com/regional/>.

- There are two methods that we could use to read this information into RStudio:
    1. Reading in the csv file that is attached to the website (Unfortunately doesnt keep our information up to date).
    2. Use web-scraping with the 'rvest' package to extract the constantly updated data table directly from the URL.

```{r, include = FALSE}
get_tbl <- function(url) {
  url %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    data.frame() }
```

```{r, include = FALSE}
USA <- get_tbl("https://spotifycharts.com/regional/us/weekly/latest")
head(USA, 3)
```


```{r, include=FALSE}
#Cleaning up U.S top 200
#Because tables imported from webpages usually need cleaning up.
clean_df <- function(df) { {
  df1 <- df %>%
    select(-c(1,3)) %>%
    separate(Track, c("Track" , "Artist"), sep = " by ") %>%
    subset(!is.na(Artist))
  df1$Track <- str_replace_all(df1$Track, "[\r\n]" , "")
  df1$Streams <- str_replace_all(df1$Streams, ",", "") %>%
    as.numeric(as.character())
  colnames(df1)[1] <- "Code" }
  return(df1) }
```

```{r, include = FALSE}
spotify_USA <- clean_df(USA)
spotify_USA$Code <- str_replace_all(spotify_USA$Code, "\\d{1,3}", "NA")
head(spotify_USA, 4)

# The data we extracted above lists the top 200 songs streamed for a given country. We are given the name of each song, the artist involved in its creation, the number of total streams, and its rank in the top 200. I created a column called "Code" which displays the continent that each our data table represents. For the USA, table we use "NA" (North America)
```


```{r, include = FALSE}
AR <- get_tbl("https://spotifycharts.com/regional/ar/weekly/latest")
spotify_AR <- clean_df(AR)
spotify_AR$Code <- str_replace_all(spotify_AR$Code, "\\d{1,3}", "SA")
```

```{r, include = FALSE}
head(spotify_AR, 2)
```

```{r, include = FALSE}
BO <- get_tbl("https://spotifycharts.com/regional/bo/weekly/latest")
spotify_BO <- clean_df(BO)
spotify_BO$Code <- str_replace_all(spotify_BO$Code, "\\d{1,3}", "SA")
```

```{r, include = FALSE}
head(spotify_BO, 2)
```

```{r, include = FALSE}
UK <- get_tbl("https://spotifycharts.com/regional/gb/weekly/latest")
spotify_UK <- clean_df(UK)
spotify_UK$Code <- str_replace_all(spotify_UK$Code, "\\d{1,3}", "EU")
```

```{r, include = FALSE}
head(spotify_UK, 2)
```

```{r, inclue = FALSE}
BE <- get_tbl("https://spotifycharts.com/regional/be/weekly/latest")
spotify_BE <- clean_df(BE)
spotify_BE$Code <- str_replace_all(spotify_BE$Code, "\\d{1,3}", "EU")
```

```{r, include = FALSE}
head(spotify_BE, 2)
```

```{r, include = FALSE}
AU <- get_tbl("https://spotifycharts.com/regional/au/weekly/latest")
spotify_AU <- clean_df(AU)
spotify_AU$Code <- str_replace_all(spotify_AU$Code, "\\d{1,3}", "AUS")
```

```{r, include = FALSE}
head(spotify_AU, 4)
```

```{r, include = FALSE}
spotify_NA <- spotify_USA
spotify_AUS <- spotify_AU
spotify_SA <- rbind(spotify_AR, spotify_BO) %>%
  group_by(Code, Track, Artist) %>%
  summarise(Streams = sum(Streams)) %>%
  arrange(desc(Streams))
spotify_EU <- rbind(spotify_UK, spotify_BE) %>%
  group_by(Code, Track, Artist) %>%
  summarise(Streams = sum(Streams)) %>%
  arrange(desc(Streams))
```


## Dataframe for Global Streams
```{r, include = FALSE}
spotify_Global <- bind_rows(spotify_EU, spotify_NA, spotify_AUS, spotify_SA) %>%
  arrange(desc(Streams)) %>%
  group_by(Code)
```

```{r}
head(spotify_Global)
```

One data frame can provide a plethora of information and can provide answers a lot of questions.

* The United States is dominating the streaming numbers!

## Total Streams for each Continent
```{r,include = FALSE}
continentStreams <- ggplot(spotify_Global, aes(Code, Streams, color = Code)) +
  geom_boxplot(lwd = 2) + scale_y_log10() + coord_flip() + labs(title = "Top 200 Streams Globally") 
```

```{r}
continentStreams 
```

```{r, include = FALSE}
streamDist <- spotify_Global %>%
  group_by(Code) %>%
  summarise(totalStreams = sum(Streams)) %>%
  arrange(desc(totalStreams))
streamDist
```

## Distribution of Total Streams.
Let's visualize the distribution of total streams for the last week.
```{r}
streamDist
```
Why is streaming so much higher in North America compared to Europe, even though Spotify was founded in Sweden, and released first in the United Kingdom?

```{r, include = FALSE}
Stream_circle <- ggplot(streamDist, aes("", totalStreams, fill = Code)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start=0) 
```

## Pie Chart of total Streams
```{r}
Stream_circle
```

```{r, include = FALSE}
spotify_topSongs <- spotify_Global %>%
  arrange(desc(Streams)) %>%
  group_by(Code) %>% slice(1:3)
```

```{r, include = FALSE}
spotify_topArtist <- spotify_Global %>%
  group_by(Code, Artist) %>%
  summarise(Streams = sum(Streams))
spotify_topArtist <- spotify_topArtist %>%
  arrange(desc(Streams)) %>%
  group_by(Code) %>% slice(1:5)
head(spotify_topArtist)
```

```{r, include = FALSE}
eachCountry <- ggplot(spotify_topSongs, aes(Code, Streams, fill = Track)) +
  geom_bar(position = "dodge", stat = "identity") + labs(title = "Top Songs in each Continent")
```

## Find the top 3 songs in each continent.
```{r, fig.align = "center", fig.width = 11}
eachCountry
```
Note: There's a lot of crossover when it comes to artists and their international audiences.

```{r, include = FALSE}
spotify_globalSongs <- spotify_Global %>%
  group_by(Track, Artist) %>%
  summarise(Streams = sum(Streams)) %>%
  arrange(desc(Streams))
```

## Popular Artist for Each Continent
```{r}
ggplot(spotify_topArtist, aes(Code, Streams, fill = Artist)) +
  geom_bar(position = "dodge", stat = "identity") + labs(title = "Top Artist in each Continent")
```

## Finding and Analyzing Aggregate Data
Now that we have looked at streaming information for each continent closely, let's look at our overall Global information.
```{r}
head(spotify_globalSongs, 6)
```
Create a graphic representation of this data frame above. Graphing the overall top 200 songs streamed on the Spotify website.

```{r, include = FALSE}
popularSongs <- ggplot(head(spotify_globalSongs, 10), aes(Track, Streams, fill = Artist)) + geom_bar(stat="identity") + scale_fill_brewer(palette="Blues") + labs(title = "The World's Most Popular Songs") +  theme(axis.text.x = element_text(angle = 90, hjust = 0.1))
```

## Finding and Analyzing Aggregate Data
```{r, fig.width = 9}
popularSongs
```

## The 'spotifyr' Package
Another method I would like to integrate into my project is the use of the Spotify Developer Tools Web API. The method to retrieving an API key is free, and simple once an account is made.
```{r}
library(spotifyr)
```
The spotifyr package pulls a variety of audio features from Spotify's Web Api. Once we obtain the web key, and authorization we can retrieve a variety of information in seconds.

```{r}
spotify_client_id <- source("/Users/chris/Documents/R/api-keysSpotify.R")
```

```{r, include = FALSE}
Sys.setenv(SPOTIFY_CLIENT_ID = api.key.spotify)
Sys.setenv(SPOTIFY_CLIENT_SECRET = api.spotify.clientID)
access_token <- get_spotify_access_token()
```

## The most streamed songs Globally
```{r}
head(spotify_globalSongs)
```
Lets take the top 5 artists on this list and find out if there's something that in their music that make their songs the most popular in the world.

## Top 50
Use the Spotify API to find the top 50 most streamed artists in the world.
```{r, include = FALSE}
spotify_globalArtists <- spotify_Global %>%
  group_by(Artist) %>%
  summarise(Streams = sum(Streams)) %>%
  arrange(desc(Streams))
head(spotify_globalArtists)
spotifyTop50 <- head(spotify_globalArtists$Artist, 50)
```

##  Characteristics of the top Songs
```{r, include = FALSE}
artist_audio_features <- map_df(spotifyTop50, function(artist) {
    search_spotify(artist, "track") %>%
    mutate(artist_name = artist)
})
```

```{r, include = FALSE}
spotifyFilter <- select(artist_audio_features, artist_name, id, name, popularity, album.release_date)
spotifyFilter$album.release_date <- as.Date(spotifyFilter$album.release_date, format = "%Y-%m-%d")
spotifyFilter1 <- subset(spotifyFilter, album.release_date > "2018-10-01")
```

```{r}
spotifytopInformation <- spotifyFilter1 %>% group_by(artist_name) %>% arrange(desc(popularity)) %>% slice(1)
head(spotifytopInformation)
```
Above is our most popular songs, by our top artists.

## Top 50 audio tracks
```{r, include = FALSE}
spotifytrackInfo <- spotifytopInformation$id
spotifytrackFeatures <- get_track_audio_features(spotifytrackInfo)
spotifytrackAnalysis <- get_tracks(spotifytrackInfo) %>% select(9,7,10)
```

```{r, include = FALSE}
trackInformation <- spotifytrackAnalysis %>%
  left_join(spotifytrackFeatures, by = "id")
```

```{r}
head(trackInformation, 2)
```
The Spotify for Developers App does a great job analyzing the musical characteristics for each and every song. These features inclue a songs, "danceability", "tempo", "liveliness", "energy", and its use of "acoustics"

## Linear Model for Popularity
```{r}
topSongs.lm <- lm(formula = popularity ~ acousticness + liveness + energy + valence + loudness + tempo, data = trackInformation)
```

```{r}
coefficients(topSongs.lm)
```

## Linear Model for Popularity
```{r}
summary(topSongs.lm)
```

## My Thoughts
Based on the information I've presented. There really isn't a conclusion that can be properly drawn. There doesn't seem to be any correlation between the popularity of any given song, and its features. 

## Graphing Representation
```{r, include = FALSE}
trackInfo <- gather(trackInformation, 'danceability':'tempo', key = 'characteristic', value = 'value')
```

```{r}
ggplot(trackInfo, aes(value, popularity)) + geom_point() + facet_wrap(~characteristic, ncol = 5, scales = "free_x")
```

## The top 10 most streamed artists in the world.
```{r, include = FALSE}
spotify_globalArtists <- spotify_Global %>%
  group_by(Artist) %>%
  summarise(Streams = sum(Streams)) %>%
  arrange(desc(Streams))
```

```{r}
head(spotify_globalArtists, 10)
```

```{r, include = FALSE}
mostStreamed <- ggplot(head(spotify_globalArtists, 10), aes(Artist, Streams, fill = Artist)) + geom_bar(stat="identity") + coord_flip() + labs(title = "Top 5 Popular Artists") + theme(axis.title = element_text(size = 30)) + theme(axis.text = element_text(size = 30)) + theme(legend.position = 'none') + theme(plot.title = element_text(size = 35)) + theme(axis.title.y = element_text(size = 30)) + theme(axis.title.x = element_text(size = 30))
```

```{r}
mostStreamed
```

## A Successful Discography
Having multiple songs in the top 200.

Counting how many songs an artist has in the global top 200.
```{r, include = FALSE}
spotify_globalAppearances <- spotify_globalSongs %>%
  group_by(Artist) %>%
  count(Artist, sort = TRUE )
```

```{r}
head(spotify_globalAppearances, 5)
```

## Create a Word Cloud
```{r, fig.align="center", fig.asp=0.62, message=FALSE, warning=FALSE, paged.print=FALSE, paged.print=FALSE}
spotify_globalAppearances %>%
with(wordcloud(words = Artist, n, max.words = 30, random.order = FALSE, colors = brewer.pal(8, "Dark2")))
```

##  Characteristics for the Most Popular Artist
Since we couldn't figure the characteristics that create a popular song, then let's find what creates a popular artist.

Using the information we've received from our wordCloud let's how our artists use their music to capture their listeners.

Let's use the 'spotifyr' package once again to retrieve information on discography for some of the more popular artists.

Let's create a graph that will compare the energy (intensity) and valence (emotion) for each of our artists.

```{r, include = FALSE}
getArtist_Information <- function(artist) {
  get_artist_audio_features(artist) %>%
  select(c(1,4,6,9:19,22,26,30,32,33,36:39))
}
```
## For example:
```{r}
Billie_Eilish <- getArtist_Information('billie eilish')
head(Billie_Eilish, 2)
```

## Billie Eilish (Pop)
```{r, include = FALSE}
getEnergy_graph <- function(df) {
  ggplot(df, aes(valence, energy, color = album_name)) +
  geom_point(size = 5) +
  lims(x=c(0,1.0),y=c(0,1.0)) + 
  geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
  annotate("text", x = c(0.20, 0.09, 0.90, 0.82), y = c(0.0, 1.0, 0.0, 1.0), label = c("Sad/Depressing", "Dark/Angry", "Calm/Peaceful", "Happy/Energetic"), size = 5.0) + theme(legend.position = "bottom")}
```

```{r} 
BillieEilish_Energy <- getEnergy_graph(Billie_Eilish)
BillieEilish_Energy
```

## PNL (French Rap)
```{r, include = FALSE}
PNL <- getArtist_Information('pnl')
head(PNL, 5)
```

```{r} 
PNL_Energy <- getEnergy_graph(PNL)
PNL_Energy
```

## SchoolBoy Q (Rap)
```{r, include = FALSE}
SchoolBoy_Q <- getArtist_Information('schoolboy q')
head(SchoolBoy_Q, 5)
```

```{r} 
SchoolBoy_Energy <- getEnergy_graph(SchoolBoy_Q)
SchoolBoy_Energy
```

## Sebastian Yatra (Latin)
```{r, include = FALSE}
Sebastian_Yatra <- getArtist_Information('sebastian yatra')
head(Sebastian_Yatra, 5)
```

```{r} 
Sebastian_Energy <- getEnergy_graph(Sebastian_Yatra)
Sebastian_Energy
```

## Post Malone (Pop)
```{r, include = FALSE}
Post_Malone <- getArtist_Information('post malone')
head(Post_Malone, 5)
```

```{r} 
PostMalone_Energy <- getEnergy_graph(Post_Malone)
PostMalone_Energy
```

## Khalid (R&B)
```{r, include = FALSE}
Khalid <- getArtist_Information('khalid')
head(Khalid, 5)
```

```{r}
Khalid_Energy <- getEnergy_graph(Khalid)
Khalid_Energy
```

## Concluding Thoughts
We can somewhat make a conclusion that some of the most popular music in this day and age, are low intensity sounds, with very dark material.

However the biggest point I want to make is:
Music tastes aren't objective.

A lot of our enjoyment in music comes from our socioeconomic backgrounds, how our environment has influenced us, and what's readily available for us to listen to.
