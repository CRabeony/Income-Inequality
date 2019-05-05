# Projects - 001
Final Project Spring
========================================================
author: Christopher Rabeony
date: 4/28/2019
autosize: true
```{r echo = FALSE, out.width = '100%'}
knitr::include_graphics('spotifyLogo.png')
```

```{r, include=FALSE}
library(tidyverse)
library(lubridate)
library(dplyr)
library(spotifyr)
library(ggjoy)
library(readr)
library(wordcloud)
library(wordcloud2)
library(purrr)
library(magrittr)
library(rvest)
library(stringr)
```

Spotify
========================================================
Background Information: Spotify is a Swedish audio streaming platform that provides music and podcasts from record labels and media companies.

  <br />

<font size = "10"> Introduction to my Project</font>

My data source for this project will be extracted from the 'Spotify Top 200' weekly streaming data. The url for this website is <https://spotifycharts.com/regional/>.

- There are two methods that we could use to read this information into RStudio:
    1. Reading in the csv file that is attached to the website (Unfortunately doesnt keep our information up to date).
    2. Use web-scraping with the 'rvest' package to extract the constantly updated data table directly from the URL.

  <br />
    
<font size = "10"> Six Different Countries</font>

Spotify not only includes streaming information from the United States, but from other countries as well. For this project we will include data from the United States, Argentina, Bolivia, the United Kingdom, Belgium, and Australia.

For a lot of our data extraction and cleaning for each of our countries I will be using functions to cut out a lot of unnecessary code.

Starting with the United States top 200
========================================================

Specify and read in the URL. Extract the table node and select the first table as a data frame.
```{r, include = FALSE}
get_tbl <- function(url) {
  url %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    data.frame() }
```

```{r}
USA <- get_tbl("https://spotifycharts.com/regional/us/weekly/latest")
head(USA, 3)
```

Cleaning up U.S top 200
========================================================
Because tables imported from webpages usually need cleaning up.
```{r, include=FALSE}
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

```{r}
spotify_USA <- clean_df(USA)
spotify_USA$Code <- str_replace_all(spotify_USA$Code, "\\d{1,3}", "NA")
head(spotify_USA, 4)
```

The data we extracted above lists the top 200 songs streamed for a given country. We are given the name of each song, the artist involved in its creation, the number of total streams, and its rank in the top 200. I created a column called "Code" which displays the continent that each our data table represents. For the USA, table we use "NA" (North America)

International information.
========================================================
Now to include the information of the other five countries in the same format.

Argentina (South America)
```{r, include = FALSE}
AR <- get_tbl("https://spotifycharts.com/regional/ar/weekly/latest")
spotify_AR <- clean_df(AR)
spotify_AR$Code <- str_replace_all(spotify_AR$Code, "\\d{1,3}", "SA")
```

```{r}
head(spotify_AR, 2)
```

Bolivia (South America)
```{r, include = FALSE}
BO <- get_tbl("https://spotifycharts.com/regional/bo/weekly/latest")
spotify_BO <- clean_df(BO)
spotify_BO$Code <- str_replace_all(spotify_BO$Code, "\\d{1,3}", "SA")
```

```{r}
head(spotify_BO, 2)
```

========================================================
United Kingdom (Europe)
```{r, include = FALSE}
UK <- get_tbl("https://spotifycharts.com/regional/gb/weekly/latest")
spotify_UK <- clean_df(UK)
spotify_UK$Code <- str_replace_all(spotify_UK$Code, "\\d{1,3}", "EU")
```

```{r}
head(spotify_UK, 2)
```

```{r}
BE <- get_tbl("https://spotifycharts.com/regional/be/weekly/latest")
spotify_BE <- clean_df(BE)
spotify_BE$Code <- str_replace_all(spotify_BE$Code, "\\d{1,3}", "EU")
```

Belgium (Europe)
```{r}
head(spotify_BE, 2)
```

========================================================
Finally Australia (Australia)
```{r, include = FALSE}
AU <- get_tbl("https://spotifycharts.com/regional/au/weekly/latest")
spotify_AU <- clean_df(AU)
spotify_AU$Code <- str_replace_all(spotify_AU$Code, "\\d{1,3}", "AUS")
```

```{r}
head(spotify_AU, 4)
```

  <br />

<font size = "10"> Next Step: Data Manipulation and Representation</font>

With our datasets now imported and cleaned we can now manipulate our data to reveal new information.

Creating a dataframe that represents Global Streams
========================================================
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

```{r}
spotify_Global <- bind_rows(spotify_EU, spotify_NA, spotify_AUS, spotify_SA) %>%
  arrange(desc(Streams)) %>%
  group_by(Code)
spotify_Global
```

One data frame can provide a plethora of information and can provide answers a lot of questions.

  <br />

* The United States is dominating the streaming numbers!

Representing the Overall Number of Spotify listeners across each Continent.
========================================================
```{r,include = FALSE}
continentStreams <- ggplot(spotify_Global, aes(Code, Streams, color = Code)) +
  geom_boxplot(lwd = 3) + scale_y_log10() + coord_flip() + labs(title = "Top 200 Streams Globally") + theme(axis.title = element_text(size = 25)) + theme(axis.text = element_text(size = 25)) + theme(legend.text = element_text(size = 25)) + theme(legend.title = element_text(size = 17)) + theme(plot.title = element_text(size = 30)) + theme(axis.title.y = element_text(size = 25)) + 
  theme(axis.title.x = element_text(size = 25))
```

```{r, fig.align = "center", out.width = "3500px", fig.width = 25, fig.height = 12}
continentStreams 
```

Why is streaming so much higher in North America compared to Europe, even though Spotify was founded in Sweden, and released first in the United Kingdom?

Distribution of Total Streams.
========================================================
Let's visualize the distribution of total streams for the last week.
```{r, include = FALSE}
streamDist <- spotify_Global %>%
  group_by(Code) %>%
  summarise(totalStreams = sum(Streams)) %>%
  arrange(desc(totalStreams))
streamDist
```

```{r}
streamDist
```

Let's create a pie chart to view how each coninent's streaming numbers compare to the other.
```{r, include = FALSE}
Stream_circle <- ggplot(streamDist, aes("", totalStreams, fill = Code)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text = element_text(size = 25)) + theme(axis.title = element_text(size = 25)) + theme(legend.title = element_text(size = 15)) + theme(legend.text = element_text(size = 20)) + theme(legend.title = element_text(size = 15))
```
How are the total number of global streams distributed to each continent?
```{r,fig.align = "center", out.width = "2300px", fig.width = 18, fig.height = 6}
Stream_circle
```
 
Find the top 3 songs in each continent.
========================================================
```{r}
spotify_topSongs <- spotify_Global %>%
  arrange(desc(Streams)) %>%
  group_by(Code) %>% slice(1:3)
```

```{r, include = FALSE}
eachCountry <- ggplot(spotify_topSongs, aes(Code, Streams, fill = Track)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text = element_text(size = 20)) + theme(axis.title = element_text(size = 25)) + theme(legend.text = element_text(size = 16)) + theme(legend.title = element_text(size = 11)) + labs(title = "Top Songs in each Continent") + theme(plot.title = element_text(size = 35)) +
  theme(legend.position = "bottom")
```

```{r, fig.align = "center", out.width = "3900px", fig.width = 20, fig.height = 10}
eachCountry
```

As we can see, there's a lot of crossover when it comes to artists and their international audiences. For example, "bad guy" by Billie Eilish is in the top 3 in streaming in the United States, Australia, and Europe.

Finding and Analyzing Aggregate Data
========================================================
Now that we have looked at streaming information for each continent closely, let's look at our overall Global information.
```{r, include = FALSE}
spotify_globalSongs <- spotify_Global %>%
  group_by(Track, Artist) %>%
  summarise(Streams = sum(Streams)) %>%
  arrange(desc(Streams))
```

```{r}
spotify_globalSongs
```

  <br />

Lets create a graphic representation of this data frame above. Graphing the overall top 200 songs streamed on the Spotify website. 


Finding and Analyzing Aggregate Data
========================================================
```{r, include = FALSE}
popularSongs <- ggplot(head(spotify_globalSongs, 10), aes(Track, Streams, fill = Artist)) + geom_bar(stat="identity") + scale_fill_brewer(palette="Blues") + labs(title = "The World's Most Popular Songs") + theme(axis.title = element_text(size = 17)) + theme(axis.text = element_text(size = 20)) + theme(legend.text = element_text(size = 15)) + theme(legend.title = element_text(size = 15)) + theme(plot.title = element_text(size = 30)) + theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.title.x = element_text(size = 20)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.1))
```

```{r, fig.align = "center", fig.width = 23, fig.height = 16, out.width = '2500px'}
popularSongs
```

The 'spotifyr' Package
========================================================
Another method I would like to integrate into my project is the use of the Spotify Developer Tools Web API. The method to retrieving an API key is free, and simple once an account is made.

```{r}
library(spotifyr)
```

The spotifyr package pulls a variety of audio features from Spotify's Web Api. Once we obtain the web key, and authorization we can retrieve a variety of information in seconds.

```{r}
spotify_client_id <- source("/Users/chris/Documents/R/api-keysSpotify.R")
```

```{r}
Sys.setenv(SPOTIFY_CLIENT_ID = api.key.spotify)
Sys.setenv(SPOTIFY_CLIENT_SECRET = api.spotify.clientID)
access_token <- get_spotify_access_token()
```

Now let's go back to the data that contained the most streamed songs globally.
```{r}
head(spotify_globalSongs)
```

Lets take the top 5 artists on this list and find out if there's something that in their music that make their songs the most popular in the world.

Find the audio track information for each song in the top 50.
========================================================
We will first take the top 50 most streamed songs in the world.
```{r, include = FALSE}
spotify_globalArtists <- spotify_Global %>%
  group_by(Artist) %>%
  summarise(Streams = sum(Streams)) %>%
  arrange(desc(Streams))
head(spotify_globalArtists)
spotifyTop50 <- head(spotify_globalArtists$Artist, 50)
```

Import our data using the 'search_spotify' function to retrieve more detailed information for each track.
```{r}
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

Find the audio track freatures for each song in the top 50.
========================================================
```{r}
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

Is there any relation between song popularity and characteristics?
========================================================
I want to create a linear model that might be able to find any strong correlation between these key characteristics and how these musical tracks will be received by the general public.
```{r}
topSongs.lm <- lm(formula = popularity ~ acousticness + liveness + energy + valence + loudness + tempo, data = trackInformation)
summary(topSongs.lm)
```

Based on the information I've presented. There really isn't a conclusion that can be properly drawn. There doesn't seem to be any correlation between the popularity of any given song, and its features. 

Graphing Representation
========================================================
```{r}
trackInfo <- gather(trackInformation, 'danceability':'tempo', key = 'characteristic', value = 'value')
```

```{r, out.width = "1500px"}
ggplot(trackInfo, aes(value, popularity)) + geom_point() + facet_wrap(~characteristic, ncol = 5, scales = "free_x")
```

The top 10 most streamed artists in the world.
========================================================
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

```{r, fig.align = "center", out.width = "2000px", fig.width = 25}
mostStreamed
```

A Successful Discography
========================================================
While an artist might have a massive amount of streams, this doesn't mean they have a successful music career overall. Having multiple songs in the top 200 could be seen as higher benchmark for success.

Counting how many songs an artist has in the global top 200.
```{r, include = FALSE}
spotify_globalAppearances <- spotify_globalSongs %>%
  group_by(Artist) %>%
  count(Artist, sort = TRUE )
```

```{r}
spotify_globalAppearances
```

Create a Word Cloud
========================================================
```{r, fig.align = "center", fig.asp =0.62, out.width = "1100px"}
spotify_globalAppearances %>%
with(wordcloud(words = Artist, n, max.words = 200, random.order = FALSE, shape = "circle", colors = brewer.pal(8, "Dark2")))
```

Take note that Lil Nas X the creator of "Old Town Road", is not even in the top 10 for amount of songs he has in the top 200. Even though he has dominated the streaming numbers. We call this type of phenomenon One-Hit Wonder.


Music Characteristics for the Most Popular Artist
========================================================
Since we couldn't figure the characteristics that create a popular song, then let's find what creates a popular artist.

Using the information we've received from our wordCloud let's how our artists use their music to capture their listeners.

  <br />
<font size = "10"> Grabbing data on our Artists</font>

Let's use the 'spotifyr' package once again to retrieve information on discography for some of the more popular artists.

```{r, include = FALSE}
getArtist_Information <- function(artist) {
  get_artist_audio_features(artist) %>%
  select(c(1,4,6,9:19,22,26,30,32,33,36:39))
}
```
For example:
```{r}
Billie_Eilish <- getArtist_Information('billie eilish')
head(Billie_Eilish, 3)
```

Graph on Billie Eilish (North American - Pop)
========================================================
```{r, include = FALSE}
getEnergy_graph <- function(df) {
  ggplot(df, aes(valence, energy, color = album_name)) +
  geom_point(size = 5) +
  lims(x=c(0,1.0),y=c(0,1.0)) +
  theme_minimal() + theme(axis.title = element_text(size = 25)) + theme(axis.text = element_text(size = 25)) + theme(legend.text = element_text(size = 17)) + theme(legend.title = element_text(size = 22)) + theme(plot.title = element_text(size = 30)) + theme(axis.title.y = element_text(size = 25)) + 
  geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
  annotate("text", x = c(0.20, 0.09, 0.90, 0.82), y = c(0.0, 1.0, 0.0, 1.0), label = c("Sad/Depressing", "Dark/Angry", "Calm/Peaceful", "Happy/Energetic"), size = 10.0) + theme(legend.position = "bottom")}
```

```{r, fig.align = "center", out.width = "1500px", fig.width = 22, fig.height = 13} 
BillieEilish_Energy <- getEnergy_graph(Billie_Eilish)
BillieEilish_Energy
```
Let's create a graph that will compare the energy (intensity) and valence (emotion) for each of our artists.

Graph on PNL (European - French Rap)
========================================================
```{r, include = FALSE}
PNL <- getArtist_Information('pnl')
head(PNL, 5)
```

```{r, fig.align = "center", out.width = "1500px", fig.width = 22, fig.height = 13} 
PNL_Energy <- getEnergy_graph(PNL)
PNL_Energy
```

Graph on SchoolBoy Q (North American - Rap/Hip-Hop)
========================================================
```{r, include = FALSE}
SchoolBoy_Q <- getArtist_Information('schoolboy q')
head(SchoolBoy_Q, 5)
```

```{r, fig.align = "center", out.width = "1500px", fig.width = 22, fig.height = 13} 
SchoolBoy_Energy <- getEnergy_graph(SchoolBoy_Q)
SchoolBoy_Energy
```

Graph on Sebastian Yatra (South American - Latin/Reggaeton)
========================================================
```{r, include = FALSE}
Sebastian_Yatra <- getArtist_Information('sebastian yatra')
head(Sebastian_Yatra, 5)
```

```{r, fig.align = "center", out.width = "1500px", fig.width = 22, fig.height = 13} 
Sebastian_Energy <- getEnergy_graph(Sebastian_Yatra)
Sebastian_Energy
```

Graph on Post Malone (American - Hip Hop/Pop)
========================================================
```{r, include = FALSE}
Post_Malone <- getArtist_Information('post malone')
head(Post_Malone, 5)
```

```{r, fig.align = "center", out.width = "1500px", fig.width = 22, fig.height = 13} 
PostMalone_Energy <- getEnergy_graph(Post_Malone)
PostMalone_Energy
```

Graph on on Khalid (American - R&B)
========================================================
```{r, include = FALSE}
Khalid <- getArtist_Information('khalid')
head(Khalid, 5)
```

```{r, fig.align = "center", out.width = "1500px", fig.width = 22, fig.height = 13} 
Khalid_Energy <- getEnergy_graph(Khalid)
Khalid_Energy
```

Concluding Thoughts
========================================================
We can somewhat make a conclusion that some of the most popular music in this day and age, are low intensity sounds, with very dark material.

However the biggest point I want to make is:
Music tastes aren't objective.

A lot of our enjoyment in music comes from our socioeconomic backgrounds, how our environment has influenced us, and what's readily available for us to listen to.