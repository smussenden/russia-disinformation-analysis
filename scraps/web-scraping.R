# Exploring web scraping using R
# Started Jan. 31, 2019
# By Roxanne Ready

#install.packages("rvest")
#install.packages("tidyverse")
library(rvest)
library(tidyverse)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtrn:       run code line/block
# cmd-opt-e:      run code until EoF
# cmd-shft-r:     insert section break

# \

# Scraper code from KDNuggets.com  ------------------------------------------------------------------
# https://www.kdnuggets.com/2018/01/primer-web-scraping-r.html

# Specify the url for desired website to be scrapped
url <- 'https://www.spj.org/ethicscode.asp'

# Store the HTML code from the website
webpage <- read_html(url)
webpage_text <- html_text(webpage)

# View selector gadget documentation
# vignette("selectorgadget")

# Use the CSS selector to select the SPJ's ethics rules
rules_html <- html_nodes(webpage,'span.headline3')  

# Convert the data to text
guidelines <- html_text(rules_html)


# Scraping, tidified ----------------------------------------------------------------

# Shortened
rules_html <- html_nodes(read_html('https://www.spj.org/ethicscode.asp'),'span.headline3') 

# Tidied using the pipe
guidelines <- read_html('https://www.spj.org/ethicscode.asp') %>%
  html_nodes('span.headline3') %>%
  html_text() %>%
  tibble() %>%
  rename(Rules = ".")
guidelines <- guidelines[-1,] # Remove the first row, which isn't one of the rules


# Faffing about -----------------------------------------------------------

# Import data mentioning Trump from 2015 through 2016, dowloaded from io-archive.org
russia <- read_csv("/Volumes/Passport/UMD/natsec/download_csv.csv")

# Count how many posts each user made
russia %>%
  group_by(name) %>%
  #summarize(n = n())
  count(name, sort = TRUE)

#Count the number of likes each user received on all posts
russia %>%
  group_by(name) %>%
  summarize(likes = sum(like_count)) %>%
  arrange(desc(likes))
  

# Scraping NYT’s social media policy --------------------------------------

.StoryBodyCompanionColumn

nyt_social_guidelines <- read_html('https://www.nytimes.com/2017/10/13/reader-center/social-media-guidelines.html') %>%
  html_nodes('.StoryBodyCompanionColumn') %>%
  html_text() %>%
  tibble() %>%
  rename(Content = ".")

#setwd("/Volumes/Passport/Programming-Lessons/r-for-data-science-lessons")
nyt_social_guidelines %>%
  write.csv("/Volumes/Passport/UMD/thesis/nyt_social_guidelines.csv")


# Lesson from Analytics Vidhya -----------------------------------------------------------------
# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/

#Specifying the url for desired website to be scraped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have a look at the rankings
head(rank_data)

#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#Using CSS selectors to scrap the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Data-Preprocessing: removing '\n'
description_data<-gsub("\n","",description_data)

#Let's have a look at the description data
head(description_data)

#Using CSS selectors to scrap the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

#Data-Preprocessing: removing mins and converting it to numerical

runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Let's have a look at the runtime data
head(runtime_data)


#Using CSS selectors to scrap the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces
genre_data<-gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Let's have a look at the genre data
head(genre_data)

#Combining all the lists to form a data frame
movies_df<-tibble(Rank = rank_data, Title = title_data,
                      Description = description_data, Runtime = runtime_data,
                      Genre = genre_data)

str(movies_df)

# Workaround check for missing info in a given selector
html_checknode <- function(checknode) {
  if (length(checknode)==0) return(NA)
  return(html_text(checknode))
}

# Function-based scraper -----------------------------------------------------------------
# From comments on above tutorial 

#Specifying the url for desired website to be scrapped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature&#039;'

#Reading the HTML code from the website
webpage <- read_html(url)

item_content <- html_nodes(webpage, '.lister-item-content')

# Function, nonfunctional but a good place to start
get_nth_film <- function(item) {
  description_data_html <- html_nodes(item, '.ratings-bar+ .text-muted')
  description_data %>% gsub("\n","",.)

  runtime_data_html <- html_nodes(item, '.text-muted .runtime')
  runtime_data %>% gsub("min","",.) %>% as.numeric()

  genre_data_html <- html_nodes(item, '.text-muted .genre')
  genre_data %>% gsub("\n|,.*", "", .) %>% as.factor()
  
  ratings_data_html <- html_nodes(item, '.ratings-imdb-rating strong')
  ratings_data %>% as.numeric()
  
  metascore_data_html <- html_nodes(item, '.metascore')
  metascore_data %>% as.numeric()
  
  votes_data_html <- html_nodes(item, '.sort-num_votes-visible span:nth-child(2)')
  votes_data %>% gsub(",","",.) %>% as.numeric()
  
  director_data_html <- html_nodes(item, '.text-muted+ p a:nth-child(1)')
  director_data %>% as.factor()
  
  star_data_html <- html_nodes(item, '.ghost+ a')
  star_data %>% as.factor()
  
  df <- data.frame(description_data, runtime_data, genre_data, ratings_data, metascore_data, votes_data, director_data, star_data)
  return(df)
}

html_checknode <- function(checknode) {
  if (length(checknode)==0) return(NA)
  return(html_text(checknode))
}

movies_df <- do.call(rbind, lapply(item_content, get_nth_film))


# Tidy structure from Rstudio blog ----------------------------------------
# https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/

library(rvest)
lego_movie <- html("http://www.imdb.com/title/tt1490017/")

lego_movie %>%
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()

lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

lego_movie %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()

# Extract the tag names with html_tag(), text with html_text(), a single attribute with html_attr() or all attributes with html_attrs().
# Detect and repair text encoding problems with guess_encoding() and repair_encoding().
# Navigate around a website as if you’re in a browser with html_session(), jump_to(), follow_link(), back(), and forward(). Extract, modify and submit forms with html_form(), set_values() and submit_form().


# From Towards Data Science ---------------------------------------------------------------------
# https://towardsdatascience.com/learn-to-create-your-own-datasets-web-scraping-in-r-f934a31748a5

#Identify the url from where you want to extract data
base_url <- "https://www.billboard.com/charts/greatest-of-all-time-pop-songs-artists"
webpage <- map(base_url, read_html)

# purrr::map(allcollegeurls$url, read_html)

# Get the artist name
artist <- html_nodes(webpage, ".chart-row__artist")
artist <- as.character(html_text(artist))

# Get the artist rank
rank <- html_nodes(webpage, ".chart-row__rank")
rank <- as.numeric(html_text(rank))

# Save it to a tibble
top_artists <- tibble('Artist' = gsub("\n", "", artist),   #remove the \n character in the artist's name
                      'Rank' = rank) %>%
  filter(rank <= 10)

#Format the link to navigate to the artists genius webpage
genius_urls <- paste0("https://genius.com/artists/",top_artists$Artist)

#Initialize a tibble to store the results
artist_lyrics <- tibble()

# Outer loop to get the song links for each artist 
for (i in 1:10) {
  genius_page <- map(genius_urls[i], read_html)
  song_links <- html_nodes(genius_page, ".mini_card_grid-song a") %>%
    html_attr("href") 
  
  #Inner loop to get the Song Name and Lyrics from the Song Link    
  for (j in 1:10) {
    
    # Get lyrics
    lyrics_scraped <- map(song_links[j], read_html) %>%
      html_nodes("div.lyrics p") %>%
      html_text()
    
    # Get song name
    song_name <- map(song_links[j], read_html) %>%
      html_nodes("h1.header_with_cover_art-primary_info-title") %>%
      html_text()
    
    # Save the details to a tibble
    artist_lyrics <- rbind(artist_lyrics, tibble(Rank = top_artists$Rank[i],
                                                 Artist = top_artists$Artist[i],
                                                 Song = song_name,
                                                 Lyrics = lyrics_scraped ))
    
    # Insert a time lag - to prevent me from getting booted from the site :)
    Sys.sleep(10)
  }
} 

#Inspect the results
artist_lyrics
