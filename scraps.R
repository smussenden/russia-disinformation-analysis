# Define variables --------------------------------------------------------

# Overview RT's RSS feed
View(tidyfeed("https://www.rt.com/rss/usa/"))
# Define the page to scrape
rt_url <- 'https://www.rt.com/usa/449757-'

# Scraper code ------------------------------------------------------------

# Function to scrape media content ----------------------------------------
get_article_media <- function(url) {
  
  working_html <- read_html(url)
  
  # Links to embedded media, and store media in a file
  # KI: pulls links to multiple sizes of the same image
  images <- working_html %>%
    # html_nodes('img.media__item') %>%
    html_nodes('img.media__item') %>%
    html_attr('data-src') %>%
    #replace xxs with l for larger file
    str_replace_all("xxs", "l")
  
  # Would like to DL and store. Nonfunctional.
  #download.file(images, destfile = basename(images))
  
  # Captions on media
  # .media__title_arcticle span
  captions <- working_html %>%
    html_node('.media__title_arcticle') %>%
    html_text()
  
  # Store scraped data in a tibble
  article_content <- tibble("Image_Link" = images, "Caption" = captions)
}

View(get_article_media(rt_url))
str(get_article_media(rt_url))
write_csv(get_article_media(rt_url), "rt_content.csv") # Save to csv file


# Function to scrape tweets and metadata ----------------------------------
# Code here

# Figuring out if/else ----------------------------------------------------
# Function to test existence
html_exists <- function(url, element) {
  if(length(html_nodes(url, element))){
    value <- TRUE
  } else {
    value <- FALSE
  }
  
  return(value)
}
#Test function
html_exists(read_html('https://www.rt.com/usa/449757-'), '.blog-autor__summary_article') # Should return FALSE
html_exists(read_html('https://www.rt.com/op-ed/449734-'), '.blog-autor__summary_article') # Should return TRUE


test_ifelse <- function(url) {
  
  working_html <- read_html(url)
  
  # Scrape unique ID# of article
  id <- url %>%
    str_extract("(?<=\\/)([^\\/\\/]\\d*)(?=\\-)")
  
  # Author (if applicable)
  # .blog-autor__summary_article
  if (html_exists(working_html, '.blog-autor__summary_article')) {
    author <- working_html %>%
      html_nodes('.blog-autor__summary_article') %>%
      html_text()
  } else {
    author <- "NA"
  }
  
  article_content <- tibble("Test" = id, "Author" = author)
  
  return(article_content)
}

# Function to access each web page and combine all articles into a corpus
# lapply?

# Implementation ----------------------------------------------------------

# Scrape the content
View(test_ifelse('https://www.rt.com/op-ed/449734-'))
str(test_ifelse(rt_url))


# Determine if tweet ------------------------------------------------------

is_tweet <- function(p) {
  # Code to test just for whether a given input is a tweet
}

# KI: calls everything in the dataset either is_tweet = TRUE or FALSE
categorize_tweets <- function(url) {
  working_html <- read_html(url)
  
  # Scrape unique ID# of article
  id <- url %>%
    str_extract("(?<=\\/)([^\\/\\/]\\d*)(?=\\-)")
  
  # Create and fill column to determine if the p is a tweet
  if(html_exists(working_html, '.EmbeddedTweet-tweet')) {
    text <- working_html %>%
      html_nodes('.article__text p') %>%
      html_text() %>%
      str_replace_all("\\n", "")
    is_tweet <- TRUE
  } else {
    text <- working_html %>%
      html_nodes('.article__text p') %>%
      html_text() %>%
      str_replace_all("\\n", "")
    is_tweet <- FALSE
  }
  
  article_content <- tibble("ID" = id, "Text" = text, "Is_Tweet" = is_tweet)
  
  return(article_content)
  
}
# Test the function
View(categorize_tweets('https://www.rt.com/op-ed/449734-'))
