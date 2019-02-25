library(rvest)
library(tidyverse)
library(tidyRSS)

# Links -----------------------
# https://gist.github.com/abelsonlive/3769469 #best practices using ldply


# Define variables --------------------------------------------------------

# Overview RT's RSS feed
View(tidyfeed("https://www.rt.com/rss/usa/"))
# Define the page to scrape
rt_url <- 'https://www.rt.com/usa/449757-'

# Scraper code ------------------------------------------------------------

# First, check whether '.EmbeddedTweet-tweet' exists at all
# if(html_exists(working_html, '.EmbeddedTweet-tweet')) {
#   # If it does, 
#   text <- working_html %>%
#     html_nodes('.article__text p') %>%
#     #if(text contains '.EmbeddedTweet-tweet') {
#       #clean up / finish storing in 'text'
#       html_text() %>%
#       str_replace_all("\\n", "")
#       #and flag type as tweet
#       #is_tweet <- TRUE
#     #} else {
#       text <- working_html %>%
#         html_nodes('.article__text p') %>%
#         html_text() %>%
#         str_replace_all("\\n", "")
#       is_tweet <- FALSE
#     # }
# } else { # Else if '.EmbeddedTweet-tweet' doesn't exist, store as normal
#   text <- working_html %>%
#     html_nodes('.article__text p') %>%
#     html_text() %>%
#     str_replace_all("\\n", "")
#   # flag type as not tweet
#   is_tweet <- FALSE
# }

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
test_fun <- function(url) {
  working_html <- read_html(url)
  
  # Author (if applicable)
  # .blog-autor__summary_article
  if (html_exists(working_html, '.blog-autor__cover')) {
    author <- working_html %>%
      html_nodes('.blog-autor__cover') %>%
      str_extract("(?<=(authors\\/)).*?(?=\\/\\\")") %>%
      str_replace("-", " ")
  } else {
    author <- "NA"
  }
  
  article_content <- tibble("Test" = author)
  
  return(article_content)
  
}
# Test the function
View(test_fun('https://www.rt.com/op-ed/449734-'))
View(test_fun('https://www.rt.com/usa/449757-'))

##############################
##############################
##############################

# Compile a list of URLs, test them for 404, store in df
compile_urls <- function(partial_url, num = 100) {
  
  # Initialize a df
  urls <- tibble()
  
  # Compile a list of URLs to test
  for(i in 1:num) {
    urls[i,"Page_URL"] <- paste0(partial_url, i, "-")
  }
  
  # Test urls for 404 errors
  for(i in 1:num) {
    tryCatch(read_html(urls[i, "Page_URL"]))
  }
  # Or maybe this should go in the actual implementation of the scraper, per https://gist.github.com/abelsonlive/3769469
  
  # Return the final df of useable links
  return(urls)
}
# Test the function
View(compile_urls("https://www.rt.com/news/", 55))



# for(i in 0:5089) {
#   webpage <- read_html(paste0("https://bra.areacodebase.com/number_type/M?page=", i))
#   data <- webpage %>%
#     html_nodes("table") %>%
#     .[[1]] %>% 
#     html_table()
# }



# Either:
# A) Build list of URLs, then loop through them (feed to lapply?)
# B) afdhsjkfhsjklhjkladfs
