# Scraping the Maryland General Assembly's bill list pages
# on http://mgaleg.maryland.gov

# Started Feb. 5, 2019
# By Roxanne Ready

#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("tidyRSS")
library(rvest)
library(tidyverse)
library(tidyRSS)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtrn:       run code line/block
# cmd-opt-e:      run code until EoF
# cmd-shft-r:     insert section break

# \


# Define variables --------------------------------------------------------

# Overview RT's RSS feed
View(tidyfeed("https://www.rt.com/rss/usa/"))
# Define the page to scrape
rt_url <- 'https://www.rt.com/usa/449757-'

# Scraper code ------------------------------------------------------------

# Function to get and store article data
get_article_content <- function(url) {
  
  working_html <- read_html(url)
  
  # Article title
  title <- working_html %>%
    html_nodes('.article__heading') %>%
    html_text() %>%
    str_replace_all("\\n", "") 
  
  # Author (if applicable)
  # .blog-autor__summary_article
  # if (!is.null(html_nodes(working_html, '.blog-autor__summary_article'))) {
  #    author <- working_html %>%
  #      html_nodes('.blog-autor__summary_article') %>%
  #      html_text()
  
  # Publication date
  # .date_article-header:nth-child(1)
  pub_date <- working_html %>%
    html_nodes('.date_article-header:nth-child(1)') %>%
    html_text() %>%
    str_replace("Published time: ", "") %>%
    str_replace("(\\s\\d{2}\\:\\d{2})", "")
  
  # Publication time
  pub_time <- working_html %>%
    html_nodes('.date_article-header:nth-child(1)') %>%
    html_text() %>%
    str_replace("Published time: ", "") %>%
    str_extract("(\\d{2}\\:\\d{2})")
  
  # Date edited (if applicable)
  # .date_article-header:nth-child(2)
  
  # Time edited (if applicable)
  # .date_article-header:nth-child(2)
  
  # Summary/lede
  lede <- working_html %>%
    html_nodes('.summary') %>%
    html_text()
  
  # Article text (by paragraph)
  # KI: pulls embedded tweets (exclude .rtcode content)
  # KI: stores empty p's in cells
  text <- working_html %>%
    html_nodes('.article__text p') %>%
    html_text() %>%
    str_replace_all("\\n", "")
  
  # Article media links
  
  # Article captions
  # .media__title_arcticle span
  
  # Embedded tweets
  # .EmbeddedTweet-tweet OR .rtcode (?)
  
  # Store scraped data in a tibble
  article_content <- tibble("Title" = title, "Date_Published" = pub_date, "Time_Published" = pub_time, "Summary_Text" = lede, "Article_Text" = text)
  #article_content <- tibble("Title" = title)
  
  return(article_content)
}

# Function to access each web page and combine all articles into a corpus
# lapply?

# Implementation ----------------------------------------------------------

# Scrape the content
View(get_article_content(rt_url))
str(get_article_content(rt_url))
