# Scraping RT's articles to create a corpus
# from https://www.rt.com/
# Process memos: https://docs.google.com/document/d/1kWMcIDzO1LKgzjhkboyfpuRLEhlPiQ69DU3C9Fpjt5k/edit

# Started Feb. 12, 2019
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
rt_url <- 'https://www.rt.com/op-ed/449734-'
#op-ed/449734-

# Function to test html node existence ----------------------------------------------
html_exists <- function(url, element) {
  if(length(html_nodes(url, element))){
    value <- TRUE
  } else {
    value <- FALSE
  }
  
  return(value)
}
# Test the function
#html_exists(read_html('https://www.rt.com/usa/449757-'), '.blog-autor__summary_article') # Should return FALSE
#html_exists(read_html('https://www.rt.com/op-ed/449734-'), '.blog-autor__summary_article') # Should return TRUE

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
  # KI: Pulls entire bio instead of just name
  if (html_exists(working_html, '.blog-autor__summary_article')) {
    author <- working_html %>%
      html_nodes('.blog-autor__summary_article') %>%
      html_text()
  } else {
    author <- "NA"
  }
  
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
  if (html_exists(working_html, '.date_article-header:nth-child(2)')) {
    edited_date <- working_html %>%
      html_nodes('.date_article-header:nth-child(2)') %>%
      html_text() %>%
      str_replace("Edited time: ", "") %>%
      str_replace("(\\s\\d{2}\\:\\d{2})", "")
  } else {
    edited_date <- "NA"
  }
  
  # Time edited (if applicable)
  # .date_article-header:nth-child(2)
  if (html_exists(working_html, '.date_article-header:nth-child(2)')) {
    edited_time <- working_html %>%
      html_nodes('.date_article-header:nth-child(2)') %>%
      html_text() %>%
      str_replace("Edited time: ", "") %>%
      str_extract("(\\d{2}\\:\\d{2})")
  } else {
    edited_time <- "NA"
  }
  
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

  # Embedded tweets
  # .EmbeddedTweet-tweet OR .rtcode (?)
  
  # Scrape unique ID# of article
  id <- url %>%
    str_extract("(?<=\\/)([^\\/\\/]\\d*)(?=\\-)")
  
  
  # Store scraped data in a tibble
  article_content <- tibble(
    "ID" = id, 
    "Title" = title, 
    "Author" = author,
    "Date_Published" = pub_date, 
    "Time_Published" = pub_time, 
    "Date_Edited" = edited_date,
    "Time_Edited" = edited_time,
    "Summary_Text" = lede, 
    "Article_Text" = text
    )
  
  # Quick tester
  #article_content <- tibble("Test" = id)
  
  # Number the rows
  article_content <- article_content %>%
    mutate(Row_Num = row_number()) %>%
    select(Row_Num, everything())
  
  return(article_content)
}

# Function to access each web page and combine all articles into a corpus
# lapply?

# Implementation ----------------------------------------------------------

# Scrape the content
View(get_article_content(rt_url))
str(get_article_content(rt_url))
write_csv(get_article_content(rt_url), "data/rt_content.csv") # Save to csv file
