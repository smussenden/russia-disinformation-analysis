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
# View(tidyfeed("https://www.rt.com/rss/"))
# 
# # Define the page to scrape
rt_url <- 'https://www.rt.com/usa/449757-'
# rt_url <- 'https://www.rt.com/op-ed/449734-'

## FUNCTIONS ## -------------------------------------------------------------------------------------

# Test html node existence ----------------------------------------------

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

# Get and store article data ------------------------------------------------------------

get_article_content <- function(url) {
  
  working_html <- read_html(url)
  
  # Pull unique ID# of article
  id <- url %>%
    str_extract("(?<=\\/)([^\\/\\/]\\d*)(?=\\-)")
  
  # Article title
  title <- working_html %>%
    html_nodes('.article__heading') %>%
    html_text() %>%
    str_replace_all("\\n", "") 
  
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
  # KI: stores empty p's in cells
  text <- working_html %>%
    html_nodes('.article__text p') %>%
    html_text() %>%
    str_replace_all("\\n", "")
  
  # Article text by paragraph including HTML
  # KI: Still strips CSS flags
  raw_p <- working_html %>%
    html_nodes('.article__text p')

  # Embedded tweets
  # .EmbeddedTweet-tweet OR .rtcode (?)
  
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
    "Article_Text" = text,
    "HTML" = raw_p
    )
  
  # Quick tester
  #article_content <- tibble("Test" = id)
  
  # Number the rows
  article_content <- article_content %>%
    mutate(Graph_Num = row_number()) %>%
    select(Graph_Num, everything()) %>% # rearanges to put Graph_Num first
    mutate("Is_Tweet" = ifelse(str_detect(HTML, "dir=\"ltr\""), "Is_Tweet" <- TRUE, "Is_Tweet" <- FALSE)) %>% # flags whether native content or tweet
    select(-one_of("HTML")) # drops HTML col
  
  return(article_content)
}
# Test the function
# View(get_article_content(rt_url))
# str(get_article_content(rt_url))
# write_csv(get_article_content(rt_url), "data/rt_content.csv") # Save to csv file

# Check for valid URLs ------------------------------------------------
url_works <- function(url){
  tryCatch(
    # Checks page's status code for success (L = integer). Other status codes: https://httpstatuses.com
    identical(status_code(HEAD(url)),200L), # Returns logical based on status code.
    error = function(e){
      FALSE # Returns FALSE if an error
    })
}
# Test the function
# url_works("https://www.rt.com/news/3-") # Should return TRUE
# url_works("https://www.rt.com/news/1-") # Should return FALSE

# Compile in a data frame a list of valid URLs ------------------------------------------------
compile_urls <- function(partial_url, num = 100) {
  
  # Initialize a df
  urls <- tibble()
  
  # Compile a list of URLs to test
  for(i in 1:num) {
    test_url <- paste0(partial_url, i, "-") # Create full URl and assign it to a variable
    
    if(url_works(test_url)){ # Test if URL is valid
      urls[i,"Page_URL"] <- test_url # Add to df
    } 
  }
  
  urls <- urls[rowSums(is.na(urls)) != ncol(urls),] # Remove empty rows (aka failed URLs)
  
  # Return the final df of useable links
  return(urls)
}
# Test the function
# View(compile_urls("https://www.rt.com/news/", 20))

# Run get_article_content() on each element of a df -------------------------
loop_it <- function(url_df) {
  
  data <- get_article_content(url_df[[1,"Page_URL"]])
  for(i in 2:nrow(url_df)) {
    data <- bind_rows(data, get_article_content(url_df[[i,"Page_URL"]]))
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
  }
  return(data)
}
# Test the function
# suppressWarnings(View(loop_it(testurls)))

# IMPLEMENTATION ## ----------------------------------------------------------

# Save working news URLs to csv file
write_csv(compile_urls("https://www.rt.com/news/", 500000), "data/rt_newsURLs.csv") 

# Save working op-ed URLs to csv file
#write_csv(compile_urls("https://www.rt.com/news/", 500000), "data/rt_op-edURLs.csv") 

