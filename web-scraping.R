# Exploring web scraping using R
# Started Jan. 31, 2019
# By Roxanne Ready

#install.packages("rvest")
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
