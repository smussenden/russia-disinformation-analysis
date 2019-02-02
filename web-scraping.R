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

