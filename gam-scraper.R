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

# GAM Scraper -------------------------------------------------------------

# Define the pages to scrape
jackson_leg <- 'http://mgaleg.maryland.gov/webmga/frmMain.aspx?stab=02&pid=sponpage&id=jackson01&tab=subject6&ys=2019RS'
pg_county_del_leg <- 'http://mgaleg.maryland.gov/webmga/frmMain.aspx?pid=sponpage&tab=subject3&id=pgcodel&stab=02&ys=2019RS'

# Choose the current page
url <- jackson_leg

# Function to scrape bill numbers and parse them into HB/SB
get_bill_numbers <- function(url) {
  bill_number <- read_html(url) %>%
    html_nodes('.grid td:nth-child(1)') %>%
    html_text() %>%
    str_replace("\\(", "") %>%
    str_replace("\\)", "") %>%
    str_replace("\\s", "") %>%
    str_replace("\\t", "") %>%
    str_replace_all("\\s+", " ")
  
  bills <- tibble('HB_Num', 'SB_Num')
  
  for (i in seq_along(bill_number)) {
    bills[i,1] <- str_extract(bill_number[i], "\\bHB\\w+")
    bills[i,2] <- str_extract(bill_number[i], "\\bSB\\w+")
  }
  
  return(bills)
}
#View(get_bill_numbers(url))


# Function to scrape all bill info and store it in a tibble
get_bills <- function(url) {
  
  working_html <- read_html(url)
  
  # Scrape bill numbers
  bill_number <- working_html %>%
    html_nodes('.grid td:nth-child(1)') %>%
    html_text() %>%
    str_replace("\\(", "") %>%
    str_replace("\\)", "") %>%
    str_replace("\\s", "") %>%
    str_replace("\\t", "") %>%
    str_replace_all("\\s+", " ")
  #Need to break these up into HB and SB as above
  
  # Scrape bill titles
  bill_title <- working_html %>%
    html_nodes('.grid td:nth-child(2)') %>%
    html_text()
  
  # Scrape bill status
  bill_status <- working_html %>%
    html_nodes('.grid td:nth-child(3)') %>%
    html_text()
  
  # Scrape originating committee
  orig_committee <- working_html %>%
    html_nodes('.grid td:nth-child(4)') %>%
    str_extract("(\\<a.*?\\>).*?(\\<\\/a.*?\\>)") %>% # Everything between <a and </a>, inlclusive
    str_extract("(?<=\\>)([^\\<\\>]*)(?=\\<)") # Capture text bewteen > and <, non-inclusive
  
  # Scrape sponsor type
  # [code here]
  
  # Combine all scraped data into a df (tibble)
  # Would be nice to include the date the status was scraped as part of the Status col name
  bill_info <- tibble("Number" = bill_number, "Title" = bill_title, "Status" = bill_status, "Originating_Committee" = orig_committee)
  
  return(bill_info)
}
View(get_bills(url))
