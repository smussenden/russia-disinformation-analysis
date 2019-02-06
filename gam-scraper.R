# Exploring web scraping using R
# Started Jan. 31, 2019
# By Roxanne Ready

#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("XML")
library(rvest)
library(tidyverse)
library(XML)

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
jackson_leg_url <- 'http://mgaleg.maryland.gov/webmga/frmMain.aspx?stab=02&pid=sponpage&id=jackson01&tab=subject6&ys=2019RS'
pg_county_del_leg_url <- 'http://mgaleg.maryland.gov/webmga/frmMain.aspx?pid=sponpage&tab=subject3&id=pgcodel&stab=02&ys=2019RS'

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
  
  # Break numbers up into HB and SB
  hb_num = c()
  sb_num = c()
  
  for (i in seq_along(bill_number)) {
    hb_num[i] <- str_extract(bill_number[i], "\\bHB\\w+")
    sb_num[i] <- str_extract(bill_number[i], "\\bSB\\w+")
  }
  
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
    str_extract("(?<=\\>)([^\\<\\>]*)(?=\\<)") %>% # Capture text bewteen > and <, non-inclusive
    as.factor()
  
  # Scrape sponsor type
  sponsorship_type <- working_html %>%
    html_nodes('.grid td:nth-child(6)') %>%
    html_text() %>%
    as.factor()
  
  # Include the direct link to the legislation's page
  # Currently, when writing to CSV, & is forced to &amp making this field useless for export
  bill_link_partial <- working_html %>%
    html_nodes('.grid td:nth-child(1)') %>%
    str_extract("(\\<a.*?\\>).*?(\\<\\/a.*?\\>)") %>% # Everything between <a and </a>, inlclusive
    str_extract("(?<=\\\")([^\\\"\\\"]*)(?=\\\")") # Capture text bewteen "s, non-inclusive
    
  bill_link <- str_c("http://mgaleg.maryland.gov/webmga/", bill_link_partial)
  
  # Combine all scraped data into a df (tibble)
  # Would be nice to include the date the status was scraped as part of the Status col name
  bill_info <- tibble("House_Bill_Num" = hb_num, "Senate_Bill_Num" = sb_num, "Title" = bill_title, "Status" = bill_status, "Originating_Committee" = orig_committee, "Sponsorship_Type" = sponsorship_type, "Link" = bill_link)
  
  return(bill_info)
}

jackson_legislation <- get_bills(jackson_leg_url)
pg_county_del_legislation <- get_bills(pg_county_del_leg_url)

write_csv(jackson_legislation, "jackson_legislation.csv")
write.csv(jackson_legislation, "jackson_legislation.csv", row.names = FALSE)


write_csv(pg_county_del_legislation, "pg_county_del_legislation.csv", fileEncoding = "")
