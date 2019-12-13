library(glue)
library(gmailr)
library(rvest)
library(httr)
library(tidyverse)
library(janitor)
library(lubridate)

get_response <- GET("https://www.nchec.org/job-postings")

get_content <- content(get_response, "text")

page <- read_html(get_content)

table <- page %>% 
  html_nodes(".jobs-list") %>% 
  html_table(fill = TRUE) %>% 
  pluck(1) %>% 
  clean_names

mn <- table %>% 
  filter(str_detect(location, "MN"))

if(nrow(mn) > 0) {
  ches_postings <- read_csv("data/ches_postings.csv")
  
  check <- ches_postings %>% 
    select(-date_found)
  
  already_found <- mn %>% 
    anti_join(
      y = check,
      by = c("title", "location", "date_posted", "member_organization")
    ) %>% 
    nrow() == 0
  
  if (!already_found) {
    title <- mn %>% 
      pull(title) %>% 
      str_split("\\r\\n") %>% 
      unlist() %>% 
      str_trim() %>% 
      str_c(collapse = ", ")
    
    location <- mn %>% 
      pull(location) %>% 
      str_replace_all(" ,", ",")
    
    posted <- mn %>% 
      pull(date_posted)
    
    test_email <- gm_mime(
      To = "leximeowskowski@gmail.com",
      From = "koderkow@gmail.com",
      Subject = "CHES: New Job Posting!",
      body = glue(
        "Hi Lexi :) <3,
      
      A new job posting has been found on the CHES website! Check it:
      
      Title: {title}
      Location: {location}
      Posted: {posted}
      
      https://www.nchec.org/job-postings
      
      Love you,
      
      Kow"
      )
    )
    
    send_message(test_email)
    
    mn <- mn %>% 
      mutate(date_found = now()) %>% 
      bind_rows(ches_postings)
    
    write_csv(
      x = mn,
      path = "data/ches_postings.csv"
    )
  }
}