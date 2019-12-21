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
    
    body <- paste0(
      "Hi Lexi :) <3, <br>
        <br>
        A new job posting has been found on the CHES website! Check it: <br>
        <ul>
          <li><b>Title:</b> ", title, "</li>
          <li><b>Location:</b> ", location, "</li>
          <li><b>Posted:</b> ", posted, "</li>
        </ul>
      <a href='https://www.nchec.org/job-postings'>Click here</a> to check out the website! <br>
      <br>
        Love you,<br>
      <br>
        Kow"
    )
    
    test_email <- gm_mime(
      To = c(
        Sys.getenv("GMAILR_MY_EMAIL"),
        Sys.getenv("GMAILR_L_EMAIL")
        ),
      From = Sys.getenv("GMAILR_MY_EMAIL"),
      Subject = "CHES: New Job Posting!") %>% 
      gm_html_body(body)
      
      gm_auth_configure(
        key = Sys.getenv("GMAILR_KK_KEY"),
        secret = Sys.getenv("GMAILR_KK_SECRET"),
        appname = "gmailr-proj"
      )
    
    gm_send_message(test_email)
    
    mn <- mn %>% 
      mutate(date_found = now()) %>% 
      bind_rows(ches_postings)
    
    write_csv(
      x = mn,
      path = "data/ches_postings.csv"
    )
  }
}