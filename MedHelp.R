library(dplyr)
library(rvest)
library(purrr)
library(RCurl)
library(stringr)
library(tidyr)


# Get all thread titles and thread links (129 pages in total)

page_urls <- paste0("https://www.medhelp.org/forums/Epilepsy/show/235?page=", seq_len(129))

page_htmls <- lapply(page_urls, GET)


scrape_thread_titles <- function(html){
  read_html(html) %>%
    html_nodes(".subj_title a") %>%
    html_text()
}

scrape_thread_links <- function(html){
  read_html(html) %>%
    html_nodes(".subj_title a") %>%
    html_attr("href") %>%
    paste0("https://www.medhelp.org", .)
}

thread_titles <- map(page_htmls, scrape_thread_titles) %>%
  discard(~ length(.x) == 0)

correct_n_pages <- length(thread_titles)

thread_titles <- thread_titles %>%
  flatten_chr()

thread_links <- map(page_htmls, scrape_thread_links) %>%
  `[`(seq_len(correct_n_pages)) %>%
  flatten_chr()

master_data <- tibble(thread_titles, thread_links)


# Create an empty list to store the responses
thread_htmls <- list()

for (url in thread_links) {
  response <- GET(url)
  if (http_type(response) == "text/html") {
    content <- content(response, as = "text")
    thread_htmls[[url]] <- content
  }
}


html <- thread_htmls[1]
link <- master_data$thread_links[1]

scrape_poster_ids <- function(html){
  read_html(html) %>%
    html_nodes(css = ".username a") %>%
    html_text()
}


scrape_time <- function(html){
  read_html(html) %>%
    html_nodes(css = ".username .mh_timestamp") %>%
    html_attr("datetime")
}


scrape_posts <- function(html){
  read_html(html) %>%
    html_nodes("#subject_msg , .resp_body , .comment_body") %>%
    html_text() %>%
    str_replace_all("\r|\n", "") %>%
    str_trim()
}


master_data <- master_data %>%
  mutate(
    poster_ids = map(thread_htmls, scrape_poster_ids),
    time = map(thread_htmls, scrape_time),
    posts = map(thread_htmls, scrape_posts)
  ) %>%
  unnest(cols = c(poster_ids, time, posts))

# Create a new column to distinguish "question" and "answer"

master_data <- master_data %>%
  group_by(thread_titles) %>%
  mutate(category = ifelse(row_number() == 1, "question", "answer")) %>%
  ungroup()


write.csv(master_data, "data/MedHelp.csv", row.names = F)
