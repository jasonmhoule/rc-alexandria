library(RSelenium)
library(rvest)
library(tidyverse)
remDr <- remoteDriver(remoteServerAddr = "selenium", port = 4444L, browserName = "chrome")
remDr$open()

# remDr$navigate("https://www.rclreads.org/")
# remDr$screenshot(display = TRUE)

search_item <- function(searchtext, remDr, screenshot = FALSE) {
  
  # Searches RC for search text, returns the results page
  url_base <- "https://rclreads.bibliocommons.com/v2/search?searchType=smart&query="
  ctext <- str_replace_all(searchtext, " ", "+")
  url <- paste0(url_base, ctext)
  
  remDr$navigate(url)
  page <- read_html(remDr$getPageSource()[[1]])
  
  if(screenshot) {
    remDr$screenshot(display = TRUE)
  }
  
  return(page)
}

get_result_tree <- function(page) {
  
  # Take a page and break down its results, returning contents in a nested list
  
  results <- page %>% 
    html_nodes(".cp-search-result-item-content")
  
  n <- length(results)
  result_tree <- vector("list", n)
  
  for(i in seq_len(n)) {
    
    result <- results[[i]]
    
    result_branch <- list()
    
    result_branch$title <- result %>% 
      html_nodes(".title-content") %>% 
      html_text()
      
    try(
    result_branch$author <- result %>% 
      html_node(".cp-by-author-block") %>% 
      html_node(".author-link") %>%
      html_text(),
    silent = TRUE
    )
    
    mitems <- result %>% 
      html_nodes(".manifestation-item")
    
    nn <- length(mitems)
    instances <- vector("list", nn)
    
    for(j in seq_len(nn)) {
      
      mitem <- mitems[[j]]
      
      inst <- list()
      
      inst$format <- mitem %>% 
        html_node(".cp-format-indicator") %>% 
        html_text()
      
      inst$pubdate <- mitem %>% 
        html_node(".cp-publication-date") %>% 
        html_text() %>% 
        str_extract("\\d{1,4}")
      
      inst$link <- mitem %>% 
        html_node(".manifestation-item-format-info-wrap") %>% 
        html_node("a") %>% 
        html_attr("href")
      
      instances[[j]] <- inst
    }
    
    result_branch$instances <- instances
    
    result_tree[[i]] <- result_branch
  }
  
  return(result_tree)
}

page <- search_item("dr. seuss green eggs and ham", remDr, TRUE)

get <- get_result_tree(page)
