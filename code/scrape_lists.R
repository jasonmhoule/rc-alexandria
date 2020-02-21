library(RSelenium)
library(rvest)
library(tidyverse)
remDr <- remoteDriver(remoteServerAddr = "selenium", port = 4444L, browserName = "chrome")
remDr$open()

# remDr$navigate("http://www.r-project.org")
# remDr$screenshot(display = TRUE)

get_list <- function(url, remDr) {
  
  # Navigate and read page html
  remDr$navigate(url)
  remDr$screenshot(display = TRUE)
  
  page <- read_html(remDr$getPageSource()[[1]])
  
  # Extract lists of elements
  headers <- page %>% 
    html_nodes("h1") %>% 
    html_text()
  
  lists <- page %>% 
    html_nodes(".list") %>% 
    as.character() %>% 
    str_replace_all("<br>","\n") %>% 
    str_replace_all("<.*?>","") %>% 
    str_replace_all("[\n]{1,}","\n") %>% 
    str_replace_all("^\n","") %>% 
    str_split("\n")
  
  if(grepl("primary", url)) {
    lists <- lists[-2]  
  }
  
  # Build a tibble
  sct <- length(headers)
  tb_1 <- map_dfr(seq_len(sct), ~ tibble(x = lists[[.x]], y = headers[[.x]]))
  tb_2 <- tb_1 %>% 
    mutate(ra = grepl("RA ?$", x)) %>% 
    mutate(x = str_replace_all(x, "&amp;", "and") %>% str_replace_all("RA ?$", "")) %>% 
    mutate(title = str_extract(x, ".*? by ") %>% str_remove("(edited)? by ") %>% str_trim(side = "both")) %>% 
    mutate(author = str_extract(x, " by .*") %>% str_remove_all(" by |,.*") %>% str_trim(side = "both")) %>% 
    mutate(tnotes = str_extract(title, "and oth.*")) %>% 
    mutate(title = str_remove(title, " and oth.*")) %>% 
    mutate(anotes = str_extract(author, "and oth.*|\\(.*\\)")) %>% 
    mutate(author = str_remove(author, " and oth.*|\\(.*\\)|publish.*")) %>% 
    rowwise() %>% 
    mutate(notes = stringi::stri_flatten(c(tnotes, anotes), ", ", na_empty = TRUE, omit_empty = TRUE)) %>% 
    select(-c(tnotes, anotes))
  
  return(tb_2)
}

urls <- c("http://www.classical-homeschooling.org/celoop/1000-primary.html",
          "http://www.classical-homeschooling.org/celoop/1000-elementary.html",
          "http://www.classical-homeschooling.org/celoop/1000-junior.html",
          "http://www.classical-homeschooling.org/celoop/1000-senior.html")

# flist <- bind_rows(tbp, tbe, tbj, tbs)
flist <- map_dfr(urls, get_list, remDr)

saveRDS(flist, "1000goodbooks.rds")
