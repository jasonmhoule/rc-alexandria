library(RSelenium)
library(rvest)
library(tidyverse)
remDr <- remoteDriver(remoteServerAddr = "selenium", port = 4444L, browserName = "chrome")
remDr$open()

# remDr$navigate("http://www.r-project.org")
# remDr$screenshot(display = TRUE)

remDr$navigate("http://www.classical-homeschooling.org/celoop/1000-primary.html")
remDr$screenshot(display = TRUE)

page <- read_html(remDr$getPageSource()[[1]])

headers <- page %>% 
  html_nodes("h1") %>% 
  html_text()

lists_raw <- page %>% 
  html_nodes(".list") %>% 
  html_text()

lists_raw <- page %>% 
  html_nodes(".list") %>% 
  as.character() %>% 
  str_replace_all("<br>","\n") %>% 
  str_replace_all("<.*?>","") %>% 
  str_replace_all("[\n]{1,}","\n") %>% 
  str_replace_all("^\n","") %>% 
  str_split("\n")

lists <- lists_raw[c(1,3:8)]  

lists[[1]]



