library(RSelenium)
library(rvest)
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

lists <- lists_raw[c(1,3:8)]  
  
str_split(lists[[1]],"\n")
str_replace_all(lists, "\n\n", "\n") %>% 
  str_split("\n")

lists_raw2 <- page %>% 
  html_nodes(".list")

lists_raw2[[1]] %>%
  html_name()

str_replace_all(lists_raw2[[1]],"<.*?>")

## ??? ###
x <- read_xml("<root>
  <a><b>1</b> <b>2</b></a>
  <a><b>3</b></a>
</root>")

x %>% 
  xml_find_all("a") %>% 
  map_chr(. %>% xml_find_all("b") %>% xml_text() %>% paste(collapse = ", "))
