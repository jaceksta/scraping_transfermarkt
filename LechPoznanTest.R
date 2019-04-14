require(rvest)
require(tidyverse)
require(stringr)

page <- "https://www.transfermarkt.pl/lech-pozna%C5%84/leistungsdaten/verein/238/reldata/PL1%262018/plus/1"

scraped_page <- read_html(page)

scrape_names <- function(x){
  return(html_nodes(scraped_page,".hide-for-small .spielprofil_tooltip") %>% 
           html_text() %>% 
           as.character())
}

scrape_age <- function(x,y) {
  z <- c()
  even <- x %>%
    html_nodes(".even") %>%
    html_nodes("td:nth-child(3)") %>%
    html_text() %>%
    as.integer()
  
  odd <- x %>%
    html_nodes(".odd") %>%
    html_nodes("td:nth-child(3)") %>%
    html_text() %>%
    as.integer()
  
  w <- length(y) %/% 2
  for(i in 1:17){
    z <- c(z, odd[i])
    z <- c(z, even[i])
    
  }
  
  return(z)
}

scrape_minutes <- function(x){
  minutes <- x %>%
    html_nodes(".rechts")%>%
    html_text()%>%
    as.character()
  
  minutes <- minutes[2:length(minutes)]
  minutes <- str_replace_all(minutes, "'", "")
  minutes <- str_replace_all(minutes, "[.]", "")
  
  return(as.integer(minutes))
  
}

PlayerNames <- scrape_names(scraped_page)

PlayerAge <- scrape_age(scraped_page, PlayerNames)

PlayerMinutes <- scrape_minutes(scraped_page)

LechPoznan <- data_frame(PlayerNames, PlayerAge, PlayerMinutes)
