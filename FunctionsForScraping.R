require(rvest)
require(tidyverse)
require(stringr)


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
  
  w <- (length(y) + 1) %/% 2
  for(i in 1:w){
    if (!is.na(odd[i]) & !is.na(even[i])){
      z <- c(z, odd[i])
      z <- c(z, even[i])
    } else if (is.na(odd[i])){
      z <- c(z,even[i])
    } else {
      z <- c(z,odd[i])
    }
    
    
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
