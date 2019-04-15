require(rvest)
require(tidyverse)
require(stringr)
install.packages("splitstackshape")
require(splitstackshape)

GetAddresses <- function(){
  
  page <- "https://www.transfermarkt.com/ekstraklasa/startseite/wettbewerb/PL1"
  scraped_page <- read_html(page)
  
  info <- scraped_page %>%
    html_nodes(".responsive-table #yw1 .items .vereinprofil_tooltip")%>%
    html_attr("href")
  
  a <- c()
  i <- 1
  
  while(i < 49){
    a <- c(a, info[i])
    i <- i + 3
  }
  
  b <- as.data.frame(a)
  
  b <- concat.split(b, 1, sep = "/")
  
  addresses <- paste("https://www.transfermarkt.pl/", b$a_2, "/leistungsdaten/verein/", b$a_5, "/reldata/PL1%262018/plus/1")
  addresses <- gsub(" ", "", addresses)
  
  clubnames <- as.character(b$a_2)
  clubnames <- gsub("%C5%84", "n", clubnames)
  
  return(data.frame(clubnames, addresses))
  
}



