for (i in 1:16){
  page <- as.character(test$addresses[i])
  scraped_page <- read_html(page)
  
  PlayerNames <- scrape_names(scraped_page)
  PlayerAge <- scrape_age(scraped_page, PlayerNames)
  PlayerMinutes <- suppressWarnings(scrape_minutes(scraped_page))
  
  assign(as.character(test$clubnames[i]), data_frame(PlayerNames, PlayerAge, PlayerMinutes))
}


ggplot(`gornik-zabrze`)+
  geom_bar(mapping = aes(x=PlayerAge, y=PlayerMinutes), stat="sum")


