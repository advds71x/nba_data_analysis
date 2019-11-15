library(rvest)

game_url = "https://www.basketball-reference.com/leagues/NBA_2019_games-november.html"
game_stats <- game_url %>% 
  read_html %>%
  html_table() %>%
  .[[1]]

